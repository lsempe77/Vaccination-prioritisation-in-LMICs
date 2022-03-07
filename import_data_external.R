date<-date_0

date <- as.Date(date, "%Y-%m-%d")

download_url <- function(url) {
  tryCatch({
    tf <- tempfile()
    code <- download.file(url, tf, mode = "wb")
    if (code != 0) {
      stop("Error downloading file")
    }
  },
  error = function(e) {
    stop(sprintf("Error downloading file '%s': %s, please check %s",
                 url, e$message))
  })
  return(tf)
}


## -----------------------------------------------------------------------------
## OWID
## -----------------------------------------------------------------------------

# # And let's add owid here
owid_url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
owid_tf <- download_url(owid_url)
owid <- read.csv(owid_tf)

#just to align
names(owid)[which(names(owid) == "iso_code")] <- "countryterritoryCode"

# save data out
saveRDS(owid, "owid.rds")

who_vacc_url <- "https://covid19.who.int/who-data/vaccination-data.csv"
who_vacc <- download_url(who_vacc_url)
who_vacc <- read.csv(who_vacc)
who_vacc$countryterritoryCode <- who_vacc$ISO3
who_vacc$DATE_UPDATED <- as.Date(who_vacc$DATE_UPDATED)

saveRDS(who_vacc, "who_vacc.rds")

who_vacc <- import_who_vaccination()

owid <- import_owid()%>%
  filter(date <= date_0)

who_vacc<-who_vacc %>% group_by(ISO3,DATE_UPDATED) %>% 
  summarise(total_vac_who=sum(TOTAL_VACCINATIONS))

owid_date_total<-owid %>%
  group_by(iso3c) %>% 
  filter(total_vaccinations==max(total_vaccinations,na.rm=T)) %>% 
  dplyr::select(1:3,14)

vaccines<-owid_date_total %>% left_join(who_vacc,by=c("iso3c"="ISO3")) %>%
  mutate(date=lubridate::ymd(date)) %>% 
  mutate(total_final = case_when(date>=DATE_UPDATED & 
                                   !is.na(total_vaccinations) ~ total_vaccinations,
                                 is.na(DATE_UPDATED)~ total_vaccinations,
                                 T ~ total_vac_who))

saveRDS(vaccines,"vaccines.rds")

#

mean_vac <- owid %>% group_by(iso3c) %>% filter (date>date_0-30) %>%
  summarise(new_vaccinations=mean(new_vaccinations,na.rm=T),
            new_vaccinations_smoothed=mean(new_vaccinations_smoothed,na.rm=T),
            mean_vac=case_when(is.nan(new_vaccinations)~new_vaccinations_smoothed,
                               T ~ new_vaccinations_smoothed)) 


is <-  mean_vac %>% filter(is.na(mean_vac) | is.nan(mean_vac)) %>% pull(iso3c)

mean_vac <- mean_vac %>% filter (!is.na(mean_vac))

mean_vac2 <- owid %>% filter (iso3c %in% is ) %>% 
  filter (date>date_0-90) %>% group_by(iso3c)%>%
  summarise(new_vaccinations=mean(new_vaccinations,na.rm=T),
            new_vaccinations_smoothed=mean(new_vaccinations_smoothed,na.rm=T),
            mean_vac=case_when(is.nan(new_vaccinations)~new_vaccinations_smoothed,
                               T ~ new_vaccinations_smoothed))

mean_vac<-mean_vac%>%full_join(mean_vac2)

saveRDS(mean_vac,"mean_vac.rds")
