#ihme<-"https://ihmecovid19storage.blob.core.windows.net/latest/data_download_file_reference_2021.csv"

#ihme_severe<-read.csv(ihme)

ihme <- read.csv("C:/Users/LUCAS/Desktop/HelpAge/covid_vaccine_allocation_FINAL/data_download_file_reference_2021.csv")

ihme<-ihme %>% mutate(iso3c=countrycode(location_name,origin = "country.name",
                                           destination = "iso3c"),
                      date=lubridate::ymd(date)) 

data_download_file_severe_omicron_2021 <- read.csv("C:/Users/LUCAS/Desktop/HelpAge/covid_vaccine_allocation_FINAL/data_download_file_severe_omicron_2021.csv")

ihme_severe<-data_download_file_severe_omicron_2021 %>% mutate(iso3c=countrycode(location_name,origin = "country.name",
                                     destination = "iso3c"),
                date=lubridate::ymd(date)) 


#

path = "C:/Users/LUCAS/Desktop/HelpAge/covid_vaccine_allocation_FINAL/Data/"

files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)

r <- lapply(files, readRDS)


#
seir_max<-purrr::map(r,"seirmax")

seir_max<-do.call(rbind.data.frame,seir_max)

seir_max<-bind_cols(seir_c,seir_max)

colnames(seir_max)[4]<-"seir_max"

seir_med<-purrr::map(r,"seirmed")

seir_med<-do.call(rbind.data.frame,seir_med)

seir_med<-bind_cols(seir_c,seir_med)

colnames(seir_med)[4]<-"seir_med"

val_seirmed<-sum(seir_med$seir_med)

seir_min<-purrr::map(r,"seirmin")

seir_min<-do.call(rbind.data.frame,seir_min)

seir_min<-bind_cols(seir_c,seir_min)

colnames(seir_min)[4]<-"seir_min"

val_seirmin<-sum(seir_min$seir_min)

#
variable.names(ihme_severe)

ihme_severe_imperial<-ihme_severe%>% dplyr::select(61,2,14)%>%
  filter (date>=date_0) 

ihme_severe_imperial<-ihme_severe_imperial %>% 
  group_by(iso3c) %>% 
  summarise(ihme=sum(seir_daily_mean)) %>% 
  right_join(seir_min) %>% 
  right_join(seir_med) %>% right_join(seir_max)
  


ihme_severe_imperial %>% group_by(iso3c) %>% filter (!is.na(ihme))%>%
  summarise (ihme=sum(ihme,na.rm = T),
             seir_min=sum(seir_min),
             seir_med=sum(seir_med),
             seir_max=sum(seir_max)) %>%
  mutate(dif_min_pct=((seir_min-ihme)/ihme)*100,
    dif_med_pct=(seir_med-ihme)/ihme*100) %>% arrange(-dif_min_pct)

seir_min %>% arrange(-seir_min) %>% head()
seir_med %>% arrange(-seir_med) %>% head()
seir_max %>% arrange(-seir_max) %>% head()

ihme_severe_imperial %>% arrange(-ihme)

ihme_severe_imperial %>% group_by(iso3c) %>% filter (!is.na(ihme))%>%
  summarise (ihme=sum(ihme,na.rm = T),
             seir_min=sum(seir_min),
             seir_med=sum(seir_med),
             seir_max=sum(seir_max)
             ) %>%
  mutate(dif_min=((seir_min-ihme)/ihme)*100,
         dif_med=(seir_med-ihme)/ihme*100) %>% #filter(dif_min<1000)%>%
  ggplot()+
  geom_point(aes(iso3c,dif_min))


ihme_severe_imperial %>% ungroup() %>%
  filter (!is.na(ihme))%>%
  summarise (ihme=sum(ihme,na.rm = T),
             seir_min=sum(seir_min),
             seir_med=sum(seir_med),
             seir_max=sum(seir_max))


