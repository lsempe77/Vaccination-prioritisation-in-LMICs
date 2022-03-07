library("xlsx")

cov2<-list()

for (i in countries) {
  
  country<-i
  
  iso3cs <- squire::population$iso3c[squire::population$country==country][1]
  
  population_total <- nimue:::init(squire::get_population(country)$n, seeding_cases = 1)
  
  population_total<-sum(population_total$S_0[,1])
  
  if (iso3cs %in% owid$iso3c) { 
    
    current_coverage<-owid %>% filter(iso3c==iso3cs) %>% 
      summarise(people=max(people_vaccinated_per_hundred,na.rm = T),
                people_fully=max(people_fully_vaccinated_per_hundred,na.rm = T),
                cov=(people_fully+(people-people_fully)/2)/100) %>% pull (cov)
    
  } else {next}
  
  if (iso3cs == "ERI") {  current_coverage <- 0 }
  if (iso3cs == "FSM") {  current_coverage <- 0.084  } # https://covidvax.live/location/fsm
  # if (iso3cs == "PLW") {   current_coverage <- 0.96 } #https://covidvax.live/location/plw
  # if (iso3cs == "MHL") {  current_coverage <- 0.389 } # https://covidvax.live/location/mhl
  
  if (iso3cs %in% owid$iso3c) {
    
    mean_vacine<- mean_vac %>%
      dplyr::filter (iso3c  == iso3cs) %>%
      pull(mean_vac) } else {next}
  
  if (iso3cs == "ERI") {  mean_vacine <- 0 }
  if (iso3cs == "FSM") {  mean_vacine <- 459  } # https://covidvax.live/location/fsm
  if (iso3cs == "TKL") {  mean_vacine <- 24 } # https://covidvax.live/location/tkl
  if (iso3cs == "TKM") {  mean_vacine <- 41379 }# https://covidvax.live/location/tkm
  if (iso3cs == "TUV") {  mean_vacine <- 34 }# https://covidvax.live/location/tuv
  if (iso3cs == "NIU") {  mean_vacine <- 0 }# https://covidvax.live/location/niu
  if (iso3cs == "MCO") {  mean_vacine <- 102 }# https://covidvax.live/location/mco
  
  R0=1.3
  
  case_iso<-cases%>% dplyr::filter (code==iso3cs)%>%
    dplyr::filter (date>(date_0-15)) %>%
    summarise(infections=floor(sum(`Daily new confirmed cases due to COVID-19 (rolling 7-day average, right-aligned)`,na.rm = T))) %>% 
    pull(infections)
  
  
  if(case_iso<0) { case_iso=1000}
  
  seeding_cases = c(case_iso)
  
  if(seeding_cases[1]==0) { 
    
    seeding_cases = 100
    
  }
  
  days=365
  tt<-seq(1:days)
  
  vaccine_efficacy_infection = as.list(vaccine_efficacy_infection)
  vaccine_efficacy_disease = as.list(vaccine_efficacy_disease)
  
  r1 <- nimue::run(
    time_period = days,
    population = NULL,
    country = country,
    R0 = c(1.3,.9),
    tt_R0 = c(1, days_v),
    seeding_cases = seeding_cases,
    max_vaccine = mean_vacine,
    dur_R = 200,
    dur_E= 2,
    dur_IMild=2.6,
    dur_ICase=3.8,
    prob_hosp_multiplier=.6,
    dur_vaccine_delay = 14, 
    vaccine_coverage_mat = strategy_matrix("Elderly",
                                           max_coverage = .9),
    vaccine_efficacy_infection = vaccine_efficacy_infection,
    vaccine_efficacy_disease = vaccine_efficacy_disease,
    tt_vaccine_efficacy_infection = tt,
    tt_vaccine_efficacy_disease = tt,
    use_dde=T)
  
  
  r1<- nimue::format(r1,summaries = c("hospitalisations","vaccines", "deaths"),
                     reduce_age = F)
  
  cov2[[i]] <- list(r1)
  
  
}

modtable2 <- lapply(cov2, function(x) do.call(rbind, x))

map(cov2,"[[")->abc

abc <- as.data.frame(names(cov2))
abc$.groups <- seq.int(nrow(abc))
colnames(abc)[1]<- "cnt"
abc <- abc %>% 
  mutate(iso_a3=countrycode(cnt, 
                            origin = 'country.name', destination = 'iso3c'))

modtable3 <- as.data.frame(do.call(rbind,modtable2))

library(groupdata2)

# Create groups with 5 members each (except last group)
modtable3 <- group(modtable3, n = 190)

variable.names(abc)

wb <- read.csv("C:/Users/LUCAS/Desktop/HelpAge/covid_vaccine_allocation_FINAL/wb.csv")

wb <- wb %>% mutate(iso_a3=countrycode(country, origin = 'country.name', destination = 'iso3c'))


table2_full<- abc %>% mutate(.groups=as.factor(.groups)) %>% right_join(modtable3) %>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% 
  group_by(iso_a3,compartment,age) %>% 
  summarise (value=sum(value,na.rm = T)) %>%
  group_by(iso_a3,compartment) %>% 
  mutate(proportion=(round(prop.table(value),3))*100) %>% 
  left_join(regions[,c(3,6)], by=c("iso_a3"="alpha-3")) %>%
    full_join(wb,by="iso_a3") %>% 
  filter (Income.group != "High income" & !is.na(Income.group))
 
mr <- read_excel("mr.xlsx")


table2_full<- table2_full %>% left_join(mr)

write.xlsx(as.data.frame(table2_full),"table2_fullb.xlsx")



write.xlsx(as.data.frame(table2_full),"table2_full.xlsx")
