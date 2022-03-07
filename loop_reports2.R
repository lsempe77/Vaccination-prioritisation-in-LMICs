library(bookdown)
library(knitr)
library(owidR)
library(tidyverse)
library(nimue)
library(readxl)
library(gtsummary)
library(future)
library(furrr)
library(patchwork)
library(flextable)
library(xfun)
library(countrycode)
library(kableExtra)

regions<-"https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"

regions <- read_csv(regions)

date_0<-Sys.Date()-2 # Sys.Date() 25/02/2022

date_end<-date_0+365

days<-as.numeric(round(difftime(date_end,date_0)))

days_v <-as.numeric(round(difftime(as.Date("2022-07-01"),date_0)))


#

source ("import_data.R") # functions to import data

source ("import_data_external.R") # import data from WHO and OWID

source ("functions_final.R")

#("waning.R")

cov1<-readRDS("cov1.rds")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

dose_df2<-readRDS("dose_df2.rds")

p<-dose_df2 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_disease, colour="disease")) +
  geom_line()+
  stat_smooth(se = FALSE, fullrange = TRUE,n=days)

vaccine_efficacy_disease<-ggplot_build(p)$data[[2]] %>% pull(y)

vaccine_efficacy_disease_no_booster<-as.list(vaccine_efficacy_disease)

q<-dose_df2 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_infection, colour="infection")) +
  geom_line()+
  stat_smooth( se = FALSE, fullrange = TRUE,n=days)

vaccine_efficacy_infection<-ggplot_build(q)$data[[2]] %>% pull(y)
vaccine_efficacy_infection_no_booster<-as.list(vaccine_efficacy_infection)

dose_df3<-readRDS("dose_df3.rds")

r<-dose_df3 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_disease, colour="disease")) +
  geom_line()+
  stat_smooth(se = FALSE, fullrange = TRUE,n=days)

vaccine_efficacy_disease_yes_booster<-ggplot_build(r)$data[[2]] %>% pull(y)

vaccine_efficacy_disease_yes_booster<-as.list(vaccine_efficacy_disease_yes_booster)

s<-dose_df3 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_infection, colour="infection")) +
  geom_line()+
  stat_smooth( se = FALSE, fullrange = TRUE,n=days)

vaccine_efficacy_infection_yes_booster<-ggplot_build(s)$data[[2]] %>% pull(y)

vaccine_efficacy_infection_yes_booster<-as.list(vaccine_efficacy_infection_yes_booster)

#

start_booster<-owid %>%
  group_by(iso3c) %>%
  summarise(total_boosters=sum(total_boosters,na.rm = T)) %>%
  mutate(start_booster=case_when(total_boosters>0 ~ "yes",
                                 total_boosters==0 ~ "no",
                                 T ~ "no"))

#

countries <- sort(unique(squire::population$country))

countries <-countries[-35]

#countries <-countries[58:200]

cases <- owid("daily-covid-cases-deaths-7-day-ra")

# loop through the list of countries

for (i in countries) {
  
  #i<-"Afghanistan"
  
  country<-i
  
  iso3cs <- squire::population$iso3c[squire::population$country==country][1]
  
  population_total <- nimue:::init(squire::get_population(country)$n, seeding_cases = 1)
  
  population_total<-sum(population_total$S_0[,1])

  if (iso3cs == "ETH") { 
    
    current_coverage<-owid %>% filter(iso3c==iso3cs) %>% 
      summarise(people=max(people_vaccinated_per_hundred,na.rm = T),
                tot_vac=max(total_vaccinations_per_hundred,na.rm = T),
                cov=(tot_vac+(people-tot_vac)/2)/100) %>% pull (cov)
    
  } else
  
  if (iso3cs %in% owid$iso3c) { 
    
        current_coverage<-owid %>% filter(iso3c==iso3cs) %>% 
      summarise(people=max(people_vaccinated_per_hundred,na.rm = T),
                people_fully=max(people_fully_vaccinated_per_hundred,na.rm = T),
                cov=(people_fully+(people-people_fully)/2)/100) %>% pull (cov)
    
  } else {next}
  
  if (iso3cs == "ERI") {  current_coverage <- 0 }
  if (iso3cs == "FSM") {  current_coverage <- 0.095  } # https://covidvax.live/location/fsm
  # if (iso3cs == "PLW") {   current_coverage <- 0.96 } #https://covidvax.live/location/plw
  # if (iso3cs == "MHL") {  current_coverage <- 0.389 } # https://covidvax.live/location/mhl
  
  if (iso3cs %in% owid$iso3c) {
    
    mean_vacine<- mean_vac %>%
      dplyr::filter (iso3c  == iso3cs) %>%
      pull(mean_vac) } else {next}
  
  if (iso3cs == "ERI") {  mean_vacine <- 1 }
  if (iso3cs == "FSM") {  mean_vacine <- 459  } # https://covidvax.live/location/fsm
  if (iso3cs == "TKL") {  mean_vacine <- 24 } # https://covidvax.live/location/tkl
  if (iso3cs == "TKM") {  mean_vacine <- 41379 }# https://covidvax.live/location/tkm
  if (iso3cs == "TUV") {  mean_vacine <- 34 }# https://covidvax.live/location/tuv
  if (iso3cs == "NIU") {  mean_vacine <- 0 }# https://covidvax.live/location/niu
  if (iso3cs == "MCO") {  mean_vacine <- 102 }# https://covidvax.live/location/mco
  
  
  to_give<-round(population_total*.7)-(current_coverage*population_total)
  
  
  to_give2<-floor((2*to_give)/days_v) 
  
  
  if (is.nan(to_give2)){to_give2<-sum(population_total)*.7/days_v}
  
  
  if(length(mean_vacine)==0 | is.nan(mean_vacine) | mean_vacine==0){mean_vacine=0}
  
  if ( length(current_coverage) == 0 ){
    next
  } else if(length(to_give2) == 0 | to_give2 < 0 | current_coverage>=.7 |
       to_give2 < mean_vacine ) { # chequear estos valores
    next
  } else {
    
    source("countries_2.R")
    
  # get country specific params
  population = squire::get_population(country)

  rmarkdown::render(input = "country_fact.Rmd",
                    output_format = "word_document2",
                    output_file = paste0(country, ".docx"),
                    output_dir = "reports")
    
  }
  
  gc()
  
}
