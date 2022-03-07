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
library(R0)

select<-dplyr::select

source("load_functions.R") # runs LMICs scripts. Provides real dose effectiveness per country

source("functions_final.R")

source("calibrate_deaths.R")

cases<-owid("biweekly-confirmed-covid-19-cases",
            rename = "cases")

# reff.file<-"https://raw.githubusercontent.com/crondonm/TrackingR/main/Estimates-Database/database.csv"
# 
# reffc<-read.csv(reff.file)



countries <- sort(unique(squire::population$country))

countries <-countries[-35]

#countries <-countries[21:166]
  
share.covid.fully.vaccinated <- owid("share-people-fully-vaccinated-covid",
                                     rename = "share")

share.covid.fully.vaccinated<-share.covid.fully.vaccinated %>% group_by(code) %>%
  summarise(share=max(share)/100)

date_end<-as.Date("2022-06-30")

days<-as.numeric(round(difftime(date_end,date_0)))

# loop through the list of countries

for (i in countries) {
  
  country<-i
  
  iso3cs <- squire::population$iso3c[squire::population$country==country][1]
  
  current_coverage_owd <- share.covid.fully.vaccinated %>% filter (code==iso3cs) %>% pull (share)
  
  current_coverage_owd
  
  mean_vac<- daily_vaccines %>% filter (iso3c==iso3cs) %>%
    filter (date > (date_0-31) ) %>%
    summarise(v=round(mean(vacc_per_day, na.rm = TRUE))) %>% pull(v)
  
  population_total <- nimue:::init(squire::get_population(country)$n, seeding_cases = 1)
  
  population_total<-sum(population_total$S_0[,1])
  
  tot_vac<-daily_vaccines %>% filter (iso3c==iso3cs) %>%
    summarise(v=sum(vacc_per_day, na.rm = TRUE)) %>% pull(v)
  
  to_give<-(sum(population_total)*.7)-(current_coverage_owd*sum(population_total))
  
  to_give2<-round((2*to_give)/days) 
  
  if ( length(current_coverage_owd) == 0 ){
    next
  } else if(length(to_give2) == 0 |
       to_give2 < mean_vac ) { # chequear estos valores
    next
  } else {
    
  # grab the json from the data exports
  file_path <- "https://raw.githubusercontent.com/mrc-ide/global-lmic-reports/master/"
  json_path <- file.path(file_path,iso3cs,"input_params.json")
  
  skip_to_next <- FALSE
    
  tryCatch(json <- jsonlite::read_json(json_path), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }   
  

  source("countries.R")
  
  # ## get country specific params
  # population = squire::get_population(country)
  # 
  # rmarkdown::render(input = "multi_new.Rmd",
  #                   output_format = "word_document2",
  #                   output_file = paste0(country, ".docx"),
  #                   output_dir = "reports")
  }
  
  gc()
  
}
