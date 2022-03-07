# data from 

https://app.powerbi.com/view?r=eyJrIjoiMWNjNzZkNjctZTNiNy00YmMzLTkxZjQtNmJiZDM2MTYxNzEwIiwidCI6ImY2MTBjMGI3LWJkMjQtNGIzOS04MTBiLTNkYzI4MGFmYjU5MCIsImMiOjh9

procurementWHO <- read.csv("C:/Users/LUCAS/Desktop/HelpAge/covid_vaccine_allocation_FINAL/procurementWHO.csv")

sing_dose_man <- c("CanSino", "Johnson&Johnson", "Janssen - Ad26.COV 2-S",
    "Janssen - Ad26.COV 2.5", "CanSino - Convidecia",
    "Gamaleya - Sputnik-Light")

procurementWHO <- procurementWHO %>% 
  mutate(sd=case_when(VACCINE_NAME %in% sing_dose_man ~ "single_dosis",
                                       T ~ "two_dose") )

sum_procurement_WHO<-procurementWHO %>% 
  group_by(ISO_3_CODE,sd) %>% 
  summarise (total_dose_WHO=sum(TOTAL_DOSES,na.rm = T))

###
IMF_bilateral <- read_excel("IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
sheet = "bilateral_deal", skip = 2)

IMF_donations <- read_excel("IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
     sheet = "donation_delivered", skip = 2)

sing_dose_IMF <- c("CanSino", "J&J")

IMF_bilateral <- IMF_bilateral %>% 
  mutate(sd=case_when(Vaccine %in% sing_dose_IMF ~ "single_dosis",
                      T ~ "two_dose") )

sum_procurement_IMF<-IMF_bilateral %>% 
  group_by(ISO3,sd) %>% 
  summarise (total_dose_IMF=sum(`Number of doses`,na.rm = T))



sum_procurement_IMF

#

IMF_donations <- IMF_donations %>% 
  mutate(sd=case_when(Vaccine %in% sing_dose_IMF ~ "single_dosis",
                      T ~ "two_dose") )

sum_procurement_IMF_donations<-IMF_donations %>% 
  group_by(ISO3,sd) %>% 
  summarise (total_dose_IMF=sum(`Number of doses delivered through direct donations`,na.rm = T))



sum_procurement_IMF<- sum_procurement_IMF_donations %>% full_join(sum_procurement_IMF)

####


procur_final<-sum_procurement_WHO %>% 
  full_join(sum_procurement_IMF,by=c("ISO_3_CODE"="ISO3","sd"))

procur_final %>% group_by(sd) %>%
  summarise(total_dose_WHO=sum(total_dose_WHO,na.rm = T),
            total_dose_IMF=sum(total_dose_IMF,na.rm=T)) %>% 
  mutate(prop_who=prop.table(total_dose_WHO),
         prop_imf=prop.table(total_dose_IMF))


procur_final %>% group_by(ISO_3_CODE,sd) %>%
  summarise(total_dose_WHO=sum(total_dose_WHO,na.rm = T),
            total_dose_IMF=sum(total_dose_IMF,na.rm=T)) %>% 
  mutate(prop_who=prop.table(total_dose_WHO),
         prop_imf=prop.table(total_dose_IMF)) %>% 
  full_join(vaccines,by=c("ISO_3_CODE"="iso3c")) %>% dplyr::select(-c(date,total_boosters,DATE_UPDATED))%>%
  filter(sd=="single_dosis") %>% 
  mutate(prop_final=total_dose_WHO/total_final)%>%  
  filter(prop_who>.1) %>% arrange(-prop_who)
