library(kableExtra)
library(rworldmap)
library(tidyverse)
library(patchwork)
library(countrycode)
library(nimue)
library(xlsx)

import_owid <- function(){
owid <- readRDS("owid.rds")
dplyr::filter(
dplyr::select(
dplyr::rename(owid, iso3c = countryterritoryCode),
c(iso3c, date, tidyselect::contains(c("vacc","boost","deat","exc","part")))
),
nchar(iso3c) == 3
)
}



##

date_0<-Sys.Date()-2

date_end<-date_0+365

days<-as.numeric(round(difftime(date_end,date_0)))

days_v <-as.numeric(round(difftime(as.Date("2022-07-01"),date_0)))


#

owid <- import_owid()%>%
  filter(date <= date_0)

mean_vac<-readRDS("mean_vac.rds")


regions<-"https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"

regions <- read_csv(regions)


#


countries <- sort(unique(squire::population$country))

countries <-countries[-35]

cov1<-list()

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
  

  to_give<-round(population_total*.7)-(current_coverage*population_total)
  
  
  to_give2<-floor((2*to_give)/days_v) 
  
  if (is.nan(to_give2)){to_give2<-sum(population_total)*.7/days_v}
  
  if(length(mean_vacine)==0 | is.nan(mean_vacine) | mean_vacine==0){mean_vacine=0}
  
  
  cov1[[i]] <- list(iso3cs,current_coverage,mean_vacine,
                    population_total,to_give2)
  

}


cov1<-stringi::stri_list2matrix(cov1, byrow=TRUE)

cov1<-as.data.frame(cov1)

colnames(cov1)[1]<-"iso_a3"
colnames(cov1)[2]<-"coverage"
colnames(cov1)[3]<-"mean_vac"
colnames(cov1)[4]<-"population"
colnames(cov1)[5]<-"to_give"

cov1<- cov1 %>% mutate(coverage=as.numeric(coverage),
                       mean_vac=as.numeric(mean_vac),
                       population=as.numeric(population),
                       to_give=as.numeric(to_give))%>%
  mutate (status = case_when(to_give > mean_vac  ~ "behind",
                                           to_give < mean_vac  ~ "on_track",
                                  coverage >.7 ~ "on_track", 
                                           to_give < 0 ~ "on_track"))

start_booster<-owid %>%
  group_by(iso3c) %>%
  summarise(total_boosters=sum(total_boosters,na.rm = T)) %>%
  mutate(start_booster=case_when(total_boosters>0 ~ "yes",
                                 total_boosters==0 ~ "no",
                                 T ~ "no"))

boost_beh<-cov1 %>% left_join(start_booster, by=c("iso_a3"="iso3c")) %>% 
  filter (start_booster=="yes" & status == "behind") %>% tally() %>% pull (n)


be<-cov1 %>% group_by(status) %>% tally() %>% pull(n)
be<-be[1]

continents<-cov1 %>% left_join(regions, by=c("iso_a3"="alpha-3")) %>% 
  filter(status=="behind")%>% group_by(region)%>%tally() %>% pull(n)

africa<-continents[1]
americas<-continents[2]
asia<-continents[3]
europe<-continents[4]
oceania<-continents[5]


count_reg<-cov1 %>% left_join(regions, by=c("iso_a3"="alpha-3")) %>% 
  filter(status=="behind")


list_countries<-cov1 %>% filter (status=="behind") %>%
  rename(average_vac_month=mean_vac,
                                      current_coverate=coverage,
                      vaccines_per_day_needed=to_give) %>% select(-status)


plot_rel<-cov1 %>% left_join(regions, by=c("iso_a3"="alpha-3")) %>% 
  mutate(iso_a3=fct_reorder(factor(iso_a3),status)) %>%
  ggplot()+
  geom_point(aes(mean_vac/population,coverage,colour=status))+
  facet_wrap(~`region` ) + theme_light() +
  scale_colour_discrete("",labels=c("Behind goal","On track",
                                    "No data"))+
  ylab("Dose coverage")+xlab("Mean vaccine last month per population")+
  theme(legend.position = "bottom")

##

# cov1 %>% left_join(regions, by=c("iso_a3"="alpha-3")) %>% 
#   dplyr::select(1:8,12)%>% 
#   filter(status=="behind") %>% filter (region=="Africa")



###

#saveRDS(cov1,"cov1.rds")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

d_points<-world%>%left_join(cov1) %>%
filter(population<600000) %>% filter (!is.na(status),sov_a3!="SUR",sov_a3!="BLZ",
                                      sov_a3!="ISL")

islands <-d_points %>% tally() %>% pull (n)

islands <- n2w(islands,cap=T)

map_res<-world %>%left_join(cov1) %>% ggplot()+
  geom_sf(aes(fill=status)) + 
  geom_sf(data = d_points, aes(colour=status),size = 4,
          show.legend = F)+
  scale_fill_discrete("",labels=c("Behind goal","On track",
                                                              "No data"))+
  theme(legend.position = "bottom")

##

#
#path = "C:/Users/LUCAS/Desktop/HelpAge/covid_vaccine_allocation_FINAL/Data365/" # check which one I'm using!!!

path = "C:/Users/LUCAS/Desktop/HelpAge/covid_vaccine_allocation_FINAL/Data/" 

files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)

data_countries <- lapply(files, readRDS)

##

seir_c<-purrr::map(data_countries, ~.[["country"]])

seir_c<-do.call(rbind.data.frame,seir_c)

colnames(seir_c)[1]<-"location"

seir_c <- seir_c %>% mutate (location = case_when(location=="Micronesia" ~ 
                                                    "Federated States of Micronesia",
                                                  T ~ location))

seir_c<- seir_c %>% mutate(iso3c=countrycode(location,origin = "country.name",
                                             destination = "iso3c"))

seir_c$id<-seq.int(nrow(seir_c))

##

current<-purrr::map(data_countries, "current_coverage")

current<-do.call(rbind.data.frame,current)

current<-current %>% cbind(seir_c)

colnames(current)[1]<-"coverage"

##

ra0<-purrr::map(data_countries, "out_format_0")


for( i in seq_along(ra0)){
  
  ra0[[i]]$country <- rep(seir_c$location[i],nrow(ra0[[i]]))
  
}

ra0<-do.call(rbind.data.frame,ra0)

dmin0<-ra0 %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=min(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max

dmed0<-ra0 %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=median(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max

dmax0<-ra0 %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=max(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths)))

dmin0;dmed0;dmax0

dmin0h<-ra0 %>% group_by(country) %>%  filter(R0==1.5)%>%
  summarise(hospitalisations_averted=min(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted))) # min, median, max

dmed0h<-ra0 %>% group_by(country) %>%  filter(R0==1.5)%>%
  summarise(hospitalisations_averted=median(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted))) # min, median, max

dmax0h<-ra0 %>% group_by(country) %>%  filter(R0==1.5)%>%
  summarise(hospitalisations_averted=max(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted)))

dmin0h;dmed0h;dmax0h


##


##

ra<-purrr::map(data_countries, "out_format")

for( i in seq_along(ra)){
  
  ra[[i]]$country <- rep(seir_c$location[i],nrow(ra[[i]]))
  
}

ra<-do.call(rbind.data.frame,ra)

n_vac<-ra %>% group_by(country) %>% summarise(mv=max(vaccine_n),
                                              medv=median(vaccine_n),
                                              minv=min(vaccine_n)) %>% ungroup()%>%
  summarise(mv=sum(mv),
            medv=sum(medv),
            minv=sum(minv)) 

n_vac1=round(n_vac,-7)

n_vac1<-signif(n_vac1[1])

n_vac1<-as.integer(n_vac1)

numb<-n2w(n_vac1)

cost<-n_vac1*10
cost1<-n2w(cost)

list_vaccine_countries<-ra %>% group_by(country) %>% summarise(
  medv=median(vaccine_n)) %>%
  rename(vaccine_dose_needed=medv)

###


worstcountries<-ra %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=max(deaths_averted)) %>% 
  ungroup() %>% mutate(pr=prop.table(deaths)*100) %>% arrange(-pr) %>% 
  head (4) %>% summarise(pr=sum(pr)) %>% pull(pr)

mediancountries<-ra %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=median(deaths_averted)) %>% 
  ungroup() %>% mutate(pr=prop.table(deaths)*100) %>% arrange(-pr) %>% 
  head (4)%>% summarise(pr=sum(pr)) %>% pull(pr)


worstcountries_n<-ra %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=max(deaths_averted)) %>% 
  ungroup() %>% mutate(pr=prop.table(deaths)*100) %>% arrange(-pr) %>% 
  head (4) %>% pull (country)

worstcountries_n<-ra %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=max(deaths_averted)) %>% 
  ungroup() %>% mutate(pr=prop.table(deaths)*100) %>% arrange(-pr) %>% 
  head (4) %>% pull (country)


cntboost<-cov1 %>% left_join(start_booster, by=c("iso_a3"="iso3c")) %>% 
  filter (start_booster=="yes" & status == "behind") %>% 
  left_join(seir_c,by=c("iso_a3"="iso3c")) %>% select (location) 
              
worstcountries_n_list<-ra %>% group_by(country) %>% filter(R0==1.5)%>%
  summarise(deaths=round(max(deaths_averted))) %>% 
  ungroup() %>% mutate(pr=prop.table(deaths)*100) %>% arrange(-pr) %>% 
  head (4) %>% select(country)

worstcountries_n2<- worstcountries_n_list %>% filter(cntboost%in%country)                                                                 



###

cnt1<-ra %>% filter(country==worstcountries_n[1])%>%
  ggplot()+
geom_point(aes(x=as.factor(R0),
y=deaths_averted,
shape=as.factor(seeding_cases)))+
theme_light() + xlab("R0")+ylab("Deaths averted")+ggtitle(worstcountries_n[1])+
scale_shape_discrete("Seeding cases", labels= c("Current value","Current value x3"))+
theme(legend.position = "none")

cnt2<-ra %>% filter(country==worstcountries_n[2])%>%
ggplot()+
geom_point(aes(x=as.factor(R0),
y=deaths_averted,
shape=as.factor(seeding_cases)))+
theme_light() + xlab("R0")+ylab("Deaths averted")+ggtitle(worstcountries_n[2])+
scale_shape_discrete("Seeding cases", labels= c("Current value","Current value x3"))+
theme(legend.position = "none")

cnt3<-ra %>% filter(country==worstcountries_n[3])%>%
ggplot()+
geom_point(aes(x=as.factor(R0),
y=deaths_averted,
shape=as.factor(seeding_cases)))+
theme_light() + xlab("R0")+ylab("Deaths averted")+ggtitle(worstcountries_n[3])+
scale_shape_discrete("Seeding cases", labels= c("Current value","Current value x3"))+
theme(legend.position = "none")

cnt4<-ra %>% filter(country==worstcountries_n[4])%>%
ggplot()+
geom_point(aes(x=as.factor(R0),
y=deaths_averted,
shape=as.factor(seeding_cases)))+
theme_light() + xlab("R0")+ylab("Deaths averted")+ ggtitle(worstcountries_n[4])+
scale_shape_discrete("Seeding cases", labels= c("Current value","Current value x3"))+
theme(legend.position = "bottom")

cnts<-cnt1+cnt2+cnt3+cnt4


##

unnested<-purrr::map(data_countries,"out_format_unnest")


for( i in seq_along(unnested)){
  
  unnested[[i]]$country <- rep(seir_c$location[i],nrow(unnested[[i]]))
  
}

unnested<-do.call(rbind.data.frame,unnested)


old_deaths<- unnested %>% group_by(country) %>% filter(R0==1.5,
                                   max_vaccine ==max(max_vaccine),
                                   seeding_cases==max(seeding_cases)) %>%
  dplyr::filter(compartment == "deaths") %>%
  mutate(age = case_when( age_group == "60-65"  |
                            age_group == "65-70"  |
                            age_group == "70-75" | 
                            age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% #dplyr::filter (age!="not")%>%
  group_by(country,R0,max_vaccine,
           seeding_cases,age)%>%
  summarise(value=sum(value)) %>%
  group_by(country,R0,max_vaccine,seeding_cases) %>% 
  summarise(pr=prop.table(value)) %>%  filter(row_number() %% 2 == 0) %>% ## Select even rows
      ggplot() + geom_point(aes(iso3c,pr))+
  guides(x = guide_axis(n.dodge = 3))+theme_light()+
  geom_hline(yintercept = .5,linetype=2,colour="darkorange",alpha=.9)+
  ylab("% avoidable deaths people > 60") + xlab("Countries")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=10))



old_hospital<-unnested %>% group_by(country) %>% filter(R0==1.5,
                                       max_vaccine ==max(max_vaccine),
                                       seeding_cases==max(seeding_cases)) %>%
  dplyr::filter(compartment == "hospitalisations") %>%
  mutate(age = case_when( age_group == "60-65"  |
                            age_group == "65-70"  |
                            age_group == "70-75" | 
                            age_group == "75-80" |
                            age_group == "80+" ~ "older",
                           T ~ "not")) %>% #dplyr::filter (age!="not")%>%
  group_by(country,R0,max_vaccine,
           seeding_cases,age)%>%
  summarise(value=sum(value)) %>%
  group_by(country,R0,max_vaccine,seeding_cases) %>% 
  summarise(pr=prop.table(value)) %>%  filter(row_number() %% 2 == 0) %>% ## Select even rows
  ggplot() + geom_point(aes(iso3c,pr))+
  guides(x = guide_axis(n.dodge = 3))+theme_light()+
  geom_hline(yintercept = .5,linetype=2,colour="darkorange",alpha=.9)+
  ylab("% of avoidable hospitalisations people > 60") + xlab("Countries")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=10))



##


old_table<-unnested %>%
  filter(R0==1.5) %>% 
  group_by(country) %>% 
  filter(seeding_cases == max(seeding_cases)) %>%
  dplyr::filter(compartment == "deaths"|  compartment == "hospitalisations") %>%
  mutate(`population group` = case_when(  age_group == "60-65"  |
                                            age_group == "65-70"  |
                                            age_group == "70-75" | 
                                            age_group == "75-80" |
                             age_group == "80+" ~ "older people",
                           T ~ "younger people"))  %>%
  group_by(compartment,`population group`) %>% 
  summarise(value=sum(value)) %>% 
  #group_by(age) %>%
  mutate(`Proportion older/younger`=prop.table(value)*100) 

colnames(old_table)[1] <- "Impact of COVID"		 	
colnames(old_table)[2] <- "Concerned population"
colnames(old_table)[3] <- "Avoidable deaths"	
colnames(old_table)[4] <- "Proportion of total deaths"


pro1<-old_table[1,4]
pro2<-old_table[3,4]

if (pro1>pro2){  pro1<-old_table[3,4]
pro2<-old_table[1,4]}

pro3<-old_table[2,4]
pro4<-old_table[4,4]

if (pro3>pro4){  pro3<-old_table[4,4]
pro4<-old_table[2,4]}

##





dmin<-ra %>% group_by(country) %>%   filter (R0==1.5) %>%
  summarise(deaths=min(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max

dmed<-ra %>% group_by(country) %>% filter (R0==1.5) %>%
  summarise(deaths=median(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths)))# min, median, max

dmax<-ra %>% group_by(country) %>% filter (R0==1.5) %>%
  summarise(deaths=max(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max



dminh<-ra %>% group_by(country) %>%   filter (R0==1.5) %>%
  summarise(hospitalisations_averted=min(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=sum(hospitalisations_averted)) # min, median, max

dmedh<-ra %>% group_by(country) %>% filter (R0==1.5) %>%
  summarise(hospitalisations_averted=median(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=sum(hospitalisations_averted)) # min, median, max

dmaxh<-ra %>% group_by(country) %>% filter (R0==1.5) %>%
  summarise(hospitalisations_averted=max(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=sum(hospitalisations_averted)) # min, median, max


# 
# ra %>% arrange(-deaths_averted) %>% ggplot()+
#   geom_point(aes(
#     y=deaths_averted,
#     x=country))+
#   facet_grid(as.factor(vaccine_efficacy_disease)~as.factor(vaccine_efficacy_infection))

###

baselineR<-purrr::map(data_countries, "baselineR")

for( i in seq_along(baselineR)){
  
  baselineR[[i]]$iso3c <- rep(seir_c$location[i],nrow(baselineR[[i]]))
  
}

baselineR<-do.call(rbind.data.frame,baselineR)


min_base<-baselineR %>% group_by(country) %>% filter (R0==1.5) %>% summarise (deaths=min(deaths)) %>%
ungroup() %>% summarise(deaths=round(sum(deaths))) %>% pull(deaths)

max_base<-baselineR %>% group_by(country) %>% filter (R0==1.5) %>% summarise (deaths=max(deaths)) %>%
ungroup() %>% summarise(deaths=round(sum(deaths))) %>% pull(deaths)



min_base_hosp<-baselineR %>% group_by(country) %>% filter (R0==1.5) %>% summarise (hosp=min(hospitalisations)) %>%
  ungroup() %>% summarise(hosp=round(sum(hosp))) %>% pull(hosp)

max_base_hosp<-baselineR %>% group_by(country) %>% filter (R0==1.5) %>% summarise (hosp=max(hospitalisations)) %>%
  ungroup() %>% summarise(hosp=round(sum(hosp))) %>% pull(hosp)





min_prop_old<- unnested %>% 
  dplyr::filter(compartment == "deaths") %>%
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% #dplyr::filter (age!="not")%>%
  group_by(country,R0,max_vaccine,
           seeding_cases,age)%>%
  summarise(value=sum(value)) %>%
  group_by(country,R0,max_vaccine,
           seeding_cases) %>% 
  filter( R0==1.5,
         max_vaccine ==max(max_vaccine )) %>%
  summarise(pr=prop.table(value)) %>%  filter(row_number() %% 2 == 0) %>% ## Select even rows
  group_by(country) %>% filter (pr==min(pr))


max_prop_old<- unnested %>% 
  dplyr::filter(compartment == "deaths") %>%
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% #dplyr::filter (age!="not")%>%
  group_by(country,R0,max_vaccine,
           seeding_cases,age)%>%
  summarise(value=sum(value)) %>%
  group_by(country,R0,max_vaccine,
           seeding_cases) %>% 
  filter( R0==1.5,
         max_vaccine ==max(max_vaccine )) %>%
  summarise(pr=prop.table(value)) %>%  filter(row_number() %% 2 == 0) %>% ## Select even rows
  group_by(country) %>% filter (pr==max(pr))

# pmin<-world %>%
#   left_join(min_prop_old,by=c("iso_a3"="iso3c")) %>% ggplot()+
#   geom_sf(aes(fill=pr)) +scale_fill_continuous(low="yellow", high="red",  
#                                                guide="colorbar",na.value="white")
# 
# pmax<-world %>%
#   left_join(max_prop_old,by=c("iso_a3"="iso3c")) %>% ggplot()+
#   geom_sf(aes(fill=pr)) +scale_fill_continuous(low="yellow", high="red", 
#                                                guide="colorbar",na.value="white")
# 
# library(ggpubr)
# 
# ggarrange(pmin + theme(legend.position="none"),
#                     pmax,
#           ncol = 1, common.legend = TRUE, legend = "bottom")
# 


# 
# 
# cov1 %>% filter (status=="behind") %>% 
#   mutate(iso_a3=forcats::fct_reorder(iso_a3,coverage))%>%
#   ggplot() + geom_point(aes(iso_a3,coverage)) + 
#   scale_x_discrete(guide = guide_axis(n.dodge = 4)) 
# 
# cov1 %>% filter (status=="behind") %>% tally()

##

dose_df2<-readRDS("dose_df2.rds")

vd1<-min(dose_df2$date_vaccine_change)

efp<-dose_df2 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>% 
  filter (date_vaccine_change<vd1+days) %>% summarise(ef0=min(vaccine_efficacy_disease)-.02,
                                                      ef1=max(vaccine_efficacy_disease)+.02)


p<-dose_df2 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_disease)) +
  geom_rect(aes(xmin=vd1, xmax=vd1+days, ymin=efp$ef0, ymax=efp$ef1),fill="darkorange",alpha=0.1) + 
  geom_line()+
  stat_smooth(se = FALSE, fullrange = TRUE,n=days,colour="blue") + theme_light()+xlab("Weeks")+ylab("% efficacy") +
  ggtitle("Disease - 2 doses") + scale_x_date(date_labels = "%U")
 

vaccine_efficacy_disease<-ggplot_build(p)$data[[2]] %>% pull(y)

vaccine_efficacy_disease_no_booster<-as.list(vaccine_efficacy_disease)


efq<-dose_df2 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>% 
  filter (date_vaccine_change<vd1+days) %>% summarise(ef0=min(vaccine_efficacy_infection)-.02,
                                                      ef1=max(vaccine_efficacy_infection)+.02)


q<-dose_df2 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_infection)) +
  geom_rect(aes(xmin=vd1, xmax=vd1+days, ymin=efq$ef0, ymax=efq$ef1),fill="darkorange",alpha=0.1) + 
  geom_line()+
  stat_smooth( se = FALSE, fullrange = TRUE,n=days,colour="darkgreen")+ theme_light()+xlab("Weeks")+ylab("")+
  ggtitle("Infection - 2 doses")+ scale_x_date(date_labels = "%U")


vaccine_efficacy_infection<-ggplot_build(q)$data[[2]] %>% pull(y)
vaccine_efficacy_infection_no_booster<-as.list(vaccine_efficacy_infection)

#####

dose_df3<-readRDS("dose_df3.rds")

efr<-dose_df3 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>% 
  filter (date_vaccine_change<vd1+days) %>% summarise(ef0=min(vaccine_efficacy_disease)-.02,
                                                      ef1=max(vaccine_efficacy_disease)+.02)

r<-dose_df3 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_disease)) +
  geom_rect(aes(xmin=vd1, xmax=vd1+days, ymin=efr$ef0, ymax=efr$ef1),fill="darkorange",alpha=0.1) + 
    geom_line()+
  stat_smooth(se = FALSE, fullrange = TRUE,n=days,colour="blue")+ theme_light()+xlab("Weeks")+ylab("% efficacy")+
ggtitle("Disease - booster")+ scale_x_date(date_labels = "%U")

vaccine_efficacy_disease_yes_booster<-ggplot_build(r)$data[[2]] %>% pull(y)

vaccine_efficacy_disease_yes_booster<-as.list(vaccine_efficacy_disease_yes_booster)

efs<-dose_df3 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>% 
  filter (date_vaccine_change<vd1+days) %>% summarise(ef0=min(vaccine_efficacy_infection)-.02,
                                                      ef1=max(vaccine_efficacy_infection)+.02)


s<-dose_df3 %>% group_by(date_vaccine_change) %>%
  summarise(vaccine_efficacy_disease=mean(vaccine_efficacy_disease,na.rm = T),
            vaccine_efficacy_infection=mean(vaccine_efficacy_infection,na.rm = T)) %>%
  ggplot(aes(date_vaccine_change,vaccine_efficacy_infection)) +
  geom_rect(aes(xmin=vd1, xmax=vd1+days, ymin=efs$ef0, ymax=efs$ef1),fill="darkorange",alpha=0.1) + 
  geom_line()+
  stat_smooth( se = FALSE, fullrange = F,n=days,
               colour="darkgreen")+ theme_light()+xlab("Weeks")+ylab("")+
  ggtitle("Infection - booster")+ scale_x_date(date_labels = "%U")

vaccine_efficacy_infection_yes_booster<-ggplot_build(s)$data[[2]] %>% pull(y)

vaccine_efficacy_infection_yes_booster<-as.list(vaccine_efficacy_infection_yes_booster)


####

mar<-ra %>% group_by (country,R0) %>%
summarise(deaths=max(deaths_averted)) %>% group_by(R0) %>%
summarise(`Max averted deaths`=round(sum(deaths)))

minr<-ra %>% group_by (country,R0) %>%
summarise(deaths=min(deaths_averted)) %>% group_by(R0) %>%
summarise(`Min averted deaths`=round(sum(deaths)))


mar_hosp<-ra %>% group_by (country,R0) %>%
summarise(hosp=max(hospitalisations_averted)) %>% group_by(R0) %>%
summarise(`Max averted hospitalisations`=round(sum(hosp)))

minr_hosp<-ra %>% group_by (country,R0) %>%
summarise(hosp=min(hospitalisations_averted)) %>% group_by(R0) %>%
summarise(`Min averted hospitalisations`=round(sum(hosp)))

tb<-cbind(mar,minr[2],mar_hosp[2],minr_hosp[2])

#

popa1<- squire::population[,c(1:3,5)] %>% as.data.frame() %>% 
  mutate(age = case_when(  age_group == "60-64"  |
                             age_group == "65-69"  |
                             age_group == "70-74" | 
                             age_group == "75-79" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% group_by(iso3c,age) %>% 
  summarise(n=sum(n)) %>%
  group_by(iso3c) %>% mutate(`Population >60 (%)`=round(prop.table(n)*100,1)) %>% 
  filter (age=="older")

table1<-
  count_reg %>% left_join(popa1,by=c("iso_a3"="iso3c")) %>%
  select(11,7,4,19,2)%>% mutate(coverage=round(coverage*100,1))%>%
  rename(Country=name,Population=population,`Current vaccination level (%)`=coverage)

write.xlsx(table1,"table1.xlsx")


#


table3a<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.max(dif)) %>%
  rename(val_max=dif) %>% 
  group_by(region,population,country,compartment) %>% 
  mutate(proportion_max=(round(prop.table(val_max),3))*100) %>%
  rename(Country=country,`Value max model`=val_max,`>60`=age,
         `Proportion max value by age group`=proportion_max) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table3b<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.min(dif) ) %>%
  rename(val_min=dif) %>% 
  group_by(country,compartment) %>% 
  mutate(proportion_min=(round(prop.table(val_min),3))*100) %>%
  rename(Country=country,`Value min model`=val_min,`>60`=age,
         `Proportion min value by age group`=proportion_min) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table3<- table3a %>% left_join(table3b)

mr <- read_excel("mr.xlsx") %>% select(2,5) %>% rename (iso3c=iso_a3)

table3<- table3 %>% left_join(mr)

write.xlsx(as.data.frame(table3),"table3.xlsx")




table4a<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.max(dif)) %>%
  rename(val_max=dif) %>% 
  group_by(region,population,country,compartment) %>% 
  mutate(proportion_max=(round(prop.table(val_max),3))*100) %>%
  rename(Country=country,`Value max model`=val_max,`>60`=age,
         `Proportion max value by age group`=proportion_max) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table4b<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.min(dif) ) %>%
  rename(val_min=dif) %>% 
  group_by(country,compartment) %>% 
  mutate(proportion_min=(round(prop.table(val_min),3))*100) %>%
  rename(Country=country,`Value min model`=val_min,`>60`=age,
         `Proportion min value by age group`=proportion_min) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table4<- table4a %>% left_join(table4b)

write.xlsx(as.data.frame(table4),"table4.xlsx")


# Cost of acceleration
# 
# Additional vaccines required to accelerate
# 
# Additional cost of acceleration (@ XX USD / vaccine)
# 




vaccine_baseline<-ra %>% 
  group_by(country) %>% 
  filter (mean_vac==min(mean_vac)) %>% 
  slice_head() %>%
  summarise(vaccine_baseline=mean_vac*days_v)



table6<-list_vaccine_countries %>% 
  left_join(count_reg,by=c("country"="name")) %>%
  select(12,1,2) %>% mutate(cost=vaccine_dose_needed*10) %>% 
  left_join(vaccine_baseline) %>%
  mutate(`vaccines to accelerate` = vaccine_dose_needed - vaccine_baseline,
         `Additional cost` = `vaccines to accelerate`*10)

