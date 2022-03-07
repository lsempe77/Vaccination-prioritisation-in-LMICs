
h_line1 <- mean_vacine                     # Position of horizontal line
h_line2 <- to_give2                      # Position of horizontal line

label1 <- "Last month daily average"                       # Position of horizontal line
label2 <- "Daily needed to cover 2 doses"                      # Position of horizontal line


plot_vaccines <- owid %>%  dplyr::filter(iso3c==iso3cs)%>%
  mutate(date=lubridate::ymd(date))%>%
          ggplot(aes(date,new_vaccinations_smoothed)) +    geom_line() + 
  geom_segment(aes(x=date_0-30,xend=date_0,y=h_line1,
                   yend=h_line1,colour=label1),
               size=2)+
  geom_segment(aes(x=date_0,xend=date_end,y=h_line2,
                   yend=h_line2,colour=label2),size=2)+
  geom_vline(xintercept = date_end,colour="darkred",linetype=3,size=2)+
  theme_minimal() +
  annotate("text", x=date_end+50,y=h_line1, size=4,colour="darkred",
           label = stringr::str_wrap("WHO limit 70% vaccinated", 4))+
ylab("Vaccines per day") + xlab ("2021/2022")  +
  scale_y_continuous(labels=scales::comma) + 
  theme(legend.title = element_blank())


#

vaccine_coverage_mat=c("Elderly")

#

# The new report (Report 49) from the Imperial College London COVID-19 
# Based on these results they estimate that the reproduction number (R) of
# Omicron was above 3 over the period studied.


#

tt<-seq(1:days)

#

booster <- start_booster %>% filter (iso3c==iso3cs) %>% pull (start_booster)

#


if (booster == "no") {
  

max_vaccine = c(mean_vacine, 
                to_give2,
                to_give2*2) 

vaccine_efficacy_infection = vaccine_efficacy_infection_no_booster
vaccine_efficacy_disease = vaccine_efficacy_disease_no_booster

} else  {

to_give_boost = to_give2*1.5

vaccine_efficacy_infection = vaccine_efficacy_infection_yes_booster
vaccine_efficacy_disease = vaccine_efficacy_disease_yes_booster

max_vaccine = c(mean_vacine, 
                to_give_boost,
                to_give_boost*2)  

                }


case_iso<-cases%>% dplyr::filter (code==iso3cs)%>%
  dplyr::filter (date>(date_0-15)) %>%
  summarise(infections=floor(sum(`Daily new confirmed cases due to COVID-19 (rolling 7-day average, right-aligned)`,na.rm = T))) %>% 
  pull(infections)


if(case_iso<0) { case_iso=1000}

seeding_cases = c(case_iso,case_iso*10,case_iso*20)

if(seeding_cases[1]==0) { 

seeding_cases = c(100,500,1000)

}

#

R0<-c(1.3,1.5,2)

#




# Matrix of scenarios


scenarios_LS_1 <- expand_grid(country = country, # ok
                              R0=R0,
  max_vaccine = max_vaccine, # w scenarios: average, 2 to 30/ june
  vaccine_coverage_mat = vaccine_coverage_mat,
  mean_vac=mean_vacine,
  seeding_cases = seeding_cases)# last month average) # 2 scenarios of prioritisation

nrow(scenarios_LS_1) # number of scenarios

scenarios<-nrow(scenarios_LS_1) # number of scenarios

# Run scenarios

out1 <- future_pmap(scenarios_LS_1,
                             run_scenario_LS, 
                             .progress = TRUE)

out_format <- format_out_LS(out1, scenarios_LS_1)

##

dmin<-out_format %>% filter (R0==1.5) %>%
  summarise(deaths=min(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max

dmed<-out_format %>%  filter (R0==1.5) %>%
  summarise(deaths=median(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max

dmax<-out_format %>% filter (R0==1.5) %>%
  summarise(deaths=max(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max
##

dminh<-out_format %>% group_by(country) %>%   filter (R0==1.5) %>%
  summarise(hospitalisations_averted=min(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted))) # min, median, max

dmedh<-out_format %>% group_by(country) %>% filter (R0==1.5) %>%
  summarise(hospitalisations_averted=median(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted))) # min, median, max

dmaxh<-out_format %>% group_by(country) %>% filter (R0==1.5) %>%
  summarise(hospitalisations_averted=max(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted))) # min, median, max



##
mar<-out_format %>% group_by (R0) %>%
  summarise(deaths=max(deaths_averted)) %>% group_by(R0) %>%
  summarise(`Max averted deaths`=round(sum(deaths)))

minr<-out_format %>% group_by (R0) %>%
  summarise(deaths=min(deaths_averted)) %>% group_by(R0) %>%
  summarise(`Max averted deaths`=round(sum(deaths)))

mar_hosp<-out_format %>% group_by (R0) %>%
  summarise(hosp=max(hospitalisations_averted)) %>% group_by(R0) %>%
  summarise(`Max averted hospitalisations`=round(sum(hosp)))

minr_hosp<-out_format %>% group_by(R0) %>% 
  summarise(hosp=min(hospitalisations_averted)) %>% group_by(R0) %>%
  summarise(`Max averted hospitalisations`=round(sum(hosp)))

tb<-cbind(mar,minr[2],mar_hosp[2],minr_hosp[2])


# out_format %>% ggplot (aes(as.factor(R0),deaths_averted,
#                            shape=as.factor(seeding_cases),colour=vaccine_coverage_mat)) + 
#   geom_point()+
#   facet_grid(vaccine_efficacy_disease~vaccine_efficacy_infection)+theme_light()
# 

scen_plot<-out_format %>% arrange(-deaths) %>% 
  dplyr::select(R0,deaths,max_vaccine,
                seeding_cases) %>% ggplot()+
  geom_point(aes(x=R0,
                 y=deaths,
                 shape=as.factor(seeding_cases)))


modelmax<-out_format %>% mutate(id=row_number())%>%
  arrange(-deaths) %>% slice_head() %>%pull(id)

modelmed<-out_format %>% mutate(id=row_number())%>% arrange(-deaths) %>%
  dplyr::slice (floor(scenarios/4)) %>% pull(id)

modelmin<-out_format %>% mutate(id=row_number())%>%
  arrange(-deaths) %>% slice_tail() %>%pull(id)

# 
# out_format %>% 
#   arrange(deaths) %>% slice_tail() %>% 
#   dplyr::select(R0,
#                 seeding_cases,max_vaccine,deaths,deaths_averted)
# 
# 
# out_format %>% mutate(id=row_number())%>%
#   arrange(-deaths) %>% slice_tail() %>% dplyr::select(R0,
#                                                seeding_cases,max_vaccine,deaths)

deaths_averted<-out_format %>% 
  group_by(R0,max_vaccine,seeding_cases) %>%
  mutate(deaths_averted=round(mean(deaths_averted))) %>% 
  dplyr::select(max_vaccine,deaths_averted) %>%
  arrange(-deaths_averted) %>% ungroup

deaths_averted_max<-out_format %>% 
  arrange(-deaths_averted) %>% dplyr::select(deaths_averted)%>%
  slice_head() %>%pull(deaths_averted)

deaths_averted_med<-out_format %>%  
  arrange(-deaths_averted) %>% dplyr::select(deaths_averted)%>%
  dplyr::slice(floor(scenarios/4)) %>%pull(deaths_averted)

deaths_averted_min<-out_format %>% 
  arrange(-deaths_averted) %>% dplyr::select(deaths_averted)%>%
  slice_tail() %>%pull(deaths_averted)

#deaths_averted_max;deaths_averted_med;deaths_averted_min


#

out_format_unnest_cf<-out_format %>% dplyr::select(c(2:6,8,19)) %>%
  unnest(output_cf) %>% rename(value_cf = value)


out_format_unnest<-out_format %>% dplyr::select(c(2:6,7,19)) %>%
  unnest(output)

# variable.names(out_format_unnest_cf)

out_format_unnest <- out_format_unnest %>% 
  bind_cols(out_format_unnest_cf[8]) %>%
  mutate(dif=value_cf-value)

#

deaths_averted_older<-out_format_unnest %>% dplyr::filter(compartment == "deaths") %>%
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% dplyr::filter (age!="not")%>%
  group_by(R0,max_vaccine,seeding_cases)%>%
  summarise(dif=round(sum(dif))) %>% ungroup()%>%
  dplyr::filter(dif==max(dif)) %>% pull (dif)


prop_deaths<-round(deaths_averted_older/deaths_averted_max*100,1)

t21<-out_format_unnest %>% dplyr::filter(compartment == "deaths") %>%
mutate(age = case_when(  age_group == "60-65"  |
                           age_group == "65-70"  |
                           age_group == "70-75"  ~ "middle_age",
                         age_group == "75-80" |
                           age_group == "80+" ~ "older",
                         T ~ "not")) %>%
 group_by(R0,max_vaccine,seeding_cases) %>%
 summarise(dif=round(sum(dif)))

n_vac<-out_format %>%  ungroup()%>%
 summarise(v=round(mean(vaccine_n))) %>% dplyr::slice(1)
  
if (n_vac>10^8) {
  
  n_vac1=round(n_vac,-7)
  
} else if (n_vac>10^7) {
  
  n_vac1=round(n_vac,-6)
  
  
} else if (n_vac>10^6) {
  
  n_vac1=round(n_vac,-5)
  
} else if (n_vac>10^5) {
  
  n_vac1=round(n_vac,-4)
  
  
} else if (n_vac>10^4) {
  
  n_vac1=round(n_vac,-3)
  
  
} else {
  
  n_vac1=round(n_vac,-2)
  
}


n_vac1<-signif(n_vac1)

n_vac1<-as.integer(n_vac1)
numb<-n2w(n_vac1)

cost<-n_vac1*10
cost1<-n2w(cost)

# test if extraction is correct

max(t21$dif) # highest - 1171253
max(out_format$deaths_averted)#  1171253


#

out2 <- future_pmap(scenarios_LS_1,
                    run_scenario_LS2, 
                    .progress = TRUE)

#out2_format<-purrr::map(out2, "output")

# m37<-as.data.frame(out2_format[[37]])
# 
# table(m37$compartment)
# 
# 
#   ggplot(m37) + geom_line(aes(t,value))+facet_wrap(~compartment,scales ="free")+
#     scale_y_continuous(labels=scales::comma)
# 
# 

states<-c("S","E","ICase","IMild" ,"R", "D","deaths")


seirplot0 <-  as.data.frame(out2[[modelmax]]$output) %>% 
  dplyr::filter (compartment %in% states) %>%
  summarise(date=seq.Date(from=date_0,to=date_0+max(t),by="day")) %>%
  mutate(t=1:n())

seirmax<-  as.data.frame(out2[[modelmax]]$output) %>% 
  dplyr::filter (compartment %in% states) %>% 
  left_join(seirplot0) %>% dplyr::filter (date=="2022-05-01", compartment=="D") %>%
  pull(value)

seirmed<-  as.data.frame(out2[[modelmed]]$output) %>%
  dplyr::filter (compartment %in% states) %>% 
  left_join(seirplot0) %>%dplyr::filter (date=="2022-05-01", compartment=="D") %>%
  pull(value)

seirmin<-  as.data.frame(out2[[modelmin]]$output) %>% dplyr::filter (compartment %in% states) %>% 
  left_join(seirplot0) %>%dplyr::filter (date=="2022-05-01", compartment=="D") %>%
  pull(value)


# seirmax;seirmed;seirmin

#

seirplot <-  as.data.frame(out2[[modelmax]]$output) %>% 
  dplyr::filter (compartment %in% states) %>% 
  left_join(seirplot0)

seplot<-seirplot %>%
  ggplot(aes(x = date, y = value,colour=compartment))  +
  geom_line() + facet_wrap(~compartment,scales = "free") +
  xlab("2022") + theme_light() + scale_y_continuous(labels = scales::label_number_si())+
  scale_x_date(breaks = "month",date_labels = "%B") + ylab("")


modelmax.p<-out_format %>% mutate(id=row_number())%>%
  dplyr::filter (deaths_averted==round(max(deaths_averted)))
# 
# modelmax.p %>% dplyr::select(max_vaccine,seeding_cases)

####

out3 <- format_out_baseline(out1,scenarios_LS_1)

modelmax_counter<-out3 %>% mutate(id=row_number())%>%
  arrange(-deaths) %>% slice_head() %>%pull(id)

modelmed_counter<-out3 %>% mutate(id=row_number())%>% arrange(-deaths) %>%
  dplyr::slice (floor(scenarios/6)) %>% pull(id)

modelmin_counter<-out3 %>% mutate(id=row_number())%>%
  arrange(-deaths) %>% slice_tail() %>%pull(id)

counter_deaths_max<-out3 %>% dplyr::slice(modelmax_counter)%>%
  summarise(deaths=round(max(deaths))) %>% pull(deaths)

counter_deaths_med<-out3 %>% dplyr::slice(modelmed_counter)%>%
  summarise(deaths=round(max(deaths))) %>% pull(deaths)

counter_deaths_min<-out3 %>% dplyr::slice(modelmin_counter)%>%
  summarise(deaths=round(max(deaths))) %>% pull(deaths)

# counter_deaths_max;counter_deaths_med;counter_deaths_min

#


min_base<-out3 %>% group_by(country) %>% filter (R0==1.5) %>% 
  summarise (deaths=min(deaths)) %>%
  ungroup() %>% summarise(deaths=round(sum(deaths))) %>% pull(deaths)

max_base<-out3 %>% group_by(country) %>% filter (R0==1.5) %>% 
  summarise (deaths=max(deaths)) %>%
  ungroup() %>% summarise(deaths=round(sum(deaths))) %>% pull(deaths)

min_base_hosp<-out3 %>% group_by(country) %>% filter (R0==1.5) %>% 
  summarise (hosp=min(hospitalisations)) %>%
  ungroup() %>% summarise(hosp=round(sum(hosp))) %>% pull(hosp)

max_base_hosp<-out3 %>% group_by(country) %>% filter (R0==1.5) %>% 
  summarise (hosp=max(hospitalisations)) %>%
  ungroup() %>% summarise(hosp=round(sum(hosp))) %>% pull(hosp)






#####

max_vaccine2 = c(0, 
                 to_give2,
                 to_give2*1.5) 

scenarios_LS_2 <- expand_grid(country = country, # ok
                              R0=R0,
                              max_vaccine = max_vaccine2, # w scenarios: 0, 2 to 30/ june
                              vaccine_coverage_mat = vaccine_coverage_mat,
                              mean_vac=mean_vacine,
                              seeding_cases = seeding_cases)# last month average) # 2 scenarios of prioritisation

out4 <- future_pmap(scenarios_LS_2,
                    run_scenario_LS, 
                    .progress = TRUE)

out_format_0 <- format_out_LS_compare_0(out4, scenarios_LS_2)

dmin0 <- out_format_0 %>%  filter(R0==1.5)%>%
  summarise(deaths=min(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max

dmed0<-out_format_0 %>% filter(R0==1.5)%>%
  summarise(deaths=median(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths))) # min, median, max

dmax0<-out_format_0 %>% filter(R0==1.5)%>%
  summarise(deaths=max(deaths_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(deaths)))


dmin0h<-out_format_0 %>%  filter(R0==1.5)%>%
  summarise(hospitalisations_averted=min(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted)))# min, median, max

dmed0h<-out_format_0 %>%  filter(R0==1.5)%>%
  summarise(hospitalisations_averted=median(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted))) # min, median, max

dmax0h<-out_format_0 %>%  filter(R0==1.5)%>%
  summarise(hospitalisations_averted=max(hospitalisations_averted)) %>% 
  ungroup() %>% summarise(d=round(sum(hospitalisations_averted)))

old_table<-out_format_unnest %>%  
  filter(R0==1.5) %>% 
  filter(seeding_cases == max(seeding_cases)) %>%
  dplyr::filter(compartment == "deaths"|  compartment == "hospitalisations") %>%
  mutate(`population group` = case_when(  age_group == "60-64"  |
                                            age_group == "65-69"  |
                                            age_group == "70-74" | 
                                            age_group == "75-79" |
                                            age_group == "80+" ~ "older people",
                                          T ~ "younger people"))  %>%
  group_by(compartment,`population group`) %>% 
  summarise(value=sum(value)) %>% 
  #group_by(age) %>%
  mutate(`Proportion older/younger`=prop.table(value)*100) 



pro1<-old_table[1,4]
pro2<-old_table[3,4]

if (pro1>pro2){  pro1<-old_table[3,4]
pro2<-old_table[1,4]}

pro3<-old_table[2,4]
pro4<-old_table[4,4]

if (pro3>pro4){  pro3<-old_table[4,4]
pro4<-old_table[2,4]}

current_coverage100<-current_coverage*100

#####

analysisSEIR <- list(country,population_total,mean_vacine,to_give2,
                     out_format,out_format_unnest, out2,
                     out3,seirmax,seirmed,seirmin,counter_deaths_max,
                     counter_deaths_med,counter_deaths_min,current_coverage,scen_plot,
                     prop_deaths,booster,out_format_0)

nm<-c("country","population_total","mean_vacine","to_give2",
    "out_format","out_format_unnest","out2",
    "baselineR","seirmax","seirmed","seirmin","counter_deaths_max",
    "counter_deaths_med","counter_deaths_min","current_coverage","scen_plot","prop_deaths",
    "booster","out_format_0")

names(analysisSEIR) <- nm


filename<-paste0("data/data_",country,".rds") 

saveRDS(analysisSEIR,file=filename)



# out_format_older %>% dplyr::filter(compartment != "vaccines")%>%
#   mutate(max_vaccine = as.factor(max_vaccine))%>%
#   ggplot() + 
#   geom_point(aes(final_coverage,dif,colour=as.factor(R0),
#                             shape=max_vaccine)) +
#   facet_grid(compartment~age,scales = "free") + 
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_continuous(labels = scales::percent,
#                      limits =  c(0,.85)) +
#   scale_shape_discrete(name= "Maximum vaccines per day",
#                        breaks=values_labels,
#                        label = c("Historical average",
#                                  "Average x5","Average x10", "Needed to cover 80%")) +
#   ylab("Averted") + theme_minimal() + xlab("Vaccination coverage") +
#   scale_color_discrete(name= "Rt scenario") + 
#   ggtitle("Scenarios of Infections, Hospitalisations and deaths averted")
#   
# 
# 

# 
# out2[[modelmed]]$output %>% as.data.frame %>% 
#   dplyr::filter (t<40 & compartment=="D") %>%
#   summarise(v=sum(value))

###

d_points<-world%>%left_join(cov1) %>%
  filter(population<600000) %>% filter (!is.na(status),sov_a3!="SUR",sov_a3!="BLZ",
                                        sov_a3!="ISL")

map_res<-world %>%left_join(cov1) %>% ggplot()+
  geom_sf(aes(fill=status)) + 
  geom_sf(data = d_points, aes(colour=status),size = 4,
          show.legend = F)+
  scale_fill_discrete("",labels=c("Behind goal","On track",
                                  "No data"))+
  theme(legend.position = "bottom")


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

islands <-d_points %>% tally() %>% pull (n)