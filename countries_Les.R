

# 

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


plot_vaccines

#

vaccine_coverage_mat=c("All","Elderly")

#

# The new report (Report 49) from the Imperial College London COVID-19 
# Based on these results they estimate that the reproduction number (R) of
# Omicron was above 3 over the period studied.

dur_R=300
dur_V=300

#

vaccine_efficacy_infection<-c(.1,.2,.3)
vaccine_efficacy_disease<-c(.6,.7,.8)

#

max_vaccine = c(mean_vacine,
                to_give2)



case_iso<-cases%>% dplyr::filter (code==iso3cs)%>%
  dplyr::filter (date>(date_0-15)) %>%
  summarise(infections=floor(sum(`Daily new confirmed cases due to COVID-19 (rolling 7-day average, right-aligned)`,na.rm = T))) %>% 
  pull(infections)

if(case_iso<0) { case_iso=1000}

seeding_cases = c(case_iso,case_iso*3)


#

RLes <-rep %>% 
  dplyr::filter(days_infectious==7) %>% 
  mutate(Date=lubridate::ymd(Date))%>%
  dplyr::filter (Date>="2021-11-1") %>%
  dplyr::filter (Country.Region=="Lesotho") %>%   filter(R>1)%>%
pull(R)

RLes<-c(RLes,rep(1,days-length(RLes)-1))

tt_R0<-seq(1:(days-1))

#RSA,REswa,RNam,RLes

R0<-RLes

length(R0) == length(tt_R0)

# Matrix of scenarios

scenarios_LS_1 <- expand_grid(country = country, # ok
  max_vaccine = max_vaccine, # w scenarios: average, 2 to 30/ june
  vaccine_coverage_mat = vaccine_coverage_mat,
  mean_vac=mean_vacine,
  seeding_cases = seeding_cases,
  vaccine_efficacy_disease=vaccine_efficacy_disease, # last month average
  vaccine_efficacy_infection=vaccine_efficacy_infection)# last month average) # 2 scenarios of prioritisation

nrow(scenarios_LS_1) # number of scenarios

scenarios<-nrow(scenarios_LS_1) # number of scenarios

# Run scenarios

out1 <- future_pmap(scenarios_LS_1,
                             run_scenario_LS, 
                             .progress = TRUE)

out_format <- format_out_LS(out1, scenarios_LS_1)

#

# out_format %>% ggplot (aes(as.factor(R0),deaths_averted,
#                            shape=as.factor(seeding_cases),colour=vaccine_coverage_mat)) + 
#   geom_point()+
#   facet_grid(vaccine_efficacy_disease~vaccine_efficacy_infection)+theme_light()
# 

scen_plot<-out_format %>% arrange(-deaths) %>% 
  dplyr::select(deaths,max_vaccine,vaccine_coverage_mat,
                seeding_cases,vaccine_efficacy_infection,
                vaccine_efficacy_disease) %>% ggplot()+
  geom_point(aes(
                 y=deaths,
                 colour=vaccine_coverage_mat,
                 x=as.factor(seeding_cases)))+
  facet_grid(as.factor(vaccine_efficacy_disease)~as.factor(vaccine_efficacy_infection))


modelmax<-out_format %>% mutate(id=row_number())%>%
  arrange(-deaths) %>% slice_head() %>%pull(id)

modelmed<-out_format %>% mutate(id=row_number())%>% arrange(-deaths) %>%
  dplyr::slice (scenarios/4) %>% pull(id)

modelmin<-out_format %>% mutate(id=row_number())%>%
  arrange(-deaths) %>% slice_tail() %>%pull(id)

modelmax
modelmed
modelmin

out_format %>% 
  arrange(deaths) %>% slice_tail() %>% 
  dplyr::select(vaccine_coverage_mat,vaccine_efficacy_infection,vaccine_efficacy_disease,
            seeding_cases,max_vaccine,deaths,deaths_averted)


out_format %>% mutate(id=row_number())%>%
  arrange(-deaths) %>% slice_tail() %>% dplyr::select(
                                               vaccine_coverage_mat,
                                               vaccine_efficacy_infection,vaccine_efficacy_disease,
                                               seeding_cases,max_vaccine,deaths)

deaths_averted<-out_format %>% 
  group_by(max_vaccine,vaccine_coverage_mat,seeding_cases,
           vaccine_efficacy_infection,vaccine_efficacy_disease) %>%
  mutate(deaths_averted=round(mean(deaths_averted))) %>% 
  dplyr::select(max_vaccine,deaths_averted) %>%
  arrange(-deaths_averted) %>% ungroup

deaths_averted_max<-out_format %>% 
  arrange(-deaths_averted) %>% dplyr::select(deaths_averted)%>%
  slice_head() %>%pull(deaths_averted)

deaths_averted_med<-out_format %>%  
  arrange(-deaths_averted) %>% dplyr::select(deaths_averted)%>%
  dplyr::slice(scenarios/4) %>%pull(deaths_averted)

deaths_averted_min<-out_format %>% 
  arrange(-deaths_averted) %>% dplyr::select(deaths_averted)%>%
  slice_tail() %>%pull(deaths_averted)

deaths_averted_max;deaths_averted_med;deaths_averted_min


#
variable.names(out_format)

out_format_unnest_cf<-out_format %>% dplyr::select(c(2:7,9,20)) %>%
  unnest(output_cf) %>% rename(value_cf = value)


out_format_unnest<-out_format %>% dplyr::select(c(2:7,8,20)) %>%
  unnest(output)

out_format_unnest <- out_format_unnest %>% 
  bind_cols(out_format_unnest_cf[9]) %>%
  mutate(dif=value_cf-value)

#

deaths_averted_older<-out_format_unnest %>% dplyr::filter(compartment == "deaths") %>%
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75"  ~ "middle_age",
                           age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% dplyr::filter (age!="not")%>%
  group_by(vaccine_coverage_mat,max_vaccine,seeding_cases,vaccine_efficacy_disease,
           vaccine_efficacy_infection)%>%
  summarise(dif=round(sum(dif))) %>% ungroup()%>%
  dplyr::filter(dif==max(dif)) %>% pull (dif)


prop<-round(deaths_averted_older/deaths_averted_max*100,1)

t21<-out_format_unnest %>% dplyr::filter(compartment == "deaths") %>%
mutate(age = case_when(  age_group == "60-65"  |
                           age_group == "65-70"  |
                           age_group == "70-75"  ~ "middle_age",
                         age_group == "75-80" |
                           age_group == "80+" ~ "older",
                         T ~ "not")) %>%
 group_by(vaccine_coverage_mat,max_vaccine,seeding_cases,
          vaccine_efficacy_infection,vaccine_efficacy_disease) %>%
 summarise(dif=sum(dif))

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

states<-c("S","E","ICase","IMild" ,"R", "D")

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


seirmax;seirmed;seirmin


# as.data.frame(out2[[modelmax]]$output) %>% dplyr::filter (compartment=="D") %>%
#   ggplot()+ geom_line(aes(t,value))+scale_y_continuous(labels = scales::comma)

#

seirplot <-  as.data.frame(out2[[modelmax]]$output) %>%dplyr::filter (compartment %in% states) %>% 
  left_join(seirplot0)

seplot<-seirplot %>%
  ggplot(aes(x = date, y = value,colour=compartment))  +
  geom_line() + facet_wrap(~compartment,scales = "free") +
  xlab("2022") + theme_light() + scale_y_continuous(labels = scales::label_number_si())+
  scale_x_date(breaks = "month",date_labels = "%B") + ylab("")

seplot

modelmax.p<-out_format %>% mutate(id=row_number())%>%
  dplyr::filter (deaths_averted==max(deaths_averted))

modelmax.p %>% dplyr::select(max_vaccine,vaccine_coverage_mat,seeding_cases,vaccine_efficacy_disease,
                      vaccine_efficacy_infection)

####

out3 <- format_out_baseline(out1,scenarios_LS_1)


counter_deaths_max<-out3 %>% dplyr::slice(modelmax)%>%
  summarise(deaths=round(max(deaths))) %>% pull(deaths)

counter_deaths_med<-out3 %>% dplyr::slice(modelmed)%>%
  summarise(deaths=round(max(deaths))) %>% pull(deaths)

counter_deaths_min<-out3 %>% dplyr::slice(modelmin)%>%
  summarise(deaths=round(max(deaths))) %>% pull(deaths)

counter_deaths_max;counter_deaths_med;counter_deaths_min

#####


analysisSEIR <- list(country,population_total,vaccine_efficacy_disease,
                     vaccine_efficacy_infection,
                     max_vaccine,mean_vacine,
                     dur_R,deaths_averted,out_format,out_format_unnest,
                     out3,seirmax,seirmed,seirmin,counter_deaths_max,
                     counter_deaths_med,counter_deaths_min,current_coverage,scen_plot)

nm<-c("country","population_total","vaccine_efficacy_disease",
      "vaccine_efficacy_infection",
    "max_vaccine","mean_vaccine",
     "dur_R","deaths_averted","out_format","out_format_unnest",
    "baselineR","seirmax","seirmed","seirmin","counter_deaths_max",
    "counter_deaths_med","counter_deaths_min","current_coverage","scen_plot")

names(analysisSEIR) <- nm


filename<-paste0("data/data_","RLes_",country,".rds") 

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


out2[[modelmed]]$output %>% as.data.frame %>% 
  dplyr::filter (t<40 & compartment=="D") %>%
  summarise(v=sum(value))
