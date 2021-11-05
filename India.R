# Parameters

country<-c("Ethiopia")

current<-df_current(country=country)

deaths_Ethiopia<-sum(current[[1]]$real,na.rm = T)

current[[1]]$Month_Year <- substr(current[[1]]$date, 1,7)

deaths_peek_Ethiopia<-current[[1]] %>% mutate(date=lubridate::ymd(date),
                        week=ISOweek::ISOweek(date))%>%
  select(week,real) %>% group_by(week) %>%
  summarise(deaths=sum(real,na.rm = T)) %>% arrange(-deaths) %>% 
  top_n(1) %>% pull (deaths)

week_peek_Ethiopia<-current[[1]] %>% mutate(date=lubridate::ymd(date),
                        week=ISOweek::ISOweek(date))%>%
  select(week,real) %>% group_by(week) %>%
  summarise(deaths=sum(real,na.rm = T)) %>% arrange(-deaths) %>% 
  top_n(1) %>% pull (week)


avg_rt_Ethiopia_max<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,2)) %>% distinct(rt) %>% arrange(rt) %>%
  top_n(1) %>% pull(rt)


avg_rt_Ethiopia_min<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,2)) %>% distinct(rt) %>% arrange(rt) %>%
  top_n(-1) %>% pull(rt)


# 
p1_Ethiopia<-plot_deaths(current[[1]],country)

p2_Ethiopia<-reff_plot(current[[1]])


avg_rt<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,1)) %>% distinct(rt) %>% filter(rt>1 & rt<2) %>%
  arrange(rt)%>% pull(rt) 


avg_vac <- current[[2]]  %>% 
  filter (dates==case_when(max_vaccine>0~dates)) %>%
  summarise(v=mean(max_vaccine, na.rm = TRUE)) %>% pull(v)

avg_vac<-c(round(avg_vac/2),
           round((avg_vac/2)*2),
           round((avg_vac/2)*3))

#

vaccine_coverage_mat=c("All","Elderly")

R0=avg_rt

max_vaccine = avg_vac

vac_m<-round(current[[3]],1)
vac_m<-current[[3]]


if (vac_m < .4) {
  seq1<-vac_m+0.05
  coverage<-c(seq(seq1,.95,.1))
}  else if (vac_m >= .4) {
  seq1<-vac_m+0.05
  coverage=c(seq(seq1,.95,.05))
}


min_coverage_Ethiopia<-min(coverage)



# Matrix of scenarios

scenarios_LS_1_Ethiopia <- expand_grid(
  coverage=coverage,
  country = country,
  R0 = R0,
  vac=vac_m,
  max_vaccine = max_vaccine,
  vaccine_coverage_mat = vaccine_coverage_mat)

#nrow(scenarios_LS_1) # number of scenarios

scenarios_Ethiopia<-nrow(scenarios_LS_1_Ethiopia) # number of scenarios


R0_Ethiopia_max<-max(R0) 
R0_Ethiopia_min<-min(R0) 

# Run scenarios

out1_Ethiopia <- future_pmap(scenarios_LS_1_Ethiopia,
                             run_scenario_LS, 
                             .progress = TRUE)

# Extract outputs
  
out_format_Ethiopia <- format_out_LS(out1_Ethiopia, 
                                     scenarios_LS_1_Ethiopia,
                                     country1=country,
                                     vac_m,
                                  min_coverage_Ethiopia)

#

deaths_averted_Ethiopia<-out_format_Ethiopia %>% 
  group_by(final_coverage,R0,vaccine_coverage_mat) %>%
  mutate(deaths_averted=mean(deaths_averted)) %>% 
  select(final_coverage,deaths_averted) %>%
  arrange(-deaths_averted)%>% pull(deaths_averted)

deaths_averted_Ethiopia<-max(round(deaths_averted_Ethiopia))


##

out_format_unnest_cf<-out_format_Ethiopia %>% select(c(1:6,8,23)) %>%
  unnest(output_cf) %>% rename(value_cf = value)

out_format_unnest<-out_format_Ethiopia %>% select(c(1:7,23)) %>%
  unnest(output)

out_format_unnest_final_Ethiopia <- out_format_unnest %>% 
  bind_cols(out_format_unnest_cf[9]) %>%
  mutate(dif=value_cf-value)

# t21<-out_format_unnest_final_Ethiopia %>% filter(compartment == "deaths") %>%
# mutate(age = case_when(  age_group == "60-65"  |
#                            age_group == "65-70"  |
#                            age_group == "70-75"  ~ "middle_age",
#                          age_group == "75-80" |
#                            age_group == "80+" ~ "older",
#                          T ~ "not")) %>%
#  group_by(coverage,R0,vaccine_coverage_mat,max_vaccine) %>%
#  summarise(dif=sum(dif))
# 
# # test if extraction is correct
# 
# max(t21$dif) # highest - 1171253
# max(out_format_Ethiopia$deaths_averted)#  1171253

final_coverage_max_Ethiopia<-out_format_unnest_final_Ethiopia %>%  ungroup %>% 
  top_n(final_coverage,n=1) %>% distinct(final_coverage) %>%
  mutate(final_coverage) %>%
  pull(final_coverage)


values_labels_Ethiopia <-out_format_unnest_final_Ethiopia %>%  ungroup %>%
  distinct(max_vaccine) %>%
  arrange(max_vaccine) %>%
  mutate (max_vaccine=as.factor(max_vaccine)) %>%
  pull (max_vaccine)


# 

out_format_2groupsages_Ethiopia<-out_format_unnest_final_Ethiopia %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                           age_group == "65-70"  |
                           age_group == "70-75"  ~ "middle_age",
                           age_group == "75-80" |
                            age_group == "80+" ~ "older",
                         T ~ "younger")) %>%
  mutate(final_coverage = round(final_coverage,1)) %>% 
  group_by(age,final_coverage,R0,vaccine_coverage_mat,max_vaccine,compartment) %>%
  mutate(dif=mean(dif,na.rm=T)) %>% filter(dif>0)%>%
  summarise(dif=sum(dif))

#

population_Ethiopia <- nimue:::init(squire::get_population(country)$n, seeding_cases = 200000)

population_Ethiopia<-population_Ethiopia$S_0

population_Ethiopia <- c(sum(population_Ethiopia[1:12,1]),
                      sum(population_Ethiopia[13:15,1]),
                      sum(population_Ethiopia[16:17,1]))

dif_2age<-out_format_2groupsages_Ethiopia %>% 
  filter(compartment != "vaccines")%>%
  mutate(final_coverage=as.factor(final_coverage),
         R0=as.factor(R0),
         max_vaccine=as.factor(max_vaccine),
         vaccine_coverage_mat=as.factor(vaccine_coverage_mat)) %>% 
  group_by(age,final_coverage,R0,max_vaccine,vaccine_coverage_mat,compartment) %>%
    mutate (dif=mean(dif,na.rm=T),
            dif=case_when(age == "older" ~ (dif/population_Ethiopia[3])*1000,
                        age == "middle_age" ~ (dif/population_Ethiopia[2])*1000,
                          T ~ (dif/population_Ethiopia[1])*1000))


dif_2age %>% ggplot() +  geom_hline(yintercept = 0,linetype=2,colour="darkorange")+
  geom_point(aes(final_coverage,dif,colour=max_vaccine,
                 shape=vaccine_coverage_mat)) + 
  facet_grid(compartment~age, scales="free") +
  scale_y_continuous(labels = scales::comma) + theme_light() + 
  ylab ("Averted x 1000 inhabitants")

# 

out_format_older_Ethiopia<-out_format_unnest_final_Ethiopia %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75"  ~ "60-75",
                           age_group == "75-80" |
                             age_group == "80+" ~ "75+",
                           T ~ "not")) %>%
  filter(age != "not") %>%
  group_by(age,final_coverage,R0,vaccine_coverage_mat,max_vaccine,compartment) %>%
  mutate(final_coverage = round(final_coverage,1)) %>% 
  mutate(dif=mean(dif,na.rm=T)) %>% filter(dif>0)%>%
  summarise(dif=sum(dif))

out_format_older_Ethiopia %>% filter(compartment != "vaccines")%>%
  mutate(max_vaccine = as.factor(max_vaccine))%>%
  ggplot() + 
  geom_point(aes(final_coverage,dif,colour=as.factor(R0),
                            shape=max_vaccine)) +
  facet_grid(compartment~age,scales = "free") + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::percent,
                     limits =  c(0,.85)) +
  scale_shape_discrete(name= "Maximum vaccines per day",
                       breaks=values_labels_Ethiopia,
                       label = c("Historical average","Average x2","Average x3"))+
  ylab("Averted") + theme_minimal() + xlab("Vaccination coverage") +
  scale_color_discrete(name= "Rt scenario") + 
  ggtitle("Scenarios of Infections, Hospitalisations and deaths averted")
  





