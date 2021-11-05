# Parameters

country<-i

current<-df_current(country=country)

deaths<-sum(current[[1]]$real,na.rm = T) %>% as.character()

current[[1]]$Month_Year <- substr(current[[1]]$date, 1,7)

deaths_peek<-current[[1]] %>% mutate(date=lubridate::ymd(date),
                        week=ISOweek::ISOweek(date))%>%
  select(week,real) %>% group_by(week) %>%
  summarise(deaths=sum(real,na.rm = T)) %>% arrange(-deaths) %>% 
  top_n(1) %>% pull (deaths)

week_peek<-current[[1]] %>% mutate(date=lubridate::ymd(date),
                        week=ISOweek::ISOweek(date))%>%
  select(week,real) %>% group_by(week) %>%
  summarise(deaths=sum(real,na.rm = T)) %>% arrange(-deaths) %>% 
  top_n(1) %>% pull (week)


avg_rt_max<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,2)) %>% distinct(rt) %>% arrange(rt) %>%
  top_n(1) %>% pull(rt)


avg_rt_min<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,2)) %>% distinct(rt) %>% arrange(rt) %>%
  top_n(-1) %>% pull(rt)


# 
p1<-plot_deaths(current[[1]],country)

p2<-reff_plot(current[[1]])


avg_rt<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,1)) %>% distinct(rt) %>% filter(rt>1 & rt<2) %>%
  arrange(rt)%>% pull(rt)

date_range<-"2021-07-01"

mean_vac<-current[[2]]  %>% filter (dates>date_range) %>%
  filter (dates==case_when(max_vaccine>0~dates)) %>%
  summarise(v=mean(max_vaccine, na.rm = TRUE)) %>% pull(v)







population_total <- nimue:::init(squire::get_population(country)$n, seeding_cases = 1)
population_total<-sum(population_total$S_0[,1])


tot_vac<-current[[2]]  %>% 
  summarise(max_vaccine=sum(max_vaccine,na.rm = T)) %>% pull(max_vaccine)

vac_m<-round(tot_vac/population_total,2)


if (vac_m < .4) {
  seq1<-vac_m+0.05
  coverage<-c(seq(seq1,.94,.1))
}  else if (vac_m >= .4) {
  seq1<-vac_m+0.05
  coverage=c(seq(seq1,.94,.05))
}

infections<-current[[1]] %>% mutate(date=lubridate::ymd(date),
                                     week=ISOweek::ISOweek(date))%>%
  select(date,week,infections) %>% group_by(week) %>% filter(date>date_range) %>%
  summarise(infections=sum(infections,na.rm = T)) %>% filter (infections>0)

infections <- infections %>% 
  filter(infections==max(infections)) %>% 
  pull(infections)

if (population_total < 1000000) {
  seeding_cases = round(infections)
} else if (population_total < 5000000){
  seeding_cases = round(infections)*2
  } else  {
    seeding_cases = round(infections)*5
  }

avg_vac <- current[[2]]  %>% filter (dates>date_range) %>%
  filter (dates==case_when(max_vaccine>0~dates)) %>%
  summarise(v=mean(max_vaccine, na.rm = TRUE)) %>% pull(v)

anual<-((avg_vac/2)*180)


if (tot_vac==0){
  
  avg_vac<-round(((population_total*.8*1.1)/185)/2)

  avg_vac_people<-avg_vac*2
  
  multipl_vaccine<-avg_vac
  
  } else {
  
  multipl_vaccine<-round(((population_total*.8*1.1)-tot_vac)/(anual),1)
  
  avg_vac<-c(round(avg_vac/2),
             round((avg_vac/2)*5),
             round((avg_vac/2)*10),
             round((avg_vac/2)*multipl_vaccine)) 
  
  if (multipl_vaccine < 1){
    avg_vac<-avg_vac[c(4,1,2,3)] 
  } else if (multipl_vaccine > 10){
    avg_vac<-avg_vac[c(1,2,3,4)] 
  } else if (multipl_vaccine <5 ){ 
    avg_vac<-avg_vac[c(1,4,2,3)] 
  } else {
    avg_vac<-avg_vac[c(1,2,4,3)] 
  }
  
ave_vac_text<-avg_vac[1]
ave2_vac_text<-avg_vac[2]
ave3_vac_text<-avg_vac[3]
ave4_vac_text<-avg_vac[4] 
  
pop_ave_vac_text<-avg_vac[1]*2
pop_ave2_vac_text<-avg_vac[2]*2
pop_ave3_vac_text<-avg_vac[3]*2
pop_ave4_vac_text<-avg_vac[4]*2  

#
  h_line1 <- avg_vac[1]                       # Position of horizontal line
  h_line2 <- avg_vac[2]                       # Position of horizontal line
  h_line3 <- avg_vac[3]                       # Position of horizontal line
  h_line4 <- avg_vac[4]  



if (multipl_vaccine < 1){
  label1 <- "Needed to cover 80% in 6 months"                      # Position of horizontal line
  label2 <- "Historical average"                       # Position of horizontal line
  label3 <- "5x average"                      # Position of horizontal line
  label4 <- "10x average" 
} else if (multipl_vaccine > 10){
  label1 <- "Historical average"                    # Position of horizontal line
  label2 <- "5x average"                      # Position of horizontal line
  label3 <- "10x average"                       # Position of horizontal line
  label4 <-  "Needed to cover 80% in 6 months"  
} else if (multipl_vaccine <5 ){ 
  label1 <- "Historical average"                      # Position of horizontal line
  label2 <-  "Needed to cover 80% in 6 months"                      # Position of horizontal line
  label3 <- "5x average"                      # Position of horizontal line
  label4 <- "10x average" 
} else {
  label1 <- "Historical average"                       # Position of horizontal line
  label2 <- "5x average"                      # Position of horizontal line
  label3 <-  "Needed to cover 80% in 6 months"                       # Position of horizontal line
  label4 <- "10x average" }

plot_vaccines<-
current[[2]] %>%
filter (dates==case_when(max_vaccine>0~dates)) %>%
complete(dates = seq.Date(min(dates), max(dates), by="day")) %>%
fill(0) %>%
ggplot() +geom_hline(aes(yintercept = h_line1,colour=label1))+
  geom_hline(aes(yintercept = h_line2,colour=label2))+
  geom_hline(aes(yintercept = h_line3,colour=label3))+ 
  geom_hline(aes(yintercept = h_line4,colour=label4))+
  geom_line(aes(dates,max_vaccine)) + theme_minimal() +
ylab("Max vaccines") + xlab ("2021")  +
  scale_y_continuous(labels=scales::comma) + 
  theme(legend.title = element_blank())


  }

#

vaccine_coverage_mat=c("All","Elderly")

R0=c(seq(1.1,1.7,.2))



max_vaccine = avg_vac




min_coverage<-min(coverage)
max_coverage<-max(coverage)


min_coverage_text<-round(min_coverage,3)*100
max_coverage_text<-round(max_coverage,3)*100

# Matrix of scenarios

scenarios_LS_1 <- expand_grid(
  seeding_cases = seeding_cases,
  coverage=coverage,
  country = country,
  R0 = R0,
  vac=vac_m,
  max_vaccine = max_vaccine,
  vaccine_coverage_mat = vaccine_coverage_mat)

#nrow(scenarios_LS_1) # number of scenarios

scenarios<-nrow(scenarios_LS_1) # number of scenarios


R0_max<-max(R0) 
R0_min<-min(R0) 

# Run scenarios

out1 <- future_pmap(scenarios_LS_1,
                             run_scenario_LS, 
                             .progress = TRUE)

# Extract outputs
  
out_format <- format_out_LS(out1, scenarios_LS_1,
                                     country1=country,
                            vac_m=vac_m,
                            min_coverage =      min_coverage,
                            seeding_cases = seeding_cases)

#

deaths_averted<-out_format %>% 
  group_by(final_coverage,R0,vaccine_coverage_mat) %>%
  mutate(deaths_averted=mean(deaths_averted)) %>% 
  select(final_coverage,deaths_averted) %>%
  arrange(-deaths_averted)%>% pull(deaths_averted)

deaths_averted<-max(round(deaths_averted))



##

out_format_unnest_cf<-out_format %>% select(c(1:6,9,21)) %>%
  unnest(output_cf) %>% rename(value_cf = value)

out_format_unnest<-out_format %>% select(c(1:8,21)) %>%
  unnest(output)

out_format_unnest_final <- out_format_unnest %>% 
  bind_cols(out_format_unnest_cf[9]) %>%
  mutate(dif=value_cf-value)

# t21<-out_format_unnest_final %>% filter(compartment == "deaths") %>%
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
# max(out_format$deaths_averted)#  1171253


final_coverage_min<-out_format_unnest_final %>%  ungroup %>% 
  top_n(-final_coverage,n=1) %>% distinct(final_coverage) %>%
  mutate(final_coverage) %>%
  pull(final_coverage)

final_coverage_max<-out_format_unnest_final %>%  ungroup %>% 
  top_n(final_coverage,n=1) %>% distinct(final_coverage) %>%
  mutate(final_coverage) %>%
  pull(final_coverage)

final_coverage_min_text<-round(final_coverage_min,2)*100

final_coverage_max_text<-round(final_coverage_max,2)*100

if (final_coverage_max_text>100) {
  final_coverage_max_text = 100
}



values_labels <-out_format_unnest_final %>%  ungroup %>%
  distinct(max_vaccine) %>%
  arrange(max_vaccine) %>%
  mutate (max_vaccine=as.factor(max_vaccine)) %>%
  pull (max_vaccine)


# 

out_format_2groupsages<-out_format_unnest_final %>% 
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

population <- nimue:::init(squire::get_population(country)$n, seeding_cases = 200000)

population<-population$S_0

population <- c(sum(population[1:12,1]),
                      sum(population[13:15,1]),
                      sum(population[16:17,1]))

dif_2age<-out_format_2groupsages %>% 
  filter(compartment != "vaccines")%>%
  mutate(final_coverage=as.factor(final_coverage),
         R0=as.factor(R0),
         max_vaccine=as.factor(max_vaccine),
         vaccine_coverage_mat=as.factor(vaccine_coverage_mat)) %>% 
  group_by(age,final_coverage,R0,max_vaccine,vaccine_coverage_mat,compartment) %>%
    mutate (dif=mean(dif,na.rm=T),
            dif=case_when(age == "older" ~ (dif/population[3])*1000,
                        age == "middle_age" ~ (dif/population[2])*1000,
                          T ~ (dif/population[1])*1000))


# dif_2age %>% mutate(age=factor(age,levels = c("younger","middle_age","older")))%>%
#     ggplot() +
#   geom_point(aes(final_coverage,dif,colour=max_vaccine,
#                  shape=vaccine_coverage_mat)) + 
#   facet_grid(compartment~age, scales="free") +
#   scale_y_continuous(labels = scales::comma) + theme_light() + 
#   ylab ("Averted x 1000 inhabitants")

# 

out_format_older<-out_format_unnest_final %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75"  ~ "60-75",
                           age_group == "75-80" |
                             age_group == "80+" ~ "75+",
                           T ~ "not")) %>%
  filter(age != "not") %>%
  group_by(age,final_coverage,R0,vaccine_coverage_mat,max_vaccine,compartment) %>%
  mutate(final_coverage = round(final_coverage,1)) %>% 
  mutate(dif=mean(dif,na.rm=T)) %>% #filter(dif>0) %>%
  summarise(dif=sum(dif))


max_older_infections<-out_format_older %>% group_by(age) %>% 
  filter (compartment=="infections") %>%
  filter(dif==max(dif)) %>% mutate(dif=round(dif)) %>%
  pull (dif)

max_older_hospitalisations<-out_format_older %>% group_by(age) %>% 
  filter (compartment=="hospitalisations") %>%
  filter(dif==max(dif)) %>% mutate(dif=round(dif)) %>%
  pull (dif)

max_older_deaths<-out_format_older %>% group_by(age) %>% filter (compartment=="deaths") %>%
  filter(dif==max(dif)) %>% mutate(dif=round(dif)) %>%
  pull (dif)

#



analysisSEIR <- list(country,vac_m, avg_rt,population_total,tot_vac,
                     multipl_vaccine,avg_vac,
                     R0_max,R0_min,out_format,deaths_averted,
                     out_format_unnest_final,
                     final_coverage_min_text,
                     final_coverage_max_text,
                     out_format_older)

filename<-paste0("data_",country,".RData") 

saveRDS(analysisSEIR,file=filename)

# out_format_older %>% filter(compartment != "vaccines")%>%
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



