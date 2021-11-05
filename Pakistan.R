# Parameters

country<-c("Pakistan")

current<-df_current(country=country)

deaths_Pakistan<-sum(current[[1]]$real,na.rm = T)

current[[1]]$Month_Year <- substr(current[[1]]$date, 1,7)

deaths_peek_Pakistan<-current[[1]] %>% mutate(date=lubridate::ymd(date),
                        week=ISOweek::ISOweek(date))%>%
  select(week,real) %>% group_by(week) %>%
  summarise(deaths=sum(real,na.rm = T)) %>% arrange(-deaths) %>% 
  top_n(1) %>% pull (deaths)

week_peek_Pakistan<-current[[1]] %>% mutate(date=lubridate::ymd(date),
                        week=ISOweek::ISOweek(date))%>%
  select(week,real) %>% group_by(week) %>%
  summarise(deaths=sum(real,na.rm = T)) %>% arrange(-deaths) %>% 
  top_n(1) %>% pull (week)


avg_rt_Pakistan_max<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,2)) %>% distinct(rt) %>% arrange(rt) %>%
  top_n(1) %>% pull(rt)


avg_rt_Pakistan_min<- current[[1]]  %>% group_by(Month_Year)%>%
  summarise(rt=mean(rt, na.rm = TRUE),
            rt=round(rt,2)) %>% distinct(rt) %>% arrange(rt) %>%
  top_n(-1) %>% pull(rt)


# 

p1_Pakistan<-plot_deaths(current[[1]],country)

p2_Pakistan<-reff_plot(current[[1]])



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

avg_vac


#

vaccine_coverage_mat=c("All","Elderly")

R0=avg_rt

max_vaccine = avg_vac

vac_m<-round(current[[3]],1)

if (vac_m < .4) {
  seq1<-vac_m+0.05
  coverage<-c(seq(seq1,.9,.1))
}  else if (vac_m >= .4) {
  seq1<-vac_m+0.05
  coverage=c(seq(seq1,.9,.05))
}

# Matrix of scenarios

scenarios_LS_1 <- expand_grid(
  coverage=coverage,
  country = country,
  R0 = R0,
  vac=vac_m,
  max_vaccine = max_vaccine,
  vaccine_coverage_mat = vaccine_coverage_mat)

nrow(scenarios_LS_1) # number of scenarios

scenarios_Pakistan<-nrow(scenarios_LS_1) # number of scenarios


R0_Pakistan_max<-max(R0) 
R0_Pakistan_min<-min(R0) 

# Run scenarios

out1 <- future_pmap(scenarios_LS_1, run_scenario_LS, .progress = TRUE)

# Extract outputs
  
out_format_Pakistan <- format_out_LS(out1, scenarios_LS_1,country1=country,vac_m)



deaths_averted_Pakistan<-out_format_Pakistan %>% 
  group_by(final_coverage,R0,vaccine_coverage_mat) %>%
  mutate(deaths_averted=mean(deaths_averted)) %>% 
  select(final_coverage,deaths_averted) %>%
  arrange(-deaths_averted)%>% pull(deaths_averted)

deaths_averted_Pakistan<-max(round(deaths_averted_Pakistan))

#
out_format_Pakistan %>% 
  group_by(coverage,R0,vaccine_coverage_mat,max_vaccine) %>%
  mutate(deaths=mean(deaths_averted))%>%
  ggplot() + geom_point(aes(coverage,deaths,
                            colour=vaccine_coverage_mat,
                            shape=as.factor(max_vaccine)))+
  facet_wrap(~R0,ncol = 4) + theme_light() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  theme(legend.position = "bottom", legend.box="vertical", legend.margin=margin()) + 
  labs(title = "Simulations of Vaccination models",
       caption = "Data source: Authors' own")+
  xlab("Population maximum coverage") +  ylab("Deaths") + 
  scale_colour_manual("Vaccine prioritisation",values = c("darkgreen", "darkorange"), 
                      labels = c("No priority", "Older People"))+
  scale_shape_discrete("Mean vaccines/day",
                       labels = c("Average", "3x average", "5x average")) +
  ggtitle(country)


#

##

out_format_unnest_cf<-out_format_Pakistan %>% select(c(1:6,8,23)) %>%
  unnest(output_cf) %>% rename(value_cf = value)

out_format_unnest<-out_format_Pakistan %>% select(c(1:7,23)) %>%
  unnest(output)

out_format_unnest_final_Pakistan <- out_format_unnest %>% 
  bind_cols(out_format_unnest_cf[9]) %>%
  mutate(dif=value_cf-value)

# t21<-out_format_unnest_final_Pakistan %>% filter(compartment == "deaths") %>%
# mutate(age = case_when(age_group == "60-65" |  age_group =="65-70" |
#                          age_group =="70-75" |age_group == "75-80" |
#                          age_group == "80+" ~ "older",
#                        T ~ "not")) %>%
# group_by(coverage,R0,vaccine_coverage_mat,max_vaccine) %>%
# summarise(dif=sum(dif))


# test if extraction is correct

# max(t21$dif) # highest - 3955593
# max(out_format_Pakistan$deaths_averted)#  3955593


out_format_older_Pakistan<-out_format_unnest_final_Pakistan %>% #filter(compartment == "deaths") %>%
  mutate(age = case_when(age_group == "60-65" |  age_group =="65-70" |
                           age_group =="70-75" |age_group == "75-80" |
                           age_group == "80+" ~ "older",
                         T ~ "not")) %>%
  filter(age == "older") %>%
  group_by(final_coverage,R0,vaccine_coverage_mat,max_vaccine,compartment) %>%
  summarise(dif=sum(dif))

out_format_older_Pakistan %>% filter(compartment != "vaccines")%>%
  mutate(max_vaccine = as.factor(max_vaccine))%>%
  ggplot() + geom_point(aes(final_coverage,dif,colour=as.factor(R0),
                            shape=max_vaccine)) +
  facet_wrap(~compartment,scales = "free") + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::percent,
                     limits =  c(0,.85)) +
  scale_shape_discrete(name= "Maximum vaccines per day",
                       label = c("Historical average","Average x2","Average x3"))+
  ylab("Averted") + theme_minimal() + xlab("Vaccination coverage") +
  scale_color_discrete(name= "Rt scenario") + 
  ggtitle("Scenarios of Infections, Hospitalisations and deaths averted - People over 60")


final_coverage_max_Pakistan<-out_format_older_Pakistan %>%  ungroup %>% 
  top_n(final_coverage,n=1) %>% distinct(final_coverage) %>% 
  mutate(final_coverage=round(final_coverage,2)) %>% pull(final_coverage)

values_labels_Pakistan <-out_format_older_Pakistan %>%  ungroup %>%
  distinct(max_vaccine) %>%
  arrange(max_vaccine) %>%
  mutate (max_vaccine=as.factor(max_vaccine)) %>%
  pull (max_vaccine)


out_format_older_Pakistan %>% filter(compartment != "vaccines" &
                                    vaccine_coverage_mat == "Elderly" ) %>%
  filter ( final_coverage > (final_coverage_max_Pakistan -.05)) %>%
  group_by(R0,max_vaccine,compartment) %>%
  slice(which.max(dif)) %>%
  mutate(max_vaccine = as.factor(max_vaccine),
         R0=as.factor(R0)) %>%
  ggplot() + geom_point(aes(R0,dif,colour=max_vaccine)) +
  facet_wrap(~compartment,scales = "free") + 
  scale_y_continuous(labels = scales::comma) +
  scale_colour_discrete(name= "Maximum vaccines per day",
                        breaks=values_labels_Pakistan,
                        label = c("Historical average","Average x2","Average x3"))+
  ylab("Averted") + theme_minimal() + xlab("R0 scenario") +
  ggtitle("Scenarios of Infections, Hospitalisations and deaths averted - People over 60")







#

vaccines_Pakistan<-current[[2]] %>% 
  filter (dates==case_when(max_vaccine>0~dates)) %>% 
  complete(dates = seq.Date(min(dates), max(dates), by="day")) %>%
  fill(0) %>%
  ggplot() + geom_line(aes(dates,max_vaccine)) + theme_minimal() + 
  ylab("Max vaccines") + xlab ("2021") + 
  geom_hline(yintercept = avg_vac,linetype=2,colour="darkgreen")+
  geom_hline(yintercept = avg_vac*2,linetype=2,colour="darkorange")+
  geom_hline(yintercept = avg_vac*3,linetype=2,colour="darkorange")+
  scale_y_continuous(labels = scales::comma)


###