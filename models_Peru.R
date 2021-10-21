#Random seeds
#as.integer(Sys.time()) %% 100 # 92
#as.integer(Sys.time()) %% 10 # 5

# as.integer(Sys.time()) %% 100 # 81
# as.integer(Sys.time()) %% 10 # 2



# Peru

#Upload data from official vaccination  website

vaccination_Peru <-  read.csv("vacPeru20oct.csv")

glimpse(vaccination_Peru)

vaccination_Peru <- vaccination_Peru %>% select(fecha_vacunacion,dosis,edad)

summary(vaccination_Peru$edad)

vaccination_Peru %>% arrange(-edad)%>% slice(1800:2000)
  
breaks=seq(0,85,5)
breaks

max(vaccination_Peru2$edad)

vaccination_Peru2 <- vaccination_Peru %>% filter (edad> 17 & edad<111) %>% 
  mutate(edad = case_when(edad > 84 ~ as.integer(84),
                          T ~ edad)) %>%
  mutate(age = cut(edad,breaks=breaks,right = F)) %>% 
  mutate(fecha_vacunacion = lubridate::dmy(fecha_vacunacion)) %>% 
  group_by(age,dosis,fecha_vacunacion) %>% tally() %>% 
  group_by(age,dosis) %>%
  complete(fecha_vacunacion = seq.Date(min(fecha_vacunacion),
                                       max(fecha_vacunacion), by="day")) %>%
  fill(n,0)



vaccination_Peru2 %>% 
  ggplot () + 
  geom_line(aes(fecha_vacunacion,n,
                group=as.factor(dosis),colour=as.factor(dosis))) + 
  facet_wrap(~age,scales = "free")


rm(vaccination_Peru)



#

run_scenario_LS_Peru <- function(R0=R0,
                            seed=seed,
                            coverage=coverage,
                            country = country,
                            max_vaccine = max_vaccine,
                            vaccine_coverage_mat = vaccine_coverage_mat
)
  
{
  
  init <- nimue:::init(squire::get_population("Peru")$n, seeding_cases = 200000)
  
  prop <- c(0,0,rep(0.333,7),rep(0.619,3),rep( 0.573,5)) # prop vaccinated (1 or 2 dosis) from https://dashboard.cowin.gov.in/
  S_0<-init$S_0
  S_0[,4]<-round(S_0[,1] * prop *.9) # 10% of vaccinated not having inmunity considering 1 year of inmunity
  #  146,712,438.00 vaccines in the first 4 months of 2020 =	15.5 
  S_0[,4] <-S_0[,4]/2 # divide SUSCEPTIBLE already vaccinated by two because of 2 dosis
  S_0[,5] <-S_0[,1]
  S_0[,1] <- S_0[,1] - S_0[,4]
  S_0[3:17,1] <- S_0[3:17,1]
  # and update the init
  init$S_0 <- S_0
  
  r1 <- nimue::run(time_period = 365,
                   seed = seed,
                   country = country,
                   R0 = R0, 
                   seeding_cases = 200000,
                   max_vaccine = max_vaccine,
                   dur_V = 365,
                   dur_R = 365,
                   vaccine_coverage_mat = strategy_matrix(vaccine_coverage_mat,max_coverage = coverage),
                   init=init,
                   vaccine_efficacy_infection = rep(0.5, 17),
                   vaccine_efficacy_disease = rep(0.9, 17)
  )
  
  
  x <- nimue::format(r1,
                     compartments = NULL,
                     summaries = c("deaths", "infections", "vaccines", "hospitalisations"),
                     reduce_age = FALSE)
  
  value_all_t <- summarise_by_age(x, 1, max(x$t), "all_t")
  
  tibble(output = list(value_all_t))
}


# Matrix of scenarios

## Paramater models

coverage_peru=c(seq(.68,.96,.02))
R0_peru=c(seq(1.1,2,.1))
seed1_peru=c(92+5^(seq(1,5,1))) # 5 pseudo random seeds
seed2_peru=c(81+2^(seq(6,10,1))) # 5 pseudo random seeds
country_peru = c("Peru")
max_vaccine_peru = c(3000000,4000000,5000000)
vaccine_coverage_mat_peru=c("All","Elderly")

#

scenarios_LS_1_peru <- expand_grid(
  coverage=coverage_peru,
  country = country_peru,
  seed=seed1_peru,
  R0 = R0_peru,
  max_vaccine = max_vaccine_peru,
  vaccine_coverage_mat = vaccine_coverage_mat_peru)

nrow(scenarios_LS_1_peru) # number of scenarios


scenarios_LS_2_peru <- expand_grid(
  coverage=coverage_peru,
  country = country_peru,
  seed=seed2_peru,
  R0 = R0_peru,
  max_vaccine = max_vaccine_peru,
  vaccine_coverage_mat = vaccine_coverage_mat_peru)

nrow(scenarios_LS_2_peru) 

# parallel processing

plan(multisession, workers = availableCores())

# Run all models for first 5 seeds

out1_peru <- future_pmap(scenarios_LS_1_peru, run_scenario_LS_Peru, .progress = TRUE)

gc()

# Extract outputs

out_format1_peru <- format_out_LS(out1_peru, scenarios_LS_1)

rm(out1_peru)
rm(scenarios_LS_1_peru)

gc()

# Repeat for other 5 seeds

# Run all models

out2_peru <- future_pmap(scenarios_LS_2_peru, run_scenario_LS_Peru, .progress = TRUE)

gc()

# Extract outputs

out_format2_peru <- format_out_LS(out2_peru, scenarios_LS_2)

gc()

rm(out2_peru)
rm(scenarios_LS_2_peru)

#

out_format_peru <- out_format1_peru %>% bind_rows(out_format2_peru)

rm(out_format1_peru,out_format2_peru)

gc()

# compute % population vaccinated

init2_peru <- nimue:::init(squire::get_population("Peru")$n, seeding_cases = 200000)
bprop_peru <- c(0,0,rep(0.333,7),rep(0.619,3),rep( 0.573,5)) # prop vaccinated (1 or 2 dosis) from https://dashboard.cowin.gov.in/
bS_0_peru<-init2_peru$S_0
bS_0_peru[,4]<-round(bS_0_peru[,1] * bprop_peru *.9) # 10% of vaccinated not having inmunity considering 1 year of inmunity
#  146,712,438.00 vaccines in the first 4 months of 2020 
bS_0_peru[,4] <-bS_0_peru[,4]/2 # divide SUSCEPTIBLE already vaccinated by two because of 2 dosis
bS_0_peru[,5] <-bS_0_peru[,1]
bS_0_peru[,1] <- bS_0_peru[,1] - bS_0_peru[,4]
bS_0_peru[3:17,1] <- bS_0_peru[3:17,1]
# and update the init
init2_peru$S_0 <- bS_0_peru

(sum(vac.Peru.Age)/2)/sum(init2_peru$S_0[,5]) # 0.3434736 => 34.3% population vaccinated


# Adjust model vaccine coverage to population coverage

out_format_peru <- out_format_peru %>% 
  mutate(final_coverage = (sum(init2_peru$S_0[,4]) + vaccine_n) / (sum(init2_peru$S_0[,5])))

summary(out_format_peru$final_coverage)

ggplot(out_format_peru)+ geom_point(aes(coverage,final_coverage)) + theme_light() +
  geom_hline(yintercept=0.3434736)+ 
  scale_x_continuous(breaks = seq(.3,1,.02))+
  scale_y_continuous(breaks = seq(.4,1,.02))+
  geom_vline(xintercept=.96)  + geom_hline(yintercept=0.9)



# out_format %>% filter (R0 == 1.5) %>%
#   ggplot() + geom_point(aes(final_coverage,deaths_averted,colour=vaccine_coverage_mat,
#                             shape=as.factor(max_vaccine)))+
#   facet_wrap(~seed) + theme_light() +
#   scale_y_continuous(labels = scales::comma) +
#    scale_x_continuous(labels = scales::percent,limits = c(.36,1)) +
#   theme(legend.position = "bottom") + 
#   labs(title = "Simulations of Vaccination models - Peru",
#        caption = "Data source: Author's own")+
#   xlab("Population maximum coverage") +  ylab("Deaths averted") + 
#    scale_colour_manual("Vaccine prioritisation",values = c("darkgreen", "darkorange"), 
#                        labels = c("No prioritisation", "Older People"))+
#   scale_shape_discrete("Mean maximum vaccines per day",
#                       labels = c("3 million", "4 million", "5 million"))


## Extract data per age groups (not used in the analysis right now)
# 
# out_format_unnest_cf<-out_format %>% select(c(1:6,8)) %>% 
#   unnest(output_cf) %>% rename(value_cf = value)
# 
# out_format_unnest<-out_format %>% select(1:7) %>% 
#   unnest(output) 
# 
# out_format_unnest_final <- out_format_unnest %>% bind_cols(out_format_unnest_cf[9]) %>% 
#     mutate(dif=value_cf-value)
# 
# final_coverage <- out_format %>% 
#   group_by(coverage)  %>% 
#   summarise (coverage=mean(coverage),
#              final_coverage=round(mean(final_coverage),2))
# 
# out_format_unnest_final <- out_format_unnest_final %>% left_join(final_coverage)
#   
#   
# out_format_unnest_final %>% filter(compartment == "deaths") %>%
#   ggplot() + geom_point(aes(coverage,dif,colour=age_group,
#                             shape=as.factor(max_vaccine))) +
#   facet_grid(vaccine_coverage_mat~R0) + theme_light()+
#   scale_y_continuous(labels = scales::comma)
# 
# 
# t21<-out_format_unnest_final %>% filter(compartment == "deaths") %>% 
#   # mutate(age = case_when(age_group == "60-65" |  age_group =="65-70" | 
#   #                          age_group =="70-75" |age_group == "75-80" | 
#   #                          age_group == "80+" ~ "older",
#   #                        T ~ "not")) %>%
#   group_by(coverage,R0,vaccine_coverage_mat,max_vaccine) %>%
#   summarise(dif=sum(dif))
# 
# 
# # test if extraction is correct
# 
# max(t21$dif) # highest - 3955593
# max(out_format$deaths_averted)#  3955593
# 
# 
# t22<-out_format_unnest_final %>% filter(compartment == "deaths") %>% 
#   mutate(age = case_when(age_group == "60-65" |  age_group =="65-70" |
#                            age_group =="70-75" |age_group == "75-80" |
#                            age_group == "80+" ~ "older",
#                          T ~ "not")) %>%
#   group_by(final_coverage,R0,vaccine_coverage_mat,max_vaccine,age) %>%
#   summarise(dif=sum(dif))
# 
# 
# t22 %>% mutate(max_vaccine = as.factor(max_vaccine))%>%
#   ggplot() + geom_point(aes(final_coverage,dif,colour=age,
#                             shape=max_vaccine)) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_continuous(labels = scales::comma,
#                      limits = c(.5,1)) +
#   scale_shape_discrete(label = c("3 million","4 million","5 million"))+
#   ylab("Deaths averted")
