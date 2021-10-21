memory.limit(size=16000)

#Random seeds
#as.integer(Sys.time()) %% 100 # 92
#as.integer(Sys.time()) %% 10 # 5

# as.integer(Sys.time()) %% 100 # 81
# as.integer(Sys.time()) %% 10 # 2



# India

#Upload data from official vaccination  website

vaccination_India <-  read_excel("dashboard_export_19oct.xlsx")

#Data wrangling

## add column with week numbers
vaccination_India$week<-c(seq(3,41,1))

vac.India.Age<-vaccination_India %>% 
  pivot_longer(vac_18_45:vac_60_above, names_to = "Age_group") %>% 
  mutate (Age_group = case_when(Age_group=="vac_18_45" ~ '18 - 44',
                                Age_group=="vac_45_60" ~ '45 - 59',
                                T ~ '60+')) %>% group_by(Age_group) %>% 
  summarise(vac.total=sum(value,na.rm=T)) %>% pull(vac.total) 

# Functions to be used

## Pull sum totals
pull_total <- function(x, outcome, time_period){
  filter(x, compartment == outcome, period == time_period) %>%
    pull(value) %>%
    sum()
}

## Estimate total years of life lost
summarise_yll <- function(x, lifespan=69.656, time_period){
  filter(x, compartment == "deaths", period == time_period) %>%
    mutate(mid_age = (((as.integer(age_group) - 1) * 5) + 2.5),
           yll = pmax(0, (lifespan - mid_age) * value)) %>%
    pull(yll) %>%
    sum()
}

summarise_outputs_age <- function(x, p) {
  mutate(x, 
         infections = round(map_dbl(output, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations = round(map_dbl(output, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths = round(map_dbl(output, pull_total, outcome = "deaths", time_period = p), 2),
         yll = round(map_dbl(output, summarise_yll, time_period = p), 2),
         infections_cf = round(map_dbl(output_cf, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations_cf = round(map_dbl(output_cf, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths_cf = round(map_dbl(output_cf, pull_total, outcome = "deaths", time_period = p), 2),
         yll_cf = round(map_dbl(output_cf, summarise_yll, time_period = p), 2),
         infections_averted = infections_cf - infections,
         hospitalisations_averted = hospitalisations_cf - hospitalisations,
         deaths_averted = deaths_cf - deaths,
         deaths_averted_prop = deaths_averted / deaths_cf,
         years_life_saved = yll_cf - yll,
         vaccine_n = round(map_dbl(output, pull_total, outcome = "vaccines", time_period = p)))
}



summarise_by_age <- function(x, t_start, t_end, period){
  filter(x, t >= t_start, t < t_end) %>%
    group_by(age_group, compartment) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(period = factor(period))
} 


run_scenario_LS <- function(R0=R0,
                            seed=seed,
                            coverage=coverage,
                            country = country,
                            max_vaccine = max_vaccine,
                            vaccine_coverage_mat = vaccine_coverage_mat
                            )
                            
{

  init <- nimue:::init(squire::get_population("India")$n, seeding_cases = 200000)
  
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

format_out_LS <- function(out, scenarios){
  
  out1 <- bind_cols(scenarios, bind_rows(out))
  
  if ("coverage" %in% colnames(out1)) {
    outcf <- filter(out1, coverage == .68) %>%
      select(-coverage) %>%
      rename(output_cf = output) %>%
      unique()
    
    summaries <- left_join(out1, outcf)
    
    m <- ncol(summaries)+1
    n <- ncol(summaries)+14
    summarise_all_t <- summarise_outputs_age(summaries, p = "all_t")
  }
}

# Matrix of scenarios

## Paramater models

coverage=c(seq(.68,.96,.02))
R0=c(seq(1.1,2,.1))
seed1=c(92+5^(seq(1,5,1))) # 5 pseudo random seeds
seed2=c(81+2^(seq(6,10,1))) # 5 pseudo random seeds
country = c("India")
max_vaccine = c(3000000,4000000,5000000)
vaccine_coverage_mat=c("All","Elderly")

#

scenarios_LS_1 <- expand_grid(
  coverage=coverage,
  country = country,
  seed=seed1,
  R0 = R0,
  max_vaccine = max_vaccine,
  vaccine_coverage_mat = vaccine_coverage_mat)

nrow(scenarios_LS_1) # number of scenarios


scenarios_LS_2 <- expand_grid(
  coverage=coverage,
  country = country,
  seed=seed2,
  R0 = R0,
  max_vaccine = max_vaccine,
  vaccine_coverage_mat = vaccine_coverage_mat)

nrow(scenarios_LS_2) 

# parallel processing

plan(multisession, workers = availableCores())

# Run all models for first 5 seeds

out1 <- future_pmap(scenarios_LS_1, run_scenario_LS, .progress = TRUE)

gc()

# Extract outputs

out_format1 <- format_out_LS(out1, scenarios_LS_1)

rm(out1)
rm(scenarios_LS_1)

gc()

# Repeat for other 5 seeds

# Run all models

out2 <- future_pmap(scenarios_LS_2, run_scenario_LS, .progress = TRUE)

gc()

# Extract outputs

out_format2 <- format_out_LS(out2, scenarios_LS_2)

gc()

rm(out2)
rm(scenarios_LS_2)

#

out_format <- out_format1 %>% bind_rows(out_format2)

rm(out_format1,out_format2)

gc()

# compute % population vaccinated

init2 <- nimue:::init(squire::get_population("India")$n, seeding_cases = 200000)
bprop <- c(0,0,rep(0.333,7),rep(0.619,3),rep( 0.573,5)) # prop vaccinated (1 or 2 dosis) from https://dashboard.cowin.gov.in/
bS_0<-init2$S_0
bS_0[,4]<-round(bS_0[,1] * bprop *.9) # 10% of vaccinated not having inmunity considering 1 year of inmunity
#  146,712,438.00 vaccines in the first 4 months of 2020 
bS_0[,4] <-bS_0[,4]/2 # divide SUSCEPTIBLE already vaccinated by two because of 2 dosis
bS_0[,5] <-bS_0[,1]
bS_0[,1] <- bS_0[,1] - bS_0[,4]
bS_0[3:17,1] <- bS_0[3:17,1]
# and update the init
init2$S_0 <- bS_0

(sum(vac.India.Age)/2)/sum(init2$S_0[,5]) # 0.3434736 => 34.3% population vaccinated


# Adjust model vaccine coverage to population coverage

out_format <- out_format %>% 
  mutate(final_coverage = (sum(init2$S_0[,4]) + vaccine_n) / (sum(init2$S_0[,5])))

summary(out_format$final_coverage)

ggplot(out_format)+ geom_point(aes(coverage,final_coverage)) + theme_light() +
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
#   labs(title = "Simulations of Vaccination models - India",
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
