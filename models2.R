r1 <- nimue::run(init=init2,
                 time_period = 365,
                 country = "India",
                 R0 = 1.1, 
                 seeding_cases = 1000000,
                 max_vaccine = 4000000,
                 dur_V = 365,
                 dur_R = 365,
                 vaccine_coverage_mat = strategy_matrix("Elderly",max_coverage = .8),
                 vaccine_efficacy_infection = rep(0.5, 17),
                 vaccine_efficacy_disease = rep(0.9, 17))

output1 <- format(r1, compartments = NULL,reduce_age = T) %>%
  mutate(Run = "1.1")


r2 <- nimue::run(init=init2,
                 time_period = 365,
                 country = "India",
                 R0 = 1.8, 
                 seeding_cases = 1000000,
                 max_vaccine = 4000000,
                 dur_V = 365,
                 dur_R = 365,
                 vaccine_coverage_mat = strategy_matrix("Elderly",max_coverage = .8),
                 vaccine_efficacy_infection = rep(0.5, 17),
                 vaccine_efficacy_disease = rep(0.9, 17))

output2 <- format(r2, compartments = NULL,reduce_age = T) %>%
  mutate(Run = "1.8")

bind_rows(output1,output2) %>% filter(compartment=="infections") %>% 
  group_by(Run) %>%
  summarise(d=sum(value,na.rm = T)) 

bind_rows(output1, output2) %>% filter(compartment=="infections") %>%
  ggplot(., aes(x = t, y = value))  +
  geom_line(size = 1) + 
  #ylab("Deaths") + 
  xlab("Time") + 
  theme_bw() + facet_wrap(~Run)+
  scale_y_continuous(labels = scales::comma)
