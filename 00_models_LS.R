library(broom)
library(squire)
library(nimue)
library(tidyverse)

r <- run_explicit_SEEIR_model(country = "Peru")

output <- format_output(r, var_select = "deaths", reduce_age = FALSE)

head(output)

plot(r, var_select = "deaths")

deaths <- format_output(x = r, var_select = "deaths", reduce_age = FALSE)  %>%
  mutate(replicate = factor(replicate))


d<-deaths %>% group_by(replicate,age_group) %>%
  mutate(cum = cumsum(y)) %>%
  ggplot () +
  geom_line(aes(x = t, y = cum,colour=replicate)) +
  ylab ("Cumulative deaths") +
  facet_wrap(~age_group)

gc()

d

### Run the model with an example population and no vaccination
no_vaccine <- run(country = "Peru",
                  max_vaccine = 0)

# Format the output selecting infection and deaths
out1 <- format(no_vaccine, compartments = NULL, summaries = c("infections", "deaths")) %>%
  mutate(Name = "No vaccine")

#Infection-blocking vaccine
#Next, we can run with an infection-blocking vaccine:

  # Run the determinstic model with an example population and infection-blocking vaccine
  infection_blocking <- run(country = "Peru",
                            max_vaccine = 40000,
                            vaccine_efficacy_disease = rep(0, 17),
                            vaccine_efficacy_infection = rep(0.9, 17))
# Format the output selecting infection and deaths
out2 <- format(infection_blocking, compartments = NULL, summaries = c("infections", "deaths")) %>%
  mutate(Name = "Infection blocking")
#Anti-disease vaccine
#And finally, a run with a disease-blocking vaccine:

  # Run the determinstic model with an example population and anti-disease vaccine
  disease_blocking <- run(country = "Peru",
                          max_vaccine = 40000,
                          vaccine_efficacy_disease = rep(0.9, 17),
                          vaccine_efficacy_infection = rep(0, 17))
# Format the output selecting infection and deaths
out3 <- format(disease_blocking, compartments = NULL, summaries = c("infections", "deaths")) %>%
  mutate(Name = "Disease  blocking")
#Compare
# Create plot data.frame
pd <- bind_rows(out1, out2, out3)
# Plot outputs
ggplot(pd, aes(x = t, y = value, group = Name, col = Name)) +
  geom_line(size = 1) +
  facet_wrap(~ compartment, scales = "free_y", ncol = 2) +
  xlim(0, 200) +
  xlab("Time") +
  theme_bw()

library(pracma)

pd2<-out3 %>% filter(compartment=="deaths" & replicate == 1 & !is.na(value))

trapz(pd2$t, pd2$value)

out3 %>% filter(compartment=="deaths" & replicate == 1 & !is.na(value)) %>%
  summarise(c=cumsum(value),c=max(c)) %>% distinct(c)
