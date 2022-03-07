

## Pull sum totals

pull_total <- function(x, outcome, time_period){
  dplyr::filter(x, compartment == outcome, period == time_period) %>%
    pull(value) %>%
    sum()
}

#pull_total = possibly(.f = pull_total, otherwise = NULL)


summarise_outputs_age <- function(x, p) {
    mutate(x, 
         infections = round(map_dbl(output, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations = round(map_dbl(output, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths = round(map_dbl(output, pull_total, outcome = "deaths", time_period = p), 2),
         infections_cf = round(map_dbl(output_cf, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations_cf = round(map_dbl(output_cf, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths_cf = round(map_dbl(output_cf, pull_total, outcome = "deaths", time_period = p), 2),
         infections_averted = infections_cf - infections,
         hospitalisations_averted = hospitalisations_cf - hospitalisations,
         deaths_averted = deaths_cf - deaths,
         deaths_averted_prop = deaths_averted / deaths_cf,
         vaccine_n = round(map_dbl(output, pull_total, outcome = "vaccines", time_period = p))
  )
}

#summarise_outputs_age = possibly(.f = summarise_outputs_age, otherwise = NULL)


summarise_outputs_age2 <- function(x, p) {
  mutate(x, 
         infections = round(map_dbl(output, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations = round(map_dbl(output, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths = round(map_dbl(output, pull_total, outcome = "deaths", time_period = p), 2),
                  vaccine_n = round(map_dbl(output, pull_total, outcome = "vaccines", time_period = p))
  )
}

#summarise_outputs_age2 = possibly(.f = summarise_outputs_age2, otherwise = NULL)


summarise_by_age <- function(x, t_start, t_end, period){
  dplyr::filter(x, t >= t_start, t < t_end) %>%
    group_by(age_group, compartment) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(period = factor(period))
} 

#summarise_by_age = possibly(.f = summarise_by_age, otherwise = NULL)

# nimue:::default_durations()
# nimue:::default_probs()
# nimue:::default_vaccine_pars()

# run_pos = possibly(.f = nimue::run, otherwise = NULL)
# 
# run_format = possibly(.f = nimue::format, otherwise = NULL)
# 

run_scenario_LS <- function(R0=R0,
                            country = country,
                            mean_vac=mean_vac,
                            max_vaccine = max_vaccine,
                            vaccine_coverage_mat = vaccine_coverage_mat,
                            seeding_cases = seeding_cases)
  
{

    r1 <- nimue::run(
                   time_period = days,
                   country = country,
                   R0 = c(R0,.9),
                   tt_R0 = c(0, 145),
                   seeding_cases = seeding_cases,
                   max_vaccine = max_vaccine,
                   dur_R = 200,
                   dur_E= 2,
                   dur_IMild=2.6,
                   dur_ICase=3.8,
                   prob_hosp_multiplier=.6,
                   dur_vaccine_delay = 14, 
                   vaccine_coverage_mat = strategy_matrix(vaccine_coverage_mat,
                                                          max_coverage = .9),
                   vaccine_efficacy_infection = vaccine_efficacy_infection,
                   vaccine_efficacy_disease = vaccine_efficacy_disease,
                   tt_vaccine_efficacy_infection = tt,
                   tt_vaccine_efficacy_disease = tt,
                   use_dde=T)
  
  
  x <- nimue::format(r1,
                     compartments = NULL,
                     summaries = c("deaths", "infections", "vaccines", "hospitalisations"),
                     reduce_age = FALSE)
  
  value_all_t <- summarise_by_age(x, 1, max(x$t), "all_t")
  
  tibble(output = list(value_all_t))
  
}

run_scenario_LS2 <- function(R0=R0,
                            country = country,
                            mean_vac=mean_vac,
                            max_vaccine = max_vaccine,
                            vaccine_coverage_mat = vaccine_coverage_mat,
                            seeding_cases = seeding_cases)
  
{

  r1 <- nimue::run(time_period = days,
                   country = country,
                   R0 = c(R0,.9),
                   tt_R0 = c(0, 145),
                   seeding_cases = seeding_cases,
                   max_vaccine = max_vaccine,
                   dur_R = 200,
                   dur_E= 2.1,
                   dur_IMild=2.6,
                   dur_ICase=3.8,
                   prob_hosp_multiplier=.6,
                   dur_vaccine_delay = 14, 
                   vaccine_coverage_mat = strategy_matrix(vaccine_coverage_mat,
                                                          max_coverage = .9),
                   vaccine_efficacy_infection = vaccine_efficacy_infection,
                   vaccine_efficacy_disease = vaccine_efficacy_disease,
                   tt_vaccine_efficacy_infection = tt,
                   tt_vaccine_efficacy_disease = tt,
                use_dde=T)
  
  
  
  x <- nimue::format(r1,summaries = c("N", "hospitalisations", "hospital_demand", "hospital_occupancy",
                                      "ICU_demand", "ICU_occupancy", "vaccines", "unvaccinated", "vaccinated",
                                      "priorvaccinated", "infections", "deaths"),
                     reduce_age = T)
  tibble(output = list(x))
}


format_out_LS <- function(out, scenarios){
  

  out1 <- bind_cols(scenarios, bind_rows(out))
  
  outcf <- out1 %>% dplyr::filter (max_vaccine == mean_vac) %>%
    dplyr::select(-max_vaccine) %>%
    rename(output_cf = output) %>%
          unique()
  
  out1 <- dplyr::filter(out1,max_vaccine > mean_vac)
        
  summaries <- left_join(out1, outcf)

  summarise_all_t <- summarise_outputs_age(summaries, p = "all_t")
        
}
      

format_out_LS_compare_0 <- function(out, scenarios){
  
  
  out1 <- bind_cols(scenarios, bind_rows(out))
  
  outcf <- out1 %>% dplyr::filter (max_vaccine == 0) %>%
    dplyr::select(-max_vaccine) %>%
    rename(output_cf = output) %>%
    unique()
  
  out1 <- dplyr::filter(out1,max_vaccine > mean_vac)
  
  summaries <- left_join(out1, outcf)
  
  summarise_all_t <- summarise_outputs_age(summaries, p = "all_t")
  
}

  
#format_out_LS = possibly(.f = format_out_LS, otherwise = NULL)


format_out_baseline <- function(out, scenarios){
  
  out1 <- bind_cols(scenarios, bind_rows(out))
  
  out1 <- dplyr::filter(out1,max_vaccine == mean_vac)
  
  summaries <- out1

  summarise_all_t <- summarise_outputs_age2(summaries, p = "all_t")
}

#format_out_baseline = possibly(.f = format_out_baseline, otherwise = NULL)
