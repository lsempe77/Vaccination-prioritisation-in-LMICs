format_out_LS_Peru <- function(out, scenarios){
  
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

## Pull sum totals
pull_total <- function(x, outcome, time_period){
  filter(x, compartment == outcome, period == time_period) %>%
    pull(value) %>%
    sum()
}

##

lifespan <- read_excel("lifespan.xlsx")

peru_lifespan <-lifespan %>% filter (Country=="Peru")%>% pull(lifespan)


## Estimate total years of life lost
summarise_yll <- function(x, lifespan=peru_lifespan, time_period){
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

# Peru

#Random seeds
# as.integer(Sys.time()) %% 100 # 17
# as.integer(Sys.time()) %% 10 # 8

# as.integer(Sys.time()) %% 100 # 46
# as.integer(Sys.time()) %% 10 # 7



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
  
  S_0<-init$S_0
  vac<-c(rep(0,3),vac.Peru.Age$full_vac_t)
  S_0[,4]<-round(vac  *.9)
  S_0[,5] <-S_0[,1]
  S_0[,1] <- S_0[,1] - S_0[,4]
  S_0[3:17,1] <- S_0[3:17,1]
  # and update the init
  init$S_0 <- S_0
  init$S_0
  
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
R0_peru=c(seq(1.1,1.8,.1))
# seed1_peru=c(17+8^(seq(1,5,1))) # 5 pseudo random seeds
# seed2_peru=c(46+7^(seq(1,5,1))) # 5 pseudo random seeds
country_peru = c("Peru")
max_vaccine_peru = c(200000/2,300000/2,400000/2)
vaccine_coverage_mat_peru=c("All","Elderly")

seed1_peru=123
seed2_peru=456

# R0_peru=1.5
# max_vaccine_peru = 400000/2
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

#plan(multisession, workers = availableCores())

# Run all models for first 5 seeds

out1_peru <- future_pmap(scenarios_LS_1_peru, run_scenario_LS_Peru, .progress = TRUE)

gc()

# Extract outputs

out_format1_peru <- format_out_LS_Peru(out1_peru, scenarios_LS_1_peru)

rm(out1_peru)
rm(scenarios_LS_1_peru)

gc()

# Repeat for other 5 seeds

# Run all models

out2_peru <- future_pmap(scenarios_LS_2_peru, run_scenario_LS_Peru, .progress = TRUE)

gc()

# Extract outputs

out_format2_peru <- format_out_LS_Peru(out2_peru, scenarios_LS_2_peru)

gc()

rm(out2_peru)

rm(scenarios_LS_2_peru)

#

out_format_peru <- out_format1_peru %>% bind_rows(out_format2_peru)

rm(out_format1_peru,out_format2_peru)

gc()


