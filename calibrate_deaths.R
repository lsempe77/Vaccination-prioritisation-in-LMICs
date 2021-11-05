df_current<-function(country){
  
iso3cs <- squire::population$iso3c[squire::population$country==country][1]
country <- squire::population$country[squire::population$country==country][1]

final_coverage <-0.8

vaccine_durability = 1095

#grab the json from the data exports
file_path <- "https://raw.githubusercontent.com/mrc-ide/global-lmic-reports/master/"
json_path <- file.path(file_path,iso3cs,"input_params.json")
json <- jsonlite::read_json(json_path)

## get country specific params
contact_matrix = squire::get_mixing_matrix(country)
population = squire::get_population(country)

# get inputs from json
betas <- unlist(lapply(json, "[[", "beta_set"))
betas_min <- unlist(lapply(json, "[[", "beta_set_min"))
betas_max <- unlist(lapply(json, "[[", "beta_set_max"))
tt_R0 <- unlist(lapply(json, "[[", "tt_beta"))
dates <- unlist(lapply(json, "[[", "date"))
deaths <- unlist(lapply(json, "[[", "deaths"))

summary(betas);summary(betas_max)

date_deaths <- unlist(lapply(json, function(x){
  if("deaths" %in% names(x)) {
    x["date"]
  } else {
    NULL
  }
}))

Rts <- unlist(lapply(json, "[[", "Rt"))
Rts <- Rts[which(dates <= max(date_deaths))]
betas <- betas[which(dates <= max(date_deaths))]
betas_min <- betas_min[which(dates <= max(date_deaths))]
betas_max <- betas_max[which(dates <= max(date_deaths))]
tt_R0 <- tt_R0[which(dates <= max(date_deaths))]
dates <- dates[which(dates <= max(date_deaths))]


# get vaccine data if in json (if statement only here as previously this did not exist)
if("max_vaccine" %in% names(json[[1]])) {
  max_vaccine <- unlist(lapply(json, "[[", "max_vaccine"))
  max_vaccine <- max_vaccine[which(dates <= max(date_deaths))]
} 
# else {
#   
#   # if null use pop size
#   if (is.null(max_vaccine)) {
#     # default in covidsim in 2.5% of the population to recieve per week so /7 here
#     max_vaccine <- as.integer(sum(population$n)*0.025/7)
#   }
#   max_vaccine <- c(rep(0, length(tt_R0) - length(max_vaccine)), max_vaccine)
#   
# }

vaccines_day<- cbind(dates,max_vaccine) %>% as_tibble()

vaccines_day<-vaccines_day %>% mutate(dates=lubridate::ymd(dates),
                                      max_vaccine=as.numeric(max_vaccine))

forecast <- 365 #How long into future to simulate

# future vaccines
  current_coverage <- sum(max_vaccine) / sum(squire::population$n[squire::population$iso3c==iso3cs])
  final_coverage <- max(final_coverage, current_coverage)
  to_give <- (final_coverage - current_coverage) * sum(squire::population$n[squire::population$iso3c==iso3cs])
  to_give <- as.integer(to_give / forecast)
  future_vaccines <- to_give
  tt_future_vaccines <- 1

new_vaccines <- c(max_vaccine, future_vaccines)
tt_vacc <- c(tt_R0+1, tt_future_vaccines + tail(tt_R0+1,1))
tt_vacc

# Vaccine strategy
vacc_json <- paste0("https://github.com/mrc-ide/nimue_js/releases/download/v1.0.10/", iso3cs, ".json")
vacc_strat_json <- jsonlite::read_json(vacc_json)
vacc_strat_json
# get what was used in the json
if("vaccine_coverage" %in% names(json[[1]])) {
  vaccine_uptake <- json[[1]]$vaccine_coverage
}
if("vaccines_available" %in% names(json[[1]])) {
  vaccine_available <- json[[1]]$vaccines_available
}

if("vaccine_strategy" %in% names(json[[1]])) {
  strategy <- json[[1]]$vaccine_strategy
}

# get cov_mat for strategy
if(strategy == "HCW and Elderly") {
  cov_mat <- matrix(unlist(vacc_strat_json$whoPriority), ncol = 17) * vaccine_uptake
} else if (strategy == "HCW, Elderly and High-Risk") {
  cov_mat <- matrix(unlist(vacc_strat_json$etagePriority), ncol = 17)  * vaccine_uptake
} else if (strategy == "Elderly") {
  cov_mat <- nimue::strategy_matrix("Elderly", max_coverage = vaccine_uptake, 0)
} else if (strategy == "All") {
  cov_mat <- nimue::strategy_matrix("All", max_coverage = vaccine_uptake, 0)
} else {
  stop('Incorrect strategy. Must be one of "HCW and Elderly", "HCW, Elderly and High-Risk", "Elderly", "All"')
}

# scale vaccine coverage for availability function
scale_cov_mat <- function(cov_mat, vaccine_available, pop) {
  
  # total vaccs available
  tot_vaccines <- sum(pop*vaccine_available)
  
  # step 1, find when max allocation exceeds capacity
  step <- 1
  step_found <- FALSE
  tot_vaccs_steps <- 0
  cov_mat_dup_ex <- rbind(0, cov_mat)
  
  while(!step_found && step <= nrow(cov_mat)) {
    
    if(nrow(cov_mat) == 1) {
      step_found <- TRUE
    }
    
    vaccs_in_step <- sum((cov_mat_dup_ex[step+1, ] - cov_mat_dup_ex[step, ]) * pop)
    tot_vaccs_steps <- tot_vaccs_steps + vaccs_in_step
    if(tot_vaccs_steps > tot_vaccines) {
      step_found <- TRUE
    } else {
      step <- step+1
    }
  }
  
  # if we have enough vaccine return now
  if(step > nrow(cov_mat)) {
    return(cov_mat)
  }
  
  # set steps after max available reached to 0
  if(step < nrow(cov_mat)) {
    cov_mat[(step+1):nrow(cov_mat),] <- 0
  }
  
  # now set this step to be correct for available
  tots_given <- sum(cov_mat[step-1,] %*% pop)
  tots_tried <- sum(cov_mat[step,] %*% pop)
  remaining <- tot_vaccines - tots_given
  
  # next_group
  next_group <- cov_mat[step,]-cov_mat[step-1,]
  poss_to_vacc <- (next_group[which(next_group > 0)] * pop[which(next_group > 0)])
  new_cov <- (remaining/sum(poss_to_vacc)) * cov_mat[step, which(next_group > 0)]
  cov_mat[step, which(next_group > 0)] <- new_cov
  return(cov_mat)
}
cov_mat <- scale_cov_mat(cov_mat, vaccine_available, population$n)

if("vaccine_efficacy_infection" %in% names(json[[1]])) {
  vaccine_efficacy_infection <- lapply(json, "[[", "vaccine_efficacy_infection")
  vaccine_efficacy_infection <- vaccine_efficacy_infection[which(dates <= max(date_deaths))]
} else {
  
  # if not use defaults
  vaccine_efficacy_infection <- inf_eff
  
}

if("vaccine_efficacy_disease" %in% names(json[[1]])) {
  vaccine_efficacy_disease <- lapply(json, "[[", "vaccine_efficacy_disease")
  vaccine_efficacy_disease <- vaccine_efficacy_disease[which(dates <= max(date_deaths))]
} else {
  
  # if not use defaults
  vaccine_efficacy_disease <- dis_eff
  
}

# format vaccine efficacies correctly
for(i in seq_along(vaccine_efficacy_infection)) {
  if(vaccine_efficacy_disease[[i]] < vaccine_efficacy_infection[[i]]) {
    vaccine_efficacy_disease[[i]] <- vaccine_efficacy_infection[[i]]
  }
  vaccine_efficacy_disease[[i]] <- (vaccine_efficacy_disease[[i]] - vaccine_efficacy_infection[[i]]) / (1 - vaccine_efficacy_infection[[i]])
}


if("vaccine_efficacy_disease" %in% names(json[[1]])) {
  vaccine_efficacy_disease <- lapply(json, "[[", "vaccine_efficacy_disease")
  vaccine_efficacy_disease <- vaccine_efficacy_disease[which(dates <= max(date_deaths))]
} else {
  
  # if not use defaults
  vaccine_efficacy_disease <- dis_eff
  
}

durs <- nimue:::default_durations()
probs <- nimue:::default_probs()

future_Rt_changes = 0
tt_Rt_changes = 0

if(length(future_Rt_changes) == 0) {
  future_beta <- nimue::beta_est_infectiousness(dur_IMild = durs$dur_IMild,
                                                dur_ICase = durs$dur_ICase,
                                                prob_hosp = probs$prob_hosp,
                                                rel_infectiousness = rep(1, 17),
                                                mixing_matrix = squire:::process_contact_matrix_scaled_age(
                                                  squire:::get_mixing_matrix(iso3c = iso3c), population$n),
                                                R0 = future_Rt)
  future_beta_changes <- future_beta / tail(betas,1)
} else {
  future_beta_changes <- future_Rt_changes
}

if(length(future_beta_changes) != length(tt_Rt_changes)) {
  stop("future_Rt or future_Rt_changes must be same length as tt_Rt_changes")
}

new_betas <- c(betas, tail(betas,1) * future_beta_changes)
tt_s <- c(tt_R0+1, tt_Rt_changes + tail(tt_R0+1,1))

# future vaccines
if(length(final_coverage) > 0) {
  current_coverage <- sum(max_vaccine) / sum(squire::population$n[squire::population$iso3c==iso3cs])
  final_coverage <- max(final_coverage, current_coverage)
  to_give <- (final_coverage - current_coverage) * sum(squire::population$n[squire::population$iso3c==iso3cs])
  to_give <- as.integer(to_give / forecast)
  future_vaccines <- to_give
  tt_future_vaccines <- 1
}
new_vaccines <- c(max_vaccine, future_vaccines)
tt_vacc <- c(tt_R0+1, tt_future_vaccines + tail(tt_R0+1,1))

# Vaccine strategy
vacc_json <- paste0("https://github.com/mrc-ide/nimue_js/releases/download/v1.0.10/", iso3cs, ".json")
vacc_strat_json <- jsonlite::read_json(vacc_json)

# get what was used in the json
if("vaccine_coverage" %in% names(json[[1]])) {
  vaccine_uptake <- json[[1]]$vaccine_coverage
}
if("vaccines_available" %in% names(json[[1]])) {
  vaccine_available <- json[[1]]$vaccines_available
}
if("vaccine_strategy" %in% names(json[[1]])) {
  strategy <- json[[1]]$vaccine_strategy
}

# get cov_mat for strategy
if(strategy == "HCW and Elderly") {
  cov_mat <- matrix(unlist(vacc_strat_json$whoPriority), ncol = 17) * vaccine_uptake
} else if (strategy == "HCW, Elderly and High-Risk") {
  cov_mat <- matrix(unlist(vacc_strat_json$etagePriority), ncol = 17)  * vaccine_uptake
} else if (strategy == "Elderly") {
  cov_mat <- nimue::strategy_matrix("Elderly", max_coverage = vaccine_uptake, 0)
} else if (strategy == "All") {
  cov_mat <- nimue::strategy_matrix("All", max_coverage = vaccine_uptake, 0)
} else {
  stop('Incorrect strategy. Must be one of "HCW and Elderly", "HCW, Elderly and High-Risk", "Elderly", "All"')
}

# scale vaccine coverage for availability function
scale_cov_mat <- function(cov_mat, vaccine_available, pop) {
  
  # total vaccs available
  tot_vaccines <- sum(pop*vaccine_available)
  
  # step 1, find when max allocation exceeds capacity
  step <- 1
  step_found <- FALSE
  tot_vaccs_steps <- 0
  cov_mat_dup_ex <- rbind(0, cov_mat)
  
  while(!step_found && step <= nrow(cov_mat)) {
    
    if(nrow(cov_mat) == 1) {
      step_found <- TRUE
    }
    
    vaccs_in_step <- sum((cov_mat_dup_ex[step+1, ] - cov_mat_dup_ex[step, ]) * pop)
    tot_vaccs_steps <- tot_vaccs_steps + vaccs_in_step
    if(tot_vaccs_steps > tot_vaccines) {
      step_found <- TRUE
    } else {
      step <- step+1
    }
  }
  
  # if we have enough vaccine return now
  if(step > nrow(cov_mat)) {
    return(cov_mat)
  }
  
  # set steps after max available reached to 0
  if(step < nrow(cov_mat)) {
    cov_mat[(step+1):nrow(cov_mat),] <- 0
  }
  
  # now set this step to be correct for available
  tots_given <- sum(cov_mat[step-1,] %*% pop)
  tots_tried <- sum(cov_mat[step,] %*% pop)
  remaining <- tot_vaccines - tots_given
  
  # next_group
  next_group <- cov_mat[step,]-cov_mat[step-1,]
  poss_to_vacc <- (next_group[which(next_group > 0)] * pop[which(next_group > 0)])
  new_cov <- (remaining/sum(poss_to_vacc)) * cov_mat[step, which(next_group > 0)]
  cov_mat[step, which(next_group > 0)] <- new_cov
  return(cov_mat)
}
cov_mat <- scale_cov_mat(cov_mat, vaccine_available, population$n)

# format vaccine efficacies correctly
for(i in seq_along(vaccine_efficacy_infection)) {
  if(vaccine_efficacy_disease[[i]] < vaccine_efficacy_infection[[i]]) {
    vaccine_efficacy_disease[[i]] <- vaccine_efficacy_infection[[i]]
  }
  vaccine_efficacy_disease[[i]] <- (vaccine_efficacy_disease[[i]] - vaccine_efficacy_infection[[i]]) / (1 - vaccine_efficacy_infection[[i]])
}


# NIMUE RUN
det_out_vac <- nimue::run(
  country = country,
  dur_R = 365,
  use_dde = TRUE,
  # all changes to Rt are set here using beta and not R0 and R0 being set below is ignored internally
  beta_set = new_betas,
  seeding_cases = 5,
  seeding_age_order = 6:10,
  tt_R0 = tt_s,
  R0 = new_betas,
  max_vaccine = new_vaccines,
  tt_vaccine = tt_vacc,
  time_period = length(betas)+forecast,
  dur_V = vaccine_durability,
  vaccine_efficacy_infection = vaccine_efficacy_infection,
  tt_vaccine_efficacy_infection = seq_along(vaccine_efficacy_infection),
  vaccine_efficacy_disease = vaccine_efficacy_disease,
  tt_vaccine_efficacy_disease = seq_along(vaccine_efficacy_disease),
  rel_infectiousness_vaccinated = 0.5,
  vaccine_coverage_mat = cov_mat)


# get results
index <- squire:::odin_index(det_out_vac$model)
D_index <- index$D
inf_cumu_index <- index$infections_cumu
hosp_demand_index <- index$hospital_demand
icu_demand_index <- index$ICU_demand
vacc_cumu_index <- index$vaccines_cumu




df <- data.frame(date = as.Date(date_deaths), real = deaths)
df2 <- data.frame(deaths = diff(rowSums(det_out_vac$output[,D_index,1])),
                  infections = diff(rowSums(det_out_vac$output[,inf_cumu_index,1])),
                  hospitilisations = rowSums(det_out_vac$output[-1,hosp_demand_index,1]),
                  critical = rowSums(det_out_vac$output[-1,icu_demand_index,1]))
df2$date <- seq.Date(as.Date(dates[2]), as.Date(dates[1]) + length(df2$deaths), 1)
df <- dplyr::left_join(df2, df, by = "date")

# make simple plot for checking deaths
# plot <- ggplot(df, aes(date, real)) +
#   geom_bar(aes(x = as.Date(date), y = real, fill = "Reported"),
#            stat = "identity",
#            fill = "#c59e96") +
#   geom_line(aes(date, zoo::rollmean(real, 7, na.pad = TRUE), color = "7-day Weekly Mean"), lwd = 1) +
#   geom_line(aes(date, deaths, color = "SEIR_Deaths"), lwd = 1) +
#   ylab("Deaths") +
#   scale_fill_manual(values = "#c59e96") +
#   scale_color_manual(values = c("black","#3f8ea7")) +
#   xlab("") +
#   scale_y_continuous(expand = c(0,0)) +
#   ggpubr::theme_pubclean() +
#   theme(axis.line = element_line(), legend.title = element_blank(), legend.key = element_blank()) +
#   ggtitle(country)



# And now get the reff out
# to do this we just need to first take our input beta and interpolate it
# for all time points explored
get_reff <- function(out, beta) {
  
  # mixing_matrix is already the mixing matrix that we pass to you in the country json files
  mixing_matrix <- squire:::process_contact_matrix_scaled_age(
    out$parameters$contact_matrix_set[[1]],
    out$parameters$population
  )
  
  t_now <- length(beta)
  
  # these parameters are found in pars_0.json that is imported in index.js
  dur_ICase <- out$parameters$dur_ICase
  dur_IMild <- out$parameters$dur_IMild
  rel_infectiousness <- out$odin_parameters$rel_infectiousness_vaccinated
  
  # vaccine efficacy is now time changing
  # so we make a list of all the arrays at each time point
  vei_list <- lapply(
    seq_len(nrow(out$odin_parameters$vaccine_efficacy_infection)),
    function(x) {
      out$odin_parameters$vaccine_efficacy_infection[x,,]
    })
  t_vei <- diff(c(out$odin_parameters$tt_vaccine_efficacy_infection, t_now))
  t_vei[1] <- t_vei[1]+1
  vei_list_long <- purrr::flatten(
    lapply(seq_along(t_vei), function(x) {
      rep(list(vei_list[[x]]), t_vei[x])
    }))
  
  prob_hosp_list <- lapply(
    seq_len(nrow(out$odin_parameters$prob_hosp)),
    function(x) {
      out$odin_parameters$prob_hosp[x,,]
    })
  t_prob_hosp <- diff(c(out$odin_parameters$tt_vaccine_efficacy_disease, t_now))
  t_prob_hosp[1] <- t_prob_hosp[1]+1
  prob_hosp_list_long <- purrr::flatten(
    lapply(seq_along(t_prob_hosp), function(x) {
      rep(list(prob_hosp_list[[x]]), t_prob_hosp[x])
    }))
  
  index <- nimue:::odin_index(out$model)
  
  # pop is a 17 length with population sizes in each age category
  pop <- out$parameters$population
  
  # in here we work out each time point the number of individuals in each age category in
  # the S compartment at each time point.
  susceptible <- array(
    out$output[seq(t_now),index$S,],
    dim=c(t_now, dim(index$S))
  )
  
  # We divide by the total population
  prop_susc <- sweep(susceptible, 2, pop, FUN='/')
  
  # We multiply by the effect of vaccines on onward infectiousness at each time point
  prop_susc <- vapply(
    seq_len(nrow(prop_susc)),
    FUN = function(i){ prop_susc[i,,]*vei_list_long[[i]]},
    FUN.VALUE = prop_susc[1,,]
  )
  
  # back into shape for next step
  prop_susc <- aperm(prop_susc, c(3,1,2))
  
  # Length 17 with relative R0 in each age category
  relative_R0_by_age <- lapply(prob_hosp_list_long, function(x) {
    x*dur_ICase + (1-x)*dur_IMild
  })
  
  # here we are looping over each time point to calculate the adjusted eigen
  # incorporating the proportion of the susceptible population in each age group
  adjusted_eigens <- vapply(
    seq(t_now),
    function(t) {
      Re(eigen(mixing_matrix * rowSums(prop_susc[t,,] * relative_R0_by_age[[t]]))$values[1])
    },
    numeric(1)
  )
  
  # multiply beta by the adjusted eigen at each time point to get Reff
  beta * adjusted_eigens
}

get_rts <- function(out, beta) {
  dur_ICase <- out$parameters$dur_ICase
  dur_IMild <- out$parameters$dur_IMild
  prob_hosp <- out$parameters$prob_hosp
  mixing_matrix <- squire:::process_contact_matrix_scaled_age(
    out$parameters$contact_matrix_set[[1]],
    out$parameters$population
  )
  beta * squire::adjusted_eigen(dur_IMild, dur_ICase, prob_hosp, mixing_matrix)
}

reff_beta <- approx(x = tt_s, y = new_betas, xout = seq_len(length(betas)+forecast), rule = 2, method = "constant")
reff <- get_reff(det_out_vac, beta = reff_beta$y)
rts <- get_rts(det_out_vac, beta = reff_beta$y)

# again we have everything indexing from t = 1 so remove the initial value and combine with dated data.frame
df$reff <- reff[-1]
df$rt <- rts[-1]

# reff_plot <- ggplot(df, aes(date, reff)) +
#   geom_line(color = "green") +
#   geom_line(aes(y = rt), color = "black") +
#   ylab("Reff (green), Rt (black)") +
#   scale_color_manual(values = c("green", "black")) +
#   xlab("") +
#   scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
#   ggpubr::theme_pubclean() +
#   theme(axis.line = element_line(), legend.title = element_blank(), 
#         legend.key = element_blank()) +
#   ggtitle(country)

current <- list(df, vaccines_day)

return(current)

}

plot_deaths<- function(df,country){
  ggplot(df, aes(date, real)) +
    geom_bar(aes(x = as.Date(date), y = real, fill = "Reported"),
             stat = "identity",
             fill = "#c59e96") +
    geom_line(aes(date, zoo::rollmean(real, 7, na.pad = TRUE), color = "7-day Weekly Mean"), lwd = 1) +
    geom_line(aes(date, deaths, color = "SEIR_Deaths"), lwd = 1) +
    ylab("Deaths") +
    scale_fill_manual(values = "#c59e96") +
    scale_color_manual(values = c("black","#3f8ea7")) +
    xlab("") +
    scale_y_continuous(expand = c(0,0)) +
    ggpubr::theme_pubclean() +
    theme(axis.line = element_line(), legend.title = element_blank(), legend.key = element_blank()) +
    ggtitle(country)
}

reff_plot <- function(df){
  df <- df %>% filter(date<Sys.Date())

  ggplot(df, aes(date, reff)) +
    geom_hline(yintercept = 1,linetype=2,colour="darkorange") +
    geom_line(color = "green") +
    geom_line(aes(y = rt), color = "black") +
    ylab("Reff (green), Rt (black)") +
    scale_color_manual(values = c("green", "black")) +
    xlab("") +
    #scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
    ggpubr::theme_pubclean() +
    theme(axis.line = element_line(), legend.title = element_blank(),
          legend.key = element_blank())
}





