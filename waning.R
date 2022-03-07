#vectorised version of the old function, removed the scale_cov_mat since that
#doesn't seem to do anything to; the model results, essentially vaccines
# are only limited by vaccine uptake and max_vaccine
get_coverage_mats <- function(iso3c, strategy, vaccine_uptake) {
  #get the strategy matrices
  strategies <- readRDS("coverage_strategies.Rds")
  #set up values for loop
  if(length(strategy) == 1){
    strategy <- rep(strategy, length(iso3c))
  }
  if(length(vaccine_uptake) == 1){
    vaccine_uptake <- rep(vaccine_uptake, length(iso3c))
  }
  if(length(strategy) != length(iso3c) |
     length(vaccine_uptake) != length(iso3c)){
    stop("Error: iso3c, strategy and vaccine_uptake must be the same length or have a length of 1.")
  }
  #loop through each country to get the scaled covareg matrix
  cov_mats <- lapply(seq_along(iso3c), function(index){
    if(strategy[index] == "HCW and Elderly") {
      cov_mat <- strategies[[iso3c[index]]]$whoPriority * vaccine_uptake[index]
    } else if (strategy[index] == "HCW, Elderly and High-Risk") {
      cov_mat <- strategies[[iso3c[index]]]$etagePriority * vaccine_uptake[index]
    } else if (strategy[index] == "Elderly") {
      cov_mat <- nimue::strategy_matrix("Elderly", max_coverage = vaccine_uptake[index], 0)
    } else if (strategy[index] == "All") {
      cov_mat <- nimue::strategy_matrix("All", max_coverage = vaccine_uptake[index], 0)
    } else {
      stop('Incorrect strategy. Must be one of "HCW and Elderly", "HCW, Elderly and High-Risk", "Elderly", "All"')
    }
    #ad-hoc adjustments
    if(vaccine_uptake[index] == 0.95){
      cov_mat <- (cov_mat/0.95)*0.9
      #add an extra row
      new_row <- cov_mat[nrow(cov_mat),]
      new_row[3] <- 0.8
      cov_mat <- rbind(cov_mat, new_row)
    } else if(vaccine_uptake[index] == 0.99){
      cov_mat <- (cov_mat/0.99)*0.9
      #add an extra row
      new_row <- cov_mat[nrow(cov_mat),]
      new_row[3] <- 0.9
      cov_mat <- rbind(cov_mat, new_row)
      #and another row to 95
      new_row <- (new_row/0.9)*0.95
      cov_mat <- rbind(cov_mat, new_row)
    }
    cov_mat
  })
  #give names
  names(cov_mats) <- iso3cs
  return(cov_mats)
}

#A function to calculate the vaccine uptakes for each country
get_vaccine_uptake <- function(iso3cs, dose_df, default_uptake, strategy){
  if(length(strategy) != length(iso3cs) & length(strategy) != 1){
    stop("Error: iso3c and strategy must be the same length or have a length of 1.")
  }
  #set up defaults
  uptakes <- rep(default_uptake, length(iso3cs))
  #calculate relevant vaccination population
  pop_df <- tibble(
    iso3c = iso3cs,
    strat = strategy
  ) %>%
    left_join(
      squire::population,
      by = "iso3c"
    ) %>%
    group_by(iso3c) %>%
    summarise(
      pop = if_else(
        strat %in% c("All", "Elderly"),
        #use all pop for these strategies
        as.double(sum(n)),
        #else remove non-adults
        as.double(sum(if_else(
          age_group %in% c("0-4", "5-9", "10-14"),
          as.double(0),
          as.double(n)
        )))
      )
    ) %>%
    ungroup()
  #check if higher uptake in data
  higher_iso3cs <- dose_df %>%
    group_by(iso3c) %>%
    summarise(vaccinated = sum(first_dose_per_day)) %>%
    left_join(
      pop_df,
      by = "iso3c"
    ) %>%
    mutate( #note some of these are >1 likely to inaccuracy in populations, but these are likely still above 80%
      vaccine_uptake = vaccinated/pop
    ) %>%
    filter(vaccine_uptake > default_uptake) %>%
    pull(iso3c)
  
  #if uptakes are larger we set the uptake to 95%, the highest we can correctly
  #model or expect to see
  uptakes[iso3cs %in% higher_iso3cs] <- 0.95
  
  #for now we also assume that for these countries the younger age group is
  #also vaccinated
  pop_df <- pop_df %>%
    unique() %>%
    group_by(iso3c) %>%
    mutate(pop =
             if_else(
               iso3c %in% higher_iso3cs,
               pop + squire::population %>%
                 rename(iso = iso3c) %>%
                 filter(
                   iso == iso3c &
                     age_group == "10-14"
                 ) %>%
                 pull(n),
               pop)
    ) %>%
    ungroup()
  higher_iso3cs_2 <- dose_df %>%
    group_by(iso3c) %>%
    summarise(vaccinated = sum(first_dose_per_day)) %>%
    left_join(
      pop_df,
      by = "iso3c"
    ) %>%
    mutate( #note some of these are >1 likely to inaccuracy in populations, but these are likely still above 80%
      vaccine_uptake = vaccinated/pop
    ) %>%
    filter(vaccine_uptake > default_uptake) %>%
    pull(iso3c)
  
  #these ones need an extra slot give it 1 for now and see
  uptakes[iso3cs %in% higher_iso3cs_2] <- 0.99
  
  return(uptakes)
}


get_single_dose_data <- function(
  owid, vacc_types, vdm, who_vacc, who_vacc_meta, single_dose_vaccines
){
  vdm %>% #get info on doses of any single dose vaccines from vdm
    filter(date == max(date)) %>%
    group_by(iso3c) %>%
    mutate(single_dose = if_else(
      vaccine %in% single_dose_vaccines,
      1,
      0
    )) %>%
    summarise(
      single_doses = sum(total_vaccinations*single_dose)
    ) %>%
    mutate(single_doses = if_else(
      single_doses == 0,
      as.numeric(NA),
      single_doses
    )) %>%
    full_join( #get info on what single dose vaccines are used, if only single we know
      #all vaccine doses are single doses
      vacc_types %>%
        dplyr::select(iso3c, vaccine_types) %>%
        rowwise() %>%
        filter(any(single_dose_vaccines %in% vaccine_types)) %>%
        mutate(
          single_dose_percentage = if_else(
            all(vaccine_types %in% single_dose_vaccines),
            1,
            as.numeric(NA)
          )
        ) %>%
        dplyr::select(iso3c, single_dose_percentage),
      by = "iso3c"
    ) %>%
    full_join(
      who_vacc %>%
        rowwise() %>%
        filter(any(VACCINES_USED %in% single_dose_vaccines)) %>%
        mutate(
          single_dose_percentage_1 = if_else(
            all(VACCINES_USED %in% single_dose_vaccines),
            1,
            as.numeric(NA)
          )
        ) %>%
        dplyr::select(iso3c, single_dose_percentage_1),
      by = "iso3c"
    ) %>%
    mutate(
      single_dose_percentage = if_else(
        is.na(single_dose_percentage),
        single_dose_percentage_1,
        single_dose_percentage
      ),
      recieved_single_dose_vaccines = TRUE #indicator for checking later
    ) %>%
    dplyr::select(iso3c, single_doses, single_dose_percentage, recieved_single_dose_vaccines)
}

single_dose_adjust_owid <- function(owid, single_dose_df){
  #only do those with single doses
  modified <- owid %>%
    group_by(iso3c) %>%
    filter(iso3c %in% single_dose_df$iso3c & ( #also only those with vaccine data, so it doesn't throw errors
      any(!is.na(people_fully_vaccinated)) |
        any(!is.na(people_fully_vaccinated_per_hundred)) |
        any(!is.na(total_vaccinations)) |
        any(!is.na(total_vaccinations_per_hundred))
    )) %>%
    left_join(single_dose_df,
              by = "iso3c") %>%
    mutate(
      people_fully_vaccinated = if_else(
        (!is.na(people_fully_vaccinated)) & (!is.na(single_doses)),
        people_fully_vaccinated*(1-single_doses/max(people_fully_vaccinated, na.rm = TRUE)),
        people_fully_vaccinated
      ),
      people_fully_vaccinated_per_hundred = if_else(
        !is.na(people_fully_vaccinated) & !is.na(single_doses),
        people_fully_vaccinated_per_hundred*(1-single_doses/max(people_fully_vaccinated, na.rm = TRUE)),
        people_fully_vaccinated_per_hundred
      )) %>% #now to do countries that only had single dose vaccines
    mutate(
      people_fully_vaccinated = if_else(
        identical(single_dose_percentage, 1) & (!is.na(people_fully_vaccinated)),
        0,
        people_fully_vaccinated
      ),
      people_fully_vaccinated_per_hundred = if_else(
        identical(single_dose_percentage, 1) & (!is.na(people_fully_vaccinated_per_hundred)),
        0,
        people_fully_vaccinated_per_hundred
      ),
      total_vaccinations = if_else(
        identical(single_dose_percentage, 1) & (!is.na(total_vaccinations)),
        people_vaccinated,
        total_vaccinations
      ),
      total_vaccinations_per_hundred = if_else(
        identical(single_dose_percentage, 1) & (!is.na(total_vaccinations)),
        people_vaccinated_per_hundred,
        total_vaccinations_per_hundred
      )
    ) %>%
    dplyr::select(!c(single_dose_percentage, single_doses)) %>%
    ungroup()
  #add back in data
  new <- rbind(
    owid %>%
      filter(!(iso3c %in% modified$iso3c)) %>%
      mutate(
        recieved_single_dose_vaccines = if_else(
          iso3c %in% single_dose_df$iso3c,
          TRUE,
          as.logical(NA)
        )
      ),
    modified
  ) %>%
    arrange(
      iso3c, date
    )
}

single_dose_adjust_who_vacc <- function(who_vacc, single_dose_df){
  who_vacc %>%
    left_join(single_dose_df,
              by = "iso3c") %>%
    mutate(PERSONS_FULLY_VACCINATED = if_else(
      (!is.na(PERSONS_FULLY_VACCINATED)) & (!is.na(single_doses)),
      PERSONS_FULLY_VACCINATED*(1-single_doses/TOTAL_VACCINATIONS),
      as.numeric(PERSONS_FULLY_VACCINATED)
    ),
    PERSONS_FULLY_VACCINATED_PER100 = if_else(
      (!is.na(PERSONS_FULLY_VACCINATED_PER100)) & (!is.na(single_doses)),
      PERSONS_FULLY_VACCINATED_PER100*(1-single_doses/TOTAL_VACCINATIONS),
      as.numeric(PERSONS_FULLY_VACCINATED_PER100)
    )) %>%
    mutate(
      PERSONS_VACCINATED_1PLUS_DOSE = if_else(
        identical(single_dose_percentage, 1) & (!is.na(PERSONS_VACCINATED_1PLUS_DOSE)),
        TOTAL_VACCINATIONS,
        as.numeric(PERSONS_VACCINATED_1PLUS_DOSE)
      ),
      PERSONS_VACCINATED_1PLUS_DOSE_PER100 = if_else(
        identical(single_dose_percentage, 1) & (!is.na(PERSONS_VACCINATED_1PLUS_DOSE_PER100)),
        TOTAL_VACCINATIONS_PER100,
        PERSONS_VACCINATED_1PLUS_DOSE_PER100
      ),
      PERSONS_FULLY_VACCINATED = if_else(
        identical(single_dose_percentage, 1) & (!is.na(PERSONS_FULLY_VACCINATED)),
        0,
        PERSONS_FULLY_VACCINATED
      ),
      PERSONS_FULLY_VACCINATED_PER100 = if_else(
        identical(single_dose_percentage, 1) & (!is.na(PERSONS_FULLY_VACCINATED_PER100)),
        0,
        PERSONS_FULLY_VACCINATED_PER100
      )
    )
}

get_dose_ts <- function(owid, who_vacc, who_vacc_meta, iso3cs, date_0){
  #owid data
  owid_ts <- get_dose_ts_owid(owid, iso3cs)
  who_ts <- get_dose_ts_who(who_vacc, who_vacc_meta, iso3cs)
  #prioritise the owid data then who
  iso_in_owid <- unique(owid_ts$iso3c)
  iso_in_who <- setdiff(unique(who_ts$iso3c), iso_in_owid)
  iso_no_data <- setdiff(iso3cs, c(iso_in_owid, iso_in_who))
  #create blank data for the no data countries
  combined_df <- rbind(
    owid_ts %>%
      filter(iso3c %in% iso_in_owid),
    who_ts%>%
      filter(iso3c %in% iso_in_who),
    data.frame(#create blank data for the no data countries
      date = rep(date_0, length(iso_no_data)),
      iso3c = iso_no_data,
      vacc_per_day = 0,
      dose_ratio = 0,
      imputed = FALSE,
      first_dose_per_day = 0,
      second_dose_per_day = 0
    )
  )
  #check that we have a point for every date between min  and max for eahc country
  date_check <- unlist(lapply(iso3cs, function(country){
    country_df <- combined_df %>% filter(iso3c == country)
    all(
      seq(min(country_df$date), max(country_df$date), by = 1) %in%
        country_df$date
    )
  }))
  if(any(!date_check)){
    stop(paste0(
      "Did not generate a observation for each day for countries: ",
      paste0(iso3cs[!date_check], collapse = ", "),
      " Please check get_dose_ts function"
    ))
  }
  
  #expand up to current date
  #first limit data to this date
  combined_df <- combined_df %>%
    filter(date <= date_0) %>%
    filter(cumsum(vacc_per_day) > 0 | length(vacc_per_day) == 1)
  #add dates up to current date
  #we'll assume that vacc_per_day stays at its final weekly average
  #percentage of vacciantions that are second doses will be its final weekly average
  combined_df <- combined_df %>% #add indicator for plotting later
    #extend values to date_0
    mutate(imputed = FALSE)%>%
    #add dates up to current date
    complete(date = seq(min(date), date_0, by = 1)) %>%
    left_join( #add averages for vacc_per_day
      combined_df %>%
        group_by(iso3c) %>%
        filter(date >= max(date) - 7) %>%
        summarise(
          vacc_per_day_week_ave = mean(vacc_per_day)
        ),
      by = "iso3c"
    ) %>%
    left_join( #add averages for percentage second dose
      combined_df %>%
        group_by(iso3c) %>%
        filter(!is.na(second_dose_per_day)) %>%
        filter(date >= max(date) - 7) %>%
        summarise(
          percentage_second_dose_week_ave = mean(
            case_when(
              #if there are vaccinations then get the percentage
              vacc_per_day > 0 ~ second_dose_per_day/vacc_per_day,
              #if there are none at all set to 0 so the aveage is 0
              all(vacc_per_day == 0) ~ 0,
              #otherwise, if only some are 0 then set these to NA so they don't count
              TRUE ~ as.numeric(NA)),
            na.rm = TRUE
          )
        ),
      by = "iso3c"
    ) %>%
    mutate(
      vacc_per_day = if_else(
        is.na(vacc_per_day),
        vacc_per_day_week_ave,
        vacc_per_day
      )
    )
  #now we have to iteratively calculate the first doses, limiting the percentage
  #second doses so that dose ratio does not exceed 1
  missing_df <- combined_df %>% summarise(
    missing = sum(is.na(dose_ratio))
  )
  
  while(sum(missing_df$missing) > 0){
    combined_df <- mutate(combined_df,
                          percentage_second_dose =
                            pmin(
                              percentage_second_dose_week_ave,
                              (vacc_per_day + dplyr::lag(cumsum(first_dose_per_day)
                                                         - cumsum(second_dose_per_day), 1))/(2*vacc_per_day)
                            ),
                          second_dose_per_day = if_else(
                            is.na(
                              second_dose_per_day
                            ),
                            vacc_per_day*percentage_second_dose,
                            second_dose_per_day
                          ),
                          first_dose_per_day = if_else(
                            is.na(first_dose_per_day),
                            vacc_per_day - second_dose_per_day,
                            first_dose_per_day
                          ),
                          dose_ratio = if_else(
                            is.na(dose_ratio),
                            cumsum(second_dose_per_day)/cumsum(first_dose_per_day),
                            dose_ratio
                          )
    ) %>%
      dplyr::select(!percentage_second_dose)
    
    
    missing_df <- combined_df %>% summarise(
      missing = sum(is.na(dose_ratio))
    )
    
    #message(sum(missing_df$missing))
  }
  #final changes
  combined_df <- combined_df %>%
    mutate(
      imputed = if_else(
        is.na(imputed),
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::select(!c(vacc_per_day_week_ave, percentage_second_dose_week_ave))
  
  #some might be less than zero (but close to)
  combined_df <- mutate(combined_df,
                        second_dose_per_day = if_else(
                          second_dose_per_day < 0 & second_dose_per_day > -1,
                          0,
                          second_dose_per_day
                        ),
                        first_dose_per_day = if_else(
                          first_dose_per_day < 0 & first_dose_per_day > -1,
                          0,
                          first_dose_per_day
                        ))
  
  #any other potential cleaning
  
  return(combined_df)
}

get_dose_ts_owid <- function(owid, iso3cs){
  #owid data
  owid <- owid %>%
    filter(iso3c %in% iso3cs) %>%
    left_join(
      squire::population %>%
        group_by(iso3c) %>%
        summarise(population = sum(n)),
      by = "iso3c"
    ) %>%
    group_by(iso3c) %>%
    mutate(date = as.Date(date))
  owid_m <- owid %>%
    mutate( #derive from per 100 if not present
      vaccinations_cum = if_else(
        rep(all(is.na(total_vaccinations)), length(total_vaccinations)),
        (total_vaccinations_per_hundred*population)/100,
        total_vaccinations
      ),
      #use smoothed if possible
      vaccinations =
        case_when(
          !is.na(new_vaccinations_smoothed) ~ new_vaccinations_smoothed,
          !is.na(new_vaccinations_smoothed_per_million) ~ (new_vaccinations_smoothed_per_million*population)/10^6,
          !is.na(new_vaccinations) ~ new_vaccinations,
          TRUE ~ as.numeric(NA)
        )
    ) %>%
    mutate( #derive from per 100 if not present
      first_dose_cum =
        case_when(
          !is.na(people_vaccinated) ~ people_vaccinated,
          !is.na(people_vaccinated_per_hundred) ~ people_vaccinated*population/100,
          TRUE ~ as.numeric(NA)
        )
    ) %>%
    mutate( #derive from per 100 if not present
      second_dose_cum =
        case_when(
          !is.na(people_fully_vaccinated) ~ people_fully_vaccinated,
          !is.na(people_fully_vaccinated_per_hundred) ~ people_fully_vaccinated_per_hundred*population/100,
          TRUE ~ as.numeric(NA)
        )
    ) %>%
    dplyr::select(iso3c, date, vaccinations, vaccinations_cum, first_dose_cum, second_dose_cum)
  #extend data so there is on entry each day
  owid_m <- complete(owid_m, date = seq(min(date), max(date), by = 1))
  #filter data and entries
  owid_m <- owid_m %>%
    mutate(aux = !is.na(vaccinations) |
             !is.na(vaccinations_cum) |
             !is.na(first_dose_cum) |
             !is.na(second_dose_cum)) %>% #only keep countries with some data
    filter(any(aux)) %>% #remove trailing zeros
    filter(rev(cumsum(rev(aux))) > 0) %>% #remove leading zeros
    filter(cumsum(aux) > 0) %>%
    dplyr::select(!aux)
  #if cumulative is greater than 0 at start add two weeks and have it build up
  countries_to_extend <- owid_m %>%
    mutate(start_vacc = case_when(
      !is.na(vaccinations_cum) ~ vaccinations_cum,
      !is.na(first_dose_cum) ~ first_dose_cum,
      !is.na(second_dose_cum) ~ second_dose_cum,
      TRUE ~ as.numeric(NA)
    )) %>%
    filter(!is.na(start_vacc)) %>%
    filter(date == min(date)) %>%
    filter(start_vacc > 0) %>%
    dplyr::select(iso3c, date, start_vacc) %>% #how many days to extend back
    mutate(extension = min(21, start_vacc))
  #extended data
  owid_m <- owid_m %>%
    mutate(imputed = FALSE) %>%
    rbind(
      do.call(
        rbind,
        lapply(
          1:nrow(countries_to_extend), function(x){
            x <- countries_to_extend[x,]
            df <- data.frame(
              iso3c = x$iso3c,
              date = seq(x$date - x$extension, x$date, by = 1),
              vaccinations = as.numeric(NA),
              vaccinations_cum = seq(0, x$start_vacc, length.out = x$extension + 1),
              first_dose_cum = as.numeric(NA),
              second_dose_cum = as.numeric(NA),
              imputed = TRUE
            )
            df[-nrow(df),]
          }
        )
      )
    ) %>%
    arrange(iso3c, date) %>%
    mutate(#fill in vacciantions
      vaccinations = if_else(
        imputed,
        diff(c(0, vaccinations_cum)),
        vaccinations
      )
    )
  #figure out whether to use daily or cumulative
  #which has more data
  owid_vaccine_per_day <- owid_m %>%
    summarise(daily = sum(is.na(vaccinations)),
              cumulative = sum(is.na(vaccinations_cum))) %>%
    transmute(
      iso3c = iso3c,
      use_daily =
        if_else(
          daily <= cumulative,
          TRUE,
          FALSE
        )
    ) %>%
    right_join(owid_m,
               by = "iso3c") %>%
    group_by(iso3c)
  
  #get doses each day
  owid_vaccine_per_day <- owid_vaccine_per_day %>% #get rid of any tailing zeros depending on method
    filter(!(use_daily & rev(cumsum(rev(!is.na(vaccinations)))) == 0),
           !((!use_daily) & rev(cumsum(rev(!is.na(vaccinations_cum)))) == 0)
    ) %>%
    filter(!(use_daily & cumsum(!is.na(vaccinations)) == 0),
           !((!use_daily) & cumsum(!is.na(vaccinations_cum)) == 0)
    )  %>%
    mutate(
      #set imputed variable
      imputed = if_else(
        (use_daily & is.na(vaccinations)) |
          (!(use_daily) & is.na(vaccinations_cum)),
        TRUE,
        imputed
      ),
      vacc_per_day =
        if_else(
          use_daily,
          #linaerly interpolate the data
          linearly_interpolate(vaccinations),
          diff(c(0, linearly_interpolate(vaccinations_cum)))
        ),
      before_lst = ((1:length(vacc_per_day)) <= which.max(vaccinations_cum)),
      lt_d = use_daily & before_lst,
      prop_multi = ((max(vaccinations_cum, na.rm = TRUE) -
                       sum(vacc_per_day[before_lst]) +
                       sum(vacc_per_day[lt_d])) /
                      sum(vacc_per_day[lt_d])),
      vacc_per_day =
        if_else(
          lt_d,
          #scale so that imputed values add to last cumulative value (if before)
          prop_multi *
            vacc_per_day,
          vacc_per_day
        )
    ) %>%
    dplyr::select(iso3c, date, vacc_per_day, imputed) %>%
    na.omit() #removes some leading nas
  
  #now add the second/first doses
  owid_merge <-  owid_vaccine_per_day %>%
    left_join(owid_m %>% dplyr::select(iso3c, date, first_dose_cum, second_dose_cum) %>%
                filter(!is.na(first_dose_cum) | !is.na(second_dose_cum)),
              by = c("iso3c", "date")
    ) %>% #linearly interpolate assuming that both start at 0 and second
    #dose stays at 0 until 18 days
    group_by(iso3c) %>%
    mutate(
      first_dose_cum = if_else(
        1:length(first_dose_cum) == 1 & is.na(first_dose_cum),
        0,
        first_dose_cum
      ),
      second_dose_cum = if_else(
        1:length(second_dose_cum) <= 18,
        0,
        second_dose_cum
      ),
      first_dose_cum = linearly_interpolate(first_dose_cum),
      second_dose_cum = linearly_interpolate(second_dose_cum),
      #adjust cumulative so they are consistent with the vaccinations per day
      percentage_second = if_else(
        first_dose_cum == 0,
        0,
        diff(c(0, second_dose_cum))/
          (diff(c(0, first_dose_cum)) +
             diff(c(0, second_dose_cum)))
      ),
      first_dose_per_day = vacc_per_day*(1-percentage_second),
      second_dose_per_day = vacc_per_day*percentage_second
    )
  
  #sort out NAs where there are no new doses in either, we'll just assume values
  #that have no effect on dose ratio
  #due to repition needed we have to do it outside dplyr
  missing_df <- owid_merge %>% summarise(
    missing = sum(is.na(first_dose_per_day))
  )
  
  while(sum(missing_df$missing) > 0){
    owid_merge <- mutate(owid_merge,
                         prev_dose_ratio =
                           dplyr::lag(cumsum(second_dose_per_day), 1)/
                           dplyr::lag(cumsum(first_dose_per_day), 1),
                         first_dose_per_day = if_else(
                           is.na(first_dose_per_day),
                           vacc_per_day/(1 + prev_dose_ratio),
                           first_dose_per_day
                         ),
                         second_dose_per_day = if_else(
                           is.na(second_dose_per_day),
                           vacc_per_day - first_dose_per_day,
                           second_dose_per_day
                         )
    ) %>%
      dplyr::select(!prev_dose_ratio)
    #any values after the first will still be NA due to cumsum return NA,
    #so we need to repeat until they are all filled
    missing_df <- owid_merge %>% summarise(
      missing = sum(is.na(first_dose_per_day))
    )
    
    #message(sum(missing_df$missing))
  }
  owid_merge <- owid_merge %>%
    mutate(
      first_dose_cum =  cumsum(first_dose_per_day),
      second_dose_cum =  cumsum(second_dose_per_day),
      #correct for if second doses are larger than first,
      #assume that these are single dose vaccines and move them over to first dose
      single_doses_vacc = if_else(
        first_dose_cum < second_dose_cum,
        diff(c(0, second_dose_cum)) - diff(c(0, first_dose_cum)),
        0
      ),
      first_dose_per_day =  diff(c(0, first_dose_cum)) + single_doses_vacc,
      second_dose_per_day =  diff(c(0, second_dose_cum)) - single_doses_vacc,
      
      
      first_dose_cum = cumsum(first_dose_per_day),
      second_dose_cum = cumsum(second_dose_per_day),
      all_dose_cum = cumsum(vacc_per_day)
    ) %>%
    dplyr::select(
      iso3c, date, vacc_per_day, first_dose_per_day, second_dose_per_day, imputed
    )
  #now get the dose ratio
  owid_merge <- owid_merge %>%
    mutate(dose_ratio = if_else(
      cumsum(first_dose_per_day) == 0,
      0,
      cumsum(second_dose_per_day)/cumsum(first_dose_per_day)
    )
    )
  
  #checl that final dose ratio is not 0.1 off from the final dose ratio in owid
  owid_rescaled <- suppressWarnings(owid_merge %>%
                                      arrange(iso3c, date) %>%
                                      summarise(
                                        dose_ratio = tail(na.omit(dose_ratio),1)
                                      ) %>%
                                      left_join(
                                        owid_m %>%
                                          group_by(iso3c) %>%
                                          summarise(
                                            final_dose_ratio = max(second_dose_cum, na.rm = TRUE)/
                                              max(first_dose_cum, na.rm = TRUE)
                                          ),
                                        by = "iso3c"
                                      ) %>%
                                      mutate(
                                        diff = abs(dose_ratio - final_dose_ratio)
                                      ) %>%
                                      filter(diff > 0.15))
  #slight issue but there is not much we can do
  
  #final adjustments where change in dose ratio is impossible for given number
  #of doses, can occur because we favour the smoothed vaccinations
  problem <- test_for_errors_in_dose_ratio(owid_merge, dose_ratio)
  if(length(problem) > 1){
    stop(paste0("Problem with calculating dose ratios in the following countries: ",
                paste0(problem, collapse = ", ")
    ))
  }
  return(owid_merge)
}

get_dose_ts_who <- function(who_vacc, who_vacc_meta, iso3cs){
  #have to impute a time series, only use if owid not available
  #get any useful info out of meta df
  meta_df <- who_vacc_meta %>%
    filter(iso3c %in% iso3cs) %>%
    group_by(iso3c) %>%
    mutate(
      START_DATE = as.Date(START_DATE),
      END_DATE = as.Date(END_DATE),
      AUTHORIZATION_DATE = as.Date(AUTHORIZATION_DATE)
    ) %>%
    summarise(
      start_date = as.Date(ifelse(
        any(!is.na(START_DATE)),
        min(START_DATE, na.rm = TRUE),
        NA
      ), origin = "1970-01-01"),
      end_date = as.Date(ifelse(
        any(!is.na(END_DATE)),
        max(END_DATE, na.rm = TRUE),
        NA
      ), origin = "1970-01-01"),
      aut = as.Date(ifelse(
        any(!is.na(AUTHORIZATION_DATE)),
        min(AUTHORIZATION_DATE, na.rm = TRUE),
        NA
      ), origin = "1970-01-01")
    ) %>%
    mutate(
      start_date = if_else(
        is.na(start_date),
        aut,
        start_date
      )
    ) %>%
    dplyr::select(iso3c, start_date, end_date)
  
  who_comb <- full_join(
    who_vacc %>% filter(iso3c %in% iso3cs),
    meta_df,
    by = "iso3c"
  )
  
  #get the data we'll use to set up vaccinations
  who_comb <- who_comb %>%
    mutate(
      start_date = if_else( #use meta start date if first date is missing
        is.na(as.Date(FIRST_VACCINE_DATE)),
        start_date,
        as.Date(FIRST_VACCINE_DATE)
      ),
      end_date = if_else( #use meta end date if first date is missing
        is.na(as.Date(DATE_UPDATED)),
        end_date,
        as.Date(DATE_UPDATED)
      ),
      single_dose_plus = as.numeric(PERSONS_VACCINATED_1PLUS_DOSE),
      two_dose = as.numeric(PERSONS_FULLY_VACCINATED),
      vaccinations_total = case_when(
        !is.na(as.numeric(TOTAL_VACCINATIONS)) ~ as.numeric(TOTAL_VACCINATIONS),
        (!is.na(single_dose_plus)) & (!is.na(two_dose)) ~ single_dose_plus + two_dose,
        !is.na(single_dose_plus) ~ single_dose_plus,
        !is.na(two_dose) ~ two_dose*2,
        TRUE ~ as.numeric(NA)
      ),
      #update single/second dose if missing
      single_dose_plus = if_else(
        is.na(single_dose_plus),
        vaccinations_total - two_dose,
        single_dose_plus
      ),
      two_dose = if_else(
        is.na(two_dose),
        vaccinations_total - single_dose_plus,
        two_dose
      )
    ) %>%
    dplyr::select(iso3c, start_date, end_date, vaccinations_total, single_dose_plus, two_dose,
           recieved_single_dose_vaccines)
  #for countries with more two dose than single dose we assume the difference
  # are single dose vaccines and swap them over to better represent one dose efficacy
  who_comb <- who_comb %>%
    mutate(
      recieved_single_dose_vaccines = if_else(
        is.na(recieved_single_dose_vaccines),
        FALSE,
        recieved_single_dose_vaccines
      ),
      single_dose_vacc = if_else(
        #recieved_single_dose_vaccines &
        (single_dose_plus < two_dose),
        two_dose - single_dose_plus,
        0
      ),
      #adjust counts
      single_dose_plus = single_dose_plus + single_dose_vacc,
      two_dose = two_dose - single_dose_vacc
    ) %>% #use the two values to calculate the dose ratio
    mutate(
      dose_ratio_final = two_dose/single_dose_plus
    ) %>%
    dplyr::select(iso3c, start_date, end_date, vaccinations_total, dose_ratio_final)
  
  #impute any missing values using regional medians etc
  who_comb <- who_comb %>%
    ungroup() %>%
    mutate(
      #get region/sub_region
      region = countrycode(iso3c, origin = "iso3c", destination = "region"),
      sub_region = countrycode(iso3c, origin = "iso3c", destination = "region23")
    ) %>%
    group_by(sub_region) %>%
    mutate(
      across(
        .cols = c(start_date, end_date, vaccinations_total, dose_ratio_final),
        ~if_else(
          is.na(.x) | is.infinite(.x),
          median(.x, na.rm = TRUE),
          .x
        )
      )
    ) %>%
    group_by(region) %>%
    mutate(
      across(
        .cols = c(start_date, end_date, vaccinations_total, dose_ratio_final),
        ~if_else(
          is.na(.x) | is.infinite(.x),
          median(.x, na.rm = TRUE),
          .x
        )
      )
    ) %>%
    ungroup() %>%
    mutate(
      across(
        .cols = c(start_date, end_date, vaccinations_total, dose_ratio_final),
        ~if_else(
          is.na(.x) | is.infinite(.x),
          median(.x, na.rm = TRUE),
          .x
        )
      )
    ) %>%
    group_by(iso3c)
  
  #generate TS
  #assume that first 21 days are a build up period (if possible), then vaccinations
  #per day remain steady across that period
  #assume dose ratio is 0 for the first 18 days and then build up to the value
  #for the next 21 days then remain at the same rate
  ts_df <- do.call(
    rbind,
    lapply(1:nrow(who_comb), function(x){
      dates <- seq(who_comb$start_date[x], who_comb$end_date[x], by = 1)
      #calculate the vaccines per day
      if(length(dates) <= 21){
        v_r_b <- who_comb$vaccinations_total[x]/sum(1:length(dates))
        vacc_per_day <- v_r_b*seq(1, length(dates))
      } else {
        v_r_b <- who_comb$vaccinations_total[x]/(sum(1:21) + 21*(length(dates)-21))
        v_r_pb <- 21*v_r_b
        vacc_per_day <- c(v_r_b*seq(1, 21), rep(v_r_pb, length(dates)-21))
      }
      #calculate dose ratio over time
      if(length(dates) <= 18){
        dr_r_0 <- who_comb$dose_ratio_final[x]/length(dates)
        dose_ratio <- c(
          dr_r_0*seq(1, length(dates))
        )
      } else if (length(dates) <= 18 + 21){
        end_0 <- floor((18/(18+21)) * length(dates))
        dr_r_0 <- 0
        dr_r_b <- who_comb$dose_ratio_final[x]/(length(dates) - end_0)
        dose_ratio <- c(
          rep(dr_r_0, end_0),
          dr_r_b*seq(1,length(dates) - end_0)
        )
      } else {
        dr_r_0 <- 0
        dr_r_b <- who_comb$dose_ratio_final[x]/21
        dr_r_pb <- who_comb$dose_ratio_final[x]
        dose_ratio <- c(
          rep(dr_r_0, 18),
          dr_r_b * seq(1, 21),
          rep(dr_r_pb, length(dates) - 21 - 18)
        )
      }
      data.frame(
        iso3c = who_comb$iso3c[x],
        date = dates,
        vacc_per_day = vacc_per_day,
        imputed = TRUE,
        dose_ratio
      )
    })
  )
  
  #calculate the first/second doses each day (should have no issues)
  ts_df <- ts_df %>%
    group_by(iso3c) %>%
    mutate(
      first_dose_per_day = diff(c(0,
                                  cumsum(vacc_per_day)/(1 + dose_ratio)
      )),
      second_dose_per_day = vacc_per_day - first_dose_per_day,
      #possible less than 0s due to round errors so we set this to 0
      second_dose_per_day = if_else(
        abs(second_dose_per_day) < 0.1,
        0,
        second_dose_per_day
      )
    )
  #final check
  problem <- test_for_errors_in_dose_ratio(ts_df, dose_ratio)
  if(length(problem) > 1){
    stop(paste0("Drop with calculating dose ratios in the following countires: ",
                paste0(problem, collapse = ", ")
    ))
  }
  return(ts_df)
}

linearly_interpolate <- function(vector){
  if_else(
    is.na({vector}),
    approx(1:length({vector}), {vector}, 1:length({vector}), yright = NA)$y,
    {vector}
  )
}

test_for_errors_in_dose_ratio <- function(df, dose_ratio){
  df %>%
    mutate(
      first_dose = diff(c(0, cumsum(vacc_per_day)/({dose_ratio} + 1))),
      problematic = first_dose < -0.1 | first_dose > vacc_per_day + 0.1
    ) %>%
    filter(problematic) %>%
    pull(iso3c) %>%
    unique()
}


get_eff_disease <- function(days_from_vacc, efficacy){
  #for now use Toms RTM values
  reduction_per_day <- reduction_per_day_disease
  no_waning_period <- no_waning_period
  if_else(days_from_vacc < no_waning_period,
          efficacy,
          efficacy*reduction_per_day^(days_from_vacc - no_waning_period)
  )
}

get_eff_disease_second <- function(days_from_vacc, days_between_doses,
                                   efficacy_first, efficacy_second){
  c(
    get_eff_disease(
      days_from_vacc[days_from_vacc<days_between_doses],
      efficacy_first),
    get_eff_disease(
      days_from_vacc[days_from_vacc>=days_between_doses],
      efficacy_second)
  )
}

get_eff_infection <- function(days_from_vacc, efficacy){
  #for now use Toms RTM values
  reduction_per_day <- reduction_per_day_infection
  no_waning_period <- no_waning_period
  if_else(days_from_vacc < no_waning_period,
          efficacy,
          efficacy*reduction_per_day^(days_from_vacc - no_waning_period)
  )
}

get_eff_infection_second <- function(days_from_vacc, days_between_doses,
                                     efficacy_first, efficacy_second){
  c(
    get_eff_infection(
      days_from_vacc[days_from_vacc<days_between_doses],
      efficacy_first),
    get_eff_infection(
      days_from_vacc[days_from_vacc>=days_between_doses],
      efficacy_second)
  )
}

#this function controls the dlay in tentry
vaccine_delay <- function(days_from_vacc, delay){
  pgamma(days_from_vacc, rate = 1/delay, shape = 2)
}

#function to calculate the waning efficacy for a country
calculate_waning_eff <- function(
  data, #data frame countaining info
  dates, #name of the variable with the dates
  first_doses, #name of variable of timeseries of first doses
  efficacy_infection_first, #names of variables of the efficacy for each
  efficacy_infection_second, #type of protection/dose
  efficacy_disease_first,
  efficacy_disease_second,
  dur_vaccine_delay, #the level of delay in first dose protection (numeric)
  days_between_doses, #the assumed delay between first dose and second dose
  countries = NULL,#name of the country variables, if null not used
  diagnostic = FALSE #return INTERNAL variables
){
  #inital prep
  if(dur_vaccine_delay == 0){
    delay_func <- function(days_from_first_dose){
      rep(1, length(days_from_first_dose))
    }
  }else{
    delay_func <- function(days_from_first_dose){
      vaccine_delay(days_from_first_dose, dur_vaccine_delay)
    }
  }
  data <-  group_by(data, {{ countries }})
  #calculate effective vaccine efficacy for each time and country
  data <- data %>% mutate(
    #calculate for disease
    INTERNAL = #get all combinations of dates
      expand.grid({{ dates }},
                  {{ dates }}) %>%
      cbind(first_dose =  {{ first_doses }}, #add doses and efficacies
            ve_d_f =  {{ efficacy_disease_first}},
            ve_d_s =  {{ efficacy_disease_second }},
            ve_i_f =  {{ efficacy_infection_first}},
            ve_i_s =  {{ efficacy_infection_second }}) %>%
      mutate( #calculate the days from
        days_from_vacc = as.numeric(Var2 - Var1),
        date = Var2
      )  %>%
      filter(days_from_vacc >= 0) %>% #remove days that are after our date
      arrange(date, days_from_vacc) %>% #arrange by date
      group_by(date) %>% #group by date
      mutate(#adjust doses for the delay
        first_dose_delay = delay_func(days_from_vacc)*first_dose,
        #get the latest efficacy data since this is the one that applies at this
        #date
        ve_d_f = head(ve_d_f, 1),
        ve_d_s = head(ve_d_s, 1),
        ve_i_f = head(ve_i_f, 1),
        ve_i_s = head(ve_i_s, 1)
      ) %>%
      summarise( #calculate the first/second dose efficacy for each type
        vaccine_efficacy_disease_first = sum(
          first_dose_delay*get_eff_disease(days_from_vacc, head(ve_d_f,1))
        )/sum(first_dose_delay),
        vaccine_efficacy_infection_first = sum(
          first_dose_delay*get_eff_infection(days_from_vacc, head(ve_i_f, 1))
        )/sum(first_dose_delay),
        vaccine_efficacy_disease_second = sum(
          first_dose_delay*get_eff_disease_second(days_from_vacc,
                                                  head({{ days_between_doses }}, 1),
                                                  head(ve_d_f, 1),
                                                  head(ve_d_s, 1))
        )/sum(first_dose_delay),
        vaccine_efficacy_infection_second = sum(
          first_dose_delay*get_eff_infection_second(days_from_vacc,
                                                    head({{ days_between_doses }}, 1),
                                                    head(ve_i_f, 1),
                                                    head(ve_i_s, 1))
        )/sum(first_dose_delay),
      ) %>%
      ungroup() %>%
      dplyr::select(!date)
  ) %>%
    mutate(
      #calculate final values
      vaccine_efficacy_disease = INTERNAL$vaccine_efficacy_disease_first*(1-dose_ratio) +
        INTERNAL$vaccine_efficacy_disease_second*dose_ratio,
      vaccine_efficacy_infection = INTERNAL$vaccine_efficacy_infection_first*(1-dose_ratio) +
        INTERNAL$vaccine_efficacy_infection_second*dose_ratio,
      #nan to 0
      vaccine_efficacy_disease = if_else(
        is.na(vaccine_efficacy_disease) | is.nan(vaccine_efficacy_disease),
        {{ efficacy_disease_first}},
        vaccine_efficacy_disease
      ),
      vaccine_efficacy_infection = if_else(
        is.na(vaccine_efficacy_infection) | is.nan(vaccine_efficacy_infection),
        {{ efficacy_infection_first}},
        vaccine_efficacy_infection
      )
    )
  if(!diagnostic){
    data %>%
      dplyr::select(!contains("INTERNAL"))
  }
  return(data)
}



##

#date_0 <- as.Date(date)

import_vaccine_agreements <- function(){
  
  vacc_loc <- readRDS("vaccine_agreements.rds")
  dplyr::rename(vacc_loc, iso3c = iso_code)
}

import_vaccine_doses_by_manufacturer <- function(){
  vacc_by_type <- readRDS("vaccine_doses_by_manufacturer.rds")
  dplyr::filter(
    dplyr::rename(vacc_by_type, iso3c = countryterritoryCode),
    !is.na(iso3c)
  )
}

import_who_vaccination <- function(){
  who_vacc <- readRDS("who_vacc.rds")
  dplyr::mutate(
    dplyr::rename(who_vacc, iso3c = countryterritoryCode),
    VACCINES_USED = strsplit(VACCINES_USED, ",")
  )
}

import_who_vaccination_meta <- function(){
  who_vacc_meta <- readRDS("who_vacc_meta.rds")
  dplyr::rename(who_vacc_meta,
                iso3c = countryterritoryCode)
}


### Get the vaccine data from online
#sort out dates from before date_0 and group data by country

vacc_types <- import_vaccine_agreements()

vdm <- import_vaccine_doses_by_manufacturer()

who_vacc <- import_who_vaccination()

who_vacc_meta <- import_who_vaccination_meta()

owid <- import_owid() %>%
  filter(date <= date_0)

## Modifications for single dose Vaccines
#we will treat as only a single dose of the other vaccines, more suitable
#with its lower efficacy
single_dose_df <- get_single_dose_data(
  owid, vacc_types, vdm, who_vacc, who_vacc_meta, single_dose_vaccines =
    c("CanSino", "Johnson&Johnson", "Janssen - Ad26.COV 2-S",
      "Janssen - Ad26.COV 2.5", "CanSino - Convidecia",
      "Gamaleya - Sputnik-Light")
)

#make adjustments needed to OWID
owid <- single_dose_adjust_owid(owid, single_dose_df)

#adjustments to who data
who_vacc <- single_dose_adjust_who_vacc(who_vacc, single_dose_df)

### Set up vaccine efficacies - countries 2 dose (no boosters)

dur_vaccine_delay <- 14

rel_infectiousness_vaccinated <- 0.5

dur_V <- 200

ve_i_low <- 0.3
ve_i_high <- 0.6
ve_d_low <- 0.4
ve_d_high <- 0.8

days_between_doses=90

reduction_per_day_disease <- .97
reduction_per_day_infection <- .96

no_waning_period <- 30

default_uptake = 0.8

strategy <- "HCW, Elderly and High-Risk"


### Derive effective vaccine efficacies over time

iso3cs <- unique(squire::population$iso3c)

#get our doses over time
dose_df <- get_dose_ts(owid, who_vacc, who_vacc_meta, iso3cs, date_0)

#just set vaccine efficacies to constant values
dose_df <- dose_df %>% mutate(
  ve_i_high = ve_i_high,
  ve_d_high = ve_d_high,
  ve_d_low = ve_d_low,
  ve_i_low = ve_i_low
) %>%
  rename(date_vaccine_change = date)



#set prioritization + coverage matrix
#standard strategy, might make particular later


#set vaccine uptake to be 80% or higher if it is in the data
vaccine_uptake <- get_vaccine_uptake(
  iso3cs = iso3cs,
  dose_df = dose_df,
  default_uptake = default_uptake,
  strategy = strategy
)

#get the matrices
vaccine_coverage_mats <- get_coverage_mats(
  iso3c = iso3cs,
  strategy = strategy,
  vaccine_uptake = vaccine_uptake
)

fitting_df <- dose_df %>%
    mutate(
      first_dose_per_day = if_else(
        imputed,
        as.numeric(NA),
        first_dose_per_day
      ),
      second_dose_per_day = if_else(
        imputed,
        as.numeric(NA),
        second_dose_per_day
      )
    ) %>%
    dplyr::select(iso3c, date_vaccine_change, first_dose_per_day, second_dose_per_day)
  
  #make an estimate of the average days between doses for each country
  #here we assume that doses go to the person vaccinated the earliest
  delays_df <- data.frame(iso3c = iso3cs) %>%
    rowwise() %>%
    mutate(
      days_between_doses = {
        country_df <- dose_df %>%
          rename(country = iso3c) %>%
          filter(country == iso3c)
        if(nrow(country_df) == 1 | sum(country_df$second_dose_per_day) == 0){
          NA
        } else {
          #set up parameters
          delays_df <- data.frame(
            days_between_doses = 1:nrow(country_df),
            frequency = 0
          )
          first_doses_available <- country_df$first_dose_per_day
          #now work though for each day
          for(t in 1:nrow(country_df)){
            second_doses_to_assign <- country_df$second_dose_per_day[t]
            if(second_doses_to_assign>0){
              assigning <- TRUE
              i <- min(which(first_doses_available>0))
              while(assigning){
                assigned <- max(
                  (second_doses_to_assign - first_doses_available[i]),
                  second_doses_to_assign
                )
                #update the delay df
                delays_df[delays_df$days_between_doses == (t - i), "frequency"] <-
                  assigned
                #update parameters
                first_doses_available[i] <- first_doses_available[i] - assigned
                second_doses_to_assign <- second_doses_to_assign - assigned
                i <- i+1
                #check if we have used up all doses to assign
                if(second_doses_to_assign == 0){
                  assigning <- FALSE
                }
              }
            }
          }
          #calculate the median delay
          middle_delay <- (sum(delays_df$frequency)+1)/2
          delays_df$days_between_doses[min(which((cumsum(delays_df$frequency)>middle_delay)))]
        }
      }
    )
  #add to data
  dose_df <- left_join(
    dose_df,
    delays_df,
    by = "iso3c"
  )
  #calculate efficacy on each day for each country with waning and dose adjustment
  dose_df2 <- dose_df %>%
    arrange(iso3c, date_vaccine_change) %>%
    mutate(max_vaccine = first_dose_per_day) %>%
    calculate_waning_eff(dates = date_vaccine_change,
                         first_doses = max_vaccine,
                         efficacy_infection_first = ve_i_low,
                         efficacy_infection_second = ve_i_high,
                         efficacy_disease_first = ve_d_low,
                         efficacy_disease_second = ve_d_high,
                         dur_vaccine_delay = dur_vaccine_delay,
                         days_between_doses = days_between_doses,
                         countries = iso3c,
                         diagnostic = TRUE)
  
  saveRDS(dose_df2,"dose_df2.rds")


  ##
  
  ### Set up vaccine efficacies - countries 3 dose (yes boosters)
  
  dur_vaccine_delay <- 14
  
  rel_infectiousness_vaccinated <- 0.5
  
  dur_V <- 200
  
  ve_i_low <- 0.5
  ve_i_high <- 0.8
  ve_d_low <- 0.8
  ve_d_high <- 0.95
  
  days_between_doses=90
  
  reduction_per_day_disease <- .97
  reduction_per_day_infection <- .96
  
  no_waning_period <- 30
  
  default_uptake = 0.8
  
  strategy <- "HCW, Elderly and High-Risk"
  
  
  ### Derive effective vaccine efficacies over time
  
  iso3cs <- unique(squire::population$iso3c)
  
  #get our doses over time
  dose_df3 <- get_dose_ts(owid, who_vacc, who_vacc_meta, iso3cs, date_0)
  
  ## Apply Delta Adjustment if needed
  #just set vaccine efficacies to constant values
  dose_df3 <- dose_df3 %>% mutate(
    ve_i_high = ve_i_high,
    ve_d_high = ve_d_high,
    ve_d_low = ve_d_low,
    ve_i_low = ve_i_low
  ) %>%
    rename(date_vaccine_change = date)
  
  
  
  #set prioritization + coverage matrix
  #standard strategy, might make particular later
  
  
  #set vaccine uptake to be 80% or higher if it is in the data
  vaccine_uptake <- get_vaccine_uptake(
    iso3cs = iso3cs,
    dose_df = dose_df,
    default_uptake = default_uptake,
    strategy = strategy
  )
  
  #get the matrices
  vaccine_coverage_mats <- get_coverage_mats(
    iso3c = iso3cs,
    strategy = strategy,
    vaccine_uptake = vaccine_uptake
  )
  
  fitting_df <- dose_df3 %>%
    mutate(
      first_dose_per_day = if_else(
        imputed,
        as.numeric(NA),
        first_dose_per_day
      ),
      second_dose_per_day = if_else(
        imputed,
        as.numeric(NA),
        second_dose_per_day
      )
    ) %>%
    dplyr::select(iso3c, date_vaccine_change, first_dose_per_day, second_dose_per_day)
  
  #make an estimate of the average days between doses for each country
  #here we assume that doses go to the person vaccinated the earliest
  delays_df <- data.frame(iso3c = iso3cs) %>%
    rowwise() %>%
    mutate(
      days_between_doses = {
        country_df <- dose_df %>%
          rename(country = iso3c) %>%
          filter(country == iso3c)
        if(nrow(country_df) == 1 | sum(country_df$second_dose_per_day) == 0){
          NA
        } else {
          #set up parameters
          delays_df <- data.frame(
            days_between_doses = 1:nrow(country_df),
            frequency = 0
          )
          first_doses_available <- country_df$first_dose_per_day
          #now work though for each day
          for(t in 1:nrow(country_df)){
            second_doses_to_assign <- country_df$second_dose_per_day[t]
            if(second_doses_to_assign>0){
              assigning <- TRUE
              i <- min(which(first_doses_available>0))
              while(assigning){
                assigned <- max(
                  (second_doses_to_assign - first_doses_available[i]),
                  second_doses_to_assign
                )
                #update the delay df
                delays_df[delays_df$days_between_doses == (t - i), "frequency"] <-
                  assigned
                #update parameters
                first_doses_available[i] <- first_doses_available[i] - assigned
                second_doses_to_assign <- second_doses_to_assign - assigned
                i <- i+1
                #check if we have used up all doses to assign
                if(second_doses_to_assign == 0){
                  assigning <- FALSE
                }
              }
            }
          }
          #calculate the median delay
          middle_delay <- (sum(delays_df$frequency)+1)/2
          delays_df$days_between_doses[min(which((cumsum(delays_df$frequency)>middle_delay)))]
        }
      }
    )
  #add to data
  dose_df3 <- left_join(
    dose_df3,
    delays_df,
    by = "iso3c"
  )
  #calculate efficacy on each day for each country with waning and dose adjustment
  dose_df3 <- dose_df3 %>%
    arrange(iso3c, date_vaccine_change) %>%
    mutate(max_vaccine = first_dose_per_day) %>%
    calculate_waning_eff(dates = date_vaccine_change,
                         first_doses = max_vaccine,
                         efficacy_infection_first = ve_i_low,
                         efficacy_infection_second = ve_i_high,
                         efficacy_disease_first = ve_d_low,
                         efficacy_disease_second = ve_d_high,
                         dur_vaccine_delay = dur_vaccine_delay,
                         days_between_doses = days_between_doses,
                         countries = iso3c,
                         diagnostic = TRUE)
  
  saveRDS(dose_df3,"dose_df3.rds")
  
  
  ##
  
  