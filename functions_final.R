covid.fully.vaccinated.by.age <- read.csv("covid-fully-vaccinated-by-age.csv", na.strings="")

#variable.names(covid.fully.vaccinated.by.age)

covid.fully.vaccinated.by.age <- covid.fully.vaccinated.by.age %>%
  pivot_longer(4:29,names_to = "age_group",values_to = "share")

age_g_ows<-levels(factor(covid.fully.vaccinated.by.age$age_group))

youngest<-c("X0.17_fully",  "X0.9_fully",  "X10.19_fully", "X12.17_fully")
  
mid<-c("X18.24_fully", "X18.29_fully", "X20.29_fully", "X25.34_fully", "X25.49_fully" ,"X30.39_fully",
   "X35.44_fully", "X40.49_fully", "X45.54_fully", "X50.54_fully", "X50.59_fully",
   "X55.59_fully", "X55.64_fully" )

old<-c("X60.64_fully" ,"X60.69_fully","X65.69_fully",
       "X65.74_fully", "X70.74_fully", "X70.79_fully")
  
oldest<-c("X75._fully","X75.79_fully","X80._fully")


covid.fully.vaccinated.by.age<-covid.fully.vaccinated.by.age %>% 
  mutate(group = case_when(age_group %in% youngest ~ "youngest",
                           age_group %in% mid ~ "mid",
                           age_group %in% old ~ "old",
                           age_group %in% oldest ~ "oldest"))


covid.fully.vaccinated.by.age <- covid.fully.vaccinated.by.age %>% 
  group_by(Entity,group) %>% 
  filter (!is.na(share)) %>%
  mutate(Day=lubridate::ymd(Day)) %>%  
  slice(which.max(Day)) %>% group_by(Entity) %>%
  mutate(sum_share=sum(share),
         prop_share=share/sum_share)

# covid.fully.vaccinated.by.age %>% ggplot() + 
#   geom_point(aes(Entity,prop_share,colour=Entity)) +
#   facet_wrap(~group)


# covid.fully.vaccinated.by.age %>% group_by(group) %>%
#   summarise(mean_prop=mean(prop_share))



#

country<-i

lifespan <- read_excel("lifespan.xlsx")

countries1 <- sort(unique(squire::population$country)) %>% as.data.frame() 

colnames(countries1)[1]<-"Country"


lifespan1 <- fuzzyjoin::stringdist_full_join(lifespan,countries1,max_dist=.5) %>%
  rename(Country=Country.y) %>% select(-Country.x)

country_lifespan<- lifespan1 %>% filter (Country==country) %>% 
    mutate(lifespan = round(lifespan,2)) %>%
    pull(lifespan)

world<-lifespan %>% filter(Country=="World") %>% pull(lifespan)

if (length(country_lifespan)==0) {
  country_lifespan <- world
}


format_out_LS <- function(out, scenarios, country1,vac_m,min_coverage,seeding_cases){
  
  country=country1
  
  out1 <- bind_cols(scenarios, bind_rows(out))
  
  seeding_cases<-seeding_cases
  
  min_coverage<-min_coverage
  
  if ("coverage" %in% colnames(out1)) {
    outcf <- filter(out1, coverage == min_coverage) %>% #### check coverage
      select(-coverage) %>%
      rename(output_cf = output) %>%
      unique()
    
    out1 <- filter(out1,coverage > min_coverage)
    
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

# Estimate total years of life lost
# summarise_yll <- function(x, country_lifespan=country_lifespan, time_period){
#   lifespan_c<-country_lifespan
#   filter(x, compartment == "deaths", period == time_period) %>%
#     mutate(mid_age = (((as.integer(age_group) - 1) * 5) + 2.5),
#            yll = pmax(0, (lifespan_c - mid_age) * value)) %>%
#     pull(yll) %>%
#     sum()
# }

summarise_outputs_age <- function(x, p) {
  init <- nimue:::init(squire::get_population(country)$n, seeding_cases = seeding_cases)
  S_0<-init$S_0
  vac<-c(rep(vac_m*0.2267,3),rep(vac_m*0.53833,9),rep(vac_m*1.955,3),
         rep(vac_m*2.805,2))
  S_0[,4]<- round(S_0[,1]*vac*.9)
  S_0[,5] <-S_0[,1]
  S_0[,1] <- S_0[,1] - S_0[,4]
  # negative to 0
  popsuc <- S_0[,1] %>% as.data.frame()
  colnames(popsuc)[1]<-"age_g"
  popsuc <- popsuc %>% mutate(age_g = case_when(age_g<0 ~0,
                                                T ~ age_g))
  S_0[,1] <- popsuc[,1]
  
  # and update the init
  init$S_0 <- S_0
 
  mutate(x, 
         infections = round(map_dbl(output, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations = round(map_dbl(output, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths = round(map_dbl(output, pull_total, outcome = "deaths", time_period = p), 2),
         #yll = round(map_dbl(output, summarise_yll, time_period = p), 2),
         infections_cf = round(map_dbl(output_cf, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations_cf = round(map_dbl(output_cf, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths_cf = round(map_dbl(output_cf, pull_total, outcome = "deaths", time_period = p), 2),
         #yll_cf = round(map_dbl(output_cf, summarise_yll, time_period = p), 2),
         infections_averted = infections_cf - infections,
         hospitalisations_averted = hospitalisations_cf - hospitalisations,
         deaths_averted = deaths_cf - deaths,
         deaths_averted_prop = deaths_averted / deaths_cf,
         #years_life_saved = yll_cf - yll,
         vaccine_n = round(map_dbl(output, pull_total, outcome = "vaccines", time_period = p)),
         final_coverage = (sum(init$S_0[,4]) + vaccine_n) / (sum(init$S_0[,5]))
  )
}


summarise_by_age <- function(x, t_start, t_end, period){
  filter(x, t >= t_start, t < t_end) %>%
    group_by(age_group, compartment) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(period = factor(period))
} 

run_scenario_LS <- function(R0=R0,
                            vac=vac_m,
                            seeding_cases = seeding_cases,
                                     coverage=coverage,
                                     country = country,
                                     max_vaccine = max_vaccine,
                                     vaccine_coverage_mat = vaccine_coverage_mat)
  
{
  
  init <- nimue:::init(squire::get_population(country)$n, seeding_cases = seeding_cases)
  
  S_0<-init$S_0
  vac<-c(rep(vac_m*0.2267,3),rep(vac_m*0.53833,9),rep(vac_m*1.955,3),
         rep(vac_m*2.805,2))
  S_0[,4]<- round(S_0[,1]*vac)
  S_0[,5] <-S_0[,1]
  S_0[,1] <- S_0[,1] - S_0[,4]
  popsuc <- S_0[,1] %>% as.data.frame()
  colnames(popsuc)[1]<-"age_g"
  popsuc <- popsuc %>% mutate(age_g = case_when(age_g<0 ~0,
                                                T ~ age_g))
  S_0[,1] <- popsuc[,1]
  
  # and update the init
  init$S_0 <- S_0


  r1 <- nimue::run(time_period = 365,
                   country = country,
                   R0 = R0, 
                   seeding_cases = seeding_cases,
                   max_vaccine = max_vaccine,
                   dur_V = 365,
                   dur_R = 365,
                   vaccine_coverage_mat = strategy_matrix(vaccine_coverage_mat,
                                                          max_coverage = coverage),
                   init=init,
                   vaccine_efficacy_infection = rep(0.6, 17),
                   vaccine_efficacy_disease = rep(0.9, 17)
  )
  
  
  x <- nimue::format(r1,
                     compartments = NULL,
                     summaries = c("deaths", "infections", "vaccines", "hospitalisations"),
                     reduce_age = FALSE)
  
  value_all_t <- summarise_by_age(x, 1, max(x$t), "all_t")
  
  tibble(output = list(value_all_t))
}

