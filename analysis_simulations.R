glimpse(out_format)

India_Peter<-out_format %>% 
  filter (R0 != 1.1 & R0 != 1.3 & R0 != 1.5 &
                         R0 != 1.7 & R0 != 1.9 & R0 != 2) %>%
            dplyr::filter ( max_vaccine == 3000000) %>%
  mutate(final_coverage=round(final_coverage,2)) %>% 
  filter (final_coverage == .6 | final_coverage == .8) %>%
  group_by(R0, final_coverage,vaccine_coverage_mat) %>% 
  summarise(deaths_averted = mean (deaths_averted),
            years_life_saved = mean (years_life_saved)) 

write.csv(India_Peter,"india_peter.csv")

out_format_peru %>% 
  filter (R0 != 1.1 & R0 != 1.3 & R0 != 1.5 &
            R0 != 1.7 & R0 != 1.9 & R0 != 2) %>%
  dplyr::filter ( max_vaccine == 2e+05) %>%
  mutate(final_coverage=round(final_coverage,2)) %>% group_by(final_coverage) %>%
  tally()


Peru_Peter<-out_format_peru %>% 
  filter (R0 != 1.1 & R0 != 1.3 & R0 != 1.5 &
            R0 != 1.7 & R0 != 1.9 & R0 != 2) %>%
  dplyr::filter ( max_vaccine == 2e+05) %>%
  mutate(final_coverage=round(final_coverage,2)) %>% 
  filter (final_coverage == .61 | final_coverage == .81) %>%
  group_by(R0, final_coverage,vaccine_coverage_mat) %>% 
  summarise(deaths_averted = mean (deaths_averted),
            years_life_saved = mean (years_life_saved)) 

write.csv(Peru_Peter,"peru_peter.csv")
