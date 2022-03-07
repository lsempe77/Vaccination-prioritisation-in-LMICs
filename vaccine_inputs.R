who_vacc <- import_who_vaccination()

owid <- import_owid() %>%
  filter(date <= date_0)
