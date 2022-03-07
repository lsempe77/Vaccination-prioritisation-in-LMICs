import_who_vaccination <- function(){
  who_vacc <- readRDS("who_vacc.rds")
  dplyr::mutate(
    dplyr::rename(who_vacc, iso3c = countryterritoryCode),
    VACCINES_USED = strsplit(VACCINES_USED, ",")
  )
}

import_owid <- function(){
  owid <- readRDS("owid.rds")
  dplyr::filter(
    dplyr::select(
    dplyr::rename(owid, iso3c = countryterritoryCode),
    c(iso3c, date, tidyselect::contains(c("vacc","boost","deat","exc","part")))
  ),
  nchar(iso3c) == 3
  )
}
