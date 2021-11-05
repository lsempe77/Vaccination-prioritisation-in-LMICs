library(bookdown)
library(knitr)

countries <- sort(unique(squire::population$country))

# loop through the list of countries

for (i in countries) {
  
  country<-i
  
  # grab the json from the data exports
  iso3cs <- squire::population$iso3c[squire::population$country==country][1]
  file_path <- "https://raw.githubusercontent.com/mrc-ide/global-lmic-reports/master/"
  json_path <- file.path(file_path,iso3cs,"input_params.json")
  
    skip_to_next <- FALSE
    
    # Note that print(b) fails since b doesn't exist
    
    tryCatch(json <- jsonlite::read_json(json_path), error = function(e) { skip_to_next <<- TRUE})
    
    if(skip_to_next) { next }     
  
  
  dates <- unlist(lapply(json, "[[", "date"))
  
  date_deaths <- unlist(lapply(json, function(x){
    if("deaths" %in% names(x)) {
      x["date"]
    } else {
      NULL
    }
  }))
  ## get country specific params
  population = squire::get_population(country)
  
  # get vaccine data if in json (if statement only here as previously this did not exist)
  if("max_vaccine" %in% names(json[[1]])) {
    max_vaccine <- unlist(lapply(json, "[[", "max_vaccine"))
    max_vaccine <- max_vaccine[which(dates <= max(date_deaths))]
  } 
  
  # future vaccines
  current_coverage <- sum(max_vaccine) / sum(squire::population$n[squire::population$iso3c==iso3cs])
  
  if (current_coverage > .3) {
    next
  }
  
  if (sum(max_vaccine)==0) {
    
   rmarkdown::render(input = "multi0.Rmd", 
                      output_format = "word_document2",
                      output_file = paste0(country, ".docx"),
                      output_dir = "reports")
  } else {
  rmarkdown::render(input = "multi.Rmd", 
                    output_format = "word_document2",
                    output_file = paste0(country, ".docx"),
                    output_dir = "reports")
  }
  
  gc()
  
}
