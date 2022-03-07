
mean_vac_15 <- owid %>% group_by(iso3c) %>% filter (date>date_0-15) %>%
  summarise(new_vaccinations=mean(new_vaccinations,na.rm=T),
            new_vaccinations_smoothed=mean(new_vaccinations_smoothed,na.rm=T),
            mean_vac=case_when(is.nan(new_vaccinations)~new_vaccinations_smoothed,
                               T ~ new_vaccinations_smoothed)) 


is_15 <-  mean_vac_15 %>% filter(is.nan(mean_vac)| is.na(mean_vac)) %>% pull(iso3c)

mean_vac_15 <- mean_vac_15 %>% filter (!is.nan(mean_vac))


mean_vac2_15 <- owid %>% filter (iso3c %in% is_15 ) %>% 
  filter (date>date_0-90) %>% group_by(iso3c)%>%
  summarise(new_vaccinations=mean(new_vaccinations,na.rm=T),
            new_vaccinations_smoothed=mean(new_vaccinations_smoothed,na.rm=T),
            mean_vac=case_when(is.nan(new_vaccinations)~new_vaccinations_smoothed,
                               T ~ new_vaccinations_smoothed))

mean_vac_15<-mean_vac_15%>%full_join(mean_vac2_15)


mean_vac_15<-mean_vac_15 %>% add_row(iso3c = "TKM", mean_vac = 7580976/200)

countries <- sort(unique(squire::population$country))

countries <-countries[-35]

cov15<-list()


for (i in countries) {
  
  country<-i
  
  iso3cs <- squire::population$iso3c[squire::population$country==country][1]
  
  population_total <- nimue:::init(squire::get_population(country)$n, seeding_cases = 1)
  
  population_total<-sum(population_total$S_0[,1])
  
  max_vaccine<- vaccines %>% filter (iso3c==iso3cs) %>%
    summarise(total_final=sum(total_final))%>% pull(total_final)
  
  
  current_coverage <-  (max_vaccine/2) / population_total
  
  current_coverage<-as.numeric(current_coverage)
  
  to_give<-(sum(population_total)*.7)-(current_coverage*sum(population_total))
  
  to_give2<-floor((2*to_give)/days) 
  
  mean_vacine<- mean_vac_15 %>%
    filter (iso3c  == iso3cs) %>%
    pull(mean_vac)
  
  
  cov15[[i]] <- list(iso3cs,current_coverage,max_vaccine,mean_vacine,
                    population_total,to_give2)
  
  
}


cov15<-stringi::stri_list2matrix(cov15, byrow=TRUE)

cov15<-as.data.frame(cov15)

colnames(cov15)[1]<-"iso_a3"
colnames(cov15)[2]<-"coverage"
colnames(cov15)[3]<-"max_vaccine"
colnames(cov15)[4]<-"mean_vac"
colnames(cov15)[5]<-"population"
colnames(cov15)[6]<-"to_give"

nrow(cov15)


cov15<- cov15 %>% mutate(coverage=as.numeric(coverage),
                       max_vaccine=as.numeric(max_vaccine),
                       mean_vac=as.numeric(mean_vac),
                       population=as.numeric(population),
                       to_give=as.numeric(to_give))%>%
  mutate (status = case_when(to_give > mean_vac  ~ "behind",
                             to_give < mean_vac  ~ "on_track",
                             to_give < 0 ~ "on_track"))

cov15 %>% filter (status=="behind") %>% 
  mutate(iso_a3=forcats::fct_reorder(iso_a3,coverage))%>%
  ggplot() + geom_point(aes(iso_a3,coverage)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 4)) 

cov15 %>% filter (status=="behind") %>% tally()


world %>%left_join(cov15) %>% ggplot()+
  geom_sf(aes(fill=status)) + scale_fill_discrete("",labels=c("Behind goal","On track",
                                                              "No data"))
