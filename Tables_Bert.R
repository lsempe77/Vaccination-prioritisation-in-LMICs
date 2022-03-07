library("xlsx")

popa1<- squire::population[,c(1:3,5)] %>% as.data.frame() %>% 
  mutate(age = case_when(  age_group == "60-64"  |
                                  age_group == "65-69"  |
                             age_group == "70-74" | 
                              age_group == "75-79" |
                             age_group == "80+" ~ "older",
                             T ~ "not")) %>% group_by(iso3c,age) %>% 
  summarise(n=sum(n)) %>%
  group_by(iso3c) %>% mutate(`Population >60 (%)`=round(prop.table(n)*100,1)) %>% 
  filter (age=="older")

table1<-
count_reg %>% left_join(popa1,by=c("iso_a3"="iso3c")) %>%
  select(11,7,4,19,2)%>% mutate(coverage=round(coverage*100,1))%>%
  rename(Country=name,Population=population,`Current vaccination level (%)`=coverage)

write.xlsx(table1,"table1.xlsx")


#


table3a<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.max(dif)) %>%
  rename(val_max=dif) %>% 
  group_by(region,population,country,compartment) %>% 
  mutate(proportion_max=(round(prop.table(val_max),3))*100) %>%
  rename(Country=country,`Value max model`=val_max,`>60`=age,
         `Proportion max value by age group`=proportion_max) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table3b<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.min(dif) ) %>%
  rename(val_min=dif) %>% 
  group_by(country,compartment) %>% 
  mutate(proportion_min=(round(prop.table(val_min),3))*100) %>%
  rename(Country=country,`Value min model`=val_min,`>60`=age,
         `Proportion min value by age group`=proportion_min) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table3<- table3a %>% left_join(table3b)

mr <- read_excel("mr.xlsx") %>% select(2,5) %>% rename (iso3c=iso_a3)

table3<- table3 %>% left_join(mr)

write.xlsx(as.data.frame(table3),"table3.xlsx")




table4a<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.max(dif)) %>%
  rename(val_max=dif) %>% 
  group_by(region,population,country,compartment) %>% 
  mutate(proportion_max=(round(prop.table(val_max),3))*100) %>%
  rename(Country=country,`Value max model`=val_max,`>60`=age,
         `Proportion max value by age group`=proportion_max) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table4b<-
  unnested %>% left_join(seir_c,by=c("country"="location"))%>%
  left_join(count_reg, by=c("iso3c"="iso_a3")) %>% 
  mutate(age = case_when(  age_group == "60-65"  |
                             age_group == "65-70"  |
                             age_group == "70-75" | 
                             age_group == "75-80" |
                             age_group == "80+" ~ "older",
                           T ~ "not")) %>% filter(R0==1.5)%>%
  filter (compartment=="deaths" | compartment =="hospitalisations") %>% 
  group_by(region,population,country,compartment,age) %>% 
  slice (which.min(dif) ) %>%
  rename(val_min=dif) %>% 
  group_by(country,compartment) %>% 
  mutate(proportion_min=(round(prop.table(val_min),3))*100) %>%
  rename(Country=country,`Value min model`=val_min,`>60`=age,
         `Proportion min value by age group`=proportion_min) %>%
  select(13,14,25,18,7,13,12,18,31,32)

table4<- table4a %>% left_join(table4b)

write.xlsx(as.data.frame(table4),"table4.xlsx")


# Cost of acceleration
# 
# Additional vaccines required to accelerate
# 
# Additional cost of acceleration (@ XX USD / vaccine)
# 




vaccine_baseline<-ra %>% 
  group_by(country) %>% 
  filter (mean_vac==min(mean_vac)) %>% 
  slice_head() %>%
  summarise(vaccine_baseline=mean_vac*days_v)



table6<-list_vaccine_countries %>% 
  left_join(count_reg,by=c("country"="name")) %>%
  select(12,1,2) %>% mutate(cost=vaccine_dose_needed*10) %>% 
  left_join(vaccine_baseline) %>%
  mutate(`vaccines to accelerate` = vaccine_dose_needed - vaccine_baseline,
         `Additional cost` = `vaccines to accelerate`*10)

write.xlsx(as.data.frame(table6),"table6.xlsx")

table6 %>% ungroup() %>% summarise(ad=sum(`Additional cost`))































