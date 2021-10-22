# compute % population vaccinated
init2_peru <- nimue:::init(squire::get_population("Peru")$n, seeding_cases = 200000)

S_0<-init2_peru$S_0
vac<-c(rep(0,3),vac.Peru.Age$full_vac_t)
S_0[,4]<-round(vac  *.9)
S_0[,5] <-S_0[,1]
S_0[,1] <- S_0[,1] - S_0[,4]
S_0[3:17,1] <- S_0[3:17,1]

# and update the init
init2_peru$S_0 <- S_0

init2_peru$S_0


vac.Peru.total<-vaccination_Peru2 %>% filter (dosis != 3) %>% ungroup %>%
  summarise(n=sum(n))

(vac.Peru.total/2)/sum(init2_peru$S_0[,5]) # 0.4903891=> 49.0% population vaccinated



# Adjust model vaccine coverage to population coverage

out_format_peru <- out_format_peru %>%
  mutate(final_coverage = (sum(init2_peru$S_0[,4]) + vaccine_n) / (sum(init2_peru$S_0[,5])))

out_format_peru %>%  
  mutate(final_coverage=round(final_coverage,2)) %>% filter (final_coverage>.54) %>%
  group_by(final_coverage,R0,max_vaccine,vaccine_coverage_mat) %>%
  summarise(infections_averted=mean(infections_averted),
            hospitalisations_averted= mean(hospitalisations_averted),
            deaths_averted=mean(deaths_averted),deaths_averted_prop=mean(deaths_averted_prop),
            years_life_saved=mean(years_life_saved))




#ggplot(out_format_peru)+ geom_point(aes(coverage,final_coverage)) + theme_light()



# out_format %>% filter (R0 == 1.5) %>%
#   ggplot() + geom_point(aes(final_coverage,deaths_averted,colour=vaccine_coverage_mat,
#                             shape=as.factor(max_vaccine)))+
#   facet_wrap(~seed) + theme_light() +
#   scale_y_continuous(labels = scales::comma) +
#    scale_x_continuous(labels = scales::percent,limits = c(.36,1)) +
#   theme(legend.position = "bottom") + 
#   labs(title = "Simulations of Vaccination models - Peru",
#        caption = "Data source: Author's own")+
#   xlab("Population maximum coverage") +  ylab("Deaths averted") + 
#    scale_colour_manual("Vaccine prioritisation",values = c("darkgreen", "darkorange"), 
#                        labels = c("No prioritisation", "Older People"))+
#   scale_shape_discrete("Mean maximum vaccines per day",
#                       labels = c("3 million", "4 million", "5 million"))


## Extract data per age groups (not used in the analysis right now)
# 
# out_format_unnest_cf<-out_format %>% select(c(1:6,8)) %>% 
#   unnest(output_cf) %>% rename(value_cf = value)
# 
# out_format_unnest<-out_format %>% select(1:7) %>% 
#   unnest(output) 
# 
# out_format_unnest_final <- out_format_unnest %>% bind_cols(out_format_unnest_cf[9]) %>% 
#     mutate(dif=value_cf-value)
# 
# final_coverage <- out_format %>% 
#   group_by(coverage)  %>% 
#   summarise (coverage=mean(coverage),
#              final_coverage=round(mean(final_coverage),2))
# 
# out_format_unnest_final <- out_format_unnest_final %>% left_join(final_coverage)
#   
#   
# out_format_unnest_final %>% filter(compartment == "deaths") %>%
#   ggplot() + geom_point(aes(coverage,dif,colour=age_group,
#                             shape=as.factor(max_vaccine))) +
#   facet_grid(vaccine_coverage_mat~R0) + theme_light()+
#   scale_y_continuous(labels = scales::comma)
# 
# 
# t21<-out_format_unnest_final %>% filter(compartment == "deaths") %>% 
#   # mutate(age = case_when(age_group == "60-65" |  age_group =="65-70" | 
#   #                          age_group =="70-75" |age_group == "75-80" | 
#   #                          age_group == "80+" ~ "older",
#   #                        T ~ "not")) %>%
#   group_by(coverage,R0,vaccine_coverage_mat,max_vaccine) %>%
#   summarise(dif=sum(dif))
# 
# 
# # test if extraction is correct
# 
# max(t21$dif) # highest - 3955593
# max(out_format$deaths_averted)#  3955593
# 
# 
# t22<-out_format_unnest_final %>% filter(compartment == "deaths") %>% 
#   mutate(age = case_when(age_group == "60-65" |  age_group =="65-70" |
#                            age_group =="70-75" |age_group == "75-80" |
#                            age_group == "80+" ~ "older",
#                          T ~ "not")) %>%
#   group_by(final_coverage,R0,vaccine_coverage_mat,max_vaccine,age) %>%
#   summarise(dif=sum(dif))
# 
# 
# t22 %>% mutate(max_vaccine = as.factor(max_vaccine))%>%
#   ggplot() + geom_point(aes(final_coverage,dif,colour=age,
#                             shape=max_vaccine)) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_continuous(labels = scales::comma,
#                      limits = c(.5,1)) +
#   scale_shape_discrete(label = c("3 million","4 million","5 million"))+
#   ylab("Deaths averted")
