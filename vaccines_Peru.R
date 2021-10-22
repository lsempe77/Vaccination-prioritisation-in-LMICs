memory.limit(size=20000)

# Peru

#Upload data from official vaccination  website

vaccination_Peru <-  read.csv("vacPeru20oct.csv")

#glimpse(vaccination_Peru)

vaccination_Peru <- vaccination_Peru %>% select(fecha_vacunacion,dosis,edad)

breaks=seq(0,85,5)

vaccination_Peru2 <- vaccination_Peru %>% filter (edad> 17 & edad<111) %>% 
  mutate(edad = case_when(edad > 84 ~ as.integer(84),
                          T ~ edad)) %>%
  mutate(age = cut(edad,breaks=breaks,right = F)) %>% 
  mutate(fecha_vacunacion = lubridate::dmy(fecha_vacunacion)) %>% 
  group_by(age,dosis,fecha_vacunacion) %>% tally() %>% 
  group_by(age,dosis) %>%
  complete(fecha_vacunacion = seq.Date(min(fecha_vacunacion),
                                       max(fecha_vacunacion), by="day")) %>%
  fill(n,0)

rm(vaccination_Peru)

gc()

vaccination_Peru2 %>% 
  ggplot () + 
  geom_line(aes(fecha_vacunacion,n,
                group=as.factor(dosis),colour=as.factor(dosis))) + 
  facet_wrap(~age,scales = "free")

#

vaccination_Peru2 %>% 
  group_by(fecha_vacunacion) %>%
  summarise(n=sum(n)) %>% 
  arrange (-n) %>% slice (1:10)


vaccination_Peru2 %>% group_by(fecha_vacunacion) %>%
  summarise(n=sum(n))%>%
  ggplot () + 
  geom_line(aes(fecha_vacunacion,n)) + 
  scale_y_continuous(labels = scales::comma)+ theme_light()


#
vac.Peru.Age<-vaccination_Peru2 %>% filter (dosis != 3) %>%
group_by(age,dosis) %>% 
  summarise(n=sum(n)) %>% 
      mutate(full_vac2 = dplyr::lag(n) - n,
         full_vac_t = round(full_vac2/2) + n ) %>% filter(dosis==2)


#

pop <- read_excel("BD_Pob_Identificada_2020_Excel.xlsx")

pop <- pop %>% filter (Residencia=="Nacional")


breaks=seq(0,85,5)

pop <- pop %>% select (Sexo,Edad,Cantidad) %>% group_by(Edad) %>%
  summarise (Cantidad = sum(Cantidad,na.rm=T)) %>%
  filter (Edad<111) %>% 
  mutate(Edad = case_when(Edad > 84 ~ 84,
                          T ~ Edad)) %>%
  mutate(age = cut(Edad,breaks=breaks,right = F)) %>% group_by(age) %>%
  summarise (Inei = sum(Cantidad,na.rm = T)) 



#

TB_POBLACION_INEI <- read_csv("TB_POBLACION_INEI.csv")

glimpse(TB_POBLACION_INEI)

inei <-TB_POBLACION_INEI %>% group_by(Edad_Anio,Sexo) %>%
  summarise (Cantidad=sum(Cantidad,na.rm = T))

inei<-inei %>% mutate (AGEcut_INEI = case_when(Edad_Anio == "0" |
                                                 Edad_Anio == "1" |
                                                 Edad_Anio == "2" |
                                                 Edad_Anio == "3" |
                                                 Edad_Anio == "4" ~ "[0,5)" ,
                                               Edad_Anio == "5" |
                                                 Edad_Anio == "6" |
                                                 Edad_Anio == "7" |
                                                 Edad_Anio == "8" |
                                                 Edad_Anio == "9" ~ "[5,10)",
                                               Edad_Anio == "10" |
                                                 Edad_Anio == "11" |
                                                 Edad_Anio == "12" |
                                                 Edad_Anio == "13" |
                                                 Edad_Anio == "14" ~ "[10,15)",
                                               Edad_Anio == "15" |
                                                 Edad_Anio == "16" |
                                                 Edad_Anio == "17" |
                                                 Edad_Anio == "18" |
                                                 Edad_Anio == "19" ~ "[15,20)",
                                               Edad_Anio == "20-24"  ~ "[20,25)",
                                               Edad_Anio == "25-29"  ~ "[25,30)",
                                               Edad_Anio == "30-34"  ~ "[30,35)",
                                               Edad_Anio == "35-39"  ~ "[35,40)",
                                               Edad_Anio == "40-44"  ~ "[40,45)",
                                               Edad_Anio == "45-49"  ~ "[45,50)",
                                               Edad_Anio == "50-54"  ~ "[50,55)",
                                               Edad_Anio == "55-59"  ~ "[55,60)",
                                               Edad_Anio == "60-64"  ~ "[60,65)",
                                               Edad_Anio == "65-69"  ~ "[65,70)",
                                               Edad_Anio == "70-74"  ~ "[70,75)",
                                               Edad_Anio == "75-79"  ~ "[75,80)",
                                               Edad_Anio == "80  +"  ~ "[80,85)")
                       
)


inei<-inei %>% mutate (AGEcut_INEI = as.factor(AGEcut_INEI))

inei$AGEcut_INEI<-relevel(inei$AGEcut_INEI,"[5,10)")

inei$AGEcut_INEI<-relevel(inei$AGEcut_INEI,"[0,5)")


inei<-inei %>% mutate (Sexo = case_when(Sexo== "M" ~ "Hombre",
                                        T ~ "Mujer"))

inei <- inei %>% select(-Edad_Anio) %>% group_by(AGEcut_INEI) %>% 
  summarise (CantidadINEI = sum (Cantidad))


#
init <- nimue:::init(squire::get_population("Peru")$n, seeding_cases = 200000)

pop_nimue<-init$S_0[,1]

#

compare_pop <-cbind(pop_nimue,pop,inei)


compare_pop %>% select(-AGEcut_INEI)%>%
  pivot_longer(-age) %>%
  ggplot() + geom_bar(aes(age,value,fill=factor(name)),
                      position="dodge", stat="identity")


