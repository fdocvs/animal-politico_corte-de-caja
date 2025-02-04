## Configuración ----

library(pacman)

p_load(readr, readxl, dplyr, stringr, ggplot2, forcats,ggthemes, scales, 
       tidyverse, ggrepel, lubridate, grid)

options(scipen=999) # Prevenir notación científica

# rm(list=ls())

## Bases de datos ----


IDV <- vroom::vroom("/Users/santiago/Downloads/IDVFC_NM_nov21.csv",
                    skip = 0, locale = vroom::locale(encoding = "CP1252"))


CONAPO <- read_excel("/Users/santiago/Documentos OFF/México Evalúa/SESNSP/pob_mun_2015-2030.xls")

CONAPO$pob_estim <- as.numeric(CONAPO$pob_estim)

CONAPO17 <- CONAPO %>% 
  filter(ao == 2017) %>% 
  select(c(1, 4, 6))

pobtot_est17 = aggregate(CONAPO17[,c("pob_estim")],by = list(entidad = CONAPO17$entidad, ao = CONAPO17$ao),FUN = sum)
pob_2017 <- pobtot_est17 %>% 
  select(c(1, 3))
names(pob_2017)[1] <- "Entidad"


CONAPO20 <- CONAPO %>% 
  filter(ao == 2020) %>% 
  select(c(1, 4, 6))

pobtot_est20 = aggregate(CONAPO20[,c("pob_estim")],by = list(entidad = CONAPO20$entidad, ao = CONAPO20$ao),FUN = sum)
pob_2020 <- pobtot_est20 %>% 
  select(c(1, 3))
names(pob_2020)[1] <- "Entidad"


##IDV <- IDV %>% 
##  filter(Año %in% c("2018", "2019", "2020", "2021"))

IDV$Enero <- as.numeric(IDV$Enero)
IDV$Febrero <- as.numeric(IDV$Febrero)
IDV$Marzo <- as.numeric(IDV$Marzo)
IDV$Abril <- as.numeric(IDV$Abril)
IDV$Mayo <- as.numeric(IDV$Mayo)
IDV$Junio <- as.numeric(IDV$Junio)
IDV$Julio <- as.numeric(IDV$Julio)
IDV$Agosto <- as.numeric(IDV$Agosto)
IDV$Septiembre <- as.numeric(IDV$Septiembre)
IDV$Octubre <- as.numeric(IDV$Octubre)
IDV$Noviembre <- as.numeric(IDV$Noviembre)
IDV$Diciembre <- as.numeric(IDV$Diciembre)


IDV_nacional <- aggregate(IDV[,c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")], 
                          by = list(IDV$Año, IDV$`Bien jurídico afectado`, IDV$`Tipo de delito`, IDV$`Subtipo de delito`, IDV$Modalidad, IDV$Sexo), FUN = sum,  na.rm = TRUE)

names(IDV_nacional)[1] <- "Año"
names(IDV_nacional)[2] <- "Bien jurídico afectado"
names(IDV_nacional)[3] <- "Tipo de delito"
names(IDV_nacional)[4] <- "Subtipo de delito"
names(IDV_nacional)[5] <- "Modalidad"
names(IDV_nacional)[6] <- "Sexo"

IDV_nacional$Entidad <- "Nacional"

INEGI_nacional[nrow(INEGI_nacional) + 1,] = list("2021","126014024")


IDV_nacional_mod <- IDV_nacional %>% 
  mutate(total_del = rowSums(IDV_nacional[ , c(7:15)], na.rm=TRUE))

IDV_nacional_sub <- IDV_nacional_mod %>%
  group_by(Año, `Tipo de delito`, `Subtipo de delito`, Sexo) %>% 
  mutate(total_deli = rowSums(IDV_nacional_mod[ , c(7:15)], na.rm=TRUE))



IDV9 <- IDV %>% 
  mutate(total_del = rowSums(IDV[ , c(10:21)], na.rm=TRUE))


IDV_homdolfem1 <- IDV %>% 
  filter(`Subtipo de delito` %in% c("Homicidio doloso", "Feminicidio")) %>% 
  group_by(Entidad, Sexo) 

names(IDV_homdolfem1)[10] <- "01"
names(IDV_homdolfem1)[11] <- "02"
names(IDV_homdolfem1)[12] <- "03"
names(IDV_homdolfem1)[13] <- "04"
names(IDV_homdolfem1)[14] <- "05"
names(IDV_homdolfem1)[15] <- "06"
names(IDV_homdolfem1)[16] <- "07"
names(IDV_homdolfem1)[17] <- "08"
names(IDV_homdolfem1)[18] <- "09"
names(IDV_homdolfem1)[19] <- "10"
names(IDV_homdolfem1)[20] <- "11"
names(IDV_homdolfem1)[21] <- "12"




ex <- pivot_longer(IDV_homdolfem1, 10:21, names_to = "Mes", values_to = "Víctimas")



VL_nacional <- ex %>% 
  group_by(Año, Mes, Sexo) %>% 
  summarise(sumatotal = sum(Víctimas)) %>% 
  filter((Año > 2015) | (Año == 2015 & Mes == 12))

write.csv(VL_nacional,'VL_nacional.csv')


VL_ent_total <- ex %>%
  group_by(Año, Entidad, Mes) %>%
  summarise(sumatotal = sum(Víctimas))


VL_ent_EPN <- VL_ent_total %>% 
  filter(Año %in% c(2015, 2016, 2017, 2018)) %>% 
  filter((Año > 2015) | (Año == 2015 & Mes == 12)) %>% 
  filter(!(Año == 2018 & Mes == 12)) %>% 
  group_by(Entidad) %>% 
  summarise(total_ent = sum(sumatotal)) %>% 
  merge(pob_2017, by = "Entidad") %>% 
  mutate(tasa = total_ent/pob_estim)

VL_ent_AMLO <- VL_ent_total %>% 
  filter(Año %in% c(2018, 2019, 2020, 2021)) %>% 
  filter((Año > 2018) | (Año == 2018 & Mes==12)) %>% 
  filter(!(Año == 2021 & Mes == 12)) %>% 
  group_by(Entidad) %>% 
  summarise(total_ent = sum(sumatotal)) %>% 
  merge(pob_2020, by = "Entidad") %>% 
  mutate(tasa2 = total_ent/pob_estim)

VL_ent_var <- merge(VL_ent_AMLO, VL_ent_EPN, by = "Entidad") %>% 
  select(c(1, 4, 7)) %>% 
  mutate(var = (tasa2-tasa)/tasa*100) %>% 
  select(c(1, 4))

write.csv(VL_ent_var,'VL_ent_var.csv')


# VL_ent_sex <- ex %>% 
#   group_by(Año, Entidad, Mes, Sexo) %>% 
#   summarise(sumatotal = sum(Víctimas)) %>% 
#   filter((Año > 2018) | (Año == 2018 & Mes==12))
# 


# filter((Año > 2018) | (Año == 2018 & Mes==12))








IDV_libper <- IDV %>% 
  filter(`Bien jurídico afectado` == "Libertad personal") %>% 
  group_by(Entidad, Sexo) 

names(IDV_libper)[10] <- "01"
names(IDV_libper)[11] <- "02"
names(IDV_libper)[12] <- "03"
names(IDV_libper)[13] <- "04"
names(IDV_libper)[14] <- "05"
names(IDV_libper)[15] <- "06"
names(IDV_libper)[16] <- "07"
names(IDV_libper)[17] <- "08"
names(IDV_libper)[18] <- "09"
names(IDV_libper)[19] <- "10"
names(IDV_libper)[20] <- "11"
names(IDV_libper)[21] <- "12"




ex2 <- pivot_longer(IDV_libper, 10:21, names_to = "Mes", values_to = "Víctimas")



LP_nacional <- ex2 %>% 
  group_by(Año, Mes, Sexo) %>% 
  summarise(sumatotal = sum(Víctimas)) %>% 
  filter((Año > 2015) | (Año == 2015 & Mes == 12))

write.csv(LP_nacional,'LP_nacional.csv')


LP_ent_total <- ex2 %>%
  group_by(Año, Entidad, Mes) %>%
  summarise(sumatotal = sum(Víctimas))

LP_ent_EPN <- LP_ent_total %>% 
  filter(Año %in% c(2015, 2016, 2017, 2018)) %>% 
  filter((Año > 2015) | (Año == 2015 & Mes == 12)) %>% 
  filter(!(Año == 2018 & Mes == 12)) %>% 
  group_by(Entidad) %>% 
  summarise(total_ent = sum(sumatotal)) %>% 
  merge(pob_2017, by = "Entidad") %>% 
  mutate(tasa = total_ent/pob_estim)

LP_ent_AMLO <- LP_ent_total %>% 
  filter(Año %in% c(2018, 2019, 2020, 2021)) %>% 
  filter((Año > 2018) | (Año == 2018 & Mes==12)) %>% 
  filter(!(Año == 2021 & Mes == 12)) %>% 
  group_by(Entidad) %>% 
  summarise(total_ent = sum(sumatotal)) %>% 
  merge(pob_2020, by = "Entidad") %>% 
  mutate(tasa2 = total_ent/pob_estim)

LP_ent_var <- merge(LP_ent_AMLO, LP_ent_EPN, by = "Entidad") %>% 
  select(c(1, 4, 7)) %>% 
  mutate(var = (tasa2-tasa)/tasa*100) %>% 
  select(c(1, 4))

write.csv(LP_ent_var,'LP_ent_var.csv')


library(mxmaps)

libperm <- LP_ent_var 
names(libperm)[1] <- "region"
names(libperm)[2] <- "value"

libperm$region <- c(1:32)


mxstate_choropleth(libperm,
                   title = "Variación porcentual por delitos contra libertad personal",
                   num_colors = 1,
                   legend = "%") 


libperm <- LP_ent_var 
names(libperm)[1] <- "region"
names(libperm)[2] <- "value"

libperm$region <- c(1:32)


mxstate_choropleth(libperm,
                   title = "Variación porcentual por delitos contra libertad personal",
                   num_colors = 1,
                   legend = "%") 


violet <- VL_ent_var 
names(violet)[1] <- "region"
names(violet)[2] <- "value"

violet$region <- c(1:32)


mxstate_choropleth(violet,
                   title = "Variación porcentual por violencia letal",
                   num_colors = 1,
                   legend = "%") 


theme_set(theme_minimal())


LP_nacional2 <- LP_nacional

names(LP_nacional2)[1] <- "Year"
names(LP_nacional2)[2] <- "Month"

LP_nacional2$date <- 
  paste(LP_nacional2$Year, LP_nacional2$Month, sep="-")


class(LP_nacional2$date)
LP_nacional2$date <- paste0(LP_nacional2$date, '-01')
LP_nacional2$date <- as.Date(LP_nacional2$date, format = '%Y-%m-%d')


LP_nacional2 <- LP_nacional2 %>% select(-c(1, 2))
LP_nacional3 = subset(LP_nacional2, select = -c(1,2) )
LP_nacional3 <- LP_nacional3[,c(3,1,2)]

LP_nacional4 <- LP_nacional3 %>% 
  pivot_wider(names_from = Sexo, values_from = sumatotal) %>%
  group_by(date)

ggplot(LP_nacional4, aes(x=date)) + 
  geom_line(aes(y = Hombre), color = "darkblue") + 
  geom_line(aes(y = Mujer), color="darkorchid4") +
  geom_vline(xintercept = 2018)




ggplot(LP_nacional2, aes(x = date, y = sumatotal)) + 
  geom_line(aes(color = Sexo)) + 
  scale_color_manual(values = c("darkblue", "darkorchid4", "yellow")) +
  labs(title = "Delitos contra la libertad personal en México",
    x = "",
    y = "") + 
  geom_vline(xintercept=as.numeric(LP_nacional2$date[c(111)]),
                       linetype=1, colour="black") +
  geom_label(aes(x = as.Date("2016-03-01", "%Y-%m-%d"), y = 1000,
                 label = "EPN"),
             size = 5, color = "black") +
  geom_label(aes(x = as.Date("2021-08-01", "%Y-%m-%d"), y = 1000,
                 label = "AMLO"),
             size = 5, color = "black")






VL_nacional2 <- VL_nacional

names(VL_nacional2)[1] <- "Year"
names(VL_nacional2)[2] <- "Month"

VL_nacional2$date <- 
  paste(VL_nacional2$Year, VL_nacional2$Month, sep="-")


class(LP_nacional2$date)
VL_nacional2$date <- paste0(VL_nacional2$date, '-01')
VL_nacional2$date <- as.Date(VL_nacional2$date, format = '%Y-%m-%d')





ggplot(VL_nacional2, aes(x = date, y = sumatotal)) + 
  geom_line(aes(color = Sexo)) + 
  scale_color_manual(values = c("darkblue", "darkorchid4", "yellow")) +
  labs(title = "Violencia letal en México",
       x = "",
       y = "") + 
  geom_vline(xintercept=as.numeric(VL_nacional2$date[c(111)]),
             linetype=1, colour="black") +
  geom_label(aes(x = as.Date("2016-01-01", "%Y-%m-%d"), y = 2800,
                 label = "EPN"),
             size = 5, color = "black") +
  geom_label(aes(x = as.Date("2021-08-01", "%Y-%m-%d"), y = 2800,
                 label = "AMLO"),
             size = 5, color = "black")

sum(LP_ent_AMLO$total_ent)
sum(LP_ent_AMLO$pob_estim)
sum(LP_ent_EPN$total_ent)
sum(LP_ent_EPN$pob_estim)
t2LP <- 73793/127792286
t1LP <- 59402/124041731
varLP <- (t2LP-t1LP)/t1LP*100


sum(VL_ent_AMLO$total_ent)
sum(VL_ent_AMLO$pob_estim)
sum(VL_ent_EPN$total_ent)
sum(VL_ent_EPN$pob_estim)
t2VL <- 105804/127792286
t1VL <- 86021/124041731
varVL <- (t2VL-t1VL)/t1VL*100










IDM <- vroom::vroom("/Users/santiago/Downloads/IDM_NM_dic21.csv",
                    skip = 0, 
                    locale = vroom::locale(encoding = "CP1252")) ## Código que elimina error al abrir base
names(IDM)[1] <- "ao"
names(IDM)[4] <- "cvemunicipio"
names(IDM)[6] <- "bj_afectado"
names(IDM)[7] <- "delito"
names(IDM)[8] <- "subdelito"
names(IDM)[9] <- "modalidad"
names(IDM)[10] <- "ene"
names(IDM)[11] <- "feb"
names(IDM)[12] <- "mar"
names(IDM)[13] <- "abr"
names(IDM)[14] <- "may"
names(IDM)[15] <- "jun"
names(IDM)[16] <- "jul"
names(IDM)[17] <- "ago"
names(IDM)[18] <- "sep"
names(IDM)[19] <- "oct"
names(IDM)[20] <- "nov"
names(IDM)[21] <- "dic"


CONAPO21 <- CONAPO %>% 
  filter(ao == 2021) %>% 
  select(c(1, 4, 5, 6))

names(CONAPO21)[3] <- "Municipio"
names(CONAPO21)[2] <- "Entidad"

IDM2 <- IDM %>% 
  left_join(CONAPO21, by= c("Municipio", "Entidad")) %>% 
  filter(pob_estim >= 100000) %>% 
  select(c(1, 3, 5, 6, 7, 8, 10:21, 23))


IDM3 <- pivot_longer(IDM2, 7:18, names_to = "Mes", values_to = "CI") %>% 
  select(-(7))

names(IDM3)[1] <- "ao"


IDM3_total <- IDM3 %>%
  group_by(ao, Municipio, Mes) %>%
  summarise(sumatotal = sum(CI))

IDM3_mun_EPN <- IDM3 %>% 
  filter(ao %in% c(2015, 2016, 2017, 2018)) %>% 
  filter((ao > 2015) | (ao == 2015 & Mes == 12)) %>% 
  filter(!(ao == 2018 & Mes == 12)) %>% 
  filter(`subdelito` %in% c("Homicidio doloso", "Feminicidio"))
  group_by(Municipio) %>% 
  summarise(total_mun = sum(sumatotal)) %>% 
  merge(pob_2017, by = "Entidad") %>% 
  mutate(tasa = total_ent/pob_estim)

LP_ent_AMLO <- LP_ent_total %>% 
  filter(Año %in% c(2018, 2019, 2020, 2021)) %>% 
  filter((Año > 2018) | (Año == 2018 & Mes==12)) %>% 
  filter(!(Año == 2021 & Mes == 12)) %>% 
  group_by(Entidad) %>% 
  summarise(total_ent = sum(sumatotal)) %>% 
  merge(pob_2020, by = "Entidad") %>% 
  mutate(tasa2 = total_ent/pob_estim)

LP_ent_var <- merge(LP_ent_AMLO, LP_ent_EPN, by = "Entidad") %>% 
  select(c(1, 4, 7)) %>% 
  mutate(var = (tasa2-tasa)/tasa*100) %>% 
  select(c(1, 4))

write.csv(LP_ent_var,'LP_ent_var.csv')



numero_sub <- IDM3 %>%
  group_by(ao, Municipio, Entidad, delito, subdelito) %>%
  summarise(total_subdelito = sum(total_del))

tasas_del <- IDM_ZMG2 %>%
  group_by(ao, cvemunicipio, delito) %>%
  summarise(tasa_delito = sum(tasa))

tasas_mod <- IDM_ZMG2 %>%
  group_by(ao, cvemunicipio, delito, subdelito, modalidad) %>%
  summarise(tasa_delito = sum(tasa))
