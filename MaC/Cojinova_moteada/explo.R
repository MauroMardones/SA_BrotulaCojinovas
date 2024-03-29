rm(list = ls())



library(dplyr)
library(ggplot2)

setwd("C:/Users/macristina.perez/Documents/Recursos/Coj_Brot/2021/cojinoba_moteada")
source('funciones.R')

data.cojinoba <- read.csv('MAustralHist.csv', header=T, sep=',')

data.cojinoba <- read.csv('data_cojinoba_arrastre_2021.csv', header=T, sep=',')

#tmp <- read.csv('matrix_PDA_ARR_HIST.csv', sep = ';')
#dim(tmp)

tserie <- data.cojinoba %>% tibble::as_tibble() %>%    #pesquer�a 5= flota merlucera (IFOP), 10= palangre hielero (IFOP) y 11=palangre f�brica (IFOP).
  select(a�oLance, h_a, catch_cojinoba, NOMBRE_BARCO) %>% 
  filter(h_a>0, catch_cojinoba > 0, a�oLance>=1997)#  filter(!(year==2015 &  COD_BARCO==400075)) 
dim(tserie)
#18367 4 
#19688 4
#26710 4 

names(tserie)

# ------------ hook time serie ----------------
nhook <- tserie %>% group_by(a�oLance) %>% #filter(COD_PESQUERIA %in% c(5,92)) %>%
  summarise (hook = mean(h_a, na.rm = TRUE),
             hooksd = sd(h_a, na.rm = TRUE),
             hookn = n(),
             hooke = qnorm(0.975)*hooksd/sqrt(hookn),
             hookl = hook - hooke,
             hookr = hook + hooke)

#write.csv(nhook, file = "nhook.csv", row.names = T)

nhook1 <- tserie %>% group_by(a�oLance,NOMBRE_BARCO) %>% #filter(COD_BARCO==400075)%>% 
   summarise(hook = mean(h_a, na.rm = TRUE),
            hookn = n())

#write.csv(nhook1, file = "nhook1.csv", row.names = T)

x11()
p1 <- ggplot(nhook, aes(a�oLance)) +# facet_wrap(~buque) + 
  geom_line(aes(y=hook), colour="blue") + 
  geom_ribbon(aes(ymin=hookl, ymax=hookr), alpha=0.2)
p1

nhook2 <- tserie %>% group_by(a�oLance, NOMBRE_BARCO) %>% 
   summarise (hook = mean(h_a, na.rm = TRUE),
             hooksd = sd(h_a, na.rm = TRUE),
             hookn = n(),
             hooke = qnorm(0.975)*hooksd/sqrt(hookn),
             hookl = hook - hooke,
             hookr = hook + hooke)

#write.csv(nhook2, file = "nhook2.csv", row.names = T)

# ------------ catch time serie ----------------
ncatch <- tserie %>% group_by(a�oLance) %>% 
  summarise (catch = mean(catch_cojinoba, na.rm = TRUE),   #catch  = sum(cap_msur/1000, na.rm = TRUE),
             catchsd = sd(catch_cojinoba, na.rm = TRUE),
             catchn = n(),
             catche = qnorm(0.975)*catchsd/sqrt(catchn),
             catchl = catch - catche,
             catchr = catch + catche)



#write.csv(ncatch, file = "ncatch.csv", row.names = T)

#######################################################################3
ncatch1 <- tserie %>% group_by(a�oLance,NOMBRE_BARCO) %>% 
  summarise (catch = sum(catch_cojinoba, na.rm = TRUE),
             catchsd = sd(catch_cojinoba, na.rm = TRUE),
             catchn = n(),
             catche = qnorm(0.975)*catchsd/sqrt(catchn),
             catchl = catch - catche,
             catchr = catch + catche)

#write.csv(ncatch1, file = "ncatch1.csv", row.names = T)
######################################################################

x11()
p2 <- ggplot(ncatch, aes(a�oLance)) + # facet_wrap(~buque) + 
  geom_line(aes(y=catch), colour="blue") + 
  geom_ribbon(aes(ymin=catchl, ymax=catchr), alpha=0.2) 

p3 <- ggplot(ncatch1, aes(a�oLance)) + # facet_wrap(~buque) + 
  geom_line(aes(y=catch), colour="blue") + 
  geom_ribbon(aes(ymin=catchl, ymax=catchr), alpha=0.2) 

# ------------ hook time serie ----------------
ncpue1 <- tserie %>% group_by(a�oLance,NOMBRE_BARCO) %>% 
  summarise (f1 = mean(catch_cojinoba, na.rm = TRUE),
             f2 = mean(h_a, na.rm = TRUE),
             cpue2 = f1/f2,
             cpue2sd = sd(catch_brotola/h_a, na.rm = TRUE),
             cpue2n = n(),
             cpue2e = qnorm(0.975)*cpue2sd/sqrt(cpue2n),
             cpue2l = cpue2 - cpue2e,
             cpue2r = cpue2 + cpue2e)

#write.csv(ncpue1, file = "ncpue1.csv", row.names = T)

ncpue <- tserie %>% group_by(a�oLance) %>% 
  summarise (f1 = mean(catch_cojinoba, na.rm = TRUE),
             f2 = mean(h_a, na.rm = TRUE),
             cpue = f1/f2,
             cpuesd = sd(catch_cojinoba/h_a, na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)

#write.csv(ncpue, file = "ncpue.csv", row.names = T)


ncpue3 <- tserie %>% group_by(a�oLance) %>%
  summarise (cpue = mean(catch_cojinoba/(h_a), na.rm = TRUE),
             cpuesd = sd(catch_cojinoba/(h_a), na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)

#write.csv(ncpue3, file = "ncpue3.csv", row.names = T)

# ------------ hook time serie ----------------

#ncpue <- tserie %>% group_by(a�oLance) %>%
  summarise (cpue = mean(catch_cojinoba/(h_a), na.rm = TRUE),
             cpuesd = sd(catch_cojinoba/(h_a), na.rm = TRUE),
             cpuen = n())
#write.csv(ncpue, file = "cpue_barco.csv", row.names = T)


p4 <- ggplot(ncpue, aes(a�oLance)) + #  facet_wrap(~buque) + 
  geom_point(aes(y=cpue, size=cpuen), colour="blue", alpha=0.5, shape=21) +
  geom_line(aes(y=cpue), colour="blue") + 
  geom_ribbon(aes(ymin=cpuel, ymax=cpuer), alpha=0.2)

####NO CORRER########

p2 <- ggplot(ncatch1, aes(year)) +  facet_wrap(~buque) + 
  geom_line(aes(y=catch), colour="blue") + 
  geom_ribbon(aes(ymin=catchl, ymax=catchr), alpha=0.2) 


p4 <- ggplot(ncpue1, aes(year)) + 
  geom_point(aes(y=cpue2, size=cpue2n), colour="blue", alpha=0.5, shape=21) +
  geom_line(aes(y=cpue2), colour="blue") + 
  geom_ribbon(aes(ymin=cpue2l, ymax=cpue2r), alpha=0.2) 

x11()
multiplot(p1, p2,p4, cols=1)


###### ----------- TODO ------------------
## - tiempo reposo
## - diferencia en profundidad inicio/fin calado
## - cambiar nombres variables por algunas mas faciles
## - repeticion de lances por flota
## - diagrama de cobertura de flotas
## - descartes clarificacion solo para 2016? - formato punto coma decimales
## - aumento esfuerzo 2016 tiene relacion con el programa descarte?
## - utilizar escenarios en la evaluacion de stock donde los cv a�os 2016-2017 sean diferentes







