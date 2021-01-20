#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><
#Programer: Peru Data Poor Workshop                
#Date:  Octubre 201                           
#Purpose: Estimar los parametros de un modelo logistico a partir de datos de CPUE
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
rm(list=ls(all=TRUE))  # Borra todas los objetos creados

#DATOS
DataDir <- "C:/Users/mauricio.mardones/Documents/IFOP/Cursos/Curso_DataPoor_2017_Chile/Dia1/EstimacionModeloLogistico/Data"
dir()
Data<-read.csv(file.path(DataDir,"Hake_data.csv"),header=T)

#PARAMETROS
pars<-c(0.4,2500,5000,0.001) #initial parameters r=0.4,K=2500,B0=5000,q=0.001
 Catch<-Data$Catch
  Effort<-Data$Effort
  CPUE_obs<-Data$CPUE

#FUNCION OBJETIVO
Schaefer <- function(pars) {
  r<-pars[1]
  K<-pars[2]
  B0<-pars[3]
  q<-pars[4]
  
  ndata <- nrow(Data)
  Catch<-Data$Catch
  Effort<-Data$Effort
  CPUE_obs<-Data$CPUE
  B_vect<-vector(length=ndata)
  B_vect[1]<-B0  #this could be equal to K if the populations was at carrying capacity
  #Model
  for(i in 2:ndata){
  B_vect[i]<-pmax(0.001,B_vect[i-1]+r*B_vect[i-1]*(1-B_vect[i-1]/K)-Catch[i-1])
  }
  CPUE_pred<-q*B_vect
 #SSQ
  SSQ<-(log(CPUE_obs)-log(CPUE_pred))^2
  SSQ <- sum(SSQ)
  return(SSQ)
}

#ESTIMACION
Schaefer.res <- optim(pars,Schaefer)
               
#RESULTADOS
print(Schaefer.res)

#PUNTOS DE REFERENCIA
r_est<-Schaefer.res$par[1]
K_est<-Schaefer.res$par[2]
B0_est<-Schaefer.res$par[3]
q_est<-Schaefer.res$par[4]

MSY<-r_est*K_est/4
B_MSY<-K_est/2
U_MSY<-r_est/2

B_vect<-vector(length=ndata)
B_vect[1]<-B0_est  #this could be equal to K if the populations was at carrying capacity
  #Model
for(i in 2:ndata){
  B_vect[i]<-pmax(0.001,B_vect[i-1]+r_est*B_vect[i-1]*(1-B_vect[i-1]/B0_est)-Catch[i-1])
}


