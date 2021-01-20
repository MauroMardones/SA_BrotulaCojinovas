 #Ajusta el modelo Schaefer utilizando optim
 #Datos de merluza sudafrica

 rm(list=ls())
 setwd <- ("C:/Users/mauricio.mardones/Documents/IFOP/Bibliografía/Data Poor/Production_Models/Mod_Prod_Schaefer_R")
 FileN <- "C:/Users/mauricio.mardones/Documents/IFOP/Bibliografía/Data Poor/Production_Models/Mod_Prod_Schaefer_R/data.txt"
dir()
Nyear <- 76
 LosDatos <- scan(FileN,what=list(NULL,Catch=0,CPUE=0),n=Nyear*3)
 print(LosDatos)	
	
 co <- list()
 co$Final <- F
 co$Nyear <- Nyear
 co$Catch <- LosDatos$Catch
 co$CPUEObs <- LosDatos$CPUE
 assign("co",co,pos=1)
 
 xinit <- c(log(1800),log(0.4))
 result<- optim(xinit,f1,method="L-BFGS-B")	
 print(result)
 co$Final <- T
 assign("co",co,pos=1)
 xfin <- result$par
 f1(xfin)	
 NULL
 print(FinalPar)

# ==========================================================================================================================================
#FUNCION PARA EL AJSUTE
f1 <- function(x)
{
 K <- exp(x[1])
 r <- exp(x[2])

 # Alamacenamiento Temporal
 CpuePred <- rep(0,co$Nyear)
 Biomasa <- rep(0,co$Nyear+1)

 # Realiza las proyecciones de biomasa
 Biomasa[1] <- K
 for (Year in 1:co$Nyear)
  {
	Biomasa[Year+1] <- Biomasa[Year] + r*Biomasa[Year]*(1.0-Biomasa[Year]/K) - co$Catch[Year]
	if (Biomasa[Year+1] < 0.01) Biomasa[Year+1] <- 0.01
  }

 # Calcula el estimado maximo versoimil de q
 qbar <- 0; npar <- 0
 for (Year in 1:co$Nyear)
  if (co$CPUE[Year] > 0)
   { npar <- npar + 1; qbar <- qbar + log(co$CPUE[Year]/Biomasa[Year]) }
 qbar <- exp(qbar / npar)

 # Encuentra la suma de cuadrados SS
 SS <- 0
 for (Year in 1:co$Nyear)
  if (co$CPUE[Year] > 0)
   {
	 CpuePred[Year] <- qbar*Biomasa[Year]
	 SS <- SS + log(CpuePred[Year]/co$CPUE[Year])^2
   }
 # print(SS)

 # Diagnostico
 if (co$Final)
  {
	par(mfrow=c(1,2))
	Years <- seq(1917,1992)
	ymax <- max(co$CPUE,CpuePred)*1.1
	plot(Years[co$CPUE > 0],co$CPUE[co$CPUE > 0],xlab="AÒo",ylab="CPUE",type='b',pch=16,ylim=c(0,ymax))
	lines(Years[co$CPUE > 0],CpuePred[co$CPUE > 0],lty=2,lwd=2,col=2)
	ymax <- max(Biomasa)*1.1

	plot(c(Years,1993),Biomasa,xlab="AÒo",ylab="Biomasa",type='l',pch=16,lty=1,lwd=2,ylim=c(0,ymax))
	FinalPar <- NULL
	FinalPar$Biomasa <- Biomasa
	FinalPar$CPUE <- CpuePred
	FinalPar$Pars <- c(r,K,qbar)
   assign("FinalPar",FinalPar,pos=1)
  }

 return(SS)
}

# ==========================================================================================================================================