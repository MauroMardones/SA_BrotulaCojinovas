## estandarizacion ARRASTRE PDA - Merluza del Sur

rm(list=ls(all=TRUE))

setwd("C:/Users/macristina.perez/Documents/Recursos/Coj_Brot/2021/cojinoba_moteada")

library(dplyr)
library(lattice)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(lubridate)
library(psych)

datos            <- read.csv('MAustralHist.csv', header=T, sep=',')
dim(datos)
#117411  69
#133524  76

datos$zona    <- cut(datos$LATITUD_CALADO_AR, breaks=c(0,412800,470000,600000,900000), labels=c('outZN','ZN','ZS','outZS'))
datos$lat     <- cut(datos$LATITUD_CALADO_AR/10000, quantile(datos$LATITUD_CALADO_AR/10000, (0:8)/8, na.rm=T))
###cojinoba brotula especie 5
datos$catch   <- datos$catch_cojinoba/1000
datos$cpue    <- datos$catch/datos$h_a
datos$prop    <- datos$catch_cojinoba/datos$captura.total
datos$logcpue <- log(datos$cpue)
dim(datos)
#117411   75
#133524   84
summary (datos)
hist(datos$h_a,ylim=c(0,120000))
hist(datos$catch, ylim=c(0,25000),xlim=c(0,50))
hist(datos$cpue, ylim=c(0,25000), xlim=c(0,50))


## --------------- sorting data ----------------------------
data.cojinoba     <- datos  %>%  filter(catch>0 & catch<20) %>% ##%>% filter( zona=='ZN' | zona=='ZS') 
                                 filter( !is.na(lat) )  %>% filter( h_a>0 & h_a<30 ) %>%   filter(añoLance >= 1997) %>% #%>% filter (PROFUNDIDAD_MINIMA_AR>0 & PROFUNDIDAD_MAXIMA_AR<600) # 
                                 filter( cpue>0 & cpue<10 & !is.infinite(cpue) & !is.na(cpue))
dim(data.cojinoba)
#18595   75
#26710   84
names(data.cojinoba)

hist(data.cojinoba$h_a)
hist(data.cojinoba$catch)
hist(data.cojinoba$cpue)



dat.cojinoba.ope <- data.cojinoba %>% select(c(1:28,62:82))
names(dat.cojinoba.ope)
dat.cojinoba.esp <- data.cojinoba %>% select(c(29:63))
dat.cojinoba.esp <- dat.cojinoba.esp %>% mutate(total = rowSums(.,na.rm=TRUE))
names(dat.cojinoba.esp)

par(mfcol=c(2,3))
nbb = 30
hist( (dat.cojinoba.ope$cpue), nbb, main='CPUE')
hist( log(dat.cojinoba.ope$cpue), nbb, main='log CPUE')
hist( (dat.cojinoba.ope$catch), nbb, main='Catch')
hist( log(dat.cojinoba.ope$catch), nbb, main='log Catch')
hist( (dat.cojinoba.ope$h_a), nbb, main='Effort')
hist( log(dat.cojinoba.ope$h_a), nbb, main='log Effort')

#write.csv(data.cojinoba, file = "data_cojinoba_arrastre_2021.csv", row.names = T)

data.cojinoba<- read.csv('data_cojinoba_arrastre_2021.csv', header=T, sep=',')

## --------------- cpue x fishery ----------------------------

a <- ggplot( data.cojinoba, aes( x=catch, y = (..count..)/sum(..count..)) )
a <- a + geom_histogram(aes(), bins=25, fill='white', color="black" ) 
a <- a + facet_grid(NOMBRE_BARCO ~ zona, scale='free_y') 
a <- a + scale_y_continuous(labels = scales::percent) + ylab('Freuqnecy')
a

a <- ggplot( data.cojinoba, aes( x=logcpue) )
a <- a + geom_histogram(aes(y = (..count..)/sum(..count..)), bins=50, fill='white', color="black"  ) 
a <- a + facet_grid(NOMBRE_BARCO ~ zona, scale='free_y') 
a <- a + scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = c(-8,-4,0,4,8)) 
a

a <- ggplot( data.cojinoba, aes( x=h_a) )
a <- a + geom_histogram(aes(y = (..count..)/sum(..count..)), bins=50, fill='white', color="black"  ) 
a <- a + facet_grid(añoLance~zona, scale='free_y') 
a <- a + scale_y_continuous(labels = scales::percent)
a

a <- ggplot( data.cojinoba, aes( x=h_a) )
a <- a + geom_histogram(aes(y = (..count..)/sum(..count..)), bins=50, fill='white', color="black"  ) 
a <- a + facet_grid(~añoLance, scale='free_y') 
a <- a + scale_y_continuous(labels = scales::percent)
a

x11()
a <- ggplot( data.cojinoba, aes( x=catch, y = (..count..)/sum(..count..)) )
a <- a + geom_histogram(aes(), bins=25, fill='white', color="black" ) 
a <- a + facet_wrap(añoLance ~ zona, scale='free_y') 
a

a <- ggplot( data.cojinoba, aes( x=catch, y = (..count..)/sum(..count..)) )
a <- a + geom_histogram(aes(), bins=25, fill='white', color="black" ) 
a <- a + facet_wrap( ~ añoLance , scale='free_y') 
a

a <- ggplot( data.cojinoba, aes( x=logcpue) )
a <- a + geom_histogram(aes(y = (..count..)/sum(..count..)), bins=50, fill='white', color="black"  ) 
a <- a + facet_wrap(añoLance ~ zona, scale='free_y') 
a

a <- ggplot( data.cojinoba, aes( x=logcpue) )
a <- a + geom_histogram(aes(y = (..count..)/sum(..count..)), bins=50, fill='white', color="black"  ) 
a <- a + facet_wrap( ~añoLance, scale='free_y') 
a

a <- ggplot( data.cojinoba, aes( x=h_a) )
a <- a + geom_histogram(aes(y = (..count..)/sum(..count..)), bins=50, fill='white', color="black"  ) 
a <- a + facet_wrap(añoLance~zona, scale='free_y') 
a

a <- ggplot( data.cojinoba, aes( x=h_a) )
a <- a + geom_histogram(aes(y = (..count..)/sum(..count..)), bins=50, fill='white', color="black"  ) 
a <- a + facet_wrap(~añoLance, scale='free_y') 
a

## --------------- cpue x estrato latitud ----------------------------

######
#DATOS LISTOS
######

rm(list=ls(all=TRUE))

setwd("C:/Users/macristina.perez/Documents/Recursos/Coj_Brot/2021/cojinoba_moteada")

data.cojinoba <- read.csv('data_cojinoba_arrastre_2021.csv', header=T, sep=',')
data.cojinoba <- select(data.cojinoba, -X)

names(data.cojinoba)

library(dplyr)

dat.cojinoba.ope <- data.cojinoba %>% select(c(1:28,64:84))
names(dat.cojinoba.ope)
dat.cojinoba.esp <- data.cojinoba %>% select(c(29:63))
dat.cojinoba.esp <- dat.cojinoba.esp %>% mutate(total = rowSums(.,na.rm=TRUE))
names(dat.cojinoba.esp)

library(reshape2)
catch.zona    <- melt(dcast(dat.cojinoba.ope, añoLance + NOMBRE_BARCO ~ zona, median, value.var=c('cpue')), id=c(1,2))
#catch.zona$ts <- make_datetime(catch.zona$year, catch.zona$mes)

##toneladas por hora de arrastre

library(ggplot2)

a <- ggplot( catch.zona, aes( x=añoLance, y=value, color=as.factor(NOMBRE_BARCO) ) )
a <- a + geom_point(size=2) + geom_line()
a <- a + facet_wrap(~variable, scale='free_x')
a <- a + theme_minimal(10)
a

## --------------- Metiers analysis ----------------------------
library(ade4)
library(stats)
library(RColorBrewer)
library(pixmap)
library(tidyr)
library(WVPlots)

source('some_fun.R')

especies <- select( dat.cojinoba.esp, -total )
names(especies)
especies <- especies[,!!colSums( especies[grep( 'X', names(especies) )] )]  # Removing all columns summing to zero

esp.sub   <- select(filter(cbind(year=dat.cojinoba.ope$añoLance, especies), year>=1997), -year)
dim(esp.sub)
#18367    34
#19370    34 
#Reducir el numero especies a aquellas que expliquen el 85% de la varianza
esp.sub   <- as.matrix(esp.sub)
pcr       <- prcomp(esp.sub, center = FALSE, scale. = FALSE)
pcr2      <- princomp(esp.sub)

#plot(pcr$sdev)

dotplot_identity(frame = data.frame(pc=1:length(pcr$sdev), magnitude=pcr$sdev), 
                 xvar="pc",yvar="magnitude") + ggtitle("Unscaled case: Magnitudes of singular values")

#decidir el numero de CP para el corte (85% var) 80%

projectedTrainIdeal <- as.data.frame(esp.sub %*% extractProjection(5,pcr)[,1:5], stringsAsFactors = FALSE)
projectedTrainIdeal$cpue <- dat.cojinoba.ope$cpue
#cambia a ortogonal
#se le agregan los datos de cupe para ver si tb separa los componentes

#scatter.hist(projectedTrainIdeal, pch=(19+as.numeric(dat.msur.ope$zona)), col=c("blue","red")[dat.msur.ope$zona], grid=TRUE)

ttg <- projectedTrainIdeal

##revisar esto
ScatterHistN(projectedTrainIdeal, xvar='PC1', yvar='PC2', zvar='cpue', title="Datos proyectados en los PC 1-2", nclus = 2)
#explicacion del grafico

rot5U <- extractProjection(5,pcr)
rot5U = as.data.frame(rot5U)
rot5U$varName = rownames(rot5U)
rot5U = gather(rot5U, "PC", "loading", starts_with("PC"))
rot5U$vartype = ifelse(grepl("\\bX2\\b|\\bX3\\b|\\bX29\\b|\\bX96\\b|\\bX6\\b|\\bX4\\b|\\bX5\\b", 
                             as.character(rot5U$varName)),
                       "target", "bycatch")
rot5U$vartype = ifelse(grepl("\\bX2\\b", as.character(rot5U$varName)),
                       "Msur", 
                       ifelse(grepl("\\bX3\\b", as.character(rot5U$varName)),
                              "M3A", 
                              ifelse(grepl("\\bX29\\b", as.character(rot5U$varName)),
                                     "Coj_moteada", 
                              ifelse(grepl("\\bX96\\b", as.character(rot5U$varName)),
                                     "Coj_sur", 
                                     ifelse(grepl("\\bX6\\b", as.character(rot5U$varName)),
                                            "Congrio", 
                                            ifelse(grepl("\\bX4\\b", as.character(rot5U$varName)),
                                                   "M cola", 
                                                       ifelse(grepl("\\bX5\\b", as.character(rot5U$varName)),
                                                          "Brotula", "bycatch")))))))

# ifelse(grepl("\\bX2\\b", as.character(rot5U$varName)),
#        "target", "bycatch")

dotplot_identity(filter(rot5U, !(loading>=-0.001 & loading<=0.001) ), "varName", "loading", "vartype") + 
  facet_wrap(~PC,nrow=1) + coord_flip() + 
  ggtitle("unscaled variable loadings, first 5 principal components") +
  scale_color_pander() + theme_minimal() + geom_hline(aes(yintercept=0), colour="#d95f02") +
  geom_point(size=3) + geom_linerange(aes(ymin=0), size=1.5)




pcl       <- unclass(pcr$rotation)  # pcl       <- extractProjection(dim(esp.sub)[2],pcr)
pcl2      <- unclass(pcr2$loadings)
pclperc   <- 100*(pcr$sdev)/sum(pcr$sdev)
pclperc2  <- 100*(pcr2$sdev)/sum(pcr2$sdev)

#componenetes que suman el 95% de 100*(pcr$sdev)/sum(pcr$sdev)

supe99    <- cumsum( pclperc ) <= 95
supe992   <- cumsum( pclperc2[grep( 'Comp.', names(pclperc2) )] ) <= 95

# que es pcr$x
ingre     <- data.frame( pcr$x[,supe99] )
ingre2    <- data.frame( pcr2$scores[,supe992] )

# Analisis de agrupamiento no jerarquico ya que el gran numero de lances no es manjeable a traves de agrupamiento jerarquico
#cuyo objetivo fue agrupar el total del lances en 2500

cl        <- kmeans( ingre, 1500 )
cl2       <- kmeans( ingre2, 1500 )

#utilizando distancia euclidiana

Y         <- dist( cl$centers, "euclidean" )
Y2        <- dist( cl2$centers, "euclidean" )

# despues se realiza el agrupamiento jerarquico (dendograma) a estas 2500 agrupaciones no jerarquicas metodo ward

Z         <- hclust( Y, "ward.D" )
Z2        <- hclust( Y2, "ward.D" )

dendo     <- as.dendrogram(Z)  # 1500 centroides
dendo2    <- as.dendrogram(Z2)  

plot(dendo)
plot(dendo2)

hcut      <- quantile( Z$height, c(0.998) ) 
ngrp      <- length( cut(dendo,hcut)$lower ) 
groupes   <- cutree( Z, k=ngrp )

hcut2     <- quantile( Z2$height, c(0.998) ) 
ngrp2     <- length( cut(dendo2,hcut2)$lower ) 
groupes2  <- cutree( Z2, k=ngrp2 )

# Rescate de datos

vector   <- matrix(cl$cluster, ncol=1) * NA
vector2  <- matrix(cl2$cluster, ncol=1) * NA

for (j in 1:1500)
{
  vector[which( cl$cluster==j ), 1] = groupes[j]
  #vector2[which( cl2$cluster==j ), 1] = groupes2[j]
}

#proporcion de especies por cluster cada cluster suma 1

prop.cluster <- NULL
for (i in 1:ngrp)
{
  cluster      <- esp.sub[vector==i,]
  prop.cluster <- as.data.frame(rbind(prop.cluster, c(colMeans(cluster, na.rm = FALSE), i, dim(cluster)[1])))
} 

prop.cluster2 <- NULL
for (i in 1:ngrp2)
{
  cluster2      <- esp.sub[vector2==i,]
  prop.cluster2 <- as.data.frame(rbind(prop.cluster2, c(colMeans(cluster2, na.rm = FALSE), i, dim(cluster2)[1])))
} 

colnames(prop.cluster) <- c(names(as.data.frame(cluster)), 'Metier', 'Nmuestras')
prop.sp <- select( prop.cluster, c(-Metier, -Nmuestras))

colnames(prop.cluster2) <- c(names(as.data.frame(cluster2)), 'Metier', 'Nmuestras')
prop.sp2 <- select( prop.cluster2, c(-Metier, -Nmuestras))


ki        <- apply(prop.sp, 1, cumsum)
pie.data  <- NULL
for (k in 1:ngrp)
{
  tmp       <- ki[,k]
  ubi       <- tmp[grep( 'X', names(tmp) )] <= 0.90
  esp.me    <- data.frame(SP=prop.sp[k, ubi], otras=1-sum(prop.sp[k, ubi]), 
                          Metier=paste0('Metier: ',prop.cluster$Metier[k]), 
                          muestras=paste0('nmuestra: ',prop.cluster$Nmuestras[k]), n=prop.cluster$Nmuestras[k])
  esp.me    <- esp.me[,esp.me!=0]
  pie.data  <- rbind(pie.data, melt(esp.me, id.vars = c('Metier','muestras','n')))
}


bb <- filter(pie.data)#, Metier!='Metier: 1')
ggplot(bb, aes(Metier, value, fill=variable, width=n/(2*max(n)) )) +
  geom_bar(stat = "identity", position="fill") + theme_minimal(14) 


## -----------------  analisis years 2014-2015 ---------------------------


bp <- ggplot(pie.data, aes(x="", y=value, fill=variable)) + 
  geom_bar(width = 1, stat = "identity") + facet_wrap(~Metier)


pie <- bp + coord_polar("y", start=0)
#pie <- pie + scale_fill_grey() + theme_minimal()

pie


#save(list = ls(all=TRUE), file = "Cluster.RData")
#write.csv(vector, file = "cluster_arrastre.csv", row.names = T)



# 
# 
# 
# pie( prop.cluster[prop.cluster > 0], edges = 200, radius = 1,
#      labels = grep( 'X', names(cluster) ),
#      col = gray( seq( 0.0, 1.0, length=16 ) ),
#      main = paste("Metier"," ",i,": ", dim(cluster)[1]," registros",sep="") )
# 
# 
# win.graph(); op <- par(mfcol = c(2,2))  #, mar = c(5,2,1,4))
# 
# xax = 1; yax = 2;
# 
# s.corcircle( pcl[,c(xax,yax)], 1, 2, sub=paste("(",xax,"-",yax+1,")",
#                                                round(sum(pclperc[c(xax,yax,yax+1)]),0), "%", sep=""),
#                                                possub="bottomright", csub=2, clabel=1)

# scatterutil.eigen(pclperc, wsel=c(xax,yax,yax+1), sub="")
# 
# 
# plot(ingre[,1:2], pch = 5, cex = 0.8, col = "grey")
# points(cl$centers, pch = 1, cex = 0.8)
# 
# 
# dendo$labels=rep("",length(Z$height))  # Z$labels
# dendo <- as.dendrogram(Z)  # "print()" method  ## plot(dendo, leaflab = "none")  # plot(Z, leaflab = "none")
# 
# 
# win.graph(); op <- par(mfrow= c(1,2))  #, mar = c(5,2,1,4))
# 
# attr(dendo,"edgetext") = round(max(Z$height),1)
# 
# plot(dendo, edgePar = list(lty=1, col=c("black","darkgrey")),
#      edge.root=FALSE,horiz=FALSE,axes=TRUE, leaflab = "none")
# 
# rect.hclust(Z, ngrp)
# 
# abline(h=hcut,col="red")
# 
# text(x=hcut,y=length(Z$height),labels=as.character(round(hcut,1)),col="red",pos=4)
# 
# colorsnames= brewer.pal(ngrp,"Dark2")
# 
# 
# ttab=table(groupes)
# 
# mp=barplot(as.vector((ttab)),horiz=T,space=0,
#            col=rev(colorsnames),xlim=c(0,max(ttab)+290),
#            axes=TRUE,axisnames=TRUE)  # Groups   ....  main="Groups"
# 
# text(ttab,mp,as.character(ttab),col=rev(colorsnames),cex=1.2,pos=4) #names.arg=paste("g",ngrp:1,sep="")
# 
# 
