
##################################
##### Datos Desembarque ##########
##################################


############################################
rm(list=ls())  # Borra todas los objetos creados
getwd()
dir()

data<- read.csv2('Desembarque_5.csv', header = T, sep =',')

head(data)
tail(data)#llenar con 0 los NA

# data[is.na(data)] <- 0
# #DESEMBARQUES
# brotula<-table(data$Brotula, data$Ano, sum)
# 
# png(filename = "Desembarques.png",
#     width =14, height =6, units = "in", res = 300)
# 

#total <- matriz$X_NOR + matriz$X_SUR + matriz$XI

Year<- data$Ano
brotula <- data$Brotula
moteada <- data$Moteada
delsur <- data$Delsur


d<- table(Year, brotula); d

library(ggplot2)
#grafico Brotula
png(filename = "DesembarquesBrotula.png",  width =14, height =6, units = "in", res = 300)
b <- ggplot(data,aes(Year,brotula))+ 
  geom_bar(stat="identity", fill="gray")+ 
  ylim(0, 8500) +  
  xlab("") + 
  ylab("Desembarque (t.)")+
  geom_text(aes(label=brotula), angle=90, vjust=0.5, color="black", size=3.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=10)) +
  theme(axis.text.y = element_text( size=8))+
  scale_x_continuous(breaks = seq(from = 1979, to = 2020, by = 2))+
  annotate("text", x=2000, y=7000, label= "Brotula", size=10) 
b 



#grafico Moteada

m <- ggplot(data,aes(Year,moteada))+ geom_bar(stat="identity", fill="gray")+ 
  ylim(0, 8500) +   
  xlab("") + 
  ylab("Desembarque (t.)")+
  geom_text(aes(label=moteada), angle=90, vjust=0.5, color="black", size=3.5)+
  theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=10)) +
    theme(axis.text.y = element_text( size=8))+
    scale_x_continuous(breaks = seq(from = 1979, to = 2020, by = 2))+
  annotate("text", x=2000, y=7000, label= "C. moteada", size=10) 
m

#grafico Brotula
ds <- ggplot(data,aes(Year,delsur))+ geom_bar(stat="identity", fill="gray")+ 
  ylim(0, 8500) +   xlab("") + ylab("Desembarque (t.)")+
  geom_text(aes(label=delsur), angle=90, vjust=0.5, color="black", size=3.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=10)) +
  theme(axis.text.y = element_text( size=8))+
  scale_x_continuous(breaks = seq(from = 1979, to = 2020, by = 2))+
  annotate("text", x=2000, y=7000, label= "C. Delsur", size=10) 
ds
b/m/ds


win.gr()
par(mfrow=c(1,1), mar =c(3,6,2,1))
plot(Year, brotula, type="l", lwd = 2, col=2, ylab = "Desembarques (t.)", bg=2, xaxp=c(1979, 2017, 38),  ylim = c(0, 9000))
lines(Year, moteada,  type="l", lwd=2, xaxp=c(1979, 2017, 38),  col=3, ylim = c(0, 9000))
lines(Year, delsur,  type="l", lwd=2,  bg=2, xaxp=c(1979, 2019, 40),  col=4, ylim = c(0, 9000))

par(mar=c(4,6,1,1))
plot(Year, brotula, type="o", lty=2,lwd = 3, ylim = c(1, 9000), pch="",cex=1,
     ylab = "Desembarques (t.)", xlab="", xaxp=c(1979, 2020, 41), cex.lab=2, cex.axis=1.5)
lines(Year, moteada, type="o",pch="", cex=1.2,lty=3, lwd = 3)
lines(Year, delsur, type="o",pch="o",  cex=1.5,lwd = 3)

legend(2005,8000, c("Brótula","C. Moteada", "C. del sur"), lty=c(2,3,1), lwd=c(3,3,3),pch = c("","","o",""),
       bty="n", horiz =F, cex = 1.2)
#ggplot


p <- ggplot(data, aes(brotula, fill = Year)) +
  geom_histogram(binwidth = 2, color = "black", fill = "black") +
#x11()
p 






plot(matriz$Year, matriz$X_NOR, type="l", lwd = 2, col="black", ylim = c(1, 35000), 
     ylab = "Capturas Corregidas (t.)", xlab = "A?os", xaxp= c(1967, 2018, 51))
lines(matriz$Year, matriz$X_SUR, type="l", col="red", lwd = 2)
lines(matriz$Year, matriz$XI, type="l", col="blue", lwd = 2)
lines(matriz$Year, matriz$total, type="l", col="green", lwd = 2)
legend("topleft", c("XNORTE","XSUR", "XI", "Total"), lty=c(1,1,1,1,1), col=c("black","red", "blue", "green"), 
       bty="n", cex=1, yjust = 2, xjust = 2, lwd = "3")
abline(h=x, lty=2, lwd=3)

#sin colores
par(mar=c(4,6,1,1))
plot(matriz$Year, matriz$X_NOR, type="o", lty=2,lwd = "2", ylim = c(1, 35000), pch="",cex=1,
     ylab = "Capturas Corregidas (t.)", xlab = "A?os", xaxp= c(1960, 2017, 57), cex.lab=2, cex.axis=1.5)
lines(matriz$Year, matriz$X_SUR, type="o",pch="", cex=1.2,lty=3, lwd = "2")
lines(matriz$Year, matriz$XI, type="o",pch="o",  cex=1.5,lwd = 1)
lines(matriz$Year, matriz$total, lwd = "3",pch="",cex=1)
legend(1970,30000, c("XNORTE","XSUR", "XI", "Total"), lty=c(2,3,1,1), lwd=c(2,2,1,3),pch = c("","","o",""),
       bty="n", horiz =F, cex = 1.2)
abline(h=15250, lty=2, lwd=3)


ggplot(matriz, aes(x=Year, y=X_SUR))+
  geom_line()


p <- ggplot(fill=TRUE) +  geom_line(data = data, aes(x = Year, y = brotula), color = "gray70") +
  geom_point(data = data, aes(x = Year, y = moteada), color = "gray70") +
  geom_line(data = data, aes(x = Year, y = delsur), color = "black") +
   xlab('Years') +
  ylab('Desembarques corregidos (t.)')+
  #scale_x_discrete()+
  #geom_hline(yintercept = 2000, color = "red")+
  theme(legend.position = "rigth", legend.background = element_rect(color = "black", fill = "grey90", size = 1, linetype = "solid"))
p + theme_gray() # the default
p + theme_bw()
p + theme_linedraw()
p + theme_light()
p + theme_dark()
p + theme_minimal()
p + theme_classic()
p + theme_void()

p + guides(fill=FALSE)


bp <- ggplot(data=PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
bp



matriz2<-read.csv("Captura_Pond.csv",header=T,sep=",")
head(matriz2)
#View(matriz2)


x11()
plot(matriz2$A?O, matriz2$XNORTE, type="l", lwd = "2", col="gray", ylim = c(1, 30000), ylab = "Capturas (ton)", xlab = "A?os")
lines(matriz2$A?O, matriz2$XSUR, type="l", col="red", lwd = "2")
lines(matriz2$A?O, matriz2$XI, type="l", col="black", lwd = "2")
legend("topleft", c("X Norte", "X Sur", "XI"), lty=c(1,1,1,1,1), col=c("grey","red", "black"), 
       bty="n", cex=1.2, yjust = 2, xjust = 2, lwd = "3")


matriz<-read.csv("Biomasas.csv",header=T,sep=";")
head(matriz)
#View(matriz)

x11()
par(mar=c(4,6,1,1))
plot(matriz$A?o, matriz$Norte, type="o", lty=2,lwd = "2", ylim = c(1, 200000), pch="",cex=1,
     ylab = "Biomasas Totales (t.)", xlab = "A?os", xaxp= c(1960, 2017, 57), cex.lab=2, cex.axis=1.5)
lines(matriz$A?o, matriz$Sur, type="o",pch="", cex=1.2,lty=3, lwd = "2")
lines(matriz$A?o, matriz$IX, type="o",pch="o",  cex=1.5,lwd = 1)
lines(matriz$A?o, matriz$Biomasa.Total, lwd = "3",pch="",cex=1)
legend(1975,200000, c("XNORTE","XSUR", "XI", "Total"), lty=c(2,3,1,1), lwd=c(2,2,1,3),pch = c("","","o",""),
       bty="n", horiz =T, cex = 1.2)



matriz<-read.csv("Biomasas_Alt.csv",header=T,sep=";")
head(matriz)
#View(matriz)

x11()
par(mar=c(4,6,1,1))
plot(matriz$A?o, matriz$XNORTE, type="o", lty=2,lwd = "2", ylim = c(1, 250000), pch="",cex=1,
     ylab = "Biomasas Totales (t.)", xlab = "A?os", xaxp= c(1960, 2017, 57), cex.lab=2, cex.axis=1.5)
lines(matriz$A?o, matriz$XSUR, type="o",pch="", cex=1.2,lty=3, lwd = "2")
lines(matriz$A?o, matriz$XI, type="o",pch="o",  cex=1.5,lwd = 1)
lines(matriz$A?o, matriz$Biomasa.Total, lwd = "3",pch="",cex=1)
legend(2005,250000, c("XNORTE","XSUR", "XI", "Total"), lty=c(2,3,1,1), lwd=c(2,2,1,3),pch = c("","","o",""),
       bty="n", horiz =F, cex = 1.5)
