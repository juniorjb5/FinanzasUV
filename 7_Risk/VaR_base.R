
install.packages("PerformanceAnalytics")

library(quantmod)
library(PerformanceAnalytics)


#########Fecha inicial de descarga de datos
maxDate = "2000-01-01"

#Serie a descargar
tick<-"AMZN"

#Obtener la serie de precios desde Yahoo Finance
prices <- Ad(getSymbols(tick, auto.assign = FALSE, from=maxDate))
plot(prices)
View(prices)


#Calcular retornos
rets <- dailyReturn(prices)
plot(rets)
hist(rets)


library(tseries)
jarque.bera.test(rets)


#Calcular VaR y CVaR
VaR(rets,p=0.95,method = "historical")
quantile(rets,0.05)

VaR(rets,p=0.99,method = "gaussian")
CVaR(rets, p = 0.99, method = "historical")
ES(rets,p=0.99,method = "gaussian")
ES(rets, p = 0.99, method = "historical")



#Series que conforman el portafolio
tickers<- c("MSFT", "AAPL", "AMZN")

#Definir pesos del portafolio
weights<-c(0.5,0.1,0.4)

#Obtener series de precios del portafolio
getSymbols(tickers, from=maxDate)
View(AAPL)

#Crear el portafolio
Port.prices <- na.omit(merge(Ad(MSFT),Ad(AAPL), Ad(AMZN)))
colnames(Port.prices)<-tickers
View(Port.prices)
plot(Port.prices)


#Retornos del portafolio
Port.returns <- ROC(Port.prices,type="discrete")[-1]
colnames(Port.returns)<-tickers
View(Port.returns)
plot(Port.returns)

VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "historical")
ES(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "historical")


#Calcular VaR Individual
VaR.Hist<- VaR(Port.returns,p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
VaR.Gaus<- VaR(Port.returns,p=0.95, weights = NULL, portfolio_method = "single", method = "gaussian")
VaR.Mod<- VaR(Port.returns,p=0.95, weights = NULL, portfolio_method = "single", method = "modified")

#Guardar datos
All.VaR<-data.frame(rbind(VaR.Hist,VaR.Gaus,VaR.Mod))
rownames(All.VaR)<-c("Hist","Gaus","Mod")

All.VaR

#Calcular VaR Portfolio
Port.VaR.Hist<- VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "historical")$hVaR
Port.VaR.Gaus<- VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "gaussian")$VaR
Port.VaR.Mod<- VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "modified")$MVaR


All.VaR$Portafolio<-c(Port.VaR.Hist,Port.VaR.Gaus,Port.VaR.Mod)
All.VaR<-abs(All.VaR)
All.VaR$Type<-c("Hist","Gaus","Mod")

All.VaR

#Base para gráfico
library(reshape2)
library(ggplot2)
plotVaR<-melt(All.VaR,variable.name = "Ticker", value.name = "VaR")


g1<-ggplot(plotVaR,aes(x=Type,y=VaR, fill=Ticker))+
  geom_bar(stat="identity", position = "dodge") 

g1











