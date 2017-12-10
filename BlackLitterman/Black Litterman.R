#BlackLitterman
rm(list=ls())
gc()

library(tidyverse)
library(tidyquant)
library(quadprog)
#Loadstidyquant,tidyverse,lubridate,quantmod,TTR,andxts
#library(quantmod)
#quantitativefinance
library(ggplot2)
library(igraph)

tickers <- c('AC.MX','ALFAA.MX','ALPEKA.MX','ALSEA.MX','AMXL.MX','ASURB.MX',
        'BIMBOA.MX','BOLSAA.MX','CEMEXCPO.MX','ELEKTRA.MX','FEMSAUBD.MX','GAPB.MX',
        'GCARSOA1.MX','GENTERA.MX','GFINBURO.MX','GFNORTEO.MX','GFREGIOO.MX','GMEXICOB.MX',
        'GRUMAB.MX','IENOVA.MX','KIMBERA.MX','KOFL.MX','LABB.MX','LALAB.MX','LIVEPOLC-1.MX',
        'MEXCHEM.MX','NEMAKA.MX','OHLMEX.MX','OMAB.MX','PE&OLES.MX','PINFRA.MX',
        'SANMEXB.MX','TLEVISACPO.MX','VOLARA.MX','WALMEX.MX')

tickers_gf <- c('BMV:AC',	'BMV:ALFAA',	'BMV:ALPEKA',	'BMV:ALSEA',	'BMV:AMXL',	'BMV:ASURB',	'BMV:BIMBOA',
                'BMV:BOLSAA',	'BMV:CEMEXCPO',	'BMV:ELEKTRA',	'BMV:FEMSAUBD',	'BMV:GAPB',	'BMV:GCARSOA1',	'BMV:GENTERA',
                'BMV:GFINBURO',	'BMV:GFNORTEO',	'BMV:GFREGIOO',	'BMV:GMEXICOB',	'BMV:GRUMAB',	'BMV:IENOVA',	'BMV:KIMBERA',
                'BMV:KOFL',	'BMV:LABB',	'BMV:LALAB',	'BMV:LIVEPOLC-1',	'BMV:MEXCHEM',	'BMV:NEMAKA',	'BMV:OHLMEX',
                'BMV:OMAB',	'BMV:PE&OLES',	'BMV:PINFRA',	'BMV:SANMEXB',	'BMV:TLEVISACPO',	'BMV:VOLARA',	'BMV:WALMEX')

mapeo_tickers <- cbind(tickers,tickers_gf)

Precios<- tickers %>%
            tq_get(get="stock.prices",from="2013-01-01")

mexbol <-  tq_get(c("^MXX"),get="stock.prices",from="2000-01-01")
?tq_get
mexbol %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(col = "darkblue", size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  scale_y_continuous(labels = scales::dollar) + theme_tq() 

mexbol_rend <- mexbol %>%
  tq_transmute(select   = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic") %>% 
                summarise(xbar = mean(monthly.returns)*12, vol = sd(monthly.returns), sharpe = (xbar - 0.065)/(vol*sqrt(12)))

cetes <- read_delim("C:/Users/Gerardo/Documents/Repository/BlackLitterman/cetes.csv",delim = ",",skip=17,na = c("", "NA","N/E"))
cetes$date <- as.Date(cetes$Fecha,format="%d/%m/%Y")

cetes <- select(cetes,date,SF282) %>% 
          filter(date >= "2000-01-01") %>% 
            mutate(rf_mens = SF282*28/36000)

eom <- function(date){
  mes = month(date)
  ano = year(date)
  if (mes == 12){
    fecha = 1010000 + (ano + 1)
  }
  else{
    fecha = (mes+1)*1000000+10000+ano
  }
  return(mdy(fecha)-1)
}

cetes$date<-as.Date(sapply(cetes$date,eom))

cetes %>% 
  ggplot(aes(x = date, y = rf_mens)) +
  geom_line(col = "darkblue", size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  scale_y_continuous(labels = scales::percent) + theme_tq() 

mexbol_mens <- mexbol %>%
  tq_transmute(select   = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic") 
mexbol_mens$date<-as.Date(sapply(mexbol_mens$date,eom))
mexbol_mens <- inner_join(mexbol_mens,cetes,by='date') %>% 
  mutate(mktER = monthly.returns - rf_mens)

mexbol_mens %>% summarise(mean = mean(mktER)*12) 

ggplot(aes(x=date,y=cumprod(1+mexbol_mens$monthly.returns)),data=mexbol_mens) +
  geom_line() +
  labs(title = "Cumulative Returns on Mexbol", x = "Time", y = "Cum. Returns", col = "") +
  scale_y_continuous(labels = scales::percent) + theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
  
(tail(cumprod(1+mexbol_mens[mexbol_mens$date >= "2010-01-01",]$monthly.returns),1))^(1/8)-1

market_cap <- tq_get(tickers_gf,get="financials") %>% 
  filter(type=="BS") %>% 
  select(-annual) %>%  
  unnest() %>% filter(date=="2017-09-30", category == "Total Common Shares Outstanding")

CommonStock <- tq_get(tickers_gf,get="financials") %>% 
  filter(type=="BS") %>% 
  select(-annual) %>%  
  unnest() %>% filter(date=="2017-09-30", category == "Common Stock, Total")

cap_mercado <- left_join(as.tibble(mapeo_tickers),market_cap, by=c("tickers_gf"="symbol"))

ult_precio <- Precios %>%  filter(date==max(date)) %>%  select(symbol,close)

cap_mercado <- left_join(cap_mercado,ult_precio,by=c("tickers"="symbol"))

#Hubo que incluir un par a mano porque no están en Google Finance, oh dear!
cap_mercado$value[cap_mercado$tickers=="VOLARA.MX"] <- 877.856219
cap_mercado$value[cap_mercado$tickers=="PE&OLES.MX"] <- 397.475747
cap_mercado$value[cap_mercado$tickers=="TLEVISACPO.MX"] <- 2557.893922
cap_mercado$value[cap_mercado$tickers=="FEMSAUBD.MX"] <- 3347.57
cap_mercado$value[cap_mercado$tickers=="CEMEXCPO.MX"] <- 14565.082738
cap_mercado$value[cap_mercado$tickers=="KOFL.MX"] <- 7233.29

cap_mercado <- cap_mercado %>%  mutate(market.cap = value*close)

cap_mercado %>%
  ggplot(aes(tickers,market.cap)) + 
  geom_col(fill="grey") +
  labs(title = "BMV 35 compañías", x = "", y="Market Capitalization (Millions)",col = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar)

rend_mensuales <- Precios %>% group_by(symbol) %>%
  tq_transmute(select   = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic")

rend_mensuales$date <- as.Date(sapply(rend_mensuales$date,eom))
rend_mensuales <- inner_join(rend_mensuales,cetes,by='date')

rend_mensuales %>%
  ggplot(aes(x = symbol, y = monthly.returns)) + 
  geom_boxplot(fill="lightblue") +
  labs(title = "Monthly returns by company", x = "", y = "", col = "") +
  scale_y_continuous(labels = scales::percent) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

rend_mensuales %>% summarise(
                             mean=mean(monthly.returns,na.rm=T),
                             sd = sd(monthly.returns,na.rm=T),
                             min=min(monthly.returns,na.rm=T),
                             p1 = quantile(monthly.returns,.01,na.rm=T),
                             p99 = quantile(monthly.returns,.99,na.rm=T),
                             maxi = max(monthly.returns,na.rm=T)) %>% ungroup() %>% 
                      arrange(desc(maxi))

Precios[Precios$symbol %in% c("AC.MX","SANMEXB.MX","GRUMAB.MX"),] %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(col = "darkblue", size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 4, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) + theme_tq() 

#Winsorizamos los rendimientos

rend_mensuales <-rend_mensuales %>% 
                  filter(!(is.na(monthly.returns))) %>% group_by(symbol) %>% 
                      mutate(maxMR = quantile(monthly.returns,.98),
                             minMR = quantile(monthly.returns,.02),
                             adj.monthly.returns = ifelse(monthly.returns > maxMR,maxMR,ifelse(monthly.returns < minMR,minMR,monthly.returns))) %>% 
                                select(-maxMR,-minMR,-monthly.returns)


rend_mensuales %>%
  ggplot(aes(x = symbol, y = adj.monthly.returns)) + 
  geom_boxplot(fill="lightblue") +
  labs(title = "Winsorized Monthly returns by company", x = "", y = "", col = "") +
  scale_y_continuous(labels = scales::percent) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


rend_mensuales %>% 
  summarise(Total = n(), xbar=mean(adj.monthly.returns,na.rm=T), vol=sd(adj.monthly.returns,na.rm=T)) %>%
  ggplot(aes(x = vol, y = xbar, label=symbol)) + geom_text(size=3) + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  theme_tq() + labs(title = "Risk vs Return Tradeoff", subtitle="monthly frequency", y = "Mean return", x = "Volatility")  

rend_mensuales<- rend_mensuales %>% 
  mutate(monthly.ER =  adj.monthly.returns - rf_mens)

rend_mensuales %>% 
  summarise(Total = n(), xbar=mean(monthly.ER,na.rm=T), vol=sd(adj.monthly.returns,na.rm=T)) %>%
  ggplot(aes(x = vol, y = xbar, label=symbol)) + geom_text(size=3) + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  theme_tq() + labs(title = "Risk vs Return Tradeoff", subtitle="monthly frequency", y = "Mean return", x = "Volatility")  

matrix_rend <- rend_mensuales %>% select(symbol,date,adj.monthly.returns) %>% 
  spread(key="symbol",value="adj.monthly.returns")
cov_mens <- cov(as.matrix(matrix_rend[,-1]),use="pairwise.complete.obs")
R_mens <- cor(as.matrix(matrix_rend[,-1]),use="pairwise.complete.obs")

cov_mens[is.na(cov_mens)] <- 0
R_mens[is.na(R_mens)] <- 0

mkt.cap.adjust <- cap_mercado$market.cap

names(mkt.cap.adjust) <- cap_mercado$tickers

pesos_mercado <- mkt.cap.adjust/sum(mkt.cap.adjust,na.rm=T)
pesos_mercado[is.na(pesos_mercado)] <- 0
sum(pesos_mercado)

aversion_riesgo = mean(mexbol_mens$mktER)/var(mexbol_mens$monthly.returns)

retornosEsperados = aversion_riesgo*(cov_mens) %*% pesos_mercado

rend_mensuales %>% 
  summarise(Total = n(), vol=sd(adj.monthly.returns,na.rm=T)) %>%
  ggplot(aes(x = vol*sqrt(12), y = retornosEsperados*12, label=symbol)) + geom_text(size=2.5) + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  theme_tq() + labs(title = "Risk and Return", subtitle="Monthly frequency (annualized)", y = "Implied CAPM excess return", x = "Volatility")  

rend_mensuales %>% 
  summarise(Total = n(), xbar=mean(monthly.ER,na.rm=T)*12) %>% 
  ggplot(aes(x = xbar, y = retornosEsperados*12, label=symbol)) + geom_text(size=2.5) + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  theme_tq() + labs(title = "Implied (Excess) Returns vs Historical", subtitle="Monthly frequency (annualized)", y = "Implied CAPM excess return", x = "Historical Excess Returns") +
  geom_vline(xintercept = c(.02))


equal <- seq(1,1,length.out = (dim(cov_mens)[1]))
equal <- matrix(equal,ncol=1)
b<-1

min_var <- quadprog::solve.QP(cov_mens,pesos_mercado,equal,b,meq=1,factorized=T)
w_min_var <- min_var$solution
sum(w_min_var)

ret_minvar <- t(w_min_var) %*% retornosEsperados
risk_minvar <- t(w_min_var) %*% cov_mens %*% w_min_var
port_min_var <- data.frame(list(retorno = ret_minvar,riesgo=risk_minvar,symbol=c("min var")))

frontera <- port_min_var
restricciones <- matrix(cbind(equal,retornosEsperados),ncol=2)

weights_portfolios <- matrix(w_min_var,nrow=1)
colnames(weights_portfolios) <- colnames(cov_mens)

for (ret in seq(as.numeric(frontera[1,c("retorno")])*1.01,max(retornosEsperados)*2.5,length.out=60)){
  b <- c(1,ret)
  optimiza_ret <- quadprog::solve.QP(cov_mens,pesos_mercado,restricciones,b,meq=2,factorized=T)
  nuevo <- data.frame(list(retorno=matrix(optimiza_ret$solution,nrow=1) %*% matrix(retornosEsperados,ncol=1),
                           riesgo=matrix(optimiza_ret$solution,nrow=1) %*% cov_mens %*% t(matrix(optimiza_ret$solution,nrow=1)),symbol="algo"))
  frontera <- rbind(frontera,nuevo)
  weights_portfolios <- rbind(weights_portfolios,matrix(optimiza_ret$solution,nrow=1))
}

portafolios <- data.frame(list(tickers = colnames(weights_portfolios),t(weights_portfolios)))

rend_mensuales %>% 
  summarise(Total = n(), vol=sd(adj.monthly.returns,na.rm=T)) %>%
  ggplot(aes(x = vol, y = retornosEsperados, label=symbol)) + geom_text(size=2.5) + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  theme_tq() + labs(title = "Risk vs Return Tradeoff", subtitle="monthly frequency", y = "Implied CAPM returns", x = "Volatility")  +
  geom_line(aes(x=riesgo,y=retorno, color="red"), data=frontera, show.legend=F)

pesos <- matrix(optimiza_ret$solution,nrow=1)
colnames(pesos) <- colnames(cov_mens)
barplot(pesos,horiz=T,cex.names=0.5,las=2)

portafolios %>%
  ggplot(aes(tickers,X10)) + 
  geom_col(fill="grey") +
  labs(title = "BMV 35 compañías", x = "", y="Portfolio Allocation",col = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)


# visualizamos las correlaciones (ojo: usamos la relación inversa por para que rojo sea 1 y blanco sea 0)
heatmap(sqrt(2*(1-R_mens)), symm = TRUE)
#Incorporando Views del Mercado Mexicano

#1 Televisa va a tener un desempeño del -1% (Conf. 75%)
#2 Los aeropuertos tendrán un desempeño inferior a los grupos financieros de 2% (50%)
#3 AC sobrepasará a KOFL en un 1% (Conf. 65%)

Q = matrix(c(-0.01,0.02,0.01),ncol = 1)

#Vectores de Views por orden

View1 <- matrix(0,nrow=1,ncol = length(colnames(cov_mens)))
View2 <- matrix(0,nrow=1,ncol = length(colnames(cov_mens)))
View3 <- matrix(0,nrow=1,ncol = length(colnames(cov_mens)))
colnames(View1) <- colnames(cov_mens)
colnames(View2) <- colnames(cov_mens)
colnames(View3) <- colnames(cov_mens)

View1[1,"TLEVISACPO.MX"] <- 1
View2[1,c("GAPB.MX","OMAB.MX","ASURB.MX")] <- c(-1/3,-1/3,-1/3)
View2[1,c("GFREGIOO.MX","SANMEXB.MX","GFNORTEO.MX","GFINBURO.MX","GENTERA.MX")] <- c(1/5,1/5,1/5,1/5,1/5)
View3[1,"AC.MX"] <- 1
View3[1,"KOFL.MX"] <- -1

P <- rbind(View1,View2,View3)
P

#Calculamos la varianza de Omega proporcional a la de nuestra matriz "prior"
varView1 <- View1 %*% cov_mens %*% t(View1)
varView2 <- View2 %*% cov_mens %*% t(View2)
varView3 <- View3 %*% cov_mens %*% t(View3)

#Tao = 1 /(Muestras - Activos)
Tao <- 1

Omega <- diag(c(varView1,varView2,varView3)*Tao,nrow=3,ncol=3)
Omega

#BlackLitterman retornos esperados

BLExpectedReturns <- solve(solve(Tao*cov_mens) + t(P) %*% solve(Omega) %*% P) %*% (solve(Tao*cov_mens)%*%retornosEsperados + t(P) %*% solve(Omega) %*% Q)

CovBL <- cov_mens + solve(solve(Tao*cov_mens) + t(P) %*% solve(Omega) %*% P)



rend_mensuales %>% 
  summarise(Total = n(), vol=sd(adj.monthly.returns,na.rm=T)) %>%
  ggplot(aes(x = vol, y = BLExpectedReturns, label=symbol)) + geom_text(size=2.5) + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent, limits =c(-.01,.01)) +
  theme_tq() + 
  labs(title = "Risk vs Return Tradeoff", subtitle="monthly frequency", y = "Mean return", x = "Volatility") 

#BL

equal <- seq(1,1,length.out = (dim(CovBL)[1]))
equal <- matrix(equal,ncol=1)
b<-1

min_var <- quadprog::solve.QP(CovBL,pesos_mercado,equal,b,meq=1,factorized=T)
w_min_var <- min_var$solution
sum(w_min_var)

ret_minvar <- t(w_min_var) %*% BLExpectedReturns
risk_minvar <- t(w_min_var) %*% CovBL %*% w_min_var
port_min_var <- data.frame(list(retorno = ret_minvar,riesgo=risk_minvar,symbol=c("min var")))

frontera <- port_min_var
restricciones <- matrix(cbind(equal,BLExpectedReturns),ncol=2)

weights_portfolios <- matrix(w_min_var,nrow=1)
colnames(weights_portfolios) <- colnames(CovBL)

for (ret in seq(as.numeric(frontera[1,c("retorno")])*1.01,max(BLExpectedReturns)*3.5,length.out=60)){
  b <- c(1,ret)
  optimiza_ret <- quadprog::solve.QP(CovBL,pesos_mercado,restricciones,b,meq=2,factorized=T)
  nuevo <- data.frame(list(retorno=matrix(optimiza_ret$solution,nrow=1) %*% matrix(BLExpectedReturns,ncol=1),
                           riesgo=matrix(optimiza_ret$solution,nrow=1) %*% CovBL %*% t(matrix(optimiza_ret$solution,nrow=1)),symbol="algo"))
  frontera <- rbind(frontera,nuevo)
  weights_portfolios <- rbind(weights_portfolios,matrix(optimiza_ret$solution,nrow=1))
}

portafolios <- data.frame(list(tickers = colnames(weights_portfolios),t(weights_portfolios)))

rend_mensuales %>% 
  summarise(Total = n(), vol=sd(adj.monthly.returns,na.rm=T)) %>%
  ggplot(aes(x = vol, y = BLExpectedReturns, label=symbol)) + geom_text(size=2.5) + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  theme_tq() + labs(title = "Risk vs Return Tradeoff", subtitle="monthly frequency", y = "BL Expected Returns", x = "Volatility")  +
  geom_line(aes(x=riesgo,y=retorno, color="red"), data=frontera, show.legend=F)

pesos <- matrix(optimiza_ret$solution,nrow=1)
colnames(pesos) <- colnames(CovBL)
barplot(pesos,horiz=T,cex.names=0.5,las=2)

barplot(as.matrix(portafolios[,3:8]))

portafolios %>%
  ggplot(aes(tickers,X10)) + 
  geom_col(fill="grey") +
  labs(title = "BMV 35 compañías", x = "", y="Portfolio Allocation",col = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)