## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy=TRUE)
summarytools::st_options(plain.ascii = FALSE, style = "rmarkdown")

def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  paste0("\n \\", "tiny","\n\n", x, "\n\n \\normalsize")
})


## ----tidyverse_load, echo=TRUE-----------------------------------------
library(tidyverse)


## ----ggplot_load, echo=TRUE--------------------------------------------
library(ggplot2)
theme_set(theme_minimal())


## ----nelplo_read, echo=TRUE--------------------------------------------
Nelson_Plosser <- read.csv2("../data/Nelson_Plosser.csv", header=TRUE, sep=",", dec = ".")
head(Nelson_Plosser)


## ----echo=TRUE, message=FALSE------------------------------------------
library(summarytools)
statdesc <- descr(Nelson_Plosser %>% select(1:6))


## ----pib_summary, echo=TRUE, results="asis"----------------------------
statdesc


## ----nelplo_corr, echo=TRUE, tidy=TRUE---------------------------------
datanum <- Nelson_Plosser %>% filter(year>1909) %>% select(1:7) 
cormat <- round(cor(datanum),2)
cormat


## ----nelplo_corr_heatmap, echo=TRUE, message=FALSE, out.width="70%", fig.align='center'----
library(reshape2)
cormat %>% melt() %>% ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_distiller(palette = "Spectral", direction = 1)


## ----GGally, echo=FALSE------------------------------------------------
library(GGally)


## ----message=FALSE-----------------------------------------------------
g <- Nelson_Plosser %>% 
  filter(year>=1909) %>%
  select(cpi, gnp.nom, gnp.capita, emp, unemp) %>% 
  ggpairs(upper = list(continuous = wrap("cor", size = 4)),
          lower = list(continuous = wrap("points", colour="blue", alpha=0.3, size=0.5)))


## ----nelplo_pairplot_graph, echo=TRUE, out.width="70%", fig.align = 'center'----
g


## ----retail_read, echo=TRUE--------------------------------------------
retail <- read.csv2("../data/RSXFSN.csv", header=TRUE, sep=",", dec = ".")
head(retail)


## ----------------------------------------------------------------------
summary(retail)


## ----------------------------------------------------------------------
retail$DATE <- as.Date(retail$DATE,format="%Y-%m-%d")
summary(retail)


## ----------------------------------------------------------------------
library(tseries)
data(NelPlo)
class(NelPlo)


## ----------------------------------------------------------------------
window(NelPlo, start=1905, end=1910)


## ----gmoney_load, echo=TRUE, message=FALSE-----------------------------
library(lmtest)
data(growthofmoney)
window(growthofmoney, end=c(1971,4))


## ----message=FALSE, tidy=TRUE, tidy.opts = list(blank = FALSE, width.cutoff = 60)----
library(strucchange)
data("USIncExp")
window(USIncExp, start=c(1959,6), end=c(1960,6))


## ----retail_ts, echo=TRUE----------------------------------------------
retail_ts <- ts(retail['RSXFSN'], frequency=12, start=c(1992,1))
window(retail_ts, start=2020)


## ----------------------------------------------------------------------
tsp(retail_ts)


## ----pib_ts_time, echo=TRUE--------------------------------------------
retail2022 <- window(retail_ts, start=c(2022,1), end=c(2022,12))
time(retail2022)


## ----pib_ts_time2, echo=TRUE-------------------------------------------
library(lubridate)
as.numeric(time(retail2022))


## ----gmoney_tsibble, echo=TRUE, message=FALSE--------------------------
library(tsibble)
gmoney_tsbl <- growthofmoney %>% as_tsibble()
gmoney_tsbl


## ----nelplo_tsibble, echo=TRUE, message=FALSE--------------------------
nelplo_tsbl <- NelPlo %>% as_tsibble()
nelplo_tsbl %>% filter(index>1980 & key=="gnp.capita")


## ----------------------------------------------------------------------
nelplo_tsbl %>% distinct(key)


## ----retail_tsibble, echo=TRUE, message=FALSE--------------------------
retail_tsbl <- retail_ts %>% as_tsibble()
head(retail_tsbl)


## ----------------------------------------------------------------------
library(feasts)
nelplo_tsbl %>% 
  filter(key=="cpi") %>% 
  ACF(value)


## ----out.width="70%", fig.align="center"-------------------------------
library(ggfortify)
autoplot(ts(rnorm(100)))+ 
  ggtitle("White Noise")


## ----------------------------------------------------------------------
box_pierce(rnorm(100))


## ----------------------------------------------------------------------
tmax <- 100
# ourDrift <- 0.005
wnoise <- rnorm(99, mean=0, sd=1)
y <- rep(0,100)

for (t in 2:tmax) {
  y[t] = y[t-1] + wnoise[t-1] 
}
rw <- tibble(time=1:tmax, y=y)
rw


## ----out.width="70%", fig.align="center"-------------------------------
ggplot(rw) + geom_line(aes(x=time, y=y), colour="blue")


## ----------------------------------------------------------------------
tmax <- 100

wnoise <- rnorm(99, mean=0, sd=1)
y <- rep(0,100)
alpha <- 0.8

for (t in 2:tmax) {
  y[t] = alpha + y[t-1] + wnoise[t-1] 
}
rwd <- tibble(time=1:tmax, y=y)
rwd


## ----out.width="70%", fig.align="center"-------------------------------
ggplot(rwd) + geom_line(aes(x=time, y=y), colour="blue")


## ----sunspot, echo=FALSE, out.width="70%", fig.align="center"----------
plot(NelPlo[,c("cpi", "unemp")],xlab="Trimestre", main="Données Nelson-Plosser")


## ----out.width="70%", fig.align="center", warning=FALSE----------------
nelplo_tsbl %>% filter(key=="cpi") %>%
  ggplot(aes(x = index, y = value))+
    geom_line(color = "#00AFBB", size = 1)


## ----out.width="70%", fig.align="center", warning=FALSE----------------
USIncExp %>% as_tsibble() %>%
  ggplot(aes(x = index, y = value))+
    geom_line(aes(color = key), size = 1)


## ----out.width="70%", fig.align="center", warning=FALSE----------------
retail_tsbl %>%
  ggplot(aes(x = index, y = value))+
    geom_line(color = "#00AFBB", size = 1)


## ----warnings=F, message=FALSE, out.width="70%", fig.align="center"----
ggplot(gmoney_tsbl, aes(x = index, y = value)) + 
  geom_line(aes(color = key), size = 1) +
  scale_color_manual(values = c("red", "blue"))


## ----warnings=F, message=FALSE, out.width="70%", fig.align="center"----
nelplo_tsbl %>% filter(key %in% c("gnp.nom", "gnp.real", "unemp", "cpi", "money.stock")) %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(aes(color = key), size = 1)


## ----warnings=F, message=FALSE, out.width="70%", fig.align='center'----
nelplo_tsbl %>% filter(key=="cpi") %>% 
  ACF(value) %>% autoplot()


## ----warnings=F, message=FALSE, , out.width="70%", fig.align='center'----
gmoney_tsbl %>% ACF() %>% autoplot()


## ----warnings=F, message=FALSE, , out.width="70%", fig.align='center'----
retail_tsbl %>% ACF() %>% autoplot()


## ----out.width='70%', fig.align='center', message=FALSE----------------
library(forecast)
ggseasonplot(window(growthofmoney[,"TG1.TG0"]))


## ----out.width='70%', fig.align='center'-------------------------------
ggseasonplot(window(retail_ts, start=c(2000,1)))


## ----out.width='70%', fig.align='center'-------------------------------
retail_tsbl %>% gg_lag(y=value, geom="point", size=0.5, lags=c(1,3,6,9,12))


## ----out.width='70%', fig.align='center'-------------------------------
nelplo_tsbl %>% filter(key=='cpi') %>% 
  gg_lag(y=value, geom="point", size=0.5, colour="blue", lags=c(1,2,5,10,15)) 


## ----------------------------------------------------------------------
nelplo1909 <- nelplo_tsbl %>% 
  filter(index>=1909) %>% 
  spread(key = key, value=value)
nelplo1909


## ----------------------------------------------------------------------
nelplo1909 %>% select(index, gnp.nom, emp)


## ----out.width="70%", fig.align='center'-------------------------------
nelplo1909 %>%
  ggplot(aes(emp, gnp.nom)) +
    geom_point(colour="blue")


## ----------------------------------------------------------------------
mod1 <- lm(gnp.nom ~ emp, data=nelplo1909)
summary(mod1)


## ----out.width="70%", fig.align='center', message=FALSE----------------
ggplot(nelplo1909, aes(x=emp, y=gnp.nom)) +
  geom_point(colour="blue") +
  geom_smooth(method='lm', color="red")


## ----------------------------------------------------------------------
mod1_diag <- tibble(gnp=nelplo1909$gnp.nom, predicted=mod1$fitted.values, residual=mod1$residuals)
mod1_diag


## ----out.width="70%", fig.align='center', echo=FALSE-------------------
ggplot(mod1_diag) + 
  geom_point(aes(x=gnp, y=predicted), colour="blue") + 
  geom_abline(colour="grey")


## ----out.width="70%", fig.align='center'-------------------------------
ggplot(mod1_diag, aes(x=residual)) + 
  geom_histogram(aes(y = after_stat(density)), fill="lightblue", binwidth = 0.05) +
  geom_density(aes(x=residual))


## ----------------------------------------------------------------------
shapiro.test(mod1_diag$residual)


## ----out.width="70%", fig.align='center'-------------------------------
qqnorm(mod1_diag$residual)


## ----out.width="70%", fig.align='center'-------------------------------
ggplot(mod1_diag) + 
  geom_point(aes(x=predicted, y=residual), colour="blue")


## ----bptest------------------------------------------------------------
library(lmtest)
bptest(mod1)


## ----------------------------------------------------------------------
gdata <- tibble(index=1:nrow(mod1_diag), residus=mod1_diag$residual)
gdata %>% ggplot() + geom_point(aes(x=index, y=residus), color="red")


## ----out.width="70%", fig.align='center'-------------------------------
acf(mod1_diag$residual)


## ----------------------------------------------------------------------
dwtest(mod1)


## ----lm_gom_data-------------------------------------------------------
data("growthofmoney")
head(growthofmoney)


## ----lm_gom_plot, out.width='70%', fig.align='center', message=FALSE----
gdata <- tibble(AG0_TG0=growthofmoney[,"AG0.TG0"], TG1_TG0=growthofmoney[,"TG1.TG0"])
ggplot(gdata) + geom_point(aes(x=AG0_TG0, y=TG1_TG0), color="blue")


## ----------------------------------------------------------------------
modelHetzel <- TG1.TG0 ~ AG0.TG0
gom.mod1 <- lm(modelHetzel, data=growthofmoney)
summary(gom.mod1)


## ----out.width='70%', fig.align='center', message=FALSE----------------
gdata <- tibble(index=1:nrow(growthofmoney), residus=gom.mod1$residual)
gdata %>% ggplot() + geom_point(aes(x=index, y=residus), color="red")


## ----out.width="70%", fig.align='center'-------------------------------
acf(gdata$residus)


## ----------------------------------------------------------------------
dwtest(modelHetzel, data=growthofmoney)


## ----------------------------------------------------------------------
nelplo1909 <- nelplo_tsbl %>% 
  filter(index>=1909) %>% 
  spread(key = key, value=value)
regdata <- nelplo1909 %>% 
  select(index, gnp.capita, unemp, cpi)


## ----------------------------------------------------------------------
mod2 <- lm(gnp.capita ~ index+unemp+cpi, data=regdata)
summary(mod2)


## ----------------------------------------------------------------------
mod3 <- lm(gnp.capita ~ index+unemp, data=regdata)
summary(mod3)


## ----------------------------------------------------------------------
AIC(mod1, mod2, mod3)


## ----------------------------------------------------------------------
sctest(modelHetzel, point=c(1973,4), data=growthofmoney, type="Chow")


## ----------------------------------------------------------------------
rdata <- Nelson_Plosser[,c("year", "gnp.nom")]
rdata[,"logGNP"] = log(rdata[,"gnp.nom"])
rdata <- rdata[rdata$year>=1909,]
rdata <- ts(rdata, start=1909)
window(rdata, end=1916)


## ----------------------------------------------------------------------
library(strucchange)
model <- logGNP ~ year
sctest(model, point=c(1933,1), data=rdata, type="Chow")


## ----out.width="60%", fig.align='center'-------------------------------
fs <- Fstats(model, from = c(1920,1), to = c(1980,1), data = rdata)
plot(fs)


## ----------------------------------------------------------------------
mod3 <- lm(gnp.capita ~ index+unemp, data=regdata)


## ----------------------------------------------------------------------
dcmp <- retail_tsbl %>%
  model(classical_decomposition(value))
components(dcmp)


## ----out.width='70%', fig.align='center'-------------------------------
components(dcmp) %>% autoplot()


## ----out.width='70%', fig.align='center'-------------------------------
components(dcmp) %>% ggplot() + 
  geom_line(aes(x=index,y=value), colour="blue") +
  geom_line(aes(x=index,y=season_adjust), colour="red")


## ----out.width="60%", fig.align='center', message=FALSE----------------
USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  ggplot(aes(x = index, y = value)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method='lm', color="red")


## ----------------------------------------------------------------------
regdata <- USIncExp %>% as_tsibble() %>% filter(key=='expenditure')
summary(lm(value ~ index, data=regdata))


## ----out.width="60%", fig.align='center', message=FALSE----------------
USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  ggplot(aes(x = index, y = log(value))) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method='lm', color="red")


## ----------------------------------------------------------------------
regdata <- USIncExp %>% as_tsibble() %>% 
  filter(key=='expenditure') %>%
  mutate(value=log(value))
summary(lm(value ~ index, data=regdata))


## ----------------------------------------------------------------------
retail$mois <- factor(month(retail$DATE), labels=month(1:12, label=TRUE))
retail$t <- c(1:nrow(retail))
head(retail)


## ----------------------------------------------------------------------
modst <- lm(RSXFSN ~ t+ mois-1, data=retail)
summary(modst)


## ----out.width='70%', fig.align='center'-------------------------------
retail$prediction <- predict.lm(modst)
ggplot(retail)+
  geom_line(mapping=aes(x=t,y=RSXFSN),color="blue")+
  geom_line(mapping=aes(x=t,y=prediction), color="red")


## ----------------------------------------------------------------------
retail_1992_2022 <- retail %>% filter(year(DATE)<2023)
annees = nrow(retail_1992_2022)/12
t=1:annees

for (i in 1:12)
{
  su=rep(0,times=12)
  su[i]=1
  s=rep(su,times=annees)
  assign(paste("s",i,sep=""),s)
}
cbind(retail_1992_2022[,"RSXFSN"],s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12)[1:12,]


## ----------------------------------------------------------------------
coefst <- modst$coefficients
coefst


## ----------------------------------------------------------------------
a <- mean(coefst[2:13])
b <- coefst[1]
c <- coefst[2:13]-mean(coefst[2:13])
y_cvs <- retail_1992_2022$RSXFSN-(c[1]*s1+c[2]*s2+c[3]*s3+c[4]*s4+c[5]*s5+c[6]*s6+c[7]*s7+c[8]*s8+c[9]*s9+c[10]*s10+c[11]*s11+c[12]*s12)


## ----out.width='60%', fig.align='center'-------------------------------
gdata <- tibble(t=retail_1992_2022$t, original=retail_1992_2022$RSXFSN, cvs = y_cvs)

ggplot(gdata)+
  geom_line(mapping=aes(x=t,y=original),color="blue")+
  geom_line(mapping=aes(x=t,y=cvs), color="red")


## ----------------------------------------------------------------------
library(forecast)
bhat = tslm(retail_ts~trend+I(trend^2)+season)
summary(bhat)


## ----fig.height=3, fig.width=8-----------------------------------------
cpi_diff <- diff(NelPlo[,"cpi"],1)

cpi_diff %>% as_tsibble() %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(colour="red", size = 1)


## ----out.width="70%", fig.align="center"-------------------------------
USExp <- USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  mutate(logExp=log(value))
ggplot(USExp, aes(x = index, y = logExp)) +
  geom_line(color = "blue", size = 1)


## ----------------------------------------------------------------------
library(urca)
exp.df <- ur.df(y=as.data.frame(USExp)[,"logExp"], lags=12, type='trend')
exp.df


## ----------------------------------------------------------------------
summary(exp.df)


## ----------------------------------------------------------------------
exp.df <- ur.df(y=as.data.frame(USExp)[,"logExp"], lags=8, type='trend')
summary(exp.df)


## ----out.width="70%", fig.align="center"-------------------------------
USExp_diff <- tibble(time=1:(nrow(USExp)-1), diff= diff(as.data.frame(USExp)[, "logExp"]))
ggplot(USExp_diff, aes(x = time, y = diff)) +
  geom_line(color = "blue", size = 1)


## ----out.width="70%", fig.align="center"-------------------------------
Unemp <- nelplo_tsbl %>% filter(key=='unemp' & index >= 1909) 
ggplot(Unemp, aes(x = index, y = value)) +
  geom_line(color = "blue", size = 1)


## ----------------------------------------------------------------------
unemp.df <- ur.df(y=as.data.frame(Unemp)[,"value"], lags=1, type='drift')
summary(unemp.df)


## ----------------------------------------------------------------------
ktest <- ur.kpss(USExp$logExp, type="tau", lags="long")
summary(ktest)


## ----------------------------------------------------------------------
ktest <- ur.kpss(USExp$logExp, type="mu", lags="long")
summary(ktest)


## ----------------------------------------------------------------------
ktest <- ur.kpss(USExp_diff$diff, type="mu", lags="long")
summary(ktest)


## ----echo=FALSE, , out.width="70%", fig.align='center'-----------------
nelplo1909p <- nelplo_tsbl %>% filter(index>1909)
nelplo1909p %>% filter(key=="gnp.capita") %>%
  gg_lag(y=value, lags=1, geom="point", colour="blue")


## ----echo=FALSE--------------------------------------------------------
nelplo1909p %>% filter(key=="gnp.capita") %>% ACF(value)


## ----------------------------------------------------------------------
rdata <- nelplo1909p %>% filter(key=="gnp.capita") 
summary(lm(value ~ lag(value), data=rdata))


## ----echo=TRUE, out.width="70%", fig.align="center"--------------------
nelplo1909p %>% filter(key=="unemp") %>% ACF(value) %>% autoplot()


## ----out.width="70%", fig.align="center"-------------------------------
nelplo1909p %>% filter(key=="unemp") %>%
  PACF(value) %>% autoplot()


## ----------------------------------------------------------------------
regdata <- nelplo1909p %>% filter(key=="unemp")
arma.mod1 <- arima(regdata[,"value"], c(2,0,2))
summary(arma.mod1)


## ----------------------------------------------------------------------
regdata <- nelplo1909p %>% filter(key=="unemp")
arma.mod2 <- arima(regdata[,"value"], c(1,0,1)) 
summary(arma.mod2)


## ----------------------------------------------------------------------
AIC(arma.mod1, arma.mod2)


## ----------------------------------------------------------------------

nelplo.arma <- auto.arima(regdata)
nelplo.arma


## ----------------------------------------------------------------------
nelplo.arma.diag <- data.frame(regdata, prediction=nelplo.arma$fitted, residus=nelplo.arma$residuals)
nelplo.arma.diag


## ----out.width="70%", fig.align='center'-------------------------------
nelplo.arma.diag %>% 
  pivot_longer(cols=3:5) %>% 
  filter(name %in% c("prediction", "value")) %>%
  ggplot() + geom_line(aes(x=index, y=value, color=name))


## ----------------------------------------------------------------------
nelplo.arma.diag %>% ggplot() + 
  geom_point(aes(x=index, y=residus))


## ----------------------------------------------------------------------
lags <- 1:3
pval <- NULL
for (l in lags) {
  pval <-c(pval, box_pierce(nelplo.arma.diag$residus, lag=l)["bp_pvalue"])
}
res <- data.frame(lags, pval)
res


## ----------------------------------------------------------------------
forecast(nelplo.arma, h=10)


## ----out.width="70%", fig.align="center"-------------------------------
plot(forecast(nelplo.arma, h=10))


## ----------------------------------------------------------------------
USExp_diff


## ----out.width="70%", fig.align="center"-------------------------------
USExp_diff %>% as_tsibble(index=time) %>% ACF(diff) %>% autoplot()


## ----out.width="70%", fig.align="center"-------------------------------
USExp_diff %>% as_tsibble(index=time) %>% PACF(diff) %>% autoplot()


## ----------------------------------------------------------------------
regdata <- USExp[,"logExp"]
arimod <- auto.arima(regdata)
arimod


## ----------------------------------------------------------------------
forecast(arimod, h=24)


## ----out.width="70%", fig.align="center"-------------------------------
plot(forecast(arimod, h=24))


## ----message=FALSE-----------------------------------------------------
library("vars")
data("Canada")
window(Canada, start=c(1980,1), end=c(1981,4))


## ----out.witdh="70%", fig.align='center'-------------------------------
plot(Canada, nc = 2, xlab = "")


## ----------------------------------------------------------------------
summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))


## ----------------------------------------------------------------------
summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags = 1))


## ----------------------------------------------------------------------
VARselect(Canada, lag.max = 8, type = "both")


## ----------------------------------------------------------------------
p1ct <- VAR(Canada, p = 1, type = "both")


## ----------------------------------------------------------------------
summary(p1ct, equation = "e")


## ----out.width="70%", fig.align="center"-------------------------------
plot(p1ct, names = "e")


## ----------------------------------------------------------------------
USIncExp2 <- window(USIncExp, start = c(1985,12))


## ----------------------------------------------------------------------
coint.res <- residuals(lm(expenditure ~ income, data = USIncExp2))


## ----------------------------------------------------------------------
coint.res <- stats::lag(ts(coint.res, start = c(1985,12), freq = 12), k = -1)
USIncExp2 <- cbind(USIncExp2, diff(USIncExp2), coint.res)
USIncExp2 <- window(USIncExp2, start = c(1986,1), end = c(2001,2))
colnames(USIncExp2) <- c("income", "expenditure", "diff.income", "diff.expenditure", "coint.res")
window(USIncExp2, start=c(1986,1), end=c(1986,12))


## ----------------------------------------------------------------------
ecm.model <- diff.expenditure ~ coint.res + diff.income
summary(lm(ecm.model, data=USIncExp2))

