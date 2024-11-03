## ----echo=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(size="scriptsize")

setwd('/home/alex/Devel/Cours-R-TS')


## ----tidyverse_load, echo=TRUE------------------------------------------------------------------------------
library(tidyverse)


## ----ggplot_load, echo=TRUE---------------------------------------------------------------------------------
library(ggplot2)
theme_set(theme_minimal())


## ----nelplo_read, echo=TRUE---------------------------------------------------------------------------------
Nelson_Plosser <- read.csv2("/home/alex/Devel/Cours-R-TS/data/Nelson_Plosser.csv", 
                            header=TRUE, sep=",", dec = ".")
head(Nelson_Plosser)


## ----echo=TRUE, message=FALSE-------------------------------------------------------------------------------
library(summarytools)
descr(Nelson_Plosser %>% select(1:6))


## ----nelplo_corr, echo=TRUE, tidy=TRUE----------------------------------------------------------------------
datanum <- Nelson_Plosser %>% filter(year>1909) %>% select(1:7) 
cormat <- round(cor(datanum),2)
cormat


## ----nelplo_corr_heatmap, echo=TRUE, message=FALSE, out.width="60%", out.height='60%', fig.align='center'----
library(reshape2)
cormat %>% melt() %>% ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_distiller(palette = "Spectral", direction = 1)


## ----GGally, echo=FALSE, message=FALSE----------------------------------------------------------------------
library(GGally)


## ----message=FALSE, eval=FALSE------------------------------------------------------------------------------
## Nelson_Plosser %>%
##   filter(year>=1909) %>%
##   select(cpi, gnp.nom, emp, unemp) %>%
##   ggpairs(upper = list(continuous = wrap("cor", size = 4)),
##           lower = list(continuous = wrap("points", colour="blue", alpha=0.3, size=0.5)))


## ----nelplo_pairplot_graph, echo=FALSE, out.width="60%", fig.align='center'---------------------------------
Nelson_Plosser %>% 
  filter(year>=1909) %>%
  select(cpi, gnp.nom, emp, unemp) %>% 
  ggpairs(upper = list(continuous = wrap("cor", size = 4)),
          lower = list(continuous = wrap("points", colour="blue", alpha=0.3, size=0.5)))


## ----retail_read, echo=TRUE---------------------------------------------------------------------------------
retail <- read.csv2("/home/alex/Devel/Cours-R-TS/data/RSXFSN.csv", header=TRUE, sep=",", dec = ".")
head(retail)


## -----------------------------------------------------------------------------------------------------------
summary(retail)


## -----------------------------------------------------------------------------------------------------------
retail$DATE <- as.Date(retail$DATE,format="%Y-%m-%d")
summary(retail)


## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------------------------------------
## library(insee)
## df_idbank_list_selected =
##   get_idbank_list("CHOMAGE-TRIM-NATIONAL") %>% #Unemployment dataset
##   add_insee_title() %>%
##   filter(INDICATEUR == "CTTXC") %>% #unemployment rate based on ILO standards
##   filter(REF_AREA == "FM") %>%  # all France excluding overseas departements
##   filter(SEXE == 0) # men and women
## 
## list_idbank = df_idbank_list_selected %>% pull(idbank)
## 
## fr_unemp = get_insee_idbank(list_idbank, startPeriod = "1950-01") %>% split_title()


## ----echo=TRUE----------------------------------------------------------------------------------------------
load("/home/alex/Devel/Cours-R-TS/data/fr_unemp.RData")
head(fr_unemp)


## -----------------------------------------------------------------------------------------------------------
fr_unemp %>% distinct(TITLE_FR)


## -----------------------------------------------------------------------------------------------------------
fr_unemp %>% summarise(début=min(DATE), fin=max(DATE))


## ----out.width='70%', out.height='50%', fig.align='center'--------------------------------------------------
ggplot(fr_unemp, aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN2)) +
  geom_line() +
  geom_point() +
  ggtitle("French unemployment rate, by age") +
  labs(subtitle = sprintf("Last updated : %s", fr_unemp$TIME_PERIOD[1]))


## ----message=FALSE, eval=FALSE------------------------------------------------------------------------------
## df_idbank_list_selected =
##   get_idbank_list("CNT-2014-PIB-EQB-RF") %>% # Gross domestic product balance
##   filter(FREQ == "T") %>% #quarter
##   add_insee_title() %>% #add titles
##   filter(OPERATION == "PIB") %>% #GDP
##   filter(NATURE == "VALEUR_ABSOLUE") %>% #rate
##   filter(CORRECTION == "CVS-CJO") #SA-WDA, seasonally adjusted, working day adjusted
## 
## idbank = df_idbank_list_selected %>% pull(idbank)
## 
## fr_pib = get_insee_idbank(idbank)


## ----echo=TRUE----------------------------------------------------------------------------------------------
load("/home/alex/Devel/Cours-R-TS/data/fr_pib.RData")
head(fr_pib)


## -----------------------------------------------------------------------------------------------------------
min(fr_pib$DATE)


## -----------------------------------------------------------------------------------------------------------
fr_pib %>% distinct(TITLE_EN)


## ----out.width='60%', out.height='50%', fig.align='center'--------------------------------------------------
ggplot(fr_pib, aes(x = DATE, y = OBS_VALUE)) +
  geom_col(color="blue") +
  ggtitle("French GDP, quarter-on-quarter, sa-wda") +
  labs(subtitle = sprintf("Last updated : %s", fr_pib$TIME_PERIOD[1]))


## ----message=FALSE, eval=FALSE------------------------------------------------------------------------------
## df_idbank_list_selected =
##   get_idbank_list("IPC-2015") %>% # Gross domestic product balance
##   filter(FREQ == "M" & idbank=="001759970") %>%
##   add_insee_title()
## 
## idbank = df_idbank_list_selected %>% pull(idbank)
## 
## fr_cpi = get_insee_idbank(idbank)


## ----echo=TRUE----------------------------------------------------------------------------------------------
load("/home/alex/Devel/Cours-R-TS/data/fr_cpi.RData")
head(fr_cpi)


## -----------------------------------------------------------------------------------------------------------
fr_cpi %>% distinct(TITLE_FR)


## -----------------------------------------------------------------------------------------------------------
fr_cpi %>% summarise(début=min(fr_cpi$DATE), fin=max(fr_cpi$DATE))


## ----out.width='60%', out.height='50%', fig.align='center'--------------------------------------------------
ggplot(fr_cpi, aes(x = DATE, y = OBS_VALUE)) +
  geom_line(color="blue") +
  ggtitle("Indice des prix à la consommation - Base 2015") +
  labs(subtitle = sprintf("Last updated : %s", fr_cpi$TIME_PERIOD[1])) +
  ylab("IPC (base 2015)")


## -----------------------------------------------------------------------------------------------------------
pib <- read.csv("/home/alex/Devel/Cours-R-TS/data/pib_fr.csv", sep=";", dec=',')
head(pib)


## -----------------------------------------------------------------------------------------------------------
load("/home/alex/Devel/Cours-R-TS/data/fr_cpi.RData")
fr_cpi <- fr_cpi %>% mutate(Année=year(DATE), Trimestre=quarter(DATE)) %>%
  group_by(Année, Trimestre) %>% 
  rename(cpi=OBS_VALUE) %>%
  summarise(cpi=mean(cpi))
head(fr_cpi)


## -----------------------------------------------------------------------------------------------------------
load("/home/alex/Devel/Cours-R-TS/data/fr_pib.RData")
fr_pib <- fr_pib %>% mutate(Année=year(DATE), Trimestre=quarter(DATE)) %>% 
  rename(gnp=OBS_VALUE) %>%
  select(Année, Trimestre, gnp)

load("/home/alex/Devel/Cours-R-TS/data/fr_unemp.RData")
fr_unemp <- fr_unemp %>% mutate(Année=year(DATE), Trimestre=quarter(DATE)) %>%
  rename(unemp=OBS_VALUE) %>%
  filter(TITLE_FR2=='Ensemble') %>%
  select(Année, Trimestre, unemp)

fr_macro <- fr_pib %>% 
  left_join(fr_unemp, by=c("Année", "Trimestre")) %>%
  left_join(fr_cpi, by=c("Année", "Trimestre"))

head(fr_macro)


## -----------------------------------------------------------------------------------------------------------
library(tseries)
data(NelPlo)
class(NelPlo)


## -----------------------------------------------------------------------------------------------------------
window(NelPlo, start=1905, end=1910)


## ----gmoney_load, echo=TRUE, message=FALSE------------------------------------------------------------------
library(lmtest)
data(growthofmoney)
window(growthofmoney, end=c(1971,4))


## ----message=FALSE, tidy=TRUE, tidy.opts = list(blank = FALSE, width.cutoff = 60)---------------------------
library(strucchange)
data("USIncExp")
window(USIncExp, start=c(1959,6), end=c(1960,6))


## ----retail_ts, echo=TRUE-----------------------------------------------------------------------------------
retail_ts <- ts(retail['RSXFSN'], frequency=12, start=c(1997,3))
window(retail_ts, start=2020)


## -----------------------------------------------------------------------------------------------------------
tsp(retail_ts)


## ----pib_ts_time, echo=TRUE---------------------------------------------------------------------------------
retail2022 <- window(retail_ts, start=c(2022,1), end=c(2022,12))
time(retail2022)


## ----pib_ts_time2, echo=TRUE--------------------------------------------------------------------------------
library(lubridate)
as.numeric(time(retail2022))


## ----gmoney_tsibble, echo=TRUE, message=FALSE---------------------------------------------------------------
library(tsibble)
gmoney_tsbl <- growthofmoney %>% as_tsibble()
gmoney_tsbl


## ----nelplo_tsibble, echo=TRUE, message=FALSE---------------------------------------------------------------
nelplo_tsbl <- NelPlo %>% as_tsibble()
nelplo_tsbl %>% filter(index>1980 & key=="gnp.capita")


## -----------------------------------------------------------------------------------------------------------
nelplo_tsbl %>% distinct(key)


## ----retail_tsibble, echo=TRUE, message=FALSE---------------------------------------------------------------
retail_tsbl <- retail_ts %>% as_tsibble()
head(retail_tsbl)


## ----warning=FALSE------------------------------------------------------------------------------------------
acf(NelPlo[,'cpi'], plot=FALSE)


## ----warning=FALSE------------------------------------------------------------------------------------------
library(feasts)
nelplo_tsbl %>% 
  filter(key=="cpi") %>% 
  ACF(value)


## -----------------------------------------------------------------------------------------------------------
bbg <- ts(rnorm(100))
window(bbg, start=1, end=10)


## ----eval=FALSE---------------------------------------------------------------------------------------------
## library(ggfortify)
## autoplot(bbg) +
##   ggtitle("White Noise")


## ----out.width="60%", out.height='60%', fig.align="center", message=FALSE, echo=FALSE-----------------------
library(ggfortify)
autoplot(bbg, color='blue') + 
  ggtitle("White Noise")


## -----------------------------------------------------------------------------------------------------------
acf(bbg, plot=FALSE)


## -----------------------------------------------------------------------------------------------------------
box_pierce(rnorm(100))


## ----echo=TRUE----------------------------------------------------------------------------------------------
tmax <- 100
wnoise <- rnorm(99, mean=0, sd=1)
y <- rep(0,tmax)

for (t in 2:tmax) {
  y[t] = y[t-1] + wnoise[t-1] 
}
rw <- tibble(time=1:tmax, y=y)
head(rw)


## -----------------------------------------------------------------------------------------------------------
acf(rw[,"y"], plot=FALSE)


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------------------
ggplot(rw) + 
  geom_line(aes(x=time, y=y), colour="blue")


## -----------------------------------------------------------------------------------------------------------
tmax <- 100
alpha <- 0.8

y <- rep(0,tmax)

for (t in 2:tmax) {
  y[t] = alpha + y[t-1] + wnoise[t-1] 
}
rwd <- tibble(time=1:tmax, y=y)
head(rwd)


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------------------
ggplot(rwd) + 
  geom_line(aes(x=time, y=y), colour="blue")


## ----echo=TRUE, out.height="60%", out.width='60%', fig.align="center"---------------------------------------
plot(NelPlo[,c("cpi", "unemp")], xlab="Trimestre", main="Données Nelson-Plosser")


## ----out.width='60%', out.height='60%', fig.align="center", message=FALSE, warning=FALSE--------------------
USIncExp %>% as_tsibble() %>% autoplot()


## ----out.width='60%', out.height='60%', fig.align="center", warning=FALSE-----------------------------------
ggplot(retail_tsbl, aes(x = index, y = value)) +
    geom_line(color = "#00AFBB", size = 0.5) +
    geom_point(color = "#00AFBB", size = 1) + ylab("Retail sales")


## ----warning=FALSE, message=FALSE, out.width='60%', out.height='60%', fig.align="center"--------------------
ggplot(gmoney_tsbl, aes(x = index, y = value)) + 
  geom_line(aes(color = key), size = 0.5) +
  geom_point(aes(color = key), size = 1) +
  scale_color_manual(values = c("red", "blue"))


## ----warning=FALSE, message=FALSE, out.width='60%', out.height='60%', fig.align="center"--------------------
nelplo_tsbl %>% filter(key %in% c("gnp.nom", "gnp.real", "unemp", "cpi", "money.stock")) %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(aes(color = key), size = 1)


## ----warning=FALSE, message=FALSE, out.height="60%", out.width='60%', fig.align='center'--------------------
nelplo_tsbl %>% filter(key=="cpi") %>% 
  ACF(value) %>% autoplot()


## ----warnings=FALSE, message=FALSE, , out.height="60%", out.width='60%', fig.align='center'-----------------
gmoney_tsbl %>% ACF() %>% autoplot()


## ----warnings=F, message=FALSE, out.height="60%", out.width='60%', fig.align='center'-----------------------
library(patchwork)
g1 <- retail_tsbl %>% autoplot(color='blue')
g2 <- ACF(retail_tsbl) %>% autoplot()
g1+g2


## ----out.height="60%", out.width='60%', fig.align='center', message=FALSE-----------------------------------
library(forecast)
ggseasonplot(growthofmoney[,"TG1.TG0"])


## ----out.width='80%', out.width='50%', fig.align='center'---------------------------------------------------
ggseasonplot(window(retail_ts, start=c(2000,1)))


## ----out.height="80%", out.width='80%', fig.align='center'--------------------------------------------------
retail_tsbl %>% gg_lag(y=value, geom="point", size=0.5, lags=c(1,3,6,9,12))


## ----out.height='80%', out.width='80%', fig.align='center'--------------------------------------------------
nelplo_tsbl %>% filter(key=='cpi') %>% 
  gg_lag(y=value, geom="point", size=0.5, colour="blue", lags=c(1,2,5,10,15)) 


## -----------------------------------------------------------------------------------------------------------
nelplo1909 <- nelplo_tsbl %>% 
  filter(index>=1909) %>% 
  spread(key = key, value=value)
head(nelplo1909)


## -----------------------------------------------------------------------------------------------------------
nelplo1909 %>% select(index, gnp.nom, emp)


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
nelplo1909 %>%
  ggplot(aes(index, gnp.nom)) +
    geom_point(colour="blue")


## -----------------------------------------------------------------------------------------------------------
mod1 <- lm(gnp.nom ~ index, data=nelplo1909)
summary(mod1)


## -----------------------------------------------------------------------------------------------------------
summary(mod1)$coefficients


## -----------------------------------------------------------------------------------------------------------
summary(mod1)$adj.r.squared


## ----out.height="60%", out.width="60%", fig.align='center', message=FALSE-----------------------------------
ggplot(nelplo1909, aes(x=index, y=gnp.nom)) +
  geom_point(colour="blue") +
  geom_smooth(method='lm', color="red")


## -----------------------------------------------------------------------------------------------------------
mod1_diag <- tibble(gnp=nelplo1909$gnp.nom, predicted=mod1$fitted.values, 
                    residual=mod1$residuals)
mod1_diag


## ----out.height="60%", out.width="60%", fig.align='center', echo=TRUE---------------------------------------
ggplot(mod1_diag) + 
  geom_point(aes(x=gnp, y=predicted), colour="blue") + 
  geom_abline(colour="grey")


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
ggplot(mod1_diag, aes(x=residual)) + 
  geom_histogram(aes(y = after_stat(density)), fill="lightblue", binwidth = 0.05) +
  geom_density(aes(x=residual))


## -----------------------------------------------------------------------------------------------------------
shapiro.test(mod1_diag$residual)


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
qqnorm(mod1_diag$residual)


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
ggplot(mod1_diag) + 
  geom_point(aes(x=predicted, y=residual), colour="blue")


## ----bptest-------------------------------------------------------------------------------------------------
library(lmtest)
bptest(mod1)


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
gdata <- tibble(index=1:nrow(mod1_diag), residus=mod1_diag$residual)
gdata %>% ggplot() + geom_point(aes(x=index, y=residus), color="red")


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
acf(mod1_diag$residual)


## -----------------------------------------------------------------------------------------------------------
dwtest(mod1)


## ----lm_gom_data--------------------------------------------------------------------------------------------
data("growthofmoney")
head(growthofmoney)


## ----lm_gom_plot, out.width='60%', out.height='60%', fig.align='center', message=FALSE----------------------
gdata <- tibble(AG0_TG0=growthofmoney[,"AG0.TG0"], TG1_TG0=growthofmoney[,"TG1.TG0"])
ggplot(gdata) + geom_point(aes(x=AG0_TG0, y=TG1_TG0), color="blue")


## -----------------------------------------------------------------------------------------------------------
modelHetzel <- TG1.TG0 ~ AG0.TG0
gom.mod1 <- lm(modelHetzel, data=growthofmoney)
summary(gom.mod1)


## ----out.width='60%', out.height='60%', fig.align='center', message=FALSE-----------------------------------
gdata <- tibble(index=1:nrow(growthofmoney), residus=gom.mod1$residual)
gdata %>% ggplot() + geom_point(aes(x=index, y=residus), color="red")


## ----out.width="60%", out.height='60%', fig.align='center'--------------------------------------------------
acf(gdata$residus)


## -----------------------------------------------------------------------------------------------------------
dwtest(modelHetzel, data=growthofmoney)


## -----------------------------------------------------------------------------------------------------------
dcook <- gdata <- data.frame(obs=1:nrow(growthofmoney), dcook=cooks.distance(gom.mod1))
head(dcook)


## -----------------------------------------------------------------------------------------------------------
tcook <- 4/nrow(growthofmoney)
tcook


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
ggplot(dcook, aes(x=obs, y=dcook)) + 
  geom_point(color="blue") +
  geom_hline(aes(yintercept=tcook), color="red")


## -----------------------------------------------------------------------------------------------------------
stres <- data.frame(obs=1:nrow(growthofmoney), stres=rstudent(gom.mod1))
head(stres)


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
ggplot(stres, aes(x=obs, y=stres)) + 
  geom_point(color="blue") +
  geom_hline(aes(yintercept=3), color="red") +
  geom_hline(aes(yintercept=-3), color="red")


## -----------------------------------------------------------------------------------------------------------
nelplo1909 <- nelplo_tsbl %>% 
  filter(index>=1909) %>% 
  spread(key = key, value=value)
regdata <- nelplo1909 %>% 
  select(index, gnp.capita, unemp, cpi)


## -----------------------------------------------------------------------------------------------------------
mod2 <- lm(gnp.capita ~ index+unemp+cpi, data=regdata)
summary(mod2)


## -----------------------------------------------------------------------------------------------------------
mod3 <- lm(gnp.capita ~ index+unemp, data=regdata)
summary(mod3)


## -----------------------------------------------------------------------------------------------------------
AIC(mod1, mod2, mod3)


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
plot(growthofmoney)


## -----------------------------------------------------------------------------------------------------------
modelHetzel


## -----------------------------------------------------------------------------------------------------------
sctest(modelHetzel, point=c(1973,4), data=growthofmoney, type="Chow")


## ----out.height="60%", out.width="60%", fig.align='center'--------------------------------------------------
rdata <- Nelson_Plosser[,c("year", "gnp.real")]
rdata <- rdata[rdata$year>=1909,]
rdata <- ts(rdata, start=1909)
plot(rdata[,"gnp.real"])


## -----------------------------------------------------------------------------------------------------------
library(strucchange)
model <- gnp.real ~ year
sctest(model, point=c(1933,1), data=rdata, type="Chow")


## ----out.width="60%", out.height='50%', fig.align='center'--------------------------------------------------
fs <- Fstats(model, from = c(1920,1), to = c(1980,1), data = rdata)
plot(fs)


## -----------------------------------------------------------------------------------------------------------
mod3 <- lm(gnp.capita ~ index+unemp, data=regdata)


## -----------------------------------------------------------------------------------------------------------
dcmp <- retail_tsbl %>%
  model(classical_decomposition(value))
components(dcmp)


## ----out.width='60%', out.height='70%', fig.align='center', warning=FALSE-----------------------------------
components(dcmp) %>% autoplot()


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
components(dcmp) %>% ggplot() + 
  geom_line(aes(x=index,y=value), colour="blue") +
  geom_line(aes(x=index,y=season_adjust), colour="red")


## ----out.width="60%", out.height="60%", fig.align='center', message=FALSE-----------------------------------
USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  ggplot(aes(x = index, y = value)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method='lm', color="red")


## -----------------------------------------------------------------------------------------------------------
regdata <- USIncExp %>% as_tsibble() %>% filter(key=='expenditure')
summary(lm(value ~ index, data=regdata))


## ----out.width="60%", out.height='60%', fig.align='center', message=FALSE-----------------------------------
USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  ggplot(aes(x = index, y = log(value))) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method='lm', color="red")


## -----------------------------------------------------------------------------------------------------------
regdata <- USIncExp %>% as_tsibble() %>% 
  filter(key=='expenditure') %>%
  mutate(value=log(value))
summary(lm(value ~ index, data=regdata))


## -----------------------------------------------------------------------------------------------------------
retail$mois <- factor(month(retail$DATE), labels=month(1:12, label=TRUE))
retail$t <- c(1:nrow(retail))
head(retail)


## -----------------------------------------------------------------------------------------------------------
modst <- lm(RSXFSN ~ t+ mois-1, data=retail)


## ----size='tiny'--------------------------------------------------------------------------------------------
summary(modst)


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
retail$prediction <- predict.lm(modst)
ggplot(retail)+
  geom_line(mapping=aes(x=t,y=RSXFSN),color="blue")+
  geom_line(mapping=aes(x=t,y=prediction), color="red")


## -----------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------
coefst <- modst$coefficients
coefst


## -----------------------------------------------------------------------------------------------------------
a <- mean(coefst[2:13])
b <- coefst[1]
c <- coefst[2:13]-mean(coefst[2:13])
y_cvs <- retail_1992_2022$RSXFSN-(c[1]*s1+c[2]*s2+c[3]*s3+c[4]*s4+c[5]*s5+c[6]*s6+
                                    c[7]*s7+c[8]*s8+c[9]*s9+c[10]*s10+c[11]*s11+c[12]*s12)


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
gdata <- tibble(t=retail_1992_2022$t, 
                original=retail_1992_2022$RSXFSN, cvs = y_cvs)

ggplot(gdata)+
  geom_line(mapping=aes(x=t,y=original),color="blue")+
  geom_line(mapping=aes(x=t,y=cvs), color="red")


## -----------------------------------------------------------------------------------------------------------
library(forecast)
bhat = tslm(retail_ts~trend+I(trend^2)+season)


## ----size='tiny'--------------------------------------------------------------------------------------------
summary(bhat)


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
set.seed(12345)
y.tsar2 <- 5 + 0.5 * seq(250) +
  arima.sim(list(ar = c(0.8, -0.2)), n = 250)


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
plot(y.tsar2, ylab="y", xlab = "Time")
abline(a=5, b=0.5, col = "red")


## -----------------------------------------------------------------------------------------------------------
set.seed(12345)
u.ar2 <- arima.sim(list(ar = c(0.8, -0.2)), n = 250)
y1 <- cumsum(u.ar2)
TD <- 5.0 + 0.7 * seq(250)
y1.d <- y1 + TD


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
layout(matrix(1:2, nrow = 1, ncol = 2))
plot.ts(y1, main = "I(1) process without drift", ylab="y", xlab = "Time")
plot.ts(y1.d, main = "I(1) process with drift", ylab="y", xlab = "Time")
abline(a=5, b=0.7, col = "red")


## -----------------------------------------------------------------------------------------------------------
cpi_diff <- diff(NelPlo[,"cpi"],1)
window(cpi_diff, start=1861, end=1900)


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------------------
cpi_diff %>% as_tsibble() %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(colour="red", size = 1) + ylab("diff(cpi)")


## ----out.height="60%", out.width='60%', fig.align="center", echo=TRUE---------------------------------------
USExp <- USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  mutate(logExp=log(value))
ggplot(USExp, aes(x = index, y = logExp)) +
  geom_line(color = "blue", size = 1)


## ----size='tiny'--------------------------------------------------------------------------------------------
library(urca)
exp.df <- ur.df(y=as.data.frame(USExp)[,"logExp"], lags=12, type='trend')
attr(exp.df, "testreg")


## ----size='tiny'--------------------------------------------------------------------------------------------
library(urca)
exp.df <- ur.df(y=as.data.frame(USExp)[,"logExp"], lags=8, type='trend')


## -----------------------------------------------------------------------------------------------------------
attr(exp.df, "teststat")


## -----------------------------------------------------------------------------------------------------------
attr(exp.df, "cval")


## ----size='tiny'--------------------------------------------------------------------------------------------
attr(exp.df, "testreg")


## ----out.height="60%", out.width='60%', fig.align="center"--------------------------------------------------
USExp.diff <- tibble(time=1:(nrow(USExp)-1), diff= diff(as.data.frame(USExp)[, "logExp"]))
ggplot(USExp.diff, aes(x = time, y = diff)) +
  geom_line(color = "blue", size = 0.5)


## -----------------------------------------------------------------------------------------------------------
USExp.diff.df <- ur.df(y=as.data.frame(USExp.diff)[, "diff"], lags=1, type='drift')


## -----------------------------------------------------------------------------------------------------------
attr(USExp.diff.df, "teststat")


## -----------------------------------------------------------------------------------------------------------
attr(USExp.diff.df, "cval")


## ----out.width='50%', out.heght='50%', fig.aling="center"---------------------------------------------------
nelplo_tsbl %>% filter(key=="gnp.real" & index>=1909) %>%
  ggplot(aes(x = index, y = value)) +
    geom_line(color='blue') + ylab("log(PIB)")


## -----------------------------------------------------------------------------------------------------------
gnpreal.df <- ur.df(y=as.data.frame(nelplo1909)[,"gnp.real"], lags=1, type='trend')


## -----------------------------------------------------------------------------------------------------------
attr(gnpreal.df, "teststat")


## -----------------------------------------------------------------------------------------------------------
attr(gnpreal.df , "cval")


## ----out.height="60%", out.width='60%', fig.align="center"--------------------------------------------------
gnpreal.diff <- tibble(time=1:(nrow(nelplo1909)-1), diff= diff(as.data.frame(nelplo1909)$gnp.real))
ggplot(gnpreal.diff, aes(x = time, y = diff)) +
  geom_line(color = "blue", size = 0.5) + ylab("diff(logGNP)")


## -----------------------------------------------------------------------------------------------------------
gnpreal.diff.df <- ur.df(y=as.data.frame(gnpreal.diff)[, "diff"], lags=1, type='drift')


## -----------------------------------------------------------------------------------------------------------
attr(gnpreal.diff.df, "teststat")


## -----------------------------------------------------------------------------------------------------------
attr(gnpreal.diff.df , "cval")


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
Unemp <- nelplo_tsbl %>% filter(key=='unemp' & index >= 1909) 
ggplot(Unemp, aes(x = index, y = value)) +
  geom_line(color = "blue", size = 1)


## ----size='tiny'--------------------------------------------------------------------------------------------
unemp.df <- ur.df(y=as.data.frame(Unemp)[,"value"], lags=1, type='drift')


## -----------------------------------------------------------------------------------------------------------
attr(unemp.df, "teststat")


## -----------------------------------------------------------------------------------------------------------
attr(unemp.df, "cval")


## -----------------------------------------------------------------------------------------------------------
adf.test(as.data.frame(Unemp)[,"value"], k=1)


## -----------------------------------------------------------------------------------------------------------
ktest <- ur.kpss(USExp$logExp, type="tau", lags="long")
summary(ktest)


## -----------------------------------------------------------------------------------------------------------
ktest <- ur.kpss(USExp$logExp, type="mu", lags="long")
summary(ktest)


## -----------------------------------------------------------------------------------------------------------
ktest <- ur.kpss(USExp.diff$diff, type="mu", lags="long")
summary(ktest)


## -----------------------------------------------------------------------------------------------------------
set.seed(123456)
wn <- rnorm(100)
AR1sim <- arima.sim(n = 100, list(ar = 0.9), innov=wn)


## -----------------------------------------------------------------------------------------------------------
pacf(AR1sim, plot=FALSE)


## ----out.width="60%", out.height='60%', fig.align='center', eval=FALSE--------------------------------------
## op <- par(no.readonly=TRUE)
## 
## layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
## 
## plot.ts(AR1sim, ylab="", main="Processus AR(1) avec B1=0.9")
## 
## acf(AR1sim, main="Autocorrelations", ylab="",
##     ylim=c(-1, 1), ci.col = "black")
## 
## pacf(AR1sim, main="Partial Autocorrelations", ylab="",
##      ylim=c(-1, 1), ci.col = "black")
## 
## par(op)


## ----out.width="70%", out.height='70%', fig.align='center', echo=FALSE--------------------------------------
op <- par(no.readonly=TRUE)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(AR1sim, ylab='')
acf(AR1sim, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(AR1sim, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

par(op)


## -----------------------------------------------------------------------------------------------------------
rusim <- 0
for (i in 1:length(wn)) {rusim[i+1] <- rusim[i] + wn[i]}

## ----out.width='60%', out.height='60%', fig.align='center', echo=FALSE--------------------------------------
op <- par(no.readonly=TRUE)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(rusim, ylab='', main='Processus AR(1) avec B1=1.0')
acf(rusim, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(rusim, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

par(op)


## ----out.width='70%', out.height='70%', fig.align='center', echo=FALSE--------------------------------------
op <- par(no.readonly=TRUE)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(rusim, ylab='', main='Processus AR(1) avec B1=1.0')
acf(rusim, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(rusim, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

par(op)


## ----echo=FALSE, out.width="60%", fig.align='center'--------------------------------------------------------
y <- window(NelPlo[,'unemp'], start=1909)

op <- par(no.readonly=TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(y, ylab='')
acf(y, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(y, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")
par(op)


## ----echo=TRUE, out.width="60%", out.height='60%', fig.align='center'---------------------------------------
unemp1909p <- nelplo_tsbl %>% filter(index>1909 & key=='unemp')
unemp1909p %>% gg_lag(y=value, lags=1, geom="point", colour="blue")


## -----------------------------------------------------------------------------------------------------------
summary(lm(value ~ lag(value), data=unemp1909p))


## -----------------------------------------------------------------------------------------------------------
summary(arima(unemp1909p[,"value"], c(1,0,0)))


## -----------------------------------------------------------------------------------------------------------
MA1sim <- arima.sim(n = 100, list(ma = 0.9), innov=wn)


## ----out.width="60%", out.height='60%', fig.align='center', eval=FALSE--------------------------------------
## op <- par(no.readonly=TRUE)
## 
## layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
## 
## plot.ts(MA1sim, ylab='')
## acf(MA1sim, main="Autocorrelations", ylab="", main="Processus MA(1) - Simulation",
##     ylim=c(-1, 1), ci.col = "black")
## pacf(MA1sim, main="Partial Autocorrelations", ylab="",
##      ylim=c(-1, 1), ci.col = "black")
## 
## par(op)


## ----out.width="60%", out.height='60%', fig.align='center', echo=FALSE--------------------------------------
op <- par(no.readonly=TRUE)
 
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(MA1sim, ylab='', main="Processus MA(1) - Simulation")
acf(MA1sim, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(MA1sim, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

par(op)


## ----echo=TRUE, out.width="60%", out.height='60%', fig.align="center"---------------------------------------
ggplot(data=unemp1909p) + geom_line(aes(x=index, y=value), color="blue") + 
  ggtitle("Taux de chômage aux USA")


## ----echo=TRUE, out.width="60%", out.height='60%', fig.align="center"---------------------------------------
unemp1909p %>% ACF(value) %>% autoplot()


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------------------
unemp1909p %>% PACF(value) %>% autoplot()


## -----------------------------------------------------------------------------------------------------------
arma.mod1 <- arima(unemp1909p[,"value"], c(2,0,2))
summary(arma.mod1)


## -----------------------------------------------------------------------------------------------------------
# Extract standard errors
se <- sqrt(diag(vcov(arma.mod1)))

# Calculate the t-values
t_values <- coef(arma.mod1) / se

# Calculate the p-values
p_values <- 2 * (1 - pnorm(abs(t_values)))

# Create a data frame with coefficients, standard errors, t-values, and p-values
results <- data.frame(
  Coefficient = coef(arma.mod1),
  Std_Error = se,
  T_Value = t_values,
  P_Value = p_values
)
print(results)


## -----------------------------------------------------------------------------------------------------------
arma.mod2 <- arima(unemp1909p[,"value"], c(1,0,1)) 
summary(arma.mod2)


## -----------------------------------------------------------------------------------------------------------
AIC(arma.mod1, arma.mod2)


## -----------------------------------------------------------------------------------------------------------
nelplo.arma <- auto.arima(unemp1909p$value)
nelplo.arma


## -----------------------------------------------------------------------------------------------------------
# Extract standard errors
se <- sqrt(diag(vcov(nelplo.arma)))

# Calculate the t-values
t_values <- coef(nelplo.arma) / se

# Calculate the p-values
p_values <- 2 * (1 - pnorm(abs(t_values)))

# Create a data frame with coefficients, standard errors, t-values, and p-values
results <- data.frame(
  Coefficient = coef(nelplo.arma),
  Std_Error = se,
  T_Value = t_values,
  P_Value = p_values
)
print(results)


## -----------------------------------------------------------------------------------------------------------
nelplo.arma.diag <- data.frame(index=unemp1909p$index, obs=unemp1909p$value, 
                               prediction=as.data.frame(nelplo.arma$fitted)$x, 
                               residus=as.data.frame(nelplo.arma$residuals)$x)
head(nelplo.arma.diag)


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
nelplo.arma.diag %>% 
  pivot_longer(cols=2:4) %>% 
  filter(name %in% c("prediction", "obs")) %>%
  ggplot() + geom_line(aes(x=index, y=value, color=name))


## ----out.width='60%', out.height='60%', fig.align='center', warning='FALSE'---------------------------------
nelplo.arma.diag %>% ggplot() + 
  geom_point(aes(x=index, y=residus), color="blue")


## ----echo=TRUE, out.width="70%", out.height="70%", fig.align='center'---------------------------------------
nelplo.arma$residuals %>% as_tsibble() %>% 
  gg_lag(y=value, lags=1:3, geom="point", colour="blue")


## -----------------------------------------------------------------------------------------------------------
lags <- 1:3
pval <- NULL
for (l in lags) {
  pval <-c(pval, box_pierce(nelplo.arma.diag$residus, lag=l)["bp_pvalue"])
}
res <- data.frame(lags, pval)
res


## ----out.width='50%', out.height='50%', message=FALSE, fig.align='center'-----------------------------------
ggplot(nelplo.arma.diag) + geom_histogram(aes(x=residus), fill='green')

## -----------------------------------------------------------------------------------------------------------
jarque.bera.test(nelplo.arma.diag$residus)


## -----------------------------------------------------------------------------------------------------------
forecast(nelplo.arma, h=10)


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------------------
plot(forecast(nelplo.arma, h=10))


## ----out.width='50%', out.height='50%', fig.align='center'--------------------------------------------------
ggplot(gnpreal.diff) + geom_line(aes(x=time, y=diff), color="blue") + ylab("diff(logGNP)")


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------------------
gnpreal.diff %>% as_tsibble(index=time) %>% ACF(diff) %>% autoplot()


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------------------
gnpreal.diff %>% as_tsibble(index=time) %>% PACF(diff) %>% autoplot()


## -----------------------------------------------------------------------------------------------------------
regdata <- nelplo1909$gnp.real
gnpreal.arima <- auto.arima(regdata)
gnpreal.arima


## -----------------------------------------------------------------------------------------------------------
# Extract standard errors
se <- sqrt(diag(vcov(gnpreal.arima)))

# Calculate the t-values
t_values <- coef(gnpreal.arima) / se

# Calculate the p-values
p_values <- 2 * (1 - pnorm(abs(t_values)))

# Create a data frame with coefficients, standard errors, t-values, and p-values
results <- data.frame(
  Coefficient = coef(gnpreal.arima),
  Std_Error = se,
  T_Value = t_values,
  P_Value = p_values
)
print(results)


## -----------------------------------------------------------------------------------------------------------
gnpreal.arima.diag <- data.frame(index=nelplo1909$index, 
                               prediction=as.data.frame(gnpreal.arima$fitted)$x, 
                               residus=as.data.frame(gnpreal.arima$residuals)$x)
head(gnpreal.arima.diag)


## ----out.width='60%', out.height='60%', fig.align='center', warning='FALSE'---------------------------------
gnpreal.arima.diag %>% ggplot(aes(x=index, y=residus)) + 
  geom_point(color="blue") +
  geom_line(color="blue")


## ----echo=TRUE, out.width="70%", out.height="70%", fig.align='center'---------------------------------------
gnpreal.arima$residuals %>% as_tsibble() %>% 
  gg_lag(y=value, lags=1:3, geom="point", colour="blue")


## -----------------------------------------------------------------------------------------------------------
lags <- 1:3
pval <- NULL
for (l in lags) {
  pval <-c(pval, box_pierce(gnpreal.arima.diag$residus, lag=l)["bp_pvalue"])
}
res <- data.frame(lags, pval)
res


## ----out.width='50%', out.height='50%', message=FALSE-------------------------------------------------------
ggplot(gnpreal.arima.diag) + geom_histogram(aes(x=residus), fill='green')

## -----------------------------------------------------------------------------------------------------------
jarque.bera.test(gnpreal.arima.diag$residus)


## -----------------------------------------------------------------------------------------------------------
forecast(gnpreal.arima, h=10)


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------------------
plot(forecast(arimod, h=10))


## ----out.width='50%', out.height='50%'----------------------------------------------------------------------
ggplot(USExp.diff) + geom_line(aes(x=time, y=diff), color="blue") + ylab("diff(USexp)")


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------------------
USExp.diff %>% as_tsibble(index=time) %>% ACF(diff) %>% autoplot()


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------------------
USExp.diff %>% as_tsibble(index=time) %>% PACF(diff) %>% autoplot()


## -----------------------------------------------------------------------------------------------------------
regdata <- USExp[,"logExp"]
arimod <- auto.arima(regdata)
arimod


## -----------------------------------------------------------------------------------------------------------
forecast(arimod, h=24)


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------------------
plot(forecast(arimod, h=24))


## -----------------------------------------------------------------------------------------------------------
gdpunemp <- read.csv('/home/alex/Devel/Cours-R-TS/data/q-gdpunemp.txt', sep=' ')
head(gdpunemp)


## -----------------------------------------------------------------------------------------------------------
gdpunemp$loggdp <- log(gdpunemp$gdp)
gdpunemp <- ts(gdpunemp[, c("loggdp", "rate")], start=1948, frequency = 4)
head(gdpunemp)


## -----------------------------------------------------------------------------------------------------------
gdpunemp_tsbl <- as_tsibble(gdpunemp)


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
g1 <- gdpunemp_tsbl %>% filter(key=="loggdp") %>% 
  ggplot() + geom_line(aes(x=index, y=log(value)), color="blue")
g2 <- gdpunemp_tsbl %>% filter(key=="rate") %>% 
  ggplot() + geom_line(aes(x=index, y=value), color="blue")
g1 + g2


## -----------------------------------------------------------------------------------------------------------
gdpunemp_i <- diff(gdpunemp) %>% as_tsibble()
head(gdpunemp_i)


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
g1 <- gdpunemp_i %>% filter(key=="loggdp") %>% 
  ggplot() + geom_line(aes(x=index, y=value), color="blue")
g2 <- gdpunemp_i %>% filter(key=="rate") %>% 
  ggplot() + geom_line(aes(x=index, y=value), color="blue")
g1 + g2


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
diff(gdpunemp) %>% as.data.frame() %>% 
  ggplot() + geom_point(aes(x=loggdp, y=rate), color="blue")


## -----------------------------------------------------------------------------------------------------------
varpib <- read.table("/home/alex/Devel/Cours-R-TS/data/q-gdp-ukcaus.txt",header=T)
varpib <- log(varpib[,3:5])
head(varpib)


## -----------------------------------------------------------------------------------------------------------
varpib_ts <- ts(varpib, start=c(1980,1), frequency=4)
pibgr <- diff(varpib_ts)*100
head(pibgr)


## -----------------------------------------------------------------------------------------------------------
varpib_ts <- ts(varpib, start=c(1980,1), frequency=4)
pibgr <- diff(varpib_ts)*100
head(pibgr)


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
plot(pibgr)


## -----------------------------------------------------------------------------------------------------------
summary(ur.df(pibgr[, "uk"], type = "drift", lags = 1))


## -----------------------------------------------------------------------------------------------------------
library("vars")
pibgr.VAR <- VAR(pibgr, p=2)
pibgr.VAR


## -----------------------------------------------------------------------------------------------------------
summary(pibgr.VAR)$varresult$uk


## -----------------------------------------------------------------------------------------------------------
summary(pibgr.VAR)$varresult$ca


## -----------------------------------------------------------------------------------------------------------
summary(pibgr.VAR)$varresult$us


## -----------------------------------------------------------------------------------------------------------
VARselect(pibgr, lag.max = 10)


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
plot(VARselect(pibgr, lag.max = 10)$criteria[1,], xlab='p', ylab='AIC')


## -----------------------------------------------------------------------------------------------------------
pibgr.VAR.serial <- serial.test(pibgr.VAR, type="PT.asymptotic")
pibgr.VAR.serial


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
plot(pibgr.VAR.serial, names="uk")


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
plot(pibgr.VAR.serial, names="us")


## -----------------------------------------------------------------------------------------------------------
pibgr.VAR.norm <- normality.test(pibgr.VAR, multivariate.only = TRUE)
pibgr.VAR.norm


## -----------------------------------------------------------------------------------------------------------
pibgr.VAR.arch <- arch.test(pibgr.VAR, lags.multi = 5, multivariate.only = TRUE)
pibgr.VAR.arch


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------------------
plot(predict(pibgr.VAR))


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
reccusum <- stability(pibgr.VAR, type = "OLS-CUSUM")
plot(reccusum)


## ----out.width='60%', out.height='60%'----------------------------------------------------------------------
reccusum <- stability(pibgr.VAR, type = "OLS-CUSUM")
plot(reccusum)


## -----------------------------------------------------------------------------------------------------------
causality(pibgr.VAR, cause=c("us", "ca"))


## ----message=FALSE------------------------------------------------------------------------------------------
data("Canada")
window(Canada, start=c(1980,1), end=c(1981,4))


## ----out.witdh="60%", out.height='60%', fig.align='center', echo=FALSE--------------------------------------
plot(Canada, nc = 2, xlab = "")


## -----------------------------------------------------------------------------------------------------------
prod.adf1 <- ur.df(Canada[, "prod"], type = 'trend', lags = 2)


## -----------------------------------------------------------------------------------------------------------
attr(prod.adf1, "teststat")


## -----------------------------------------------------------------------------------------------------------
attr(prod.adf1, "cval")


## -----------------------------------------------------------------------------------------------------------
prod.diff <- diff(Canada[, "prod"])
prod.diff.adf1 <- ur.df(prod.diff, type = "drift", lags = 1)


## -----------------------------------------------------------------------------------------------------------
attr(prod.diff.adf1, "teststat")


## -----------------------------------------------------------------------------------------------------------
attr(prod.diff.adf1, "cval")


## -----------------------------------------------------------------------------------------------------------
adf.test(diff(Canada[, "prod"]), k = 1)


## -----------------------------------------------------------------------------------------------------------
VARselect(Canada, lag.max = 8, type = "both")


## -----------------------------------------------------------------------------------------------------------
p1ct <- VAR(Canada, p = 1, type = 'both')
p1ct


## ----size='tiny'--------------------------------------------------------------------------------------------
summary(p1ct, equation = "e")


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------------------
plot(p1ct, names = "e")


## -----------------------------------------------------------------------------------------------------------
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial


## ----out.width='60%', out.height='50%', fig.aling='center'--------------------------------------------------
library(lmtest)
set.seed(123456)
e1 <- rnorm(500)
e2 <- rnorm(500)
trd <- 1:500
y1 <- 0.8 * trd + cumsum(e1)
y2 <- 0.6 * trd + cumsum(e2)
spdata <- data.frame(index=1:500, y1,y2)
ggplot(spdata) + 
  geom_line(aes(x=index, y=y1), color="blue") +
  geom_line(aes(x=index, y=y2), color="red")


## -----------------------------------------------------------------------------------------------------------
sr.reg <- lm(y1 ~ y2)
sr.dw <- dwtest(sr.reg)$statistic
summary(sr.reg)


## -----------------------------------------------------------------------------------------------------------
USIncExp2 <- window(USIncExp, start = c(1985,12))


## -----------------------------------------------------------------------------------------------------------
coint.res <- residuals(lm(expenditure ~ income, data = USIncExp2))


## -----------------------------------------------------------------------------------------------------------
coint.res <- stats::lag(ts(coint.res, start = c(1985,12), freq = 12), k = -1)
USIncExp2 <- cbind(USIncExp2, diff(USIncExp2), coint.res)
USIncExp2 <- window(USIncExp2, start = c(1986,1), end = c(2001,2))
colnames(USIncExp2) <- c("income", "expenditure", "diff.income", "diff.expenditure", "coint.res")
window(USIncExp2, start=c(1986,1), end=c(1986,12))


## -----------------------------------------------------------------------------------------------------------
ecm.model <- diff.expenditure ~ coint.res + diff.income
summary(lm(ecm.model, data=USIncExp2))


## -----------------------------------------------------------------------------------------------------------
data("GermanM1")

LTW <- dm ~ dy2 + dR + dR1 + dp + m1 + y1 + R1 + season

