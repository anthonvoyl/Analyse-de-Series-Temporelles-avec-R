## ----echo=FALSE---------------------------------------------------------------------------------
knitr::opts_chunk$set(size="tiny")

setwd('/home/alex/Devel/Cours-R-TS/supports')


## ----tidyverse_load, echo=TRUE------------------------------------------------------------------
library(tidyverse)


## ----ggplot_load, echo=TRUE---------------------------------------------------------------------
library(ggplot2)
theme_set(theme_minimal())


## ----nelplo_read, echo=TRUE---------------------------------------------------------------------
Nelson_Plosser <- read.csv2("../data/Nelson_Plosser.csv", 
                            header=TRUE, sep=",", dec = ".")
head(Nelson_Plosser)


## ----echo=TRUE, message=FALSE-------------------------------------------------------------------
library(summarytools)
descr(Nelson_Plosser[,1:7])


## ----nelplo_corr, echo=TRUE, tidy=TRUE----------------------------------------------------------
datanum <- Nelson_Plosser %>% filter(year>1909) %>% select(2:8) 
cormat <- round(cor(datanum),2)
cormat


## ----message=FALSE, fig.dim=c(6,4), out.width='60%', fig.align='center'-------------------------
library(reshape2)
cormat %>% melt() %>% ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_distiller(palette = "Spectral", direction = 1)


## ----GGally, echo=FALSE, message=FALSE----------------------------------------------------------
library(GGally)


## ----message=FALSE, eval=FALSE------------------------------------------------------------------
# Nelson_Plosser %>%
#   filter(year>=1909) %>%
#   select(cpi, gnp.nom, emp, unemp) %>%
#   ggpairs(upper = list(continuous = wrap("cor", size = 4)),
#           lower = list(continuous = wrap("points", colour="blue", alpha=0.3, size=0.5)))


## ----nelplo_pairplot_graph, echo=FALSE, fig.dim=c(8,8), out.width="60%", fig.align='center'-----
Nelson_Plosser %>% 
  filter(year>=1909) %>%
  select(cpi, gnp.nom, emp, unemp) %>% 
  ggpairs(upper = list(continuous = wrap("cor", size = 4)),
          lower = list(continuous = wrap("points", colour="blue", alpha=0.3, size=0.5)))


## ----retail_read, echo=TRUE---------------------------------------------------------------------
retail <- read.csv2("../data/RSXFSN.csv", header=TRUE, sep=",", dec = ".")
head(retail)


## -----------------------------------------------------------------------------------------------
summary(retail)


## -----------------------------------------------------------------------------------------------
retail$DATE <- as.Date(retail$DATE,format="%Y-%m-%d")
summary(retail)


## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------------------------
# library(insee)
# df_idbank_list_selected =
#   get_idbank_list("CHOMAGE-TRIM-NATIONAL") %>% #Unemployment dataset
#   add_insee_title() %>%
#   filter(INDICATEUR == "CTTXC") %>% #unemployment rate based on ILO standards
#   filter(REF_AREA == "FM") %>%  # all France excluding overseas departements
#   filter(SEXE == 0) # men and women
# 
# list_idbank = df_idbank_list_selected %>% pull(idbank)
# 
# fr_unemp = get_insee_idbank(list_idbank, startPeriod = "1950-01") %>% split_title()


## ----echo=TRUE----------------------------------------------------------------------------------
load("../data/fr_unemp.RData")
head(fr_unemp)


## -----------------------------------------------------------------------------------------------
fr_unemp %>% distinct(TITLE_FR)


## -----------------------------------------------------------------------------------------------
fr_unemp %>% summarise(début=min(DATE), fin=max(DATE))


## ----fig.dim=c(10,6), out.width='60%', fig.align='center'---------------------------------------
ggplot(fr_unemp, aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN2)) +
  geom_line() +
  ggtitle("French unemployment rate, by age") +
  labs(subtitle = sprintf("Last updated : %s", fr_unemp$TIME_PERIOD[1]))


## ----message=FALSE, eval=FALSE------------------------------------------------------------------
# df_idbank_list_selected =
#   get_idbank_list("CNT-2014-PIB-EQB-RF") %>% # Gross domestic product balance
#   filter(FREQ == "T") %>% #quarter
#   add_insee_title() %>% #add titles
#   filter(OPERATION == "PIB") %>% #GDP
#   filter(NATURE == "VALEUR_ABSOLUE") %>% #rate
#   filter(CORRECTION == "CVS-CJO") #SA-WDA, seasonally adjusted, working day adjusted
# 
# idbank = df_idbank_list_selected %>% pull(idbank)
# 
# fr_pib = get_insee_idbank(idbank)


## ----echo=TRUE----------------------------------------------------------------------------------
load("../data/fr_pib.RData")
head(fr_pib)


## -----------------------------------------------------------------------------------------------
min(fr_pib$DATE)


## -----------------------------------------------------------------------------------------------
fr_pib %>% distinct(TITLE_EN)


## ----fig.dim=c(8,6), out.width='60%', fig.align='center'----------------------------------------
ggplot(fr_pib, aes(x = DATE, y = OBS_VALUE)) +
  geom_col(color="blue") +
  ggtitle("French GDP, quarter-on-quarter, sa-wda") +
  labs(subtitle = sprintf("Last updated : %s", fr_pib$TIME_PERIOD[1]))


## ----message=FALSE, eval=FALSE------------------------------------------------------------------
# df_idbank_list_selected =
#   get_idbank_list("IPC-2015") %>% # Gross domestic product balance
#   filter(FREQ == "M" & idbank=="001759970") %>%
#   add_insee_title()
# 
# idbank = df_idbank_list_selected %>% pull(idbank)
# 
# fr_cpi = get_insee_idbank(idbank)


## ----echo=TRUE----------------------------------------------------------------------------------
load("../data/fr_cpi.RData")
head(fr_cpi)


## -----------------------------------------------------------------------------------------------
fr_cpi %>% distinct(TITLE_FR)


## -----------------------------------------------------------------------------------------------
fr_cpi %>% summarise(début=min(fr_cpi$DATE), fin=max(fr_cpi$DATE))


## ----fig.dim=c(6,4), out.width='60%', out.height='50%', fig.align='center'----------------------
ggplot(fr_cpi, aes(x = DATE, y = OBS_VALUE)) +
  geom_line(color="blue") +
  ggtitle("Indice des prix à la consommation - Base 2015") +
  labs(subtitle = sprintf("Last updated : %s", fr_cpi$TIME_PERIOD[1])) +
  ylab("IPC (base 2015)")


## -----------------------------------------------------------------------------------------------
pib <- read.csv("../data/pib_fr.csv", sep=";", dec=',')
head(pib)


## -----------------------------------------------------------------------------------------------
load("../data/fr_cpi.RData")
fr_cpi <- fr_cpi %>% mutate(Année=year(DATE), Trimestre=quarter(DATE)) %>%
  group_by(Année, Trimestre) %>% 
  rename(cpi=OBS_VALUE) %>%
  summarise(cpi=mean(cpi))
head(fr_cpi)


## -----------------------------------------------------------------------------------------------
load("../data/fr_pib.RData")
fr_pib <- fr_pib %>% mutate(Année=year(DATE), Trimestre=quarter(DATE)) %>% 
  rename(gnp=OBS_VALUE) %>%
  filter(TITLE_FR==unique(fr_pib$TITLE_FR)[1]) %>%
  select(Année, Trimestre, gnp)

load("../data/fr_unemp.RData")
fr_unemp <- fr_unemp %>% mutate(Année=year(DATE), Trimestre=quarter(DATE)) %>%
  rename(unemp=OBS_VALUE) %>%
  filter(TITLE_FR2=='Ensemble') %>%
  select(Année, Trimestre, unemp)

fr_macro <- fr_pib %>% 
  left_join(fr_unemp, by=c("Année", "Trimestre")) %>%
  left_join(fr_cpi, by=c("Année", "Trimestre")) %>% 
  arrange(Année, Trimestre)

head(fr_macro)


## -----------------------------------------------------------------------------------------------
library(tseries)
data(NelPlo)
class(NelPlo)


## -----------------------------------------------------------------------------------------------
class(NelPlo)


## -----------------------------------------------------------------------------------------------
window(NelPlo, start=1905, end=1910)


## ----gmoney_load, echo=TRUE, message=FALSE------------------------------------------------------
library(lmtest)
data(growthofmoney)
window(growthofmoney, end=c(1971,4))


## ----message=FALSE, tidy=TRUE, tidy.opts = list(blank = FALSE, width.cutoff = 60)---------------
library(strucchange)
data("USIncExp")
window(USIncExp, start=c(1959,6), end=c(1960,6))


## ----retail_ts, echo=TRUE-----------------------------------------------------------------------
retail_ts <- ts(retail['RSXFSN'], frequency=12, start=c(1997,3))
window(retail_ts, start=2020)


## -----------------------------------------------------------------------------------------------
tsp(retail_ts)


## ----pib_ts_time, echo=TRUE---------------------------------------------------------------------
retail2022 <- window(retail_ts, start=c(2022,1), end=c(2022,12))
time(retail2022)


## ----pib_ts_time2, echo=TRUE--------------------------------------------------------------------
library(lubridate)
as.numeric(time(retail2022))


## ----gmoney_tsibble, echo=TRUE, message=FALSE---------------------------------------------------
library(tsibble)
gmoney_tsbl <- growthofmoney %>% as_tsibble()
gmoney_tsbl


## -----------------------------------------------------------------------------------------------
class(gmoney_tsbl)


## ----echo=TRUE, message=FALSE-------------------------------------------------------------------
gmoney_tsbl %>% distinct(key)


## ----nelplo_tsibble, echo=TRUE, message=FALSE---------------------------------------------------
nelplo_tsbl <- NelPlo %>% as_tsibble()
nelplo_tsbl %>% filter(index>1980 & key=="gnp.capita")


## -----------------------------------------------------------------------------------------------
nelplo_tsbl %>% distinct(key)


## ----retail_tsibble, echo=TRUE, message=FALSE---------------------------------------------------
retail_tsbl <- retail_ts %>% as_tsibble()
head(retail_tsbl)


## -----------------------------------------------------------------------------------------------
pib_ts <- pib %>% select(-PERIODE) %>% ts(start=1949, frequency=4)
window(pib_ts, start=1949, end=1950)
pib_tsbl <- as_tsibble(pib_ts)
pib_tsbl


## -----------------------------------------------------------------------------------------------
fr_macro_ts <- fr_macro %>% select(-c(Année, Trimestre)) %>% ts(start=1949, frequency=4)
window(fr_macro_ts, start=1989, end=1992)


## -----------------------------------------------------------------------------------------------
fr_macro_tsbl <- as_tsibble(fr_macro_ts)
fr_macro_tsbl
save(fr_macro_ts, fr_macro_tsbl, file="../data/fr_macro.RData")


## ----warning=FALSE------------------------------------------------------------------------------
acf(NelPlo[,'cpi'], plot=FALSE)


## ----warning=FALSE------------------------------------------------------------------------------
library(feasts)
nelplo_tsbl %>% 
  filter(key=="cpi") %>% 
  ACF(value)


## -----------------------------------------------------------------------------------------------
bbg <- ts(rnorm(100))
window(bbg, start=1, end=10)


## -----------------------------------------------------------------------------------------------
mean(bbg)
sd(bbg)


## ----fig.dim=c(6,4), out.width='70%', fig.align="center", message=FALSE-------------------------
library(ggfortify)
bbg %>% as_tsibble() %>% autoplot(color="blue") + 
  ggtitle("Bruit blanc gaussien") +
  xlab("t")


## -----------------------------------------------------------------------------------------------
acf(bbg, plot=FALSE)


## -----------------------------------------------------------------------------------------------
box_pierce(bbg)


## ----echo=TRUE----------------------------------------------------------------------------------
set.seed(2025)
tmax <- 100
wnoise <- rnorm(99, mean=0, sd=1)
y <- rep(0,tmax)

for (t in 2:tmax) {
  y[t] = y[t-1] + wnoise[t-1] 
}
rw <- tibble(time=1:tmax, y=y)
head(rw)


## ----echo=TRUE----------------------------------------------------------------------------------
mean(rw$y)


## -----------------------------------------------------------------------------------------------
acf(rw[,"y"], plot=FALSE)


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------
ggplot(rw) + 
  geom_line(aes(x=time, y=y), colour="blue")


## -----------------------------------------------------------------------------------------------
tmax <- 100
alpha <- 0.8

y <- rep(0,tmax)

for (t in 2:tmax) {
  y[t] = alpha + y[t-1] + wnoise[t-1] 
}
rwd <- tibble(time=1:tmax, y=y)
head(rwd)


## ----fig.dim=c(6,4), out.width="70%", fig.align="center"----------------------------------------
ggplot(rwd) + 
  geom_abline(slope=0.8, color="cyan") +
  geom_line(aes(x=time, y=y), colour="blue")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
plot(NelPlo[,c("cpi", "unemp")], xlab="Trimestre", main="Données Nelson-Plosser")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
USIncExp %>% as_tsibble() %>% autoplot() + ylab("Millions de dollars")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
ggplot(retail_tsbl, aes(x = index, y = value)) +
    geom_line(color = "purple", size = 0.5) +
    geom_point(color = "green", size = 0.5) + 
    xlab("Mois") + ylab("Ventes du commerce de détail") +
    ggtitle("Données retail: Chronogramme")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
ggplot(gmoney_tsbl, aes(x = index, y = value)) + 
  geom_line(aes(color = key), size = 1) +
  scale_colour_brewer(palette="Accent") +
  ggtitle("Données growthofmoney: Chronogramme")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
nelplo_tsbl %>% filter(key %in% c("gnp.nom", "gnp.real", "unemp", "cpi", "money.stock")) %>%
  ggplot(aes(x = index, y = value)) + 
  geom_line(aes(color = key), size = 1) +
  ggtitle("Données nelplo: Chronogramme")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
library(forecast)
ggseasonplot(growthofmoney[,"TG1.TG0"])


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
ggseasonplot(window(retail_ts, start=c(2000,1), end=c(2020,12))) + 
  ggtitle("Données retail (2000-2020): Season plot")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
ggseasonplot(window(retail_ts, start=c(2000,1)), polar=TRUE) + 
  ggtitle("Données retail (2000-2020): Season plot")


## ----fig.dim=c(10,6), out.height='60%', fig.align='center', message=FALSE-----------------------
library(ggtime)
retail_tsbl %>% ggtime::gg_lag(y=value, geom="point", size=0.5, lags=c(1,2, 3,6,9,12))


## ----fig.dim=c(10,6), out.height='60%', fig.align='center', message=FALSE-----------------------
nelplo_tsbl %>% filter(key=='cpi') %>% 
  ggtime::gg_lag(y=value, geom="point", size=0.5, colour="blue", lags=c(1,2,5,10,15)) 


## ----fig.dim=c(8,6), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
nelplo_tsbl %>% filter(key=="cpi") %>% 
  ACF(value) %>% autoplot()


## ----fig.dim=c(8,6), out.width='70%', fig.align='center', message=FALSE, warning=FALSE----------
gmoney_tsbl %>% ACF() %>% autoplot()


## ----warnings=F, message=FALSE, fig.dim=c(10,6), out.width='70%', fig.align='center'------------
library(patchwork)
g1 <- retail_tsbl %>% autoplot(color='blue')
g2 <- ACF(retail_tsbl) %>% autoplot()
g1+g2


## -----------------------------------------------------------------------------------------------
nelplo1909 <- nelplo_tsbl %>% 
  filter(index>=1909) %>% 
  spread(key = key, value=value)
head(nelplo1909)


## -----------------------------------------------------------------------------------------------
nelplo1909 %>% select(index, gnp.nom)


## ----fig.dim=c(6,4), out.width="70%", fig.align='center'----------------------------------------
nelplo1909 %>%
  ggplot(aes(index, gnp.nom)) +
    geom_point(colour="blue") +
  xlab("Year") + ylab('log(gnp.nom)')


## -----------------------------------------------------------------------------------------------
mod1 <- lm(gnp.nom ~ index, data=nelplo1909)


## -----------------------------------------------------------------------------------------------
summary(mod1)


## -----------------------------------------------------------------------------------------------
names(summary(mod1))


## -----------------------------------------------------------------------------------------------
summary(mod1)$coefficients


## -----------------------------------------------------------------------------------------------
summary(mod1)$adj.r.squared


## ----fig.dim=c(6,4), out.width="70%", fig.align='center', message=FALSE-------------------------
ggplot(nelplo1909, aes(x=index, y=gnp.nom)) +
  geom_point(colour="blue") +
  geom_smooth(method='lm', color="red")


## -----------------------------------------------------------------------------------------------
mod1_diag <- tibble(observed=nelplo1909$gnp.nom, predicted=mod1$fitted.values, 
                    residual=mod1$residuals)
mod1_diag


## ----fig.dim=c(6,4), out.width="70%", fig.align='center', echo=TRUE-----------------------------
ggplot(mod1_diag) + 
  geom_point(aes(x=observed, y=predicted), colour="blue") + 
  geom_abline(colour="grey")


## ----fig.dim=c(6,4), out.width="70%", fig.align='center'----------------------------------------
ggplot(mod1_diag, aes(x=residual)) + 
  geom_histogram(aes(y = after_stat(density)), fill="lightblue", binwidth = 0.05) +
  geom_density(aes(x=residual))


## -----------------------------------------------------------------------------------------------
shapiro.test(mod1_diag$residual)


## ----fig.dim=c(6,4), out.width="60%", fig.align='center'----------------------------------------
ggplot() + 
  geom_qq(aes(sample=rstandard(mod1)), color="red") + 
  geom_abline(color = "grey")


## ----fig.dim=c(6,4), out.width="70%", fig.align='center'----------------------------------------
ggplot(mod1_diag) + 
  geom_point(aes(x=predicted, y=residual), colour="red")


## ----bptest-------------------------------------------------------------------------------------
library(lmtest)
bptest(mod1)


## ----fig.dim=c(10,6), out.width="80%", fig.align='center'---------------------------------------
gdata <- tibble(index=1:nrow(mod1_diag), residus=mod1_diag$residual)
g1 <- gdata %>% ggplot() + geom_point(aes(x=index, y=residus), color="red")
g2 <- gg_lag(tsibble(gdata, index=index), y=residus, color="red", lags=1, geom="point") 
g1 + g2


## ----fig.dim=c(6,4), out.width="70%", fig.align='center'----------------------------------------
acf(mod1_diag$residual)


## -----------------------------------------------------------------------------------------------
dwtest(mod1)


## ----lm_gom_data--------------------------------------------------------------------------------
data("growthofmoney")
head(growthofmoney)


## ----lm_gom_plot, fig.dim=c(6,4), out.width="60%", fig.align='center', message=FALSE------------
gdata <- tibble(AG0_TG0=growthofmoney[,"AG0.TG0"], TG1_TG0=growthofmoney[,"TG1.TG0"])
ggplot(gdata) + geom_point(aes(x=AG0_TG0, y=TG1_TG0), color="blue")


## -----------------------------------------------------------------------------------------------
modelHetzel <- TG1.TG0 ~ AG0.TG0
class(modelHetzel)


## -----------------------------------------------------------------------------------------------
gom.mod1 <- lm(modelHetzel, data=growthofmoney)


## -----------------------------------------------------------------------------------------------
summary(gom.mod1)


## ----fig.dim=c(10,6), out.width="70%", fig.align='center', message=FALSE------------------------
gdata <- tibble(index=1:nrow(growthofmoney), residus=gom.mod1$residual)
g1 <- gdata %>% ggplot() + geom_point(aes(x=index, y=residus), color="red")
g2 <- gdata %>% as_tsibble(index=index) %>% ACF() %>% autoplot()
g1 + g2


## -----------------------------------------------------------------------------------------------
dwtest(modelHetzel, data=growthofmoney)


## -----------------------------------------------------------------------------------------------
dcook <- gdata <- data.frame(obs=1:nrow(growthofmoney), dcook=cooks.distance(gom.mod1))
head(dcook)


## -----------------------------------------------------------------------------------------------
tcook <- 4/nrow(growthofmoney)
tcook


## ----fig.dim=c(6,4), out.width="60%", fig.align='center'----------------------------------------
ggplot(dcook, aes(x=obs, y=dcook)) + 
  geom_point(color="blue") +
  geom_hline(aes(yintercept=tcook), color="red")


## -----------------------------------------------------------------------------------------------
stres <- data.frame(obs=1:nrow(growthofmoney), stres=rstudent(gom.mod1))
head(stres)


## -----------------------------------------------------------------------------------------------
stres[abs(stres$stres)>3,]


## -----------------------------------------------------------------------------------------------
as_tsibble(growthofmoney)[5:6,]


## ----fig.dim=c(6,4), out.width="70%", fig.align='center'----------------------------------------
ggplot(stres, aes(x=obs, y=stres)) + 
  geom_point(color="blue") +
  geom_hline(aes(yintercept=3), color="red") +
  geom_hline(aes(yintercept=-3), color="red")


## -----------------------------------------------------------------------------------------------
nelplo1909 <- nelplo_tsbl %>% 
  filter(index>=1909) %>% 
  spread(key = key, value=value)
regdata <- nelplo1909 %>% 
  select(index, gnp.nom, emp, ip, nom.wages)


## -----------------------------------------------------------------------------------------------
mod2 <- lm(gnp.nom ~ index+emp+ip+nom.wages, data=regdata)
summary(mod2)


## -----------------------------------------------------------------------------------------------
mod3 <- lm(gnp.nom ~ index+emp+nom.wages, data=regdata)
summary(mod3)


## -----------------------------------------------------------------------------------------------
AIC(mod1, mod2, mod3)


## -----------------------------------------------------------------------------------------------
mod3.diag <- tibble(observed=nelplo1909$gnp.nom, predicted=mod3$fitted.values, 
                    residual=mod3$residuals)
mod3.diag


## ----fig.dim=c(6,4), out.width="70%", fig.align='center'----------------------------------------
ggplot(mod3.diag, aes(x=residual)) + 
  geom_histogram(aes(y = after_stat(density)), fill="lightblue", binwidth = 0.05) +
  geom_density(aes(x=residual))


## -----------------------------------------------------------------------------------------------
shapiro.test(mod3.diag$residual)


## ----fig.dim=c(10,6), out.width="80%", fig.align='center'---------------------------------------
gdata <- tibble(index=1:nrow(mod3.diag), residus=mod3.diag$residual)
g1 <- gdata %>% ggplot() + geom_point(aes(x=index, y=residus), color="red")
g2 <- gg_lag(tsibble(gdata, index=index), y=residus, color="red", lags=1, geom="point") 
g1 + g2


## ----fig.dim=c(6,4), out.width="70%", fig.align='center'----------------------------------------
acf(mod3.diag$residual)


## -----------------------------------------------------------------------------------------------
dwtest(mod3)


## ----fig.dim=c(6,4), out.width='70%', fig.align='center'----------------------------------------
growthofmoney %>% as_tsibble() %>% autoplot(value)


## -----------------------------------------------------------------------------------------------
modelHetzel


## -----------------------------------------------------------------------------------------------
sctest(modelHetzel, point=c(1973,4), data=growthofmoney, type="Chow")


## ----fig.dim=c(6,4), out.width='70%', fig.align='center'----------------------------------------
rdata <- Nelson_Plosser[,c("year", "gnp.real")]
rdata <- rdata[rdata$year>=1909,]
tsibble(rdata, index=year) %>% autoplot(gnp.real, color="blue")


## -----------------------------------------------------------------------------------------------
library(strucchange)
model <- gnp.real ~ year
sctest(model, point=which(rdata$year==1933), data=rdata, type="Chow")


## ----out.width="60%", out.height='50%', fig.align='center'--------------------------------------
fs <- Fstats(model, from=0.1, data = rdata)
plot(fs)


## -----------------------------------------------------------------------------------------------
dcmp <- retail_tsbl %>%
  model(classical_decomposition(value))
components(dcmp)


## ----fig.dim=c(10,6), out.width='70%', fig.align='center', warning=FALSE------------------------
components(dcmp) %>% autoplot()


## ----fig.dim=c(8,6), out.width='60%', fig.align='center'----------------------------------------
components(dcmp) %>% ggplot() + 
  geom_line(aes(x=index,y=value), colour="blue") +
  geom_line(aes(x=index,y=season_adjust), colour="red")


## ----out.width="60%", out.height="60%", fig.align='center', message=FALSE-----------------------
USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  ggplot(aes(x = index, y = value)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method='lm', color="red")


## -----------------------------------------------------------------------------------------------
regdata <- USIncExp %>% as_tsibble() %>% filter(key=='expenditure')
summary(lm(value ~ index, data=regdata))


## ----out.width="60%", out.height='60%', fig.align='center', message=FALSE-----------------------
USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  ggplot(aes(x = index, y = log(value))) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method='lm', color="red")


## -----------------------------------------------------------------------------------------------
regdata <- USIncExp %>% as_tsibble() %>% 
  filter(key=='expenditure') %>%
  mutate(value=log(value))
summary(lm(value ~ index, data=regdata))


## -----------------------------------------------------------------------------------------------
retail$mois <- factor(month(retail$DATE), labels=month(1:12, label=TRUE))
retail$t <- c(1:nrow(retail))
head(retail)


## -----------------------------------------------------------------------------------------------
modst <- lm(RSXFSN ~ t+ mois, data=retail)


## ----size='tiny'--------------------------------------------------------------------------------
summary(modst)


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------
retail$prediction <- predict.lm(modst)
ggplot(retail)+
  geom_line(mapping=aes(x=t,y=RSXFSN),color="blue")+
  geom_line(mapping=aes(x=t,y=prediction), color="red")


## -----------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------
coefst <- modst$coefficients
coefst


## -----------------------------------------------------------------------------------------------
a <- mean(coefst[2:13])
b <- coefst[1]
c <- coefst[2:13]-mean(coefst[2:13])
y_cvs <- retail_1992_2022$RSXFSN-(c[1]*s1+c[2]*s2+c[3]*s3+c[4]*s4+c[5]*s5+c[6]*s6+
                                    c[7]*s7+c[8]*s8+c[9]*s9+c[10]*s10+c[11]*s11+c[12]*s12)


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------
gdata <- tibble(t=retail_1992_2022$t, 
                original=retail_1992_2022$RSXFSN, cvs = y_cvs)

ggplot(gdata)+
  geom_line(mapping=aes(x=t,y=original),color="blue")+
  geom_line(mapping=aes(x=t,y=cvs), color="red")


## -----------------------------------------------------------------------------------------------
library(forecast)
bhat <- tslm(retail_ts ~ trend + season)


## -----------------------------------------------------------------------------------------------
summary(bhat)


## ----fig.dim=c(6,4), out.width='70%', fig.align='center'----------------------------------------
plot(forecast(bhat, h=20))


## ----fig.dim=c(6,4), out.width='60%', fig.align='center'----------------------------------------
set.seed(12345)
y.tsar2 <- 5 + 0.5 * seq(250) +
  arima.sim(list(ar = c(0.8, -0.2)), n = 250)

plot(y.tsar2, ylab="y", xlab = "Time")
abline(a=5, b=0.5, col = "red")


## -----------------------------------------------------------------------------------------------
set.seed(12345)
u.ar2 <- arima.sim(list(ar = c(0.8, -0.2)), n = 250)
y1 <- cumsum(u.ar2)
TD <- 5.0 + 0.7 * seq(250)
y1.d <- y1 + TD
tstoch <- ts(data.frame(nodrift=y1, drift=y1.d))
head(tstoch)


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
as_tsibble(tstoch) %>% autoplot(value) +
  geom_abline(intercept=5, slope=0.7, col = "red")


## -----------------------------------------------------------------------------------------------
cpi_diff <- diff(NelPlo[,"cpi"],1)
window(cpi_diff, start=1861, end=1900)


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------
cpi_diff %>% as_tsibble() %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(colour="red", size = 1) + ylab("diff(cpi)")


## ----fig.dim=c(6,4), out.width="70%", fig.align="center", echo=TRUE-----------------------------
USExp <- USIncExp %>% as_tsibble() %>% filter(key=='expenditure') %>%
  mutate(logExp=log(value))
ggplot(USExp, aes(x = index, y = logExp)) +
  geom_line(color = "blue", size = 1)


## ----s------------------------------------------------------------------------------------------
library(urca)
exp.df <- ur.df(y=as.data.frame(USExp)[,"logExp"], lags=12, type='trend')
exp.df


## -----------------------------------------------------------------------------------------------
names(attr(exp.df, "testreg"))
attr(exp.df, "testreg")$call


## -----------------------------------------------------------------------------------------------
attr(exp.df, "testreg")


## -----------------------------------------------------------------------------------------------
library(urca)
exp.df <- ur.df(y=as.data.frame(USExp)[,"logExp"], lags=8, type='trend')


## -----------------------------------------------------------------------------------------------
attr(exp.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(exp.df, "cval")


## -----------------------------------------------------------------------------------------------
attr(exp.df, "testreg")


## ----fig.dim=c(6,4), out.height="60%", fig.align="center"---------------------------------------
USExp.diff <- tibble(time=1:(nrow(USExp)-1), diff=diff(as.data.frame(USExp)[, "logExp"]))
ggplot(USExp.diff, aes(x = time, y = diff)) +
  geom_line(color = "blue", size = 0.5)


## -----------------------------------------------------------------------------------------------
USExp.diff.df <- ur.df(y=as.data.frame(USExp.diff)[, "diff"], lags=1, type='drift')


## -----------------------------------------------------------------------------------------------
attr(USExp.diff.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(USExp.diff.df, "cval")


## ----fig.dim=c(6,4), out.height="70%", fig.align="center"---------------------------------------
nelplo_tsbl %>% filter(key=="gnp.real" & index>=1909) %>%
  ggplot(aes(x = index, y = value)) +
    geom_line(color='blue') + ylab("log(PIB)")


## -----------------------------------------------------------------------------------------------
gnpreal.df <- ur.df(y=as.data.frame(nelplo1909)[,"gnp.real"], lags=1, type='trend')


## -----------------------------------------------------------------------------------------------
attr(gnpreal.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(gnpreal.df , "cval")


## ----fig.dim=c(6,4), out.height="60%", fig.align="center"---------------------------------------
gnpreal.diff <- tibble(time=1:(nrow(nelplo1909)-1), diff= diff(as.data.frame(nelplo1909)$gnp.real))
ggplot(gnpreal.diff, aes(x = time, y = diff)) +
  geom_line(color = "blue", size = 0.5) + ylab("diff(logGNP)")


## -----------------------------------------------------------------------------------------------
gnpreal.diff.df <- ur.df(y=as.data.frame(gnpreal.diff)[, "diff"], lags=1, type='drift')


## -----------------------------------------------------------------------------------------------
attr(gnpreal.diff.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(gnpreal.diff.df , "cval")


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------
Unemp <- nelplo_tsbl %>% filter(key=='unemp' & index >= 1909) 
ggplot(Unemp, aes(x = index, y = value)) +
  geom_line(color = "blue", size = 1)


## ----size='tiny'--------------------------------------------------------------------------------
unemp.df <- ur.df(y=as.data.frame(Unemp)[,"value"], lags=1, type='drift')


## -----------------------------------------------------------------------------------------------
attr(unemp.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(unemp.df, "cval")


## -----------------------------------------------------------------------------------------------
adf.test(as.data.frame(Unemp)[,"value"], k=1)


## -----------------------------------------------------------------------------------------------
ktest <- ur.kpss(USExp$logExp, type="tau", lags="long")
summary(ktest)


## -----------------------------------------------------------------------------------------------
ktest <- ur.kpss(USExp$logExp, type="mu", lags="long")
summary(ktest)


## -----------------------------------------------------------------------------------------------
ktest <- ur.kpss(USExp.diff$diff, type="mu", lags="long")
summary(ktest)


## -----------------------------------------------------------------------------------------------
ktest <- ur.kpss(diff(USExp.diff$diff), type="mu", lags="long")
summary(ktest)


## -----------------------------------------------------------------------------------------------
set.seed(13)
wn <- rnorm(100)
AR1sim <- arima.sim(n = 100, list(ar = 0.9), innov=wn)


## -----------------------------------------------------------------------------------------------
pacf(AR1sim, plot=FALSE)


## ----eval=FALSE---------------------------------------------------------------------------------
# op <- par(no.readonly=TRUE)
# 
# layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
# 
# plot.ts(AR1sim, ylab="", main="Processus AR(1) avec B1=0.9")
# 
# acf(AR1sim, main="Autocorrelations", ylab="",
#     ylim=c(-1, 1), ci.col = "black")
# 
# pacf(AR1sim, main="Partial Autocorrelations", ylab="",
#      ylim=c(-1, 1), ci.col = "black")
# 
# par(op)


## ----fig.dim=c(8,6), out.height='70%', fig.align='center', echo=FALSE---------------------------
op <- par(no.readonly=TRUE)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(AR1sim, ylab='')
acf(AR1sim, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(AR1sim, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

par(op)


## -----------------------------------------------------------------------------------------------
rusim <- 0
for (i in 1:length(wn)) {rusim[i+1] <- rusim[i] + wn[i]}

## ----out.width='60%', out.height='60%', fig.align='center', echo=FALSE--------------------------
op <- par(no.readonly=TRUE)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(rusim, ylab='', main='Processus AR(1) avec B1=1.0')
acf(rusim, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(rusim, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

par(op)


## ----echo=FALSE, out.width="60%", fig.align='center'--------------------------------------------
y <- window(NelPlo[,'unemp'], start=1909)

op <- par(no.readonly=TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(y, ylab='')
acf(y, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(y, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")
par(op)


## ----fig.dim=c(6,4), out.width='60%', fig.align='center'----------------------------------------
unemp1909p <- nelplo_tsbl %>% filter(index>1909 & key=='unemp')
unemp1909p %>% gg_lag(y=value, lags=1, geom="point", colour="blue")


## -----------------------------------------------------------------------------------------------
nelplo.lm <- lm(value ~ lag(value), data=unemp1909p) 
summary(nelplo.lm)


## -----------------------------------------------------------------------------------------------
class(nelplo.lm)


## -----------------------------------------------------------------------------------------------
names(nelplo.lm)


## -----------------------------------------------------------------------------------------------
nelplo.lm$coefficients


## -----------------------------------------------------------------------------------------------
nelplo.ar1 <- arima(unemp1909p[,"value"], c(1,0,0))
summary(nelplo.ar1)


## -----------------------------------------------------------------------------------------------
names(nelplo.ar1)
nelplo.ar1$coef


## -----------------------------------------------------------------------------------------------
MA1sim <- arima.sim(n = 100, list(ma = 0.9), innov=wn)


## ----out.width="60%", out.height='60%', fig.align='center', eval=FALSE--------------------------
# op <- par(no.readonly=TRUE)
# 
# layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
# 
# plot.ts(MA1sim, ylab='')
# 
# acf(MA1sim, main="Autocorrelations", ylab="", main="Processus MA(1) - Simulation",
#     ylim=c(-1, 1), ci.col = "black")
# pacf(MA1sim, main="Partial Autocorrelations", ylab="",
#      ylim=c(-1, 1), ci.col = "black")
# 
# par(op)


## ----out.width="60%", out.height='60%', fig.align='center', echo=FALSE--------------------------
op <- par(no.readonly=TRUE)
 
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(MA1sim, ylab='', main="Processus MA(1) - Simulation")
acf(MA1sim, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(MA1sim, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

par(op)


## ----fig.dim=c(6,4), out.width='70%', fig.align="center"----------------------------------------
unemp1909p %>% autoplot(value, col="blue") + 
  ggtitle("Taux de chômage aux USA")


## ----fig.dim=c(6,4), out.width='70%', fig.align="center"----------------------------------------
unemp1909p %>% ACF(value) %>% autoplot() + 
  ggtitle("Taux de chômage aux USA: ACF")


## ----fig.dim=c(6,4), out.width="60%", fig.align="center"----------------------------------------
unemp1909p %>% PACF(value) %>% autoplot() + 
  ggtitle("Taux de chômage aux USA: PACF")


## -----------------------------------------------------------------------------------------------
arma.mod1 <- arima(unemp1909p[,"value"], c(2,0,2))
summary(arma.mod1)


## -----------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------
arma.mod2 <- arima(unemp1909p[,"value"], c(1,0,1)) 
summary(arma.mod2)


## -----------------------------------------------------------------------------------------------
AIC(arma.mod1, arma.mod2)


## -----------------------------------------------------------------------------------------------
nelplo.arma <- auto.arima(unemp1909p$value)
nelplo.arma


## -----------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------
nelplo.arma.diag <- data.frame(index=unemp1909p$index, obs=unemp1909p$value, 
                               prediction=as.data.frame(nelplo.arma$fitted)$x, 
                               residus=as.data.frame(nelplo.arma$residuals)$x)
head(nelplo.arma.diag)


## ----fig.dim=c(6,4), out.width='70%', fig.align='center'----------------------------------------
nelplo.arma.diag %>% 
  pivot_longer(cols=2:4) %>% 
  filter(name %in% c("prediction", "obs")) %>%
  ggplot() + geom_line(aes(x=index, y=value, color=name))


## ----fig.dim=c(8,6), out.width="70%", fig.align='center', warning='FALSE'-----------------------
nelplo.arma.diag %>% ggplot() + 
  geom_point(aes(x=index, y=residus), color="blue")


## ----fig.dim=c(10,6), out.width="70%", fig.align='center'---------------------------------------
nelplo.arma$residuals %>% as_tsibble() %>% 
  gg_lag(y=value, lags=1:3, geom="point", colour="blue")


## -----------------------------------------------------------------------------------------------
lags <- 1:3
pval <- NULL
for (l in lags) {
  pval <-c(pval, box_pierce(nelplo.arma.diag$residus, lag=l)["bp_pvalue"])
}
res <- data.frame(lags, pval)
res


## ----fig.dim=c(6,4), out.width='60%', message=FALSE, fig.align='center'-------------------------
ggplot(nelplo.arma.diag) + geom_histogram(aes(x=residus), fill='green')

## -----------------------------------------------------------------------------------------------
jarque.bera.test(nelplo.arma.diag$residus)


## -----------------------------------------------------------------------------------------------
forecast(nelplo.arma, h=10)


## ----fig.dim=c(6,4), out.width='60%', fig.align="center"----------------------------------------
plot(forecast(nelplo.arma, h=10))


## ----out.width='50%', out.height='50%', fig.align='center'--------------------------------------
ggplot(gnpreal.diff) + geom_line(aes(x=time, y=diff), color="blue") + ylab("diff(logGNP)")


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------
gnpreal.diff %>% as_tsibble(index=time) %>% ACF(diff) %>% autoplot()


## ----fig.dim=c(6,4), out.width='70%', fig.align="center"----------------------------------------
gnpreal.diff %>% as_tsibble(index=time) %>% PACF(diff) %>% autoplot()


## -----------------------------------------------------------------------------------------------
regdata <- nelplo1909$gnp.real
gnpreal.arima <- auto.arima(regdata)
gnpreal.arima


## -----------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------
gnpreal.arima.diag <- data.frame(index=nelplo1909$index, 
                               prediction=as.data.frame(gnpreal.arima$fitted)$x, 
                               residus=as.data.frame(gnpreal.arima$residuals)$x)
head(gnpreal.arima.diag)


## ----out.width='60%', out.height='60%', fig.align='center', warning='FALSE'---------------------
gnpreal.arima.diag %>% ggplot(aes(x=index, y=residus)) + 
  geom_point(color="blue") +
  geom_line(color="blue")


## ----echo=TRUE, fig.dim=c(6,4), out.width="70%", fig.align='center'-----------------------------
gnpreal.arima$residuals %>% as_tsibble() %>% 
  gg_lag(y=value, lags=1:3, geom="point", colour="blue")


## -----------------------------------------------------------------------------------------------
lags <- 1:3
pval <- NULL
for (l in lags) {
  pval <-c(pval, box_pierce(gnpreal.arima.diag$residus, lag=l)["bp_pvalue"])
}
res <- data.frame(lags, pval)
res


## ----out.width='50%', out.height='50%', message=FALSE-------------------------------------------
ggplot(gnpreal.arima.diag) + geom_histogram(aes(x=residus), fill='green')

## -----------------------------------------------------------------------------------------------
jarque.bera.test(gnpreal.arima.diag$residus)


## ----out.width='50%', out.height='50%', message=FALSE-------------------------------------------
checkresiduals(gnpreal.arima, test=FALSE)


## -----------------------------------------------------------------------------------------------
checkresiduals(gnpreal.arima, plot=FALSE)


## -----------------------------------------------------------------------------------------------
forecast(gnpreal.arima, h=10)


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------
plot(forecast(gnpreal.arima, h=10))


## ----out.height="60%", out.width='60%', fig.align="center"--------------------------------------
USExp.diff2 <- tibble(time=1:(nrow(USExp)-2), diff= diff(as.data.frame(USExp)[, "logExp"], diff=2))


## ----out.height="60%", out.width='60%', fig.align="center"--------------------------------------
ggplot(USExp.diff2, aes(x = time, y = diff)) +
  geom_line(color = "blue", size = 0.5)


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------
USExp.diff2 %>% as_tsibble(index=time) %>% ACF(diff) %>% autoplot()


## ----out.width='60%', out.height='60%', fig.align="center"--------------------------------------
USExp.diff2 %>% as_tsibble(index=time) %>% PACF(diff) %>% autoplot()


## -----------------------------------------------------------------------------------------------
arimod <- auto.arima(USExp[,"logExp"])
arimod


## ----out.width='80%', out.height='60%', fig.align="center"--------------------------------------
checkresiduals(arimod, test=FALSE)


## ----out.width='80%', out.height='60%', fig.align="center"--------------------------------------
checkresiduals(arimod, plot=FALSE)


## -----------------------------------------------------------------------------------------------
forecast(arimod, h=24)


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------
plot(forecast(arimod, h=24))


## -----------------------------------------------------------------------------------------------
gdpunemp <- read.csv('../data/q-gdpunemp.txt', sep=' ')
head(gdpunemp)


## -----------------------------------------------------------------------------------------------
gdpunemp$loggdp <- log(gdpunemp$gdp)
gdpunemp <- ts(gdpunemp[, c("loggdp", "rate")], start=1948, frequency = 4)
head(gdpunemp)


## -----------------------------------------------------------------------------------------------
gdpunemp_tsbl <- as_tsibble(gdpunemp)
head(gdpunemp_tsbl)


## ----fig.asp=0.3, fig.align='center', size='tiny'-----------------------------------------------
g1 <- gdpunemp_tsbl %>% filter(key=="loggdp") %>% 
  ggplot() + geom_line(aes(x=index, y=log(value)), color="blue")
g2 <- gdpunemp_tsbl %>% filter(key=="rate") %>% 
  ggplot() + geom_line(aes(x=index, y=value), color="green")
g1 + g2


## ----fig.dim=c(12,4), out.height='50%', fig.align='center'--------------------------------------
gdpunemp_i <- diff(gdpunemp) %>% as_tsibble()
g1 <- gdpunemp_i %>% filter(key=="loggdp") %>% 
  ggplot() + geom_line(aes(x=index, y=value), color="blue") + ylab("Log GDP (diff)")
g2 <- gdpunemp_i %>% filter(key=="rate") %>% 
  ggplot() + geom_line(aes(x=index, y=value), color="green") + ylab("Unemp. rate (diff)")
g1 + g2


## -----------------------------------------------------------------------------------------------
library(MTS)
library(mvtnorm)
sig=diag(2)
x=rmvnorm(300,rep(0,2),sig)
head(x)


## ----fig.dim=c(10,6), out.height='60%', fig.align='center'--------------------------------------
stmv <- ts(x)
plot(stmv)


## -----------------------------------------------------------------------------------------------
ccm(x, lags=2, level=TRUE)


## -----------------------------------------------------------------------------------------------
ccm(diff(gdpunemp), lags=3, level=TRUE)


## ----fig.dim=c(4,4), out.height='60%', fig.align='center'---------------------------------------
diff(gdpunemp) %>% as.data.frame() %>% 
  ggplot() + geom_point(aes(x=loggdp, y=rate), color="blue")


## -----------------------------------------------------------------------------------------------
y <- diff(gdpunemp)
mq(y, lag=10)


## -----------------------------------------------------------------------------------------------
usdebt <- read.table("../data/q-fdebt.txt", header=T)
head(usdebt)


## -----------------------------------------------------------------------------------------------
usdebt <- ts(usdebt[, 3:5], frequency=4, start=c(1970,1))
window(usdebt, start=c(1970,1), end=c(1971,4))
log_usdebt <- log(usdebt)
diff_log_usdebt <- diff(log_usdebt)


## ----out.width='80%', out.height='60%'----------------------------------------------------------
library(patchwork)
g1 <- as_tsibble(usdebt) %>% autoplot()
g2 <- as_tsibble(log_usdebt) %>% autoplot()
g3 <- as_tsibble(diff_log_usdebt) %>% autoplot()
g1+g2+g3


## ----fig.dim=c(8,8), out.height='60%', fig.align='center'---------------------------------------
ccm(as.data.frame(diff_log_usdebt), lag=5)


## ----out.width='80%', out.height='60%'----------------------------------------------------------
ccm(as.data.frame(diff_log_usdebt), lag=5)


## -----------------------------------------------------------------------------------------------
varpib <- read.table("../data/q-gdp-ukcaus.txt", header=T)
varpib <- log(varpib[,3:5])
head(varpib)


## -----------------------------------------------------------------------------------------------
varpib_ts <- ts(varpib, start=c(1980,1), frequency=4)
pibgr <- diff(varpib_ts)*100
head(pibgr)


## ----fig.dim=c(6,4), out.height='60%', fig.align='center'---------------------------------------
pibgr %>% as_tsibble() %>% autoplot()


## -----------------------------------------------------------------------------------------------
pibuk.df <- ur.df(pibgr[, "uk"], type = "drift", lags = 1)


## -----------------------------------------------------------------------------------------------
attr(pibuk.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(pibuk.df, "cval")


## ----message=FALSE------------------------------------------------------------------------------
library("vars")
pibgr.VAR <- VAR(pibgr, p=2)


## ----message=FALSE------------------------------------------------------------------------------
names(pibgr.VAR)


## -----------------------------------------------------------------------------------------------
summary(pibgr.VAR)$varresult$uk


## -----------------------------------------------------------------------------------------------
summary(pibgr.VAR)$varresult$ca


## -----------------------------------------------------------------------------------------------
summary(pibgr.VAR)$varresult$us


## -----------------------------------------------------------------------------------------------
VARselect(pibgr, lag.max = 10)


## ----fig.dim=c(10,4), out.height='60%', fig.align='center'--------------------------------------
plot(VARselect(pibgr, lag.max = 10)$criteria[1,], xlab='p', ylab='AIC')


## -----------------------------------------------------------------------------------------------
library(MTS)
m1.mts <- MTS::VAR(pibgr,2)


## -----------------------------------------------------------------------------------------------
m2.mts <- refVAR(m1.mts, thres=1.96)


## -----------------------------------------------------------------------------------------------
m2.mts$aic


## -----------------------------------------------------------------------------------------------
pibgr.VAR.serial <- serial.test(pibgr.VAR, type="PT.asymptotic")
pibgr.VAR.serial


## -----------------------------------------------------------------------------------------------
MTSdiag(m2.mts, gof=4, adj=12)


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
plot(pibgr.VAR.serial, names="uk")


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
plot(pibgr.VAR.serial, names="ca")


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
plot(pibgr.VAR.serial, names="us")


## -----------------------------------------------------------------------------------------------
pibgr.VAR.norm <- normality.test(pibgr.VAR, multivariate.only = TRUE)
pibgr.VAR.norm


## -----------------------------------------------------------------------------------------------
pibgr.VAR.arch <- arch.test(pibgr.VAR, lags.multi = 5, multivariate.only = TRUE)
pibgr.VAR.arch


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
plot(predict(pibgr.VAR))


## -----------------------------------------------------------------------------------------------
reccusum <- stability(pibgr.VAR, type = "OLS-CUSUM")


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
plot(reccusum)


## -----------------------------------------------------------------------------------------------
causality(pibgr.VAR, cause=c("us", "ca"))


## -----------------------------------------------------------------------------------------------
causality(pibgr.VAR, cause=c("uk", "ca"))


## -----------------------------------------------------------------------------------------------
causality(pibgr.VAR, cause=c("uk", "us"))


## ----message=FALSE------------------------------------------------------------------------------
data("Canada")
window(Canada, start=c(1980,1), end=c(1981,4))


## ----fig.dim=c(8,4), out.height='60%', fig.align='center'---------------------------------------
plot(Canada, nc = 2, xlab = "")


## -----------------------------------------------------------------------------------------------
prod.adf1 <- ur.df(Canada[, "prod"], type = 'trend', lags = 2)


## -----------------------------------------------------------------------------------------------
attr(prod.adf1, "teststat")


## -----------------------------------------------------------------------------------------------
attr(prod.adf1, "cval")


## -----------------------------------------------------------------------------------------------
prod.diff <- diff(Canada[, "prod"])
prod.diff.adf1 <- ur.df(prod.diff, type = "drift", lags = 1)


## -----------------------------------------------------------------------------------------------
attr(prod.diff.adf1, "teststat")


## -----------------------------------------------------------------------------------------------
attr(prod.diff.adf1, "cval")


## -----------------------------------------------------------------------------------------------
adf.test(diff(Canada[, "prod"]), k = 1)


## -----------------------------------------------------------------------------------------------
VARselect(Canada, lag.max = 8, type = "both")


## -----------------------------------------------------------------------------------------------
p1ct <- VAR(Canada, p = 1, type = 'both')


## -----------------------------------------------------------------------------------------------
summary(p1ct, equation = "e")$varresult


## ----out.width="60%", out.height='60%', fig.align="center"--------------------------------------
plot(p1ct, names = "e")


## -----------------------------------------------------------------------------------------------
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial


## -----------------------------------------------------------------------------------------------
ccm(residuals(p1ct))


## -----------------------------------------------------------------------------------------------
p1ct.irf.uk <- irf(pibgr.VAR, impulse='uk', boot=FALSE)
p1ct.irf.uk


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
plot(irf(pibgr.VAR, impulse="uk", ortho=FALSE), plot.type="multiple")


## ----fig.dim=c(10,6), out.width='70%', fig.align='center'---------------------------------------
plot(irf(pibgr.VAR, impulse="uk", cumulative = TRUE), plot.type="multiple")


## ----out.width='20%', out.height='40%', fig.align='center'--------------------------------------
plot(irf(pibgr.VAR, cumulative = FALSE, ortho = TRUE), plot.type="single")


## ----results='hide'-----------------------------------------------------------------------------
m1 <- MTS::VAR(pibgr, 2)
m2 <- refVAR(m1, thres=1.96)


## -----------------------------------------------------------------------------------------------
Phi <- m2$Phi
Phi


## -----------------------------------------------------------------------------------------------
Sig <- m2$Sigma
Sig


## ----fig.dim=c(10,6), out.height='60%', fig.align='center'--------------------------------------
m2.irf <- VARMAirf(Phi=Phi, Sigma=Sig)
m2.irf$psi


## -----------------------------------------------------------------------------------------------
pibgr.VAR.fevd <- fevd(pibgr.VAR)
pibgr.VAR.fevd$uk


## ----fig.dim=c(12,6), out.width='70%', fig.align='center'---------------------------------------
plot(pibgr.VAR.fevd, nc=3)


## ----out.width='60%', out.height='50%', fig.aling='center'--------------------------------------
library(lmtest)

set.seed(123456)
e1 <- rnorm(500)
e2 <- rnorm(500)
trd <- 1:500
y1 <- 0.8 * trd + cumsum(e1)
y2 <- 0.6 * trd + cumsum(e2)
spdata <- data.frame(index=1:500, y1,y2)


## ----out.width='60%', out.height='50%', fig.align='center'--------------------------------------
ggplot(spdata) + 
  geom_line(aes(x=index, y=y1), color="blue") +
  geom_line(aes(x=index, y=y2), color="red")


## -----------------------------------------------------------------------------------------------
sr.reg <- lm(y1 ~ y2)
sr.dw <- dwtest(sr.reg)$statistic
summary(sr.reg)


## ----out.width='60%', out.height='50%', fig.align='center'--------------------------------------
set.seed (123456)
e1 <- rnorm(100)
e2 <- rnorm(100)
y1 <- cumsum(e1)
y2 <- 0.6 * y1 + e2

g1 <- ggplot() + geom_line(aes(x=1:100, y=y1), color="blue") + xlab("t")
g2 <- ggplot() + geom_line(aes(x=1:100, y=y2), color='red') + xlab("t")
g1+g2


## -----------------------------------------------------------------------------------------------
lr.reg <- lm(y2 ~ y1)
summary(lr.reg)


## -----------------------------------------------------------------------------------------------
errors <- residuals(lr.reg)
errors.df <- ur.df(y=errors, lags=1, type='drift')


## -----------------------------------------------------------------------------------------------
attr(errors.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(errors.df, "cval")


## ----out.width='60%', out.height='60%', fig.align='center'--------------------------------------
ggplot() + geom_line(aes(x=1:100, y=errors), color="red")


## -----------------------------------------------------------------------------------------------
errors.lagged <- errors[- c(99, 100)]
dy1 <- diff(y1)
dy2 <- diff(y2)
diff.dat <- data.frame(embed(cbind(dy1,dy2) , 2))
colnames(diff.dat) <- c('dy1' , 'dy2' , 'dy1.1' , 'dy2.1')
head(diff.dat)


## -----------------------------------------------------------------------------------------------
ecm.reg <- lm(dy2 ~ errors.lagged + dy1.1 + dy2.1,
  data= diff.dat)
summary(ecm.reg)


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
USIncExp2 <- window(USIncExp, start = c(1985,12))
USIncExp2 %>% as_tsibble() %>% ggplot() + geom_line(aes(x=index, y=value, color=key))


## -----------------------------------------------------------------------------------------------
USIncExp.mod1 <- lm(expenditure ~ income, data = USIncExp2)
summary(USIncExp.mod1)


## -----------------------------------------------------------------------------------------------
coint.res <- residuals(lm(expenditure ~ income, data = USIncExp2))
coint.res.df <- ur.df(y=coint.res, lags=1, type='drift')


## -----------------------------------------------------------------------------------------------
attr(coint.res.df, "teststat")


## -----------------------------------------------------------------------------------------------
attr(coint.res.df, "cval")


## -----------------------------------------------------------------------------------------------
coint.res <- stats::lag(ts(coint.res, start = c(1985,12), freq = 12), k = -1)
USIncExp2 <- cbind(USIncExp2, diff(USIncExp2), coint.res)
USIncExp2 <- window(USIncExp2, start = c(1986,1), end = c(2001,2))
colnames(USIncExp2) <- c("income", "expenditure", "diff.income", "diff.expenditure", "coint.res")
window(USIncExp2, start=c(1986,1), end=c(1986,12))


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
plot(USIncExp2[,c("diff.income", "diff.expenditure", "coint.res")])


## -----------------------------------------------------------------------------------------------
ecm.model <- diff.expenditure ~ coint.res + diff.income
summary(lm(ecm.model, data=USIncExp2))


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
ocus <- efp(ecm.model, type="OLS-CUSUM", data=USIncExp2)
plot(ocus)


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
me <- efp(ecm.model, type="ME", data=USIncExp2, h=0.2)
plot(me)


## ----size='tiny'--------------------------------------------------------------------------------
data("GermanM1")
head(GermanM1)


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
GermanM1.ts <- ts(GermanM1[, c("m","p", "y", "R")], start=c(1960,1), frequency=4)
plot(GermanM1.ts)


## ----size='tiny'--------------------------------------------------------------------------------
LTW.model <- dm ~ dy2 + dR + dR1 + dp + m1 + y1 + R1 + season


## ----size='tiny'--------------------------------------------------------------------------------
LTW.model.res <- summary(lm(LTW.model, data=historyM1))
LTW.model.res$coefficients


## -----------------------------------------------------------------------------------------------
LTW.model.res$adj.r.squared


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
ols <- efp(LTW.model, data = GermanM1, type = "OLS-CUSUM")
plot(ols)


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
re <- efp(LTW.model, data = GermanM1, type = "RE")
plot(re)


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
fs <- Fstats(LTW.model, data = GermanM1, from = 0.1)
plot(fs)


## ----size='tiny'--------------------------------------------------------------------------------
M1.model <- dm ~ dy2 + dR + dR1 + dp + ecm.res + season
summary(lm(M1.model, data=historyM1))


## -----------------------------------------------------------------------------------------------
M1 <- historyM1
ols.efp <- efp(M1.model, type = "OLS-CUSUM", data = M1)
ols.mefp <- mefp(ols.efp)


## -----------------------------------------------------------------------------------------------
M1 <- GermanM1
ols.mon <- monitor(ols.mefp)
ols.mon


## ----out.height='60%', out.width='60%', fig.align='center'--------------------------------------
plot(ols.mon)

