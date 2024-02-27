# Importation de fichiers csv - Données PIB

library(tidyverse)
pib <- read_csv2("../data/pib_fr.csv")


# Aperçu du jeu de données

class(pib)
head(pib)

# Liste des séries:
# PIB = Produit intérieur brut
# P7 = Importations
# P3M = Dépenses de consommation des ménages
# P3P = Dépenses de consommation des ISBLSM
# P31G = Dépenses de consommation des APU (individual.)
# P32G = Dépenses de consommation des APU (collectives)
# P3 = Dépenses de consommation totale
# P51S = FBCF (formation brute de capital fixe) des entreprises non financières
# P51B = FBCF (formation brute de capital fixe) des entreprises financières
# P51G = FBCF (formation brute de capital fixe) des APU
# P51G = FBCF (formation brute de capital fixe) des ménages
# P51P = FBCF (formation brute de capital fixe) des ISBLSM
# P51 = FBCF (formation brute de capital fixe) des total
# P54 = Variations de stock
# P6 = Exportations

# Extraction de la date
# On extrait l'information sur l'année et le trimestre à partir de la colonne `PERIODE`

pib <- pib %>% mutate(ANNEE=substring(PERIODE, 1, 4), TRIMESTRE=substring(PERIODE, 6, 7))
pib %>% dplyr::select("PIB", "ANNEE", "TRIMESTRE")

# Transformation des données `pib` en objet `ts`

pib_ts <- ts(pib[,2:(ncol(pib)-2)], frequency=4,start=c(1949,1))
window(pib_ts, start=2022, end=2023)


# Création d'un objet `tsibble` - Données `pib`

library(tsibble)

pib_tbl_long <- pib %>% 
  mutate(ANNEE=as.numeric(ANNEE), TRIMESTRE=as.numeric(TRIMESTRE)) %>%   
  mutate(index=make_yearquarter(year=ANNEE, quarter=TRIMESTRE)) %>%
  dplyr::select(-ANNEE, -TRIMESTRE, -PERIODE) %>% 
 pivot_longer(cols=c(PIB, P7, P3M, P3P, P31G, P32G, P3, P51S, P51B, P51G, P51M, P51P, P51, P54, P6), names_to="key")
pib_tbl_long

pib_tsbl <- pib_tbl_long %>% as_tsibble(index=index, key=key)
pib_tsbl %>% filter(index > yearquarter("2022 Q1") & key=="PIB") 


# Représenter des séries temporelles

plot(pib_ts[,c("PIB", "P7")],xlab="Trimestre",ylab="PIB", main="Données PIB")


# Chronogramme - Série univariée - PIB

pib_tsbl %>% filter(key=="PIB") %>%
  ggplot(aes(x = index, y = value))+
    geom_line(color = "#00AFBB", size = 1)

# Chronogramme - PIB - Echelle logarithmique

pib_tsbl %>% filter(key=="PIB") %>%
  ggplot(aes(x = index, y = log(value))) +
    geom_line(color = "#00AFBB", size = 1)

## Chronogramme - Données `pib` - Série multivariée

pib_tsbl %>%
  filter(key %in% c("PIB", "P3M", "P7","P54")) %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(aes(color = key), size = 1) +
    scale_color_manual(values = c("red", "blue", "green","yellow"))

# Fonction d'autocorrélation - Données `pib`

library(feasts)
pib_tsbl %>% filter(key=="PIB") %>% 
  ACF(value) %>% autoplot() + theme_minimal()

# Simulez un bruit blanc Gausssien

bbg <- ts(rnorm(1000, mean=0, sd=3))
window(bbg, start=1, end=10)

library(ggfortify)
autoplot(bbg)+ 
  ggtitle("White Noise")

# Simulez une marche aléatoire

tmax <- 100
wnoise <- rnorm(99, mean=0, sd=0.41)
y <- rep(0,tmax)
y[1] = 1.39

for (t in 2:tmax) {
  y[t] = y[t-1] + wnoise[t-1] 
}
rw <- tibble(time=1:tmax, y=y)
rw

ggplot(rw) + geom_line(aes(x=time, y=y), colour="blue")


# Fonction d'autocorrélation

acf(rw["y"])

# Test de Portmanteau

box_pierce(rw["y"])

# Représentez la série du PIB

# Chronogramme

pib_tsbl %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(aes(color = key), size = 1)

pib_tsbl %>% filter(key=="P54") %>%
  ggplot(aes(x = index, y = value)) + 
    geom_line(aes(color = key), size = 1)

## Season plot

pib_tsbl %>% filter(key=="PIB" & index>yearquarter("2000 Q1")) %>% gg_season()

# Lag plot

pib_tsbl %>% filter(key=="PIB" & index>yearquarter("2000 Q1")) %>% gg_lag()
pib_tsbl %>% filter(key=="P54" & index>yearquarter("2000 Q1")) %>% gg_lag(lags=12)


# ACF

pib_tsbl %>% filter(key=="PIB") %>% ACF() %>% autoplot()


pib_tsbl %>% filter(key=="P54") %>% ACF() %>% autoplot()


