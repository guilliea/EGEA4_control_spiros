# Script permettant de déterminer le nombre de participants par centre + quelques
# stats liées à la qualité

library(readxl)
library(dplyr)
library(haven)
library(lubridate)
library(rspiro)
library(ggplot2)
library(readr)
library(tidyr)

# Import data ----

data <- readRDS("data/data_fin/data_spiros_egea4_20260106.rds") %>%
  mutate(Centre = factor(Centre, levels = c("Paris", "Montpellier", "Lyon", "Marseille", "Grenoble")))

# Data-management ----

# On crée une base avec une ligne courbe par individu (la "meilleure" courbe quand spiro ok, sinon
# juste la ligne du CRF)
# On rajoute le sujet 770118101  pour qui on a les valeurs de la courbe mais pas d'analyse
# de qualité
data %>% select(nodos) %>% pull %>% unique() %>% length

data_unique <- data %>%
  filter(is.na(spiro) | Numero_courbe == Best_curve | (nodos == 770118101 & Numero_courbe == 1))

table(data_unique$eCRF == 1 & data_unique$spiro == 1 & data_unique$quali == 1)
table(data_unique$eCRF == 1 & data_unique$spiro == 1 & is.na(data_unique$quali))
table(data_unique$eCRF == 1 & is.na(data_unique$spiro) & is.na(data_unique$quali))
data_unique %>% filter(eCRF == 1 & is.na(spiro) & is.na(quali)) %>% select(Centre) %>% table
table(data_unique$eCRF == 1 & is.na(data_unique$spiro) & data_unique$quali == 1)
table(is.na(data_unique$eCRF) & is.na(data_unique$spiro) & data_unique$quali == 1)
table(is.na(data_unique$eCRF) & data_unique$spiro == 1 & data_unique$quali == 1)
data_unique %>% filter(is.na(eCRF) & spiro == 1 & quali == 1) %>% select(Centre) %>% table
table(is.na(data_unique$eCRF) & data_unique$spiro == 1 & is.na(data_unique$quali))

# Export 
data_unique %>%
  select(nodos, Centre, date_CRF, date_spiro, date_quali, eCRF, spiro, quali) %>%
  rio::export("data/export/data_export_spiro_260106.xlsx")

# Stats et analyse qualité ----

# Uniquement sur les participants avec spiros
data_spiros <- data %>%
  filter(spiro == 1)

## Nombre participants ----

# Total
data_spiros %>% select(nodos) %>% pull %>% unique() %>% length
# Par centre
n_centre <- data_spiros %>%
  distinct(nodos, Centre) %>%
  group_by(Centre) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre

## ≥1 courbe GOOD ----

# Total
n_1good <- data_spiros %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_1good
#Par centre
n_centre_1good <- data_spiros %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, Centre) %>%
  group_by(Centre) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_1good

## ≥2 courbes GOOD ----

# Total
n_2good <- data %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_2good
#Par centre
n_centre_2good <- data %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(Centre) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2good

## ≥2 courbes GOOD + repro ----

# Total
n_2repro_good <- data_spiros %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_2repro_good
#Par centre
n_centre_2repro_good <- data_spiros %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(Centre) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2repro_good

## ≥3 courbes GOOD ----

# Total
n_3good <- data %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_3good
#Par centre
n_centre_3good <- data %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(Centre) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3good

## ≥3 courbe GOOD + repro ----

# Total
n_3repro_good <- data_spiros %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_3repro_good
#Par centre
n_centre_3repro_good <- data_spiros %>%
  filter(!is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(Centre) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3repro_good

tab <- cbind(levels(data_spiros$Centre), n_centre, n_centre_1good, round(n_centre_1good/n_centre*100, 0),
             n_centre_2good, round(n_centre_2good/n_centre*100, 0),
             n_centre_2repro_good, round(n_centre_2repro_good/n_centre*100, 0),
             n_centre_3good, round(n_centre_3good/n_centre*100, 0),
             n_centre_3repro_good, round(n_centre_3repro_good/n_centre*100, 0))
colnames(tab) <- c("Centre", "n total", "n avec ≥1 spiro bonne qualité", "% avec ≥1 spiro bonne qualité", 
                   "n avec ≥2 spiros bonne qualité", "% avec ≥2 spiros bonne qualité",
                   "n avec ≥2 spiros bonne qualité et reproductibles", "% avec ≥2 spiros bonne qualité et reproductibles",
                   "n avec ≥3 spiros bonne qualité", "% avec ≥3 spiros bonne qualité",
                   "n avec ≥3 spiros bonne qualité et reproductibles", "% avec ≥3 spiros bonne qualité et reproductibles")

rio::export(tab, "res/02_analyse_qualite/Resume_quali_par_centre_20251204.xlsx")

rm(n_1good, n_2good, n_2repro_good, n_3good, n_3repro_good, n_centre,
   n_centre_1good, n_centre_2good, n_centre_2repro_good, n_centre_3good,
   n_centre_3repro_good, tab)



## Selon la date d'inclusion ----

# On sépare selo la date du dernier séminaire où il y a eu une "formation" spiro
data_spiros <- data_spiros %>% mutate(
  date_rec = ifelse(data_spiros$date_quali < "2025-04-01" , 0, ifelse(data_spiros$date_quali >= "2025-04-01", 1, NA))
)
table(data_spiros$date_rec, useNA = "ifany")

### Total ----

# Nombre de sujets total
n_tot <- data_spiros %>% filter(!is.na(date_rec) & !is.na(decision)) %>% distinct(nodos, date_rec) %>% group_by(date_rec) %>% summarise(nombre_participants = n()) %>% select(nombre_participants) %>% pull

# Nombre de sujets par centre
n_centre <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  distinct(nodos, date_rec, Centre) %>%
  group_by(date_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

### ≥1 courbe GOOD ----

# Nombre de sujets total avec au moins une courbe de bonne qualité
n_1good <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, date_rec) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

# Nombre de sujets par centre avec au moins une courbe de bonne qualité
n_1good_centre <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, date_rec, Centre) %>%
  group_by(date_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

### ≥2 courbes GOOD ----

# Nombre de sujets total avec au moins 2 courbes de bonne qualité
n_2good <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité
n_2good_centre <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, Centre, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec, Centre) %>%
  summarise(nombre_participants = n())%>% as.data.frame

### ≥2 courbes GOOD + repro ----

# Nombre de sujets total avec au moins 2 courbes de bonne qualité et reproductible sur VEMS+CVF
n_2good_repro <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité et reproductible sur VEMS+CVF
n_2good_repro_centre <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos, date_rec, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

### ≥3 courbes GOOD ----

# Nombre de sujets total avec au moins 3 courbes de bonne qualité
n_3good <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité
n_3good_centre <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, Centre, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

### ≥3 courbes GOOD + repro ----

# Nombre de sujets total avec au moins 3 courbes de bonne qualité et reproductible sur VEMS+CVF
n_3good_repro <- data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité et reproductible sur VEMS+CVF
n_3good_repro_centre <-  data_spiros %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos, date_rec, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame

### Summary - Barplot for all ----

var_quali <- c("≥1 courbe de bonne qualité",
               "≥2 courbes de bonne qualité", "≥2 courbes de bonne qualité + reproductibilité",
               "≥3 courbes de bonne qualité", "≥3 courbes de bonne qualité + reproductibilité")

data_barplot <- data.frame(
  var = rep(var_quali, 2),
  valeurs = c(n_1good[1,2]/n_tot[1]*100, n_2good[1,2]/n_tot[1]*100, n_2good_repro[1,2]/n_tot[1]*100, n_3good[1,2]/n_tot[1]*100, n_3good_repro[1,2]/n_tot[1]*100,
              n_1good[2,2]/n_tot[2]*100, n_2good[2,2]/n_tot[2]*100, n_2good_repro[2,2]/n_tot[2]*100, n_3good[2,2]/n_tot[2]*100, n_3good_repro[2,2]/n_tot[2]*100),
  time = c(rep("Avant séminaire", 5), rep("Après séminaire", 5))) %>%
  mutate(var = factor(var, levels = rev(unique(var_quali))))

p <- ggplot(data=data_barplot, aes(x = valeurs, y = var, fill = time)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw(base_size = 10) +
  ylab("") +
  xlab("Fréquence (%)") +
  # Permet d'obtenir une légende dans le même ordre que celui affiché dans le graphique
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme(axis.text.x = element_text(size=10),  
        axis.text.y = element_text(size = 10, hjust = 0), #hjust permet d'aligner tous les éléments à gauche
        legend.text = element_text(size = 10),
        legend.title = element_blank()) # supprimer titre de la légende

ggsave(p, filename = "res/02_analyse_qualite/Qualite_avant_apres_seminaire_20251007.png", dpi = 300, width = 1000, height = 600, scale = 1/90)

### Par centre ----

data_barplot <- data.frame(
  var = rep(var_quali, each = 9),
  centre = rep(n_1good_centre[, 2], 5),
  time = rep(n_1good_centre[, 1], 5),
  valeurs = c(n_1good_centre[,3]/n_centre[,3]*100,
              n_2good_centre[,3]/n_centre[,3]*100,
              n_2good_repro_centre[,3]/n_centre[,3]*100,
              n_3good_centre[, 3]/n_centre[,3]*100,
              n_3good_repro_centre[, 3]/n_centre[,3]*100)
  ) %>%
  mutate(var = factor(var, levels = rev(unique(var_quali))),
         time = factor(time, levels = 1:0, labels = c("Après séminaire", "Avant séminaire")))

# Uniquement en fonction du centre (pas avant vs après sémianire)
p <- ggplot(data=data_barplot, aes(x = valeurs, y = var, fill = Centre)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw(base_size = 10) +
  ylab("") +
  xlab("Fréquence (%)") +
  # Permet d'obtenir une légende dans le même ordre que celui affiché dans le graphique
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme(axis.text.x = element_text(size=10),  
        axis.text.y = element_text(size = 10, hjust = 0), #hjust permet d'aligner tous les éléments à gauche
        legend.text = element_text(size = 10),
        legend.title = element_blank()) # supprimer titre de la légende

p

# Par centre, avant vs après séminaire
p <- ggplot(data=data_barplot, aes(x = valeurs, y = var, fill = time)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw(base_size = 10) +
  ylab("") +
  xlab("Fréquence (%)") +
  facet_grid(. ~ centre, scales = "free")  +
  # Permet d'obtenir une légende dans le même ordre que celui affiché dans le graphique
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme(axis.text.x = element_text(size=10),  
        axis.text.y = element_text(size = 10, hjust = 0), #hjust permet d'aligner tous les éléments à gauche
        legend.text = element_text(size = 10),
        legend.title = element_blank()) # supprimer titre de la légende

p
ggsave(p, filename = "res/02_analyse_qualite/Qualite_avant_apres_seminaire_par_centre_20251007.png", dpi = 300, width = 1200, height = 600, scale = 1/90)

rm(data_barplot, n_1good, n_1good_centre, n_2good, n_2good_centre,
   n_2good_repro, n_2good_repro_centre, n_3good, n_3good_centre,
   n_3good_repro, n_3good_repro_centre, p, n_tot, var_quali, n_centre)


## Selon l'âge des patients ----

data_spiros <- data_spiros %>% mutate(
  age_rec = ifelse(age <= 65, 0, ifelse(age >65, 1, NA))
)

### Total ----

# Nombre de sujets par centre
n_tot <- data_spiros %>% filter(!is.na(age_rec) & !is.na(decision)) %>% distinct(nodos, age_rec) %>% group_by(age_rec) %>% summarise(nombre_participants = n()) %>% select(nombre_participants) %>% pull

# Nombre de sujets par centre
n_centre <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  distinct(nodos, age_rec, Centre) %>%
  group_by(age_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame
n_centre

### ≥1 courbe GOOD ----

# Nombre de sujets avec au moins une courbe de bonne qualité
n_1good <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, age_rec) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  as.data.frame()
n_1good

# Nombre de sujets par centre avec au moins une courbe de bonne qualité
n_centre_1good <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, age_rec, Centre) %>%
  group_by(age_rec, Centre) %>%
  summarise(nombre_participants = n()) %>%
  as.data.frame()
n_centre_1good

### ≥2 courbes GOOD ----

# Nombre de sujets avec au moins 2 courbes de bonne qualité
n_2good <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame()
n_2good

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité
n_2good_centre <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, age_rec, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame()
n_2good_centre

### ≥2 courbes GOOD + repro ----

# Nombre de sujets total avec au moins 2 courbes de bonne qualité et reproductible sur VEMS+CVF
n_2good_repro <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame
n_2good_repro

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité et reproductible sur VEMS+CVF
n_2good_repro_centre <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos, age_rec, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame
n_2good_repro_centre

### ≥3 courbes GOOD ----

# Nombre de sujets avec au moins 3 courbes de bonne qualité
n_3good <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  as.data.frame()
n_3good

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité
n_3good_centre <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, age_rec, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec, Centre) %>%
  summarise(nombre_participants = n()) %>%
  as.data.frame()
n_3good_centre

### ≥3 courbes GOOD + repro ----

# Nombre de sujets total avec au moins 3 courbes de bonne qualité et reproductible sur VEMS+CVF
n_3good_repro <- data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>% as.data.frame
n_3good_repro

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité et reproductible sur VEMS+CVF
n_3good_repro_centre <-  data_spiros %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos, age_rec, Centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec, Centre) %>%
  summarise(nombre_participants = n()) %>% as.data.frame
n_3good_repro_centre

### Summary - Barplot for all ----

var_quali <- c("≥1 courbe de bonne qualité",
               "≥2 courbes de bonne qualité", "≥2 courbes de bonne qualité + reproductibilité",
               "≥3 courbes de bonne qualité", "≥3 courbes de bonne qualité + reproductibilité")

data_barplot <- data.frame(
  var = rep(var_quali, 2),
  valeurs = c(n_1good[1,2]/n_tot[1]*100, n_2good[1,2]/n_tot[1]*100, n_2good_repro[1,2]/n_tot[1]*100, n_3good[1,2]/n_tot[1]*100, n_3good_repro[1,2]/n_tot[1]*100,
              n_1good[2,2]/n_tot[2]*100, n_2good[2,2]/n_tot[2]*100, n_2good_repro[2,2]/n_tot[2]*100, n_3good[2,2]/n_tot[2]*100, n_3good_repro[2,2]/n_tot[2]*100),
  time = c(rep("<65 ans", 5), rep("≥65 ans", 5))) %>%
  mutate(var = factor(var, levels = rev(unique(var_quali))))

p <- ggplot(data=data_barplot, aes(x = valeurs, y = var, fill = time)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw(base_size = 10) +
  ylab("") +
  xlab("Fréquence (%)") +
  # Permet d'obtenir une légende dans le même ordre que celui affiché dans le graphique
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme(axis.text.x = element_text(size=10),  
        axis.text.y = element_text(size = 10, hjust = 0), #hjust permet d'aligner tous les éléments à gauche
        legend.text = element_text(size = 10),
        legend.title = element_blank()) # supprimer titre de la légende

ggsave(p, filename = "res/02_analyse_qualite/Qualite_selon_age_20251007.png", dpi = 300, width = 1000, height = 600, scale = 1/90)

rm(data_barplot, n_1good, n_1good_centre, n_2good, n_2good_centre,
   n_2good_repro, n_2good_repro_centre, n_3good, n_3good_centre,
   n_3good_repro, n_3good_repro_centre, p, n_tot, var_quali, n_centre)

# Base best curve ----

data_best <- data %>%
  filter(Numero_courbe == Best_curve & decision != "BAD")

summary(data_best)
table(data_best$decision, useNA = "ifany")

# Histogramme valeurs spiros ----

plot_CVFp100 <- ggplot(data_best, aes(x = CVF_p100)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +  # Histogramme
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", size = 1) +  # Ligne verticale
  labs(
    x = "Capacité Vitale Forcée (%théorique)",
    y = "Fréquence"
  ) +
  theme_bw() 
ggsave("res/01_stats_descriptives/Hist_CVF_EGEA4.png", plot = plot_CVFp100, width = 8, height = 6, dpi = 300)

plot_VEMSp100 <- ggplot(data_best, aes(x = VEMS_p100)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +  # Histogramme
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", size = 1) +  # Ligne verticale
  labs(
    x = "Volume Expiratoire Maximal en 1s (%théorique)",
    y = "Fréquence"
  ) +
  theme_bw() 
ggsave("res/01_stats_descriptives/Hist_VEMS_EGEA4.png", plot = plot_VEMSp100, width = 8, height = 6, dpi = 300)

rm(plot_VEMSp100, plot_CVFp100)

