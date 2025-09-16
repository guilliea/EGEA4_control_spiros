# Script permettant de créer la base de données avec les valeurs de spiros
# Une base sera faite avec toutes les courbes (permet de regarder la reproductibilité)
# puis la base finale sera construite en ne gardant que la meilleure courbe de chaque individu (participants
# avec uniquement des courbes de mauvaises qualité seront exclus)

# Dans ce script les données des trois sources (eCRF, valeurs spiros,
# et analyse de la qualité) sont fusionnées puis les %theo et les
# z-score sont calculés

library(readxl)
library(dplyr)
library(haven)
library(lubridate)
library(rspiro)
library(ggplot2)
library(readr)
library(tidyr)

# Import data ----
data_quali <- readRDS("data/tmp/data_qualit.rds")
data_spiro_Grenoble <- read_delim("data/Spiros/Spiromètrie_Grenoble_250902/Export spiro 140825.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
data_spiro_autre <- read_excel("data/Spiros/Spiros_EGEA4_250902.xlsx")
load("~/Documents/Projets/EGEA/EGEA_4/EGEA4_control_spiros/data/data_ecrf/ecrf_alicia_250902.rdata")
data_eCRF <- x
rm(x)

# Data management ----

## Données eCRF ----

data_eCRF$`1. Date_de_naissance`[data_eCRF$nodos == 410077202] <- "10/1959"
data_eCRF$`1. Date_de_naissance`[data_eCRF$nodos == 710226303] <- "11/1960"
data_eCRF$`1. Date_de_naissance`[data_eCRF$nodos == 310029101] <- "02/1942"
data_eCRF$`1. Date_de_naissance`[data_eCRF$nodos == 310028200] <- "05/1958"
data_eCRF$`1. Date_de_naissance`[data_eCRF$nodos == 770069101] <- "09/1955"

data_eCRF_to_merge <- data_eCRF %>%
  rename(
    Centre_eCRF = "2. Centre",
    date_CRF = "3. Date_de_lexamen",
    poids = "29. Poids_kg",
    taille = "30. Taille_cm",
    Nom_ARC = "114. Personne_ayant_effectue_la_spirometrie",
    Export_ok = "115. Resultats_spirometrie_exportes",
    Donnees_transmises = "116. donnees_spiro_transmises_a_la_coordi",
    Heure_spiro = "117. Heure_spiro",
    CVF_L_eCRF = "118. CVF_l",
    VEMS_L_eCRF = "119. VEMS_l",
    DEP_Ls_eCRF = "120. DEP_ls",
    DEM75_Ls_eCRF = "121. DEM75_ls",
    DEM50_Ls_eCRF = "122. DEM50_ls",
    DEM25_Ls_eCRF = "123. DEM25_ls",
    DEM2575_Ls_eCRF = "124. DEM_25-75_ls",
    Nb_essais_eCRF = "125. Nombre_dessais_effectues_spiro",
    Nb_essais_rejetes_eCRF = "126. Nombre_dessais_rejetes_spiro",
    Res_ok_eCRF = "127. Resultats_et_courbes_debit_volume_a_sauv",
    Res_ok_autre_eCRF = "128. Resultats_et_courbes_debit_volume__Autre",
    Pb_real_eCRF = "129. Problemes_dans_la_realisation_du_test",
    Pb_real_autre_eCRF = "130. Problemes_dans_la_realisation_du_t_Autre",
    Raison_non_fait_eCRF = "131. Si_spiro_non_fait_raison",
    sexe = "219. Sexe",
    Best_curve_ARC_eCRF = "280. Numero_de_courbe_retenue",
    ddn = "1. Date_de_naissance",
    visite_faite = "278. non_concerne_pas_de_visite"
  ) %>%
  mutate(
    date_CRF = as.Date(date_CRF, format = "%d/%m/%Y"),
    ddn = as.Date(paste0("01/", ddn), format = "%d/%m/%Y"),
    age = as.numeric(interval(ddn, date_CRF) / years(1)),
    sexe = ifelse(sexe == "Féminin", 2, ifelse(sexe == "Masculin", 1, NA)),
    eCRF = 1
  )  %>%
  select(nodos, date_CRF, visite_faite, Export_ok, Donnees_transmises, Nb_essais_eCRF, Nb_essais_rejetes_eCRF,
         Res_ok_eCRF, Res_ok_autre_eCRF, Pb_real_eCRF, Pb_real_autre_eCRF, Raison_non_fait_eCRF,
         Best_curve_ARC_eCRF, sexe, age, taille, poids, Centre_eCRF, eCRF)

# Doublons ?
data_eCRF_to_merge %>%
  group_by(nodos) %>%  
  filter(n() > 1) %>%  # Garde uniquement les lignes en double
  ungroup() %>%
  nrow # OK

table(data_eCRF_to_merge$visite_faite, useNA = "ifany")

# On élimine les doublons et les participants n'ayant pas réalisé la spirométrie
data_eCRF_to_merge <- data_eCRF_to_merge %>%
  distinct(nodos, .keep_all = T) %>%
  filter(is.na(visite_faite))

## Données qualité ----

data_quali_to_merge <- data_quali %>%
  select(nodos, date, centre, Numero_courbe, decision) %>%
  rename(date_quali = date, Centre_quali = centre) %>%
  mutate(
    quali = 1,
    # Un participant avec le mauvais nodos à changer
    nodos = ifelse(nodos == 770012202, 770112202, nodos)
  )

# Doublons ?
data_quali_to_merge %>%
  group_by(nodos, Numero_courbe) %>%  
  filter(n() > 1) %>%  # Garde uniquement les lignes en double
  ungroup() %>%
  nrow
  
## Données spiro ----

# Pour Grenoble les données de spiro sont issues directement du logiciel EasyOne (export fait régulièrement
# par Joane et transmis sur le cloud en .csv)
data_spiro_Grenoble$LastName[data_spiro_Grenoble$`Patient ID` == "770032202"] <- "EGEA"
data_spiro_Grenoble_clear <- data_spiro_Grenoble %>%
  filter(LastName %in% c("EGEA", "EGEA4", "Egea", "Egea4")) %>%
  select(`Patient ID`, `Trial Nr`, `Test Date`, FVC, FEV1, PEF, `FEF75%`, `FEF50%`, `FEF25%`, `FEF25-75%`) %>%
  rename(
    nodos = "Patient ID",
    Numero_courbe = "Trial Nr",
    date_spiro = "Test Date",
    CVF_L = "FVC",
    VEMS_L = "FEV1",
    DEP_Ls = "PEF",
    DEM75_Ls = "FEF75%",
    DEM50_Ls = "FEF50%",
    DEM25_Ls = "FEF25%",
    DEM2575_Ls = "FEF25-75%"
  ) %>%
  mutate(
    Centre_spiro = "GRENOBLE",
    # Un participant avec le mauvais nodos à changer
    nodos = ifelse(nodos == 770012202, 770112202, nodos),
    ) %>%
  select(nodos, Numero_courbe, date_spiro, Centre_spiro, CVF_L, VEMS_L, DEP_Ls, DEM75_Ls, DEM50_Ls, DEM25_Ls, DEM2575_Ls)

# Il y a une problème avec le sujet 770118101 qui n'apparait pas dans sqlite général mais uniquement dans
# le fichier Spiro EXT00157 22052
data_spiro_Grenoble2 <- read_delim("data/Spiros/Spiromètrie_Grenoble_250902/Spiro EXT00157 220525.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

data_spiro_Grenoble2 <- data_spiro_Grenoble2 %>%
  filter(`Patient ID` == "770118101") %>%
  select(`Patient ID`, `Trial Nr`, `Test Date`, FVC, FEV1, PEF, `FEF75%`, `FEF50%`, `FEF25%`, `FEF25-75%`) %>%
  rename(
    nodos = "Patient ID",
    Numero_courbe = "Trial Nr",
    date_spiro = "Test Date",
    CVF_L = "FVC",
    VEMS_L = "FEV1",
    DEP_Ls = "PEF",
    DEM75_Ls = "FEF75%",
    DEM50_Ls = "FEF50%",
    DEM25_Ls = "FEF25%",
    DEM2575_Ls = "FEF25-75%"
  ) %>%
  mutate(Centre_spiro = "GRENOBLE") %>%
  select(nodos, Numero_courbe, date_spiro, Centre_spiro, CVF_L, VEMS_L, DEP_Ls, DEM75_Ls, DEM50_Ls, DEM25_Ls, DEM2575_Ls)
table(colnames(data_spiro_Grenoble_clear) == colnames(data_spiro_Grenoble2))

data_spiro_Grenoble_clear <- rbind(data_spiro_Grenoble_clear, data_spiro_Grenoble2)
rm(data_spiro_Grenoble2)

# Verif des doublons
data_spiro_Grenoble_clear %>%
  group_by(nodos, Numero_courbe) %>%  
  filter(n() > 1) %>%  # Garde uniquement les lignes en double
  ungroup() %>% nrow# OK

# On récupère le numéro de la meilleure courbe (identifié visuellement lors du contrôle qualité par Alicia) 
data_spiro_Grenoble_clear <- merge(data_spiro_Grenoble_clear, data_spiro_autre[, c("nodos", "Numero_courbe", "Best_curve")], all.x = T, all.y = F, by = c("nodos", "Numero_courbe")) %>%
  mutate(Best_curve = ifelse(nodos == 770112202, 2, Best_curve))

# Pour Marseille, Lyon, Montpellier et Paris j'ai saisi les données
data_spiro_autre_sansGre <- data_spiro_autre %>%
  rename(date_spiro = Date, Centre_spiro = Centre) %>%
  select(nodos, Numero_courbe, date_spiro, Centre_spiro, CVF_L, VEMS_L, DEP_Ls, DEM75_Ls, DEM50_Ls, DEM25_Ls, DEM2575_Ls, Best_curve) %>%
  filter(Centre_spiro != "GRENOBLE")

# On fusionne les 5 centres
data_spiro_to_merge <- rbind(data_spiro_Grenoble_clear, data_spiro_autre_sansGre) 

# Attention pour un sujet de Grenoble, la courbe n'était pas dans le fichier fourni par Joane lors de l'export
# mais dans un doc PDF à part (car spiro faite en EFR) donc les valeurs sont dans le fichier data_spiro_autre
data_to_add <- data_spiro_autre %>%
  filter(nodos == 710252200) %>%
  rename(date_spiro = Date, Centre_spiro = Centre) %>%
  select(nodos, Numero_courbe, date_spiro, Centre_spiro, CVF_L, VEMS_L, DEP_Ls, DEM75_Ls, DEM50_Ls, DEM25_Ls, DEM2575_Ls, Best_curve) 
data_spiro_to_merge <- rbind(data_spiro_to_merge, data_to_add) %>%
  mutate(spiro = 1) 

# Verif des doublons
data_spiro_to_merge %>%
  group_by(nodos, Numero_courbe) %>%  
  filter(n() > 1) %>%  # Garde uniquement les lignes en double
  ungroup() %>% nrow # OK


# 1 sujet 770091101 dont le nodos a été mal renseigné sur EasyConnect => à changer
data_spiro_to_merge$nodos[data_spiro_to_merge$nodos == "710009101"] <- "770091101"
data_spiro_to_merge$Best_curve[data_spiro_to_merge$nodos == "770091101"] <- 2

# Merge data ----

data <- merge(x = data_spiro_to_merge, y = data_quali_to_merge, by = c("nodos", "Numero_courbe"), all.x = T, all.y = T) %>%
  merge(data_eCRF_to_merge, by = "nodos", all.x = T, all.y = T) %>%
  mutate(
    Centre = case_when(
      Centre_eCRF == "Grenoble" | Centre_spiro == "GRENOBLE" | Centre_quali == "GRENOBLE" ~ "Grenoble",
      Centre_eCRF == "Lyon" | Centre_spiro == "LYON" | Centre_quali == "LYON" ~ "Lyon",
      Centre_eCRF == "Marseille" | Centre_spiro == "MARSEILLE" | Centre_quali == "MARSEILLE" ~ "Marseille",
      Centre_eCRF == "Montpellier" | Centre_spiro == "MONTPELLIER" | Centre_quali == "MONTPELLIER" ~ "Montpellier",
      Centre_eCRF == "Paris" | Centre_spiro == "PARIS" | Centre_quali == "PARIS" ~ "Paris",
      .default = NA
    )
  )

rm(data_to_add, data_spiro_autre_sansGre, data_spiro_Grenoble_clear,
   data_eCRF_to_merge, data_quali_to_merge, data_spiro_to_merge)

# Doublons ?
data %>% group_by(nodos, Numero_courbe) %>% filter(n() > 1) %>% ungroup() %>% nrow # OK

# Calcul %theo et z-score ----

data <- data %>%
  mutate(
    VEMS_sur_CVF = VEMS_L/CVF_L*100,
    CVF_p100 = CVF_L/pred_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, param = "FVC")*100,
    VEMS_p100 = VEMS_L/pred_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, param = "FEV1")*100,
    DEM2575_p100 = DEM2575_Ls/pred_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, param = "FEF2575")*100,
    DEM75_p100 = DEM75_Ls/pred_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, param = "FEF75")*100,
  )

# On est obligé de calculer les z-scores à part car fonction zscore_GLI ne sait pas gérer les données manquantes pour age, sexe et taille
data_zscore <- data %>%
  select(nodos, Numero_courbe, sexe, age, taille, CVF_L, VEMS_L, VEMS_sur_CVF, DEM2575_Ls, DEM75_Ls) %>%
  drop_na(age, sexe, taille) %>%
  mutate(
    CVF_zscore = zscore_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, FVC = CVF_L),
    VEMS_zscore = zscore_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, FEV1 = VEMS_L),
    VEMSsurCVF_zscore = zscore_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, FEV1FVC = VEMS_sur_CVF/100),
    DEM2575_zscore = zscore_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, FEF2575 = DEM2575_Ls),
    DEM75_zscore = zscore_GLI(age = age, height = taille/100, gender = sexe, ethnicity = 1, FEF75 = DEM75_Ls)
  ) %>%
  select(-c(age, sexe, taille, CVF_L, VEMS_L, VEMS_sur_CVF, DEM2575_Ls, DEM75_Ls))

data <- left_join(x = data, y = data_zscore, by = c("nodos", "Numero_courbe")) %>%
  select(nodos, Centre, date_CRF, date_spiro, date_quali, sexe, age, taille, poids, Numero_courbe, Best_curve, decision,
         CVF_L, CVF_p100, CVF_zscore, VEMS_L, VEMS_p100, VEMS_zscore, VEMS_sur_CVF, VEMSsurCVF_zscore,
         DEP_Ls, DEM2575_Ls, DEM2575_p100, DEM2575_zscore, DEM75_Ls, DEM75_p100, DEM75_zscore, 
         DEM50_Ls, DEM25_Ls, Export_ok, Donnees_transmises, Nb_essais_eCRF, Nb_essais_rejetes_eCRF, 
         Res_ok_eCRF, Res_ok_autre_eCRF, Pb_real_eCRF, Pb_real_autre_eCRF, Raison_non_fait_eCRF, Best_curve_ARC_eCRF,
         eCRF, quali, spiro) %>%
  mutate(
    date_CRF = as.Date(date_CRF, format = "%d/%m/%Y"),
    date_quali = as.Date(date_quali, format = "%d/%m/%Y"),
    date_spiro = as.Date(date_spiro, format = "%d/%m/%Y")
  )

rm(data_zscore)

# Calcul reproductibilité ----

data <- data %>%
  group_by(nodos) %>%
  mutate(
    # Identifier la valeur de VEMS de la meilleure courbe
    Best_VEMS = VEMS_L[Numero_courbe == Best_curve],
    Best_CVF = CVF_L[Numero_courbe == Best_curve],
    # Cas 1 : uniquement les courbes de qualité GOOD
    repro_VEMS_good_nb = sum(
      abs(VEMS_L - Best_VEMS) <= 0.15 & 
        Numero_courbe != Best_curve & 
        decision == "GOOD", na.rm = T) + 1,
    repro_CVF_good_nb = sum(
      abs(CVF_L - Best_CVF) <= 0.15 & 
        Numero_courbe != Best_curve & 
        decision == "GOOD", na.rm = T) + 1,
    # Cas 2 : les courbes de qualité GOOD ou DOUBT
    repro_VEMS_good_doubt_nb = sum(
      abs(VEMS_L - Best_VEMS) <= 0.15 & 
        Numero_courbe != Best_curve & 
        decision %in% c("GOOD", "DOUBT"), na.rm = T) + 1,
    repro_CVF_good_doubt_nb = sum(
      abs(CVF_L - Best_CVF) <= 0.15 & 
        Numero_courbe != Best_curve & 
        decision %in% c("GOOD", "DOUBT"), na.rm = T) + 1,
  ) %>%
  ungroup()


# Indicateurs suivi ----

## Quali vs valeurs spiro ----

data %>% filter(quali == 1 & is.na(spiro)) %>% select(nodos) # OK
data %>% filter(is.na(quali) & spiro == 1) %>% select(nodos, date_spiro, Centre) %>% distinct 
# Un participant de Grenoble pour qui on a la spiro en csv 
# (dans un deuxième fichier Spiro EXT00157 220525) mais pas possible de voir la courbe

## eCRF vs qualité ----

a <- data %>% filter(eCRF == 1 & is.na(quali)) %>% select(nodos, date_CRF, Centre) %>% distinct
data %>% filter(is.na(eCRF) & quali == 1) %>% select(nodos, date_quali,date_CRF,date_spiro, Centre) %>% distinct 
# 5 sujets de Lyon et 1 de Paris 



## eCRF vs valeurs spiro ----

b <- data %>% filter(eCRF == 1 & is.na(spiro)) %>% select(nodos, date_CRF, date_spiro, date_quali, Centre) 
a %>% filter(!nodos %in% b$nodos)
# b <- data_eCRF %>% filter(nodos %in% a$nodos)
rm(a, b)

data %>% filter(is.na(eCRF) & spiro == 1) %>% select(nodos, date_spiro, Centre) %>% distinct 

saveRDS(data, "data/data_fin/data_spiros_egea4_20250909.rds")
