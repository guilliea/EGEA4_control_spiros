# Script permettant de vérifier la qualité des spiros faites dans EGEA
# Toutes les courbes sont revues par Alicia et Nadia et un commentaire est fait pour chacune
# d'elle : BAD, DOUBT ou GOOD

# Le but ici est d'identifier le nb de courbe par participant, le nb de courbes de bonne qualité, etc

library(readxl)
library(dplyr)
library(haven)

source("scripts/fonctions.R")

# Import data ----
data <- read_excel("data/fichiers_comparaison/Verif_spiro_EGEA_Alicia_Nadiia_250902.xlsx") %>%
  filter(!is.na(nodos)) %>%
  mutate(centre = as.factor(centre))

data_doublons <- data %>%
  group_by(nodos, Numero_courbe) %>%  
  filter(n() > 1) %>%  # Garde uniquement les lignes en double
  ungroup()
rm(data_doublons)

data %>% filter(centre == "PARIS") %>% select(nodos) %>% pull() %>% unique() %>% length()
data %>% filter(centre == "GRENOBLE") %>% select(nodos) %>% pull() %>% unique() %>% length()
data %>% filter(centre == "LYON") %>% select(nodos) %>% pull() %>% unique() %>% length()
data %>% filter(centre == "MARSEILLE") %>% select(nodos) %>% pull() %>% unique() %>% length()
data %>% filter(centre == "MONTPELLIER") %>% select(nodos) %>% pull() %>% unique() %>% length()

# Comparaison double check quali ----


good <- c(21, 30, 32, 34, 35, 37:39, 66, 75, 78, 82, 132, 152, 153, 184, 193, 194, 195,
          211, 212, 224, 293, 295, 298, 328, 389, 393, 396, 424, 522, 559, 561, 571, 581, 586,
          595, 596, 599, 601, 602, 603, 608, 618, 639, 644, 664, 674, 690, 740, 742, 747, 748, 
          749, 779, 812, 819, 822, 842, 856, 857, 997, 1019, 1025, 1041, 1051, 1061, 1067,
          1076, 1096, 1124, 1125)
bad <- c(22, 24, 27:29, 31, 33, 36, 59, 61, 62, 69, 72, 77, 130, 134, 135, 140:142, 148, 149, 181,
         188, 201, 202, 233, 239, 318, 329, 331, 355, 357, 359, 372, 376, 392, 426, 475, 477, 532,
         534, 535, 536, 537, 553, 567, 597, 610, 628, 629, 632, 647, 649, 650, 675, 677, 685, 687,
         696, 712, 713, 719, 721, 743, 882, 884, 1000, 1005, 1006, 1032, 1034, 1039, 1057, 1068, 
         1070, 1104, 1114, 1117, 1130, 1131)
doubt <- c(101, 226, 249, 289, 297, 317, 353, 403, 439, 465, 466, 467, 498, 519, 520, 523, 565, 573,
           582, 587, 588, 643, 648, 656, 657, 681, 684, 707, 708, 709, 717, 718, 720, 735, 765, 767,
           780, 803, 811, 835, 867, 868, 877, 889, 1013, 1015, 1016, 1038, 1040, 1062, 1088, 1089,
           1110, 1113, 1119, 1120, 1122, 1125)
all_control <- c(good, bad, doubt)

desaccord <- data %>%
  filter(Coherence == 0 & !ID_Alicia %in% all_control & !is.na(Decision_Nadiia))


# Decision finale (après comparaison des décisions de Nadiia et Alicia)
data <- data %>%
  mutate(
    decision = case_when(
      Coherence == 1 ~ Decision_Alicia,
      ID_Alicia %in% good  ~ "GOOD" ,
      ID_Alicia %in% bad  ~ "BAD",
      ID_Alicia %in% doubt  ~ "DOUBT",
      .default = Decision_Alicia # En attendant la revue des incohérences on garde mon analyse de qualité
    )
  )
table(data$decision, data$centre, useNA = "ifany")

to_do <- data %>%
  filter(is.na(Coherence))

rm(desaccord, to_do, all_control, good, bad, doubt)

saveRDS(data, "data/tmp/data_qualit.rds")

# Stats desc ----

# Nombre de sujets
data %>% 
  distinct(nodos) %>% 
  count()

# Nombre de sujets par centre
n_centre <- data %>%
  distinct(nodos, centre) %>%
  group_by(centre) %>%
  summarise(nombre_participants = n())
n_centre

# Nombre de sujets par centre avec au moins une courbe de bonne qualité
n_centre_1good <- data %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, centre) %>%
  group_by(centre) %>%
  summarise(nombre_participants = n())
n_centre_1good

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité
n_centre_3good <- data %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, centre) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(centre) %>%
  summarise(nombre_participants = n())

tab <- data_frame("Centre" = levels(data$centre),
                  "Nombre de participants" = n_centre$nombre_participants,
                  "Participants avec ≥1 courbe GOOD" = paste(n_centre_1good$nombre_participants, " (",
                            round(n_centre_1good$nombre_participants/n_centre$nombre_participants*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD" = paste(n_centre_3good$nombre_participants, " (",
                                                              round(n_centre_3good$nombre_participants/n_centre$nombre_participants*100, 0), "%)", sep = ""))
                    
tab
rm(n_centre, n_centre_1good, n_centre_3good, tab)

