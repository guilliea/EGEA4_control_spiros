# Script permettant de vérifier la qualité des spiros faites dans EGEA
# Toutes les courbes sont revues par Alicia et Nadia et un commentaire est fait pour chacune
# d'elle : BAD, DOUBT ou GOOD

# Le but ici est d'identifier le nb de courbe par participant, le nb de courbes de bonne qualité, etc

library(readxl)
library(dplyr)
library(haven)

source("scripts/fonctions.R")

# Import data ----
data <- read_excel("data/fichiers_comparaison/Verif_spiro_EGEA_Alicia_Nadiia_260204.xlsx") %>%
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
          749, 779, 812, 819, 822, 842, 856, 857, 901, 911, 917, 928:930, 936, 950, 954, 963,
          969, 980, 981, 997, 1019, 1025, 1041, 1051, 1061, 1067, 1076, 1096, 1124, 1125,
          1144, 1152, 1157, 1158, 1174, 1186, 1197, 1257, 1284, 1285, 1293, 1299, 1348, 1349,
          1355, 1407, 1409, 1414, 1446, 1464, 1469, 1484, 1486, 1494, 1530, 1531, 1533, 1534, 1559, 1566, 1567,
          1579, 1580, 1593, 1599)
bad <- c(22, 24, 27:29, 31, 33, 36, 59, 61, 62, 69, 72, 77, 130, 134, 135, 140:142, 148, 149, 181,
         188, 201, 202, 233, 239, 318, 329, 331, 355, 357, 359, 372, 376, 392, 426, 475, 477, 532,
         534, 535, 536, 537, 553, 567, 597, 610, 628, 629, 632, 647, 649, 650, 675, 677, 685, 687,
         696, 712, 713, 719, 721, 743, 882, 884, 903, 932, 933, 934, 970, 971, 
         1000, 1005, 1006, 1032, 1034, 1039, 1057, 1068, 1154, 
         1070, 1104, 1114, 1117, 1130, 1131, 1168, 1169, 1184, 1194, 1195, 1203, 1231, 1256, 1291,
         1296, 1302, 1304, 1305, 1314:1318, 1363, 1371, 1382, 1431:1434, 1447, 1451, 1452, 1458, 1459,
         1467, 1489, 1510, 1522, 1527, 1529, 1575, 1605)
doubt <- c(101, 226, 249, 289, 297, 317, 353, 403, 439, 465, 466, 467, 498, 519, 520, 523, 565, 573,
           582, 587, 588, 643, 648, 656, 657, 681, 684, 707, 708, 709, 717, 718, 720, 735, 765, 767,
           780, 803, 811, 835, 867, 868, 877, 889, 902, 906, 922, 924, 926, 931, 947, 952,
           966, 1013, 1015, 1016, 1038, 1040, 1062, 1088, 1089, 1110, 1113, 1119, 1120, 1122, 1125, 
           1159, 1163, 1165, 1166, 1176, 1177, 1193, 1196, 1199, 1219, 1225, 1229, 1286, 1306, 1308, 1309,
           1320, 1324, 1328, 1334, 1350, 1384, 1410, 1411, 1418, 1435, 1437, 1453, 1455, 1456, 1465, 1505,
           1541, 1542, 1543, 1541, 1554, 1561, 1563, 1564,  1578, 1601, 1602)
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

