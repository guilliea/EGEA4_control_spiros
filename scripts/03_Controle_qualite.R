# Script analyse de qualité stratifiée sur certains facteurs (age, ARC, ...)

# Indicateurs de qualité ----

## Selon l'âge des patients ----

data <- data %>% mutate(
  age_rec = ifelse(age <= 65, 0, ifelse(age >65, 1, NA))
)


# Nombre de sujets par centre
n_centre <- data %>% filter(!is.na(age_rec) & !is.na(decision)) %>% distinct(nodos, age_rec) %>% group_by(age_rec) %>% summarise(nombre_participants = n()) %>% select(nombre_participants) %>% pull

# Nombre de sujets par centre avec au moins une courbe de bonne qualité
n_centre_1good <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, age_rec) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_1good

# Nombre de sujets par centre avec au moins une courbe de bonne/moyenne qualité
n_centre_1good_doubt <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT")) %>%
  distinct(nodos, age_rec) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_1good_doubt

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité
n_centre_2good <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2good

# Nombre de sujets par centre avec au moins 2 courbes de bonne/moyenne qualité
n_centre_2good_doubt <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT")) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2good_doubt

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité
n_centre_3good <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3good

# Nombre de sujets par centre avec au moins 3 courbes de bonne/moyenne qualité
n_centre_3good_doubt <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT")) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3good_doubt

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité et reproductible sur VEMS+CVF
n_centre_3repro_good <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3repro

n_centre_3repro_doubt <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT") & repro_CVF_good_doubt_nb >= 3 & repro_VEMS_good_doubt_nb >= 3) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3repro_doubt

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité et reproductible sur VEMS+CVF
n_centre_2repro_good <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2repro_good

n_centre_2repro_doubt <- data %>%
  filter(!is.na(age_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT") & repro_CVF_good_doubt_nb >= 2 & repro_VEMS_good_doubt_nb >= 2) %>%
  group_by(nodos, age_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(age_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3repro_doubt


centre <- c("<= 65 ans", "> 65 ans")

tab <- data_frame("Age" = centre,
                  "Nombre de participants" = n_centre,
                  "Participants avec ≥1 courbe GOOD" = paste(n_centre_1good, " (",
                                                             round(n_centre_1good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥1 courbe GOOD/DOUBT" = paste(n_centre_1good_doubt, " (", 
                                                                   round(n_centre_1good_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD" = paste(n_centre_2good, " (",
                                                              round(n_centre_2good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD/DOUBT" = paste(n_centre_2good_doubt, " (",
                                                                    round(n_centre_2good_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD" = paste(n_centre_3good, " (",
                                                              round(n_centre_3good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD/DOUBT" = paste(n_centre_3good_doubt, " (",
                                                                    round(n_centre_3good_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD reproductibles" = paste(n_centre_2repro_good, " (",
                                                                             round(n_centre_2repro_good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD/DOUBT reproductibles" = paste(n_centre_2repro_doubt, " (",
                                                                                   round(n_centre_2repro_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD reproductibles" = paste(n_centre_3repro_good, " (",
                                                                             round(n_centre_3repro_good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD/DOUBT reproductibles" = paste(n_centre_3repro_doubt, " (",
                                                                                   round(n_centre_3repro_doubt/n_centre*100, 0), "%)", sep = ""),
)

tab

## Selon la date d'inclusion ----

data <- data %>% mutate(
  date_rec = ifelse(data$date_quali <= median(data$date_quali, na.rm = T), 0, ifelse(data$date_quali > median(data$date_quali, na.rm = T), 1, NA))
)
table(data$date_rec, useNA = "ifany")

# Nombre de sujets par centre
n_centre <- data %>% filter(!is.na(date_rec) & !is.na(decision)) %>% distinct(nodos, date_rec) %>% group_by(date_rec) %>% summarise(nombre_participants = n()) %>% select(nombre_participants) %>% pull

# Nombre de sujets par centre avec au moins une courbe de bonne qualité
n_centre_1good <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  distinct(nodos, date_rec) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_1good

# Nombre de sujets par centre avec au moins une courbe de bonne/moyenne qualité
n_centre_1good_doubt <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT")) %>%
  distinct(nodos, date_rec) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_1good_doubt

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité
n_centre_2good <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2good

# Nombre de sujets par centre avec au moins 2 courbes de bonne/moyenne qualité
n_centre_2good_doubt <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT")) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2good_doubt

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité
n_centre_3good <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD") %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3good

# Nombre de sujets par centre avec au moins 3 courbes de bonne/moyenne qualité
n_centre_3good_doubt <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT")) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3good_doubt

# Nombre de sujets par centre avec au moins 3 courbes de bonne qualité et reproductible sur VEMS+CVF
n_centre_3repro_good <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 3 & repro_VEMS_good_nb >= 3) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3repro

n_centre_3repro_doubt <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT") & repro_CVF_good_doubt_nb >= 3 & repro_VEMS_good_doubt_nb >= 3) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 3) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3repro_doubt

# Nombre de sujets par centre avec au moins 2 courbes de bonne qualité et reproductible sur VEMS+CVF
n_centre_2repro_good <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision == "GOOD" & repro_CVF_good_nb >= 2 & repro_VEMS_good_nb >= 2) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_2repro_good

n_centre_2repro_doubt <- data %>%
  filter(!is.na(date_rec) & !is.na(decision)) %>%
  filter(decision %in% c("GOOD", "DOUBT") & repro_CVF_good_doubt_nb >= 2 & repro_VEMS_good_doubt_nb >= 2) %>%
  group_by(nodos, date_rec) %>%
  summarise(nombre_courbes_good = n()) %>%
  filter(nombre_courbes_good >= 2) %>%
  group_by(date_rec) %>%
  summarise(nombre_participants = n()) %>%
  select(nombre_participants) %>% pull
n_centre_3repro_doubt


centre <- c("Avant octobre 2024", "Après octobre 2024")

tab <- data_frame("Date" = centre,
                  "Nombre de participants" = n_centre,
                  "Participants avec ≥1 courbe GOOD" = paste(n_centre_1good, " (",
                                                             round(n_centre_1good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥1 courbe GOOD/DOUBT" = paste(n_centre_1good_doubt, " (", 
                                                                   round(n_centre_1good_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD" = paste(n_centre_2good, " (",
                                                              round(n_centre_2good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD/DOUBT" = paste(n_centre_2good_doubt, " (",
                                                                    round(n_centre_2good_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD" = paste(n_centre_3good, " (",
                                                              round(n_centre_3good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD/DOUBT" = paste(n_centre_3good_doubt, " (",
                                                                    round(n_centre_3good_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD reproductibles" = paste(n_centre_2repro_good, " (",
                                                                             round(n_centre_2repro_good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥2 courbes GOOD/DOUBT reproductibles" = paste(n_centre_2repro_doubt, " (",
                                                                                   round(n_centre_2repro_doubt/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD reproductibles" = paste(n_centre_3repro_good, " (",
                                                                             round(n_centre_3repro_good/n_centre*100, 0), "%)", sep = ""),
                  "Participants avec ≥3 courbes GOOD/DOUBT reproductibles" = paste(n_centre_3repro_doubt, " (",
                                                                                   round(n_centre_3repro_doubt/n_centre*100, 0), "%)", sep = ""),
)

tab
