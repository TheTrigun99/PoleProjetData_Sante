library(lme4)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
df <- read_excel("Data_salive_steroides_cinetique.xlsx")
#on chope les patients
df <- df %>%
  mutate(
    Patient_number = as.integer(stringr::str_extract(PATIENT, "(?<=P)\\d+")),
    Time_Point = as.integer(stringr::str_extract(PATIENT, "(?<=_T)\\d+"))
  )

# Log-transformation du stéroïde choisi histoire de normaliser
df <- df %>%
  mutate(log_steroid = log(`8h_CORTISONE` + 1e-6)) # Ajouter un petit constant pour éviter log(0)
df$Groupe <- factor(df$Groupe, levels = c("Gp non contrôlé", "Gp contrôlé"))
# Modèle mixte logistique
model <- glmer(Groupe ~ log_steroid + Time_Point +Gender+`Puberty stage`+ (1 | Patient_number),
               data = df,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),verbose=1)

# Résumé du modèle
summary(model)