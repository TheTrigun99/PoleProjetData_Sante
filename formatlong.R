library(lme4)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
df <- read_excel("Data_salive_steroides_cinetique.xlsx")
df <- df %>%
  mutate(
    Puberty_stage_simple = case_when(
      `Puberty stage` %in% c(1, 2) ~ "prepubertal",
      TRUE ~ "pubertal"
    ),
    Puberty_stage_simple = factor(Puberty_stage_simple)
  )

## Passage au format long : une ligne pour un patient*time + on rend binaire certaines variables (pq puberty ? ) 
df_long_17OHP <- df %>%
  pivot_longer(
    cols = c(`8h_17OHP`, `12h_17OHP`, `16h_17OHP`, `20h_17OHP`),
    names_to = "Time",
    values_to = "steroid"
  ) %>%
  mutate(
    # Nettoyer l'heure pour avoir un truc propre
    Time = factor(Time,
                  levels = c("8h_17OHP", "12h_17OHP", "16h_17OHP", "20h_17OHP"),
                  labels = c("8", "12", "16", "20")),
    Groupe_bin = ifelse(Groupe == "Gp non contrôlé", 1, 0),
    Groupe_bin = factor(Groupe_bin),
    Gender = factor(Gender),
    `Puberty stage` = factor(`Puberty stage`),
    Patient_number = factor(`Patient number`)
  )

##Là on garde juste 6 variables
df_17OHP <- df_long_17OHP %>%
  select(Patient_number, Gender, Puberty_stage_simple,
         Time, steroid, Groupe_bin)

## Le modèle glmer, avec famille binomiale
mod_17OHP_time <- glmer(
  Groupe_bin ~ log(steroid + 1e-6) +
    Gender  + Puberty_stage_simple+
    (1 | Patient_number),
  data = df_17OHP,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(mod_17OHP_time)
