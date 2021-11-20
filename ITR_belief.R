# packages -----
library(tidyverse)
library(mirt)
library(readxl)

# Data reading ----
datos_pais <- read_excel("Doc_Evelyn/itr_creencias/Datos_TEDS_M_Colombia.xlsx",
                         sheet = "bd_Norm_Licen")
  
dic_creencias <- read_excel("Doc_Evelyn/itr_creencias/Datos_TEDS_M_Colombia.xlsx",
                            sheet = "dic")

# Function for calculating rasch scores----
puntaje_rasch <- function(model,
                          dimension = NULL,
                          mean = 10, sd = 1, ...) {
  fscores <- fscores(model)
  scores <- round(sd * scale(fscores) + mean,1) 
  return(scores)
}

# Rules and Procedures ----
rules_and_proc <- dic_creencias %>% 
  filter(subdominio == "Rules and Procedures") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# model rasch: Rules and Procedures
datos_pais %>% 
  dplyr::select(all_of(rules_and_proc)) %>% 
  na.omit() %>% 
  mirt(1, itemtype = "Rasch") -> mod_rasch_rules_and_proc

# tranformation to 10 scores: Rules and Procedures
datos_pais$rules_and_proc <- as.vector(puntaje_rasch(mod_rasch_rules_and_proc))

# summary: Rules and Procedures
datos_pais %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(rules_and_proc),
            media = mean(rules_and_proc),
            sd = sd(rules_and_proc),
            median = median(rules_and_proc),
            min = min(rules_and_proc),
            max = max(rules_and_proc),
            riq = IQR(rules_and_proc)) 

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.1  0.740   10.1   7.9    12 0.800
# Normalista   405  9.98 1.11    10.2   7.9    12 1.4 

# Process of Inquiry ----
process_of_inquiry <- dic_creencias %>% 
  filter(subdominio == "Process of Inquiry") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# model rasch: process_of_inquiry
datos_pais %>% 
  dplyr::select(all_of(process_of_inquiry)) %>% 
  na.omit() %>% 
  mirt(1, itemtype = "Rasch") -> mod_rasch_process_of_inquiry

# tranformation to 10 scores: process_of_inquiry
datos_pais$process_of_inquiry <- as.vector(puntaje_rasch(mod_rasch_process_of_inquiry))

# summary: process_of_inquiry
datos_pais %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(process_of_inquiry),
            media = mean(process_of_inquiry),
            sd = sd(process_of_inquiry),
            median = median(process_of_inquiry),
            min = min(process_of_inquiry),
            max = max(process_of_inquiry),
            riq = IQR(process_of_inquiry)) 

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.4  0.866   10.2     8  11.7   0.9
# Normalista   405  9.82 1.01     9.9     8  11.7   1.4

# Teacher Direction ----
teacher_direction <- dic_creencias %>% 
  filter(subdominio == "Teacher Direction") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# model rasch: teacher_direction
datos_pais %>% 
  dplyr::select(all_of(teacher_direction)) %>% 
  na.omit() %>% 
  mirt(1, itemtype = "Rasch") -> mod_rasch_teacher_direction

# tranformation to 10 scores: teacher_direction
datos_pais$teacher_direction <- as.vector(puntaje_rasch(mod_rasch_teacher_direction))

# summary: teacher_direction
datos_pais %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(teacher_direction),
            media = mean(teacher_direction),
            sd = sd(teacher_direction),
            median = median(teacher_direction),
            min = min(teacher_direction),
            max = max(teacher_direction),
            riq = IQR(teacher_direction)) 

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213  9.94 0.827   10     7.8  12.5   0.9
# Normalista   405 10.0  1.07    10.2   7.8  13     1 

# Active Learning ----
active_learning <- dic_creencias %>% 
  filter(subdominio == "Active Learning") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# model rasch: active_learning
datos_pais %>% 
  dplyr::select(all_of(active_learning)) %>% 
  na.omit() %>% 
  mirt(1, itemtype = "Rasch") -> mod_rasch_active_learning

# tranformation to 10 scores: active_learning
datos_pais$active_learning <- as.vector(puntaje_rasch(mod_rasch_active_learning))

# summary: active_learning
datos_pais %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(active_learning),
            media = mean(active_learning),
            sd = sd(active_learning),
            median = median(active_learning),
            min = min(active_learning),
            max = max(active_learning),
            riq = IQR(active_learning))

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.3  0.868   10.3     8  11.8 0.800
# Normalista   405  9.83 1.04    10       8  11.8 1.4 

# Fixed Ability ----
fixed_ability <- dic_creencias %>% 
  filter(subdominio == "Fixed Ability") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# model rasch: fixed_ability
datos_pais %>% 
  dplyr::select(all_of(fixed_ability)) %>% 
  na.omit() %>% 
  mirt(1, itemtype = "Rasch") -> mod_rasch_fixed_ability

# tranformation to 10 scores: fixed_ability
datos_pais$fixed_ability <- as.vector(puntaje_rasch(mod_rasch_fixed_ability))

# summary: fixed_ability
datos_pais %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(fixed_ability),
            media = mean(fixed_ability),
            sd = sd(fixed_ability),
            median = median(fixed_ability),
            min = min(fixed_ability),
            max = max(fixed_ability),
            riq = IQR(fixed_ability))

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213  9.98 0.869   10.1   7.9  12.5   1  
# Normalista   405 10.0  1.06    10.2   7.9  12.5   1.1

# Preparedness for Teaching Mathematics ----
Prep_for_teach_math <- dic_creencias %>% 
  filter(subdominio == "Preparedness for Teaching Mathematics") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# model rasch: Prep_for_teach_math
datos_pais %>% 
  dplyr::select(all_of(Prep_for_teach_math)) %>% 
  na.omit() %>% 
  mirt(1, itemtype = "Rasch") -> mod_rasch_Prep_for_teach_math

# tranformation to 10 scores: Prep_for_teach_math
datos_pais$Prep_for_teach_math <- as.vector(puntaje_rasch(mod_rasch_Prep_for_teach_math))

# summary: Prep_for_teach_math
datos_pais %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Prep_for_teach_math),
            media = mean(Prep_for_teach_math),
            sd = sd(Prep_for_teach_math),
            median = median(Prep_for_teach_math),
            min = min(Prep_for_teach_math),
            max = max(Prep_for_teach_math),
            riq = IQR(Prep_for_teach_math))

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.2  0.807   10.2   8.1  11.6 0.900
# Normalista   405  9.88 1.07    10     8.1  11.6 1.5 

# Quality of Instruction ----
quality_of_instr <- dic_creencias %>% 
  filter(subdominio == "Preparedness for Teaching Mathematics") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# model rasch: quality_of_instr
datos_pais %>% 
  dplyr::select(all_of(quality_of_instr)) %>% 
  na.omit() %>% 
  mirt(1, itemtype = "Rasch") -> mod_rasch_quality_of_instr

# tranformation to 10 scores: quality_of_instr
datos_pais$quality_of_instr <- as.vector(puntaje_rasch(mod_rasch_quality_of_instr))

# summary: quality_of_instr
datos_pais %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(quality_of_instr),
            media = mean(quality_of_instr),
            sd = sd(quality_of_instr),
            median = median(quality_of_instr),
            min = min(quality_of_instr),
            max = max(quality_of_instr),
            riq = IQR(quality_of_instr))

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.2  0.807   10.2   8.1  11.6 0.900
# Normalista   405  9.88 1.07    10     8.1  11.6 1.5