# packages -----
library(tidyverse)
library(mirt)
library(readxl)
library(nortest)
library(effsize)
library(effectsize)
library(car)

# Data reading ----
datos_pais <- read_excel("Doc_Evelyn/itr_creencias/Datos_TEDS_M_Colombia.xlsx",
                         sheet = "bd_Norm_Licen")
  
dic_creencias <- read_excel("Doc_Evelyn/itr_creencias/Datos_TEDS_M_Colombia.xlsx",
                            sheet = "dic")


# subset data ----
datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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
            riq = IQR(rules_and_proc)) -> rp

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.1  0.740   10.1   7.9    12 0.800
# Normalista   405  9.98 1.11    10.2   7.9    12 1.4 

# statistical inference

shapiro.test(datos_pais$rules_and_proc)
ks.test(scale(datos_pais$rules_and_proc), "pnorm")
lillie.test(datos_pais$rules_and_proc)

qqnorm(datos_pais$rules_and_proc, las=1, pch=18, 
       main="rules and process of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$rules_and_proc)

shapiro.test(datos_normalista$rules_and_proc)
# W = 0.94623, p-value = 5.879e-11
ks.test(scale(datos_normalista$rules_and_proc), "pnorm")
# D = 0.1083, p-value = 0.0001497
lillie.test(datos_normalista$rules_and_proc)
# D = 0.1083, p-value = 2.362e-12

qqnorm(datos_normalista$rules_and_proc, las=1, pch=18, 
       main="rules and process of the normalistas", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$rules_and_proc)

shapiro.test(datos_licenciado$rules_and_proc)
# W = 0.92293, p-value = 4.119e-09
ks.test(scale(datos_licenciado$rules_and_proc), "pnorm")
# D = 0.10959, p-value = 0.012
lillie.test(datos_licenciado$rules_and_proc)
# D = 0.10959, p-value = 1.598e-06

qqnorm(datos_licenciado$rules_and_proc, las=1, pch=18, 
       main="rules and process de los licenciados", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$rules_and_proc)

# result of the normal distribution
# the data don't have the behavior of a normal distribution

# non-parametric test
wilcox.test(datos_licenciado$rules_and_proc,
            datos_normalista$rules_and_proc,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 42184, p-value = 0.6528
# result of the non-parametric test
# there aren't statistically significant differences 

# effect size non-parametric 
r1 <- rank_biserial(datos_normalista$rules_and_proc,
              datos_licenciado$rules_and_proc)

# result
# r (rank biserial) |        95% CI
# 0.02              | [-0.07, 0.12]

interpret_r(r1$r_rank_biserial, rules = "funder2019")
# [1] "tiny"

# parametric test

# homoscedasticity test
var.test(x = datos_licenciado$rules_and_proc,
         y = datos_normalista$rules_and_proc)
# result
# F = 0.44476, num df = 212, denom df = 404, p-value = 1.597e-10

fligner.test(rules_and_proc ~ PROGRAMA, data= datos_pais)
# result
# Fligner-Killeen:med chi-squared = 38.529, df = 1, p-value = 5.394e-10

leveneTest(rules_and_proc ~ factor(PROGRAMA), data = datos_pais, center = "median")
# result
# Df F value    Pr(>F)    
# group   1  38.037 1.257e-09 ***
#  616                      

bartlett.test(list(datos_licenciado$rules_and_proc,
                   datos_normalista$rules_and_proc))
# Bartlett's K-squared = 41.058, df = 1, p-value = 1.478e-10

# result of homoscedasticity test
# there are statistically significant differences in variances

t.test(x = datos_licenciado$rules_and_proc,
       y = datos_normalista$rules_and_proc,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = 1.1914, df = 582.44, p-value = 0.234
# there aren't statistically significant differences in average

# effect size: rules and proc

cohen.d(formula = rules_and_proc ~ PROGRAMA,
        data= datos_pais, paired = FALSE)
# d estimate: 0.08942412 (negligible)

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
            riq = IQR(process_of_inquiry)) -> pi

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.4  0.866   10.2     8  11.7   0.9
# Normalista   405  9.82 1.01     9.9     8  11.7   1.4

# statistical inference
shapiro.test(datos_pais$process_of_inquiry)
ks.test(scale(datos_pais$process_of_inquiry), "pnorm")
lillie.test(datos_pais$process_of_inquiry)

qqnorm(datos_pais$process_of_inquiry, las=1, pch=18, 
       main="process of inquiry of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$process_of_inquiry)

shapiro.test(datos_normalista$process_of_inquiry)
# W = 0.96047, p-value = 5.647e-09
ks.test(scale(datos_normalista$process_of_inquiry), "pnorm")
# D = 0.081137, p-value = 0.009665
lillie.test(datos_normalista$process_of_inquiry)
# D = 0.081137, p-value = 1.012e-06

qqnorm(datos_normalista$process_of_inquiry, las=1, pch=18, 
       main="process of inquiry of the normalistas", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$process_of_inquiry)

shapiro.test(datos_licenciado$process_of_inquiry)
# W = 0.9187, p-value = 1.967e-09
ks.test(scale(datos_licenciado$process_of_inquiry), "pnorm")
# D = 0.10975, p-value = 0.01182
lillie.test(datos_licenciado$process_of_inquiry)
# D = 0.10975, p-value = 1.526e-06

qqnorm(datos_licenciado$process_of_inquiry, las=1, pch=18, 
       main="process of inquiry of the licenciados", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$process_of_inquiry)

# result of the normal distribution
# the data don't have the behavior of a normal distribution

# non-parametric test
wilcox.test(datos_licenciado$process_of_inquiry,
            datos_normalista$process_of_inquiry,paired = F,
            exact = F,correct = T,conf.int = 0.95)
# W = 57356, p-value = 1.381e-11
# result of the non-parametric test
# there are statistically significant differences 

# effect size non-parametric 
r2 <- rank_biserial(datos_licenciado$process_of_inquiry,
              datos_normalista$process_of_inquiry)

# result 
# r (rank biserial) |       95% CI
# 0.33              | [0.24, 0.41]

interpret_r(r2$r_rank_biserial, rules = "funder2019")
# [1] "large"

# parametric test

# homoscedasticity test
var.test(x = datos_licenciado$process_of_inquiry,
         y = datos_normalista$process_of_inquiry)
# result
# F = 0.73595, num df = 212, denom df = 404, p-value = 0.01251

fligner.test(process_of_inquiry ~ PROGRAMA, data= datos_pais)
# result
# Fligner-Killeen:med chi-squared = 6.4617, df = 1, p-value = 0.01102

leveneTest(process_of_inquiry ~ factor(PROGRAMA), data = datos_pais,
           center = "median")
# result
# Df F value   Pr(>F)   
# group   1  9.2151 0.002502 **
#   616

bartlett.test(list(datos_licenciado$process_of_inquiry,
                   datos_normalista$process_of_inquiry))
# result
# Bartlett's K-squared = 6.2985, df = 1, p-value = 0.01208

# result of homoscedasticity test
# there are statistically significant differences in variances

t.test(x = datos_licenciado$process_of_inquiry,
       y = datos_normalista$process_of_inquiry,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)

# t = 6.9792, df = 491.54, p-value = 9.654e-12
# there are statistically significant differences in average

# effect size: process of inquiry

cohen.d(formula = process_of_inquiry ~ PROGRAMA,
        data= datos_pais, paired = FALSE)
# d estimate: 0.5633947 (medium)

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
            riq = IQR(teacher_direction)) -> td

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213  9.94 0.827   10     7.8  12.5   0.9
# Normalista   405 10.0  1.07    10.2   7.8  13     1 

# statistical inference
shapiro.test(datos_pais$teacher_direction)
ks.test(scale(datos_pais$teacher_direction), "pnorm")
lillie.test(datos_pais$teacher_direction)

qqnorm(datos_pais$teacher_direction, las=1, pch=18, 
       main="Teacher Direction", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$teacher_direction)

shapiro.test(datos_normalista$teacher_direction)
# W = 0.90567, p-value = 3.612e-15
ks.test(scale(datos_normalista$teacher_direction), "pnorm")
# D = 0.15874, p-value = 2.732e-09
lillie.test(datos_normalista$teacher_direction)
# D = 0.15874, p-value < 2.2e-16

qqnorm(datos_normalista$teacher_direction, las=1, pch=18, 
       main="Normalista Teacher Direction ", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$teacher_direction)

shapiro.test(datos_licenciado$teacher_direction)
# W = 0.95679, p-value = 4.757e-06
ks.test(scale(datos_licenciado$teacher_direction), "pnorm")
# D = 0.085889, p-value = 0.08634
lillie.test(datos_licenciado$teacher_direction)
# D = 0.085889, p-value = 0.0006209

qqnorm(datos_licenciado$teacher_direction, las=1, pch=18, 
       main="Licenciados Teacher Direction", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$teacher_direction)

# result of the normal distribution
# the data don't have the behavior of a normal distribution
# non-parametric test

wilcox.test(datos_licenciado$teacher_direction,
            datos_normalista$teacher_direction,paired = F,
            exact = F,correct = T,conf.int = 0.95)
# W = 36962, p-value = 0.003392
# result of the non-parametric test
# there are statistically significant differences 

# effect size non-parametric 
r3 <- rank_biserial(datos_normalista$teacher_direction,
              datos_licenciado$teacher_direction)

# r (rank biserial) |       95% CI
# 0.14              | [0.05, 0.24]

interpret_r(r3$r_rank_biserial, rules = "funder2019")
# [1] "small"

# parametric test

# homoscedasticity test
var.test(x = datos_licenciado$teacher_direction,
         y = datos_normalista$teacher_direction)
# result
# F = 0.59896, num df = 212, denom df = 404, p-value = 3.659e-05

fligner.test(teacher_direction ~ PROGRAMA, data= datos_pais)
# result
# Fligner-Killeen:med chi-squared = 4.897, df = 1, p-value = 0.0269

leveneTest(teacher_direction ~ factor(PROGRAMA), data = datos_pais,
           center = "median")
# result
# Df F value  Pr(>F)  
# group   1  6.6594 0.01009 *
#   616

bartlett.test(list(datos_licenciado$teacher_direction,
                   datos_normalista$teacher_direction))
# result
# Bartlett's K-squared = 17.141, df = 1, p-value = 3.471e-05

# result of homoscedasticity test
# there are statistically significant differences in variances

t.test(x = datos_normalista$teacher_direction,
       y = datos_licenciado$teacher_direction,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = 1.31, df = 532.37, p-value = 0.1908
# there aren't statistically significant differences in average

# effect size: Teacher Direction

cohen.d(formula = teacher_direction ~ factor(PROGRAMA),
        data= datos_pais, paired = FALSE)
# d estimate: 0.1025368 (negligible)

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
            riq = IQR(active_learning)) -> al

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.3  0.868   10.3     8  11.8 0.800
# Normalista   405  9.83 1.04    10       8  11.8 1.4 

# statistical inference
shapiro.test(datos_pais$active_learning)
ks.test(scale(datos_pais$active_learning), "pnorm")
lillie.test(datos_pais$active_learning)

qqnorm(datos_pais$active_learning, las=1, pch=18, 
       main="Active Learning de los profesores", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$active_learning)

shapiro.test(datos_normalista$active_learning)
# W = 0.94281, p-value = 2.211e-11
ks.test(scale(datos_normalista$active_learning), "pnorm")
# D = 0.1195, p-value = 1.893e-05
lillie.test(datos_normalista$active_learning)
# D = 0.1195, p-value = 3.307e-15

qqnorm(datos_normalista$active_learning, las=1, pch=18, 
       main="Active Learning de los normalistas", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$active_learning)

shapiro.test(datos_licenciado$active_learning)
# W = 0.89812, p-value = 7.387e-11
ks.test(scale(datos_licenciado$active_learning), "pnorm")
# D = 0.14792, p-value = 0.000179
lillie.test(datos_licenciado$active_learning)
# D = 0.14792, p-value = 2.836e-12

qqnorm(datos_licenciado$active_learning, las=1, pch=18, 
       main="Active Learning de los licenciados", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$active_learning)

# result of the normal distribution
# the data don't have the behavior of a normal distribution

# non-parametric test
wilcox.test(datos_licenciado$active_learning,
            datos_normalista$active_learning,paired = F,
            exact = F,correct = T,conf.int = 0.95)
# W = 55250, p-value = 8.551e-09
# there are statistically significant differences

# effect size non-parametric 
r4 <- rank_biserial(datos_licenciado$active_learning,
                    datos_normalista$active_learning)
# r (rank biserial) |       95% CI
# 0.28              | [0.19, 0.37]

interpret_r(r4$r_rank_biserial, rules = "funder2019")
# "medium"

# parametric test

# homoscedasticity test
var.test(x = datos_licenciado$active_learning,
         y = datos_normalista$active_learning)
# result 
# F = 0.69649, num df = 212, denom df = 404, p-value = 0.003299

fligner.test(active_learning ~ PROGRAMA, data= datos_pais)
# result 
# Fligner-Killeen:med chi-squared = 7.3174, df = 1, p-value = 0.006829

leveneTest(active_learning ~ factor(PROGRAMA), data = datos_pais,
           center = "median")
# result
# Df F value   Pr(>F)    
# group   1  13.747 0.000228 ***
#   616

bartlett.test(list(datos_licenciado$active_learning,
                   datos_normalista$active_learning))
# Bartlett's K-squared = 8.7063, df = 1, p-value = 0.003171

# result of homoscedasticity test
# there are statistically significant differences in variances

t.test(x = datos_licenciado$active_learning,
       y = datos_normalista$active_learning,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)

# t = 6.0243, df = 502.65, p-value = 3.284e-09
# there aren't statistically significant differences in average

# effect size: Active Learning

cohen.d(formula = active_learning ~ PROGRAMA,
        data= datos_pais, paired = FALSE)
# d estimate: 0.4822629 (small)

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
            riq = IQR(fixed_ability)) -> fa

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213  9.98 0.869   10.1   7.9  12.5   1  
# Normalista   405 10.0  1.06    10.2   7.9  12.5   1.1

# statistical inference
shapiro.test(datos_pais$fixed_ability)
ks.test(scale(datos_pais$fixed_ability), "pnorm")
lillie.test(datos_pais$fixed_ability)

qqnorm(datos_pais$fixed_ability, las=1, pch=18, 
       main="Fixed Ability de los profesores", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$fixed_ability)

shapiro.test(datos_normalista$fixed_ability)
# W = 0.92111, p-value = 9.538e-14
ks.test(scale(datos_normalista$fixed_ability), "pnorm")
# D = 0.14597, p-value = 6.392e-08
lillie.test(datos_normalista$fixed_ability)
# D = 0.14597, p-value < 2.2e-16

qqnorm(datos_normalista$fixed_ability, las=1, pch=18, 
       main="Fixed Ability de los normalistas", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$fixed_ability)

shapiro.test(datos_licenciado$fixed_ability)
# W = 0.94703, p-value = 4.84e-07
ks.test(scale(datos_licenciado$fixed_ability), "pnorm")
# D = 0.086804, p-value = 0.08072
lillie.test(datos_licenciado$fixed_ability)
# D = 0.086804, p-value = 0.000509

qqnorm(datos_licenciado$fixed_ability, las=1, pch=18, 
       main="Fixed Ability de los licenciados", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$fixed_ability)

# result of the normal distribution
# the data don't have the behavior of a normal distribution

# non-parametric test
wilcox.test(datos_licenciado$fixed_ability,
            datos_normalista$fixed_ability,paired = F,
            exact = F,correct = T,conf.int = 0.95)
# W = 40408, p-value = 0.1958
# there aren't statistically significant differences

# effect size non-parametric 
r5 <- rank_biserial(datos_normalista$fixed_ability,
                    datos_licenciado$fixed_ability)
# r (rank biserial) |        95% CI
# 0.06              | [-0.03, 0.16]

interpret_r(r5$r_rank_biserial, rules = "funder2019")
# "very small"

# parametric test

# homoscedasticity test
var.test(x = datos_licenciado$fixed_ability,
         y = datos_normalista$fixed_ability)
# F = 0.67057, num df = 212, denom df = 404, p-value = 0.001196

fligner.test(fixed_ability ~ PROGRAMA, data= datos_pais)
# Fligner-Killeen:med chi-squared = 5.3318, df = 1, p-value = 0.02094

leveneTest(fixed_ability ~ factor(PROGRAMA), data = datos_pais,
           center = "median")
# Df F value  Pr(>F)  
# group   1  6.0567 0.01413 *
#   616

bartlett.test(list(datos_licenciado$fixed_ability,
                   datos_normalista$fixed_ability))
# Bartlett's K-squared = 10.576, df = 1, p-value = 0.001146

# result of homoscedasticity test
# there are statistically significant differences in variances

t.test(x = datos_normalista$fixed_ability,
       y = datos_licenciado$fixed_ability,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = 0.38241, df = 510.25, p-value = 0.7023
# there aren't statistically significant differences in average

# effect size: Fixed Ability

cohen.d(formula = fixed_ability ~ PROGRAMA,
        data= datos_pais, paired = FALSE)
# d estimate: -0.03043892 (negligible)

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
            riq = IQR(Prep_for_teach_math)) -> pt

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.2  0.807   10.2   8.1  11.6 0.900
# Normalista   405  9.88 1.07    10     8.1  11.6 1.5 

# statistical inference
shapiro.test(datos_pais$Prep_for_teach_math)
ks.test(scale(datos_pais$Prep_for_teach_math), "pnorm")
lillie.test(datos_pais$Prep_for_teach_math)

qqnorm(datos_pais$Prep_for_teach_math, las=1, pch=18, 
       main="Preparedness for Teaching Mathematics de los profesores", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$Prep_for_teach_math)

shapiro.test(datos_normalista$Prep_for_teach_math)
# W = 0.93088, p-value = 9.588e-13
ks.test(scale(datos_normalista$Prep_for_teach_math), "pnorm")
# D = 0.10295, p-value = 0.0003741
lillie.test(datos_normalista$Prep_for_teach_math)
# D = 0.10295, p-value = 4.235e-11

qqnorm(datos_normalista$Prep_for_teach_math, las=1, pch=18, 
       main="Preparedness for Teaching Mathematics de los normalistas", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$Prep_for_teach_math)

shapiro.test(datos_licenciado$Prep_for_teach_math)
# W = 0.93266, p-value = 2.487e-08
ks.test(scale(datos_licenciado$Prep_for_teach_math), "pnorm")
# D = 0.11223, p-value = 0.009352
lillie.test(datos_licenciado$Prep_for_teach_math)
# D = 0.11223, p-value = 7.407e-07

qqnorm(datos_licenciado$Prep_for_teach_math, las=1, pch=18, 
       main="Preparedness for Teaching Mathematics de los licenciados", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$Prep_for_teach_math)

# result of the normal distribution
# the data don't have the behavior of a normal distribution

# non-parametric test
wilcox.test(datos_licenciado$Prep_for_teach_math,
            datos_normalista$Prep_for_teach_math,paired = F,
            exact = F,correct = T,conf.int = 0.95)
# W = 50018, p-value = 0.001075
# there are statistically significant differences

# effect size non-parametric 
r6 <- rank_biserial(datos_licenciado$Prep_for_teach_math,
                    datos_normalista$Prep_for_teach_math)
# r (rank biserial) |       95% CI
# 0.16              | [0.07, 0.25]

interpret_r(r6$r_rank_biserial, rules = "funder2019")
# "small"

# parametric test

# homoscedasticity test
var.test(x = datos_licenciado$Prep_for_teach_math,
         y = datos_normalista$Prep_for_teach_math)
# result
# F = 0.56646, num df = 212, denom df = 404, p-value = 5.086e-06

fligner.test(Prep_for_teach_math ~ PROGRAMA, data= datos_pais)
# result
# Fligner-Killeen:med chi-squared = 14.334, df = 1, p-value = 0.0001531

leveneTest(Prep_for_teach_math ~ factor(PROGRAMA), data = datos_pais,
           center = "median")
# result
# Df F value    Pr(>F)    
# group   1  25.811 4.999e-07 ***
#   616 

bartlett.test(list(datos_licenciado$Prep_for_teach_math,
                   datos_normalista$Prep_for_teach_math))
# result
# Bartlett's K-squared = 20.915, df = 1, p-value = 4.802e-06

# result of homoscedasticity test
# there are statistically significant differences in variances

t.test(x = datos_licenciado$Prep_for_teach_math,
       y = datos_normalista$Prep_for_teach_math,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = 4.293, df = 542.85, p-value = 2.088e-05
# there are statistically significant differences in average

# effect size: Preparedness for Teaching Mathematics

cohen.d(formula = Prep_for_teach_math ~ PROGRAMA,
        data= datos_pais, paired = FALSE)
# d estimate: 0.3333059 (small)

# Quality of Instruction ----
quality_of_instr <- dic_creencias %>% 
  filter(subdominio == "Quality of Instruction") %>% 
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
            riq = IQR(quality_of_instr)) -> qi

# result
# PROGRAMA       n media    sd median   min   max   riq
# Licenciado   213 10.1  0.771   10     8.3  11.8 0.800
# Normalista   405  9.97 1.09    10.1   8.3  11.8 1.70 

# statistical inference
shapiro.test(datos_pais$quality_of_instr)
ks.test(scale(datos_pais$quality_of_instr), "pnorm")
lillie.test(datos_pais$quality_of_instr)

qqnorm(datos_pais$quality_of_instr, las=1, pch=18, 
       main="Quality of Instruction de los profesores", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$quality_of_instr)

shapiro.test(datos_normalista$quality_of_instr)
# W = 0.93809, p-value = 6.093e-12
ks.test(scale(datos_normalista$quality_of_instr), "pnorm")
# D = 0.10967, p-value = 0.0001174
lillie.test(datos_normalista$quality_of_instr)
# D = 0.10967, p-value = 1.096e-12

qqnorm(datos_normalista$quality_of_instr, las=1, pch=18, 
       main="Quality of Instruction de los normalistas", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$quality_of_instr)

shapiro.test(datos_licenciado$quality_of_instr)
# W = 0.94883, p-value = 7.235e-07
ks.test(scale(datos_licenciado$quality_of_instr), "pnorm")
# D = 0.10237, p-value = 0.02303
lillie.test(datos_licenciado$quality_of_instr)
# D = 0.10237, p-value = 1.177e-05

qqnorm(datos_licenciado$quality_of_instr, las=1, pch=18, 
       main="Quality of Instruction de los licenciados", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$quality_of_instr)

# result of the normal distribution
# the data don't have the behavior of a normal distribution

# non-parametric test
wilcox.test(datos_licenciado$quality_of_instr,
            datos_normalista$quality_of_instr,paired = F,
            exact = F,correct = T,conf.int = 0.95)
# W = 44532, p-value = 0.5063
# there are statistically significant differences

# effect size non-parametric 
r7 <- rank_biserial(datos_licenciado$quality_of_instr,
                    datos_normalista$quality_of_instr)
# r (rank biserial) |       95% CI
# 0.03              | [-0.06, 0.13]

interpret_r(r7$r_rank_biserial, rules = "funder2019")
# "tiny"

# parametric test

# homoscedasticity test
var.test(x = datos_licenciado$quality_of_instr,
         y = datos_normalista$quality_of_instr)
# result
# F = 0.49712, num df = 212, denom df = 404, p-value = 2.689e-08

fligner.test(quality_of_instr ~ PROGRAMA, data= datos_pais)
# result
# Fligner-Killeen:med chi-squared = 26.72, df = 1, p-value = 2.352e-07

leveneTest(quality_of_instr ~ factor(PROGRAMA), data = datos_pais,
           center = "median")
# result 
# Df F value    Pr(>F)    
# group   1  43.712 8.253e-11 ***
#   616

bartlett.test(list(datos_licenciado$quality_of_instr,
                   datos_normalista$quality_of_instr))
# result
# Bartlett's K-squared = 31.053, df = 1, p-value = 2.511e-08

# result of homoscedasticity test
# there are statistically significant differences in variances

t.test(x = datos_licenciado$quality_of_instr,
       y = datos_normalista$quality_of_instr,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = 1.4767, df = 565.63, p-value = 0.1403
# there aren't statistically significant differences in average

# effect size: Quality of Instruction
cohen.d(formula = quality_of_instr ~ PROGRAMA,
        data= datos_pais, paired = FALSE)
# d estimate: 0.1125414 (negligible)
