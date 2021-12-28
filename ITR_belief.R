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

# age ----
datos_pais %>%
  summarise(n = length(MFA001),
            media = mean(MFA001),
            sd = sd(MFA001),
            mediana = median(MFA001),
            min = min(MFA001),
            max = max(MFA001))

# n media    sd mediana   min   max
# 1   618  23.1  6.68      21    16    55

datos_pais %>%
  group_by(PROGRAMA)%>%
  summarise(n = length(MFA001),
            media = mean(MFA001),
            sd = sd(MFA001),
            mediana = median(MFA001),
            min = min(MFA001),
            max = max(MFA001))

# PROGRAMA       n media    sd mediana   min   max
# Licenciado   213  24.3  6.21      22    19    55
# Normalista   405  22.4  6.82      19    16    51

# statistical inference

shapiro.test(datos_pais$MFA001)
ks.test(scale(datos_pais$MFA001), "pnorm")
lillie.test(datos_pais$MFA001)

qqnorm(datos_pais$MFA001, las=1, pch=18, 
       main="age of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_pais$MFA001)

shapiro.test(datos_normalista$MFA001)
# W = 0.7138, p-value < 2.2e-16
ks.test(scale(datos_normalista$MFA001), "pnorm")
# D = 0.27467, p-value < 2.2e-16
lillie.test(datos_normalista$MFA001)
# D = 0.27467, p-value < 2.2e-16

qqnorm(datos_normalista$MFA001, las=1, pch=18, 
       main="age of the normalists", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_normalista$MFA001)

shapiro.test(datos_licenciado$MFA001)
# W = 0.69095, p-value < 2.2e-16
ks.test(scale(datos_licenciado$MFA001), "pnorm")
# D = 0.23531, p-value = 1.14e-10
lillie.test(datos_licenciado$MFA001)
# D = 0.23531, p-value < 2.2e-16

qqnorm(datos_licenciado$MFA001, las=1, pch=18, 
       main="age of the graduaded", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(datos_licenciado$MFA001)

# result of the normal distribution
# the data don't have the behavior of a normal distribution

# non-parametric test
wilcox.test(datos_licenciado$MFA001,
            datos_normalista$MFA001,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 61924, p-value < 2.2e-16
# result of the non-parametric test
# there are statistically significant differences

fligner.test(list(datos_licenciado$MFA001,datos_normalista$MFA001))

leveneTest(MFA001 ~ PROGRAMA, data = datos_col, center = "median")

var.test(x = datos_normalista$MFA001,
         y = datos_licenciado$MFA001)

bartlett.test(list(datos_normalista$MFA001,datos_licenciado$MFA001))

t.test(x = datos_licenciado$MFA001,
       y = datos_normalista$MFA001,
       alternative = "two.sided", 
       mu = 0, var.equal = TRUE, 
       conf.level = 0.95)

# gender ----

datos_pais %>%
  count(MFA002)%>%
  mutate(P = n/sum(n)*100)

d <-table(datos_pais$PROGRAMA,datos_pais$MFA002)
(d[,1]/423)*100
(d[,2]/195)*100

chisq.test(d)
#X-squared = 68.043, df = 1, p-value < 2.2e-16

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

datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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

# percentage of rules and process ----
datos_pais %>% 
  dplyr::select(PROGRAMA, all_of(rules_and_proc)) %>%
  mutate(MFD001A_d = ifelse(MFD001A == 6 | MFD001A == 5,1,0),
         MFD001B_d = ifelse(MFD001B == 6 | MFD001B == 5,1,0),
         MFD001E_d = ifelse(MFD001E == 6 | MFD001E == 5,1,0),
         MFD001G_d = ifelse(MFD001G == 6 | MFD001G == 5,1,0),
         MFD001K_d = ifelse(MFD001K == 6 | MFD001K == 5,1,0),
         MFD001L_d = ifelse(MFD001L == 6 | MFD001L == 5,1,0)) -> datos_rp

datos_rp %>% 
  mutate(porc_rp = ((MFD001A_d+MFD001B_d+MFD001E_d+
                       MFD001G_d+MFD001K_d+MFD001L_d)/6)*100) -> datos_rp

datos_rp %>%
  group_by(PROGRAMA) %>%
  summarise(media = mean(porc_rp)) -> p_rp

# PROGRAMA    media
# Licenciado  52.0
# Normalista  52.0

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

datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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

# percentage of process of inquiry ----
datos_pais %>% 
  dplyr::select(PROGRAMA, all_of(process_of_inquiry)) %>%
  mutate(MFD001C_d = ifelse(MFD001C == 6 | MFD001C == 5,1,0),
         MFD001D_d = ifelse(MFD001D == 6 | MFD001D == 5,1,0),
         MFD001F_d = ifelse(MFD001F == 6 | MFD001F == 5,1,0),
         MFD001H_d = ifelse(MFD001H == 6 | MFD001H == 5,1,0),
         MFD001I_d = ifelse(MFD001I == 6 | MFD001I == 5,1,0),
         MFD001J_d = ifelse(MFD001J == 6 | MFD001J == 5,1,0)) -> datos_pi

datos_pi %>% 
  mutate(porc_pi = ((MFD001C_d+MFD001D_d+MFD001F_d+
                       MFD001H_d+MFD001I_d+MFD001J_d)/6)*100) -> datos_pi

datos_pi %>%
  group_by(PROGRAMA) %>%
  summarise(media = mean(porc_pi)) -> p_pi

# PROGRAMA       n media
# Licenciado   213  60.3
# Normalista   405  41.0

# graph percentage of rules and process and process of inquiry ----
graf_porc_rp_pi <- data.frame("Program" = c("Graduaded","Normalist",
                                             "Graduaded","Normalist"),
                              "M"=c("Rules and procedures","Rules and procedures",
                                    "Process of inquiry","Process of inquiry"),
                              "Porcentaje_promedio" = c(p_rp$media,p_pi$media))

graf_porc_rp_pi$M <- factor(graf_porc_rp_pi$M,
                            levels = c("Rules and procedures",
                                       "Process of inquiry"),
                               ordered = TRUE)

graf_porc_rp_pi %>% 
  ggplot(., aes(x=M, y=Porcentaje_promedio, fill=Program)) + 
  geom_col(position="dodge",colour="black",width=0.5)+
  labs(x= "Mathematics as",
       y="Average percent of support")+
  #scale_fill_manual(values = c("#8cc482","#6a9cad"))+
  scale_fill_manual(values = c("#919191","#dedede"))+
  annotate("text", label = c("52.03%", "51.98%","60.33%","40.99%"),
           x = c(0.88,1.15,1.88,2.15),
           y = c(55,54.5,63,44),
           size = 5)+
  scale_y_continuous(limit=c(0,100))+
  facet_grid(.~ "Nature of mathematics")+
  theme_bw()+
  theme(text=element_text(family="Times New Roman",
                          face="bold",
                          size=14),
        axis.text.x  = element_text(face="bold",colour="#000000"))


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

datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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

# percentage of teacher direction ----
datos_pais %>% 
  dplyr::select(PROGRAMA, all_of(teacher_direction)) %>%
  mutate(MFD002A_d = ifelse(MFD002A == 6 | MFD002A == 5,1,0),
         MFD002B_d = ifelse(MFD002B == 6 | MFD002B == 5,1,0),
         MFD002C_d = ifelse(MFD002C == 6 | MFD002C == 5,1,0),
         MFD002D_d = ifelse(MFD002D == 6 | MFD002D == 5,1,0),
         MFD002E_d = ifelse(MFD002E == 6 | MFD002E == 5,1,0),
         MFD002F_d = ifelse(MFD002F == 6 | MFD002F == 5,1,0),
         MFD002I_d = ifelse(MFD002I == 6 | MFD002I == 5,1,0),
         MFD002J_d = ifelse(MFD002J == 6 | MFD002J == 5,1,0)) -> datos_td

datos_td %>% 
  mutate(porc_td = ((MFD002A_d+MFD002B_d+MFD002C_d+MFD002D_d+
                     MFD002E_d+MFD002F_d+MFD002I_d+MFD002J_d)/8)*100) -> datos_td

datos_td %>%
  group_by(PROGRAMA) %>%
  summarise(media = mean(porc_td)) -> p_td

# PROGRAMA   media
# Licenciado  17.4
# Normalista  25.8


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

datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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

# percentage of active learning ----
datos_pais %>% 
  dplyr::select(PROGRAMA, all_of(active_learning)) %>%
  mutate(MFD002G_d = ifelse(MFD002G == 6 | MFD002G == 5,1,0),
         MFD002H_d = ifelse(MFD002H == 6 | MFD002H == 5,1,0),
         MFD002K_d = ifelse(MFD002K == 6 | MFD002K == 5,1,0),
         MFD002L_d = ifelse(MFD002L == 6 | MFD002L == 5,1,0),
         MFD002M_d = ifelse(MFD002M == 6 | MFD002M == 5,1,0),
         MFD002N_d = ifelse(MFD002N == 6 | MFD002N == 5,1,0)) -> datos_al

datos_al %>% 
  mutate(porc_al = ((MFD002G_d+MFD002H_d+MFD002K_d+MFD002L_d+
                     MFD002M_d+MFD002N_d)/6)*100) -> datos_al

datos_al %>%
  group_by(PROGRAMA) %>%
  summarise(media = mean(porc_al)) -> p_al

# PROGRAMA   media
# Licenciado  68.6
# Normalista  48.2

# graph percentage of teacher direction and active learning ----
graf_porc_td_al <- data.frame("Program" = c("Graduaded","Normalist",
                                            "Graduaded","Normalist"),
                              "M"=c("Teacher direction","Teacher direction",
                                    "Active learning","Active learning"),
                              "Porcentaje_promedio" = c(p_td$media,p_al$media))

graf_porc_td_al$M <- factor(graf_porc_td_al$M,
                            levels = c("Teacher direction",
                                       "Active learning"),
                            ordered = TRUE)

graf_porc_td_al %>% 
  ggplot(., aes(x=M, y=Porcentaje_promedio, fill=Program)) + 
  geom_col(position="dodge",colour="black",width=0.5)+
  labs(x= "Learning with",
       y="Average percent of support")+
  #scale_fill_manual(values = c("#8cc482","#6a9cad"))+
  scale_fill_manual(values = c("#919191","#dedede"))+
  annotate("text", label = c("17.43%", "25.83%","68.62%","48.19%"),
           x = c(0.88,1.15,1.88,2.15),
           y = c(20,28.5,71,51),
           size = 5)+
  scale_y_continuous(limit=c(0,100))+
  facet_grid(.~ "Learning mathematics")+
  theme_bw()+
  theme(text=element_text(family="Times New Roman",
                          face="bold",
                          size=14),
        axis.text.x  = element_text(face="bold",colour="#000000"))

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

datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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

# percentage of fixed ability ----
datos_pais %>% 
  dplyr::select(PROGRAMA, all_of(fixed_ability)) %>%
  mutate(MFD003A_d = ifelse(MFD003A == 6 | MFD003A== 5,1,0),
         MFD003B_d = ifelse(MFD003B == 6 | MFD003B == 5,1,0),
         MFD003C_d = ifelse(MFD003C == 6 | MFD003C == 5,1,0),
         MFD003D_d = ifelse(MFD003D == 6 | MFD003D == 5,1,0),
         MFD003E_d = ifelse(MFD003E == 6 | MFD003E == 5,1,0),
         MFD003F_d = ifelse(MFD003F == 6 | MFD003F == 5,1,0),
         MFD003G_d = ifelse(MFD003G == 6 | MFD003G == 5,1,0),
         MFD003H_d = ifelse(MFD003H == 6 | MFD003H == 5,1,0)) -> datos_fa

datos_fa %>% 
  mutate(porc_fa = ((MFD003A_d+MFD003B_d+MFD003C_d+MFD003D_d+
                     MFD003E_d+MFD003F_d+MFD003G_d+MFD003H_d)/8)*100) -> datos_fa

datos_fa %>%
  group_by(PROGRAMA) %>%
  summarise(media = mean(porc_fa)) -> p_fa

# PROGRAMA   media
# Licenciado  16.3
# Normalista  20.8

# graph percentage of fixed ability ----
graf_porc_fa <- data.frame("Program" = c("Graduaded","Normalist"),
                              "M"=c("Fixed ability","Fixed ability"),
                              "Porcentaje_promedio" = c(p_fa$media))

graf_porc_fa %>% 
  ggplot(., aes(x="", y=Porcentaje_promedio, fill=Program)) + 
  geom_col(position="dodge",colour="black",width=0.5)+
  labs(x= "", y="Average percent of support")+
  #scale_fill_manual(values = c("#8cc482","#6a9cad"))+
  scale_fill_manual(values = c("#919191","#dedede"))+
  annotate("text", label = c("16.3%", "20.8%"),
           x = c(0.88,1.15),
           y = c(20,24),
           size = 5)+
  scale_y_continuous(limit=c(0,100))+
  facet_grid(.~ "Fixed ability")+
  theme_bw()+
  theme(text=element_text(family="Times New Roman",
                          face="bold",
                          size=14),
        axis.text.x  = element_text(face="bold",colour="#000000"))

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

datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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

# percentage of Preparedness for Teaching Mathematics ----
datos_pais %>% 
  dplyr::select(PROGRAMA, all_of(Prep_for_teach_math)) %>%
  mutate(MFD004A_d = ifelse(MFD004A == 4 | MFD004A== 3,1,0),
         MFD004B_d = ifelse(MFD004B == 4 | MFD004B== 3,1,0),
         MFD004C_d = ifelse(MFD004C == 4 | MFD004C== 3,1,0),
         MFD004D_d = ifelse(MFD004D == 4 | MFD004D== 3,1,0),
         MFD004E_d = ifelse(MFD004E == 4 | MFD004E== 3,1,0),
         MFD004F_d = ifelse(MFD004F == 4 | MFD004F== 3,1,0),
         MFD004G_d = ifelse(MFD004G == 4 | MFD004G== 3,1,0),
         MFD004H_d = ifelse(MFD004H == 4 | MFD004H== 3,1,0),
         MFD004I_d = ifelse(MFD004I == 4 | MFD004I== 3,1,0),
         MFD004J_d = ifelse(MFD004J == 4 | MFD004J== 3,1,0),
         MFD004K_d = ifelse(MFD004K == 4 | MFD004K== 3,1,0),
         MFD004L_d = ifelse(MFD004L == 4 | MFD004L== 3,1,0),
         MFD004M_d = ifelse(MFD004M == 4 | MFD004M== 3,1,0)) -> datos_pftm

datos_pftm %>% 
  mutate(porc_pftm = ((MFD004A_d+MFD004B_d+MFD004C_d+MFD004D_d+MFD004E_d+
                       MFD004F_d+MFD004G_d+MFD004H_d+MFD004I_d+MFD004J_d+
                       MFD004K_d+MFD004L_d+MFD004M_d)/13)*100) -> datos_pftm

datos_pftm %>%
  group_by(PROGRAMA) %>%
  summarise(media = mean(porc_pftm)) -> p_pftm

# PROGRAMA   media
# Licenciado  81.2
# Normalista  63.6

# graph percentage of Preparedness for Teaching Mathematics ----
graf_porc_pftm <- data.frame("Program" = c("Graduaded","Normalist"),
                           "M"=c("Preparedness for teaching mathematics",
                                 "Preparedness for teaching mathematics"),
                           "Porcentaje_promedio" = c(p_pftm$media))

graf_porc_pftm %>% 
  ggplot(., aes(x="", y=Porcentaje_promedio, fill=Program)) + 
  geom_col(position="dodge",colour="black",width=0.5)+
  labs(x= "", y="Average percent of support")+
  #scale_fill_manual(values = c("#8cc482","#6a9cad"))+
  scale_fill_manual(values = c("#919191","#dedede"))+
  annotate("text", label = c("81.2%", "63.6%"),
           x = c(0.88,1.15),
           y = c(84.2,66.6),
           size = 5)+
  scale_y_continuous(limit=c(0,100))+
  facet_grid(.~ "Preparedness for teaching mathematics")+
  theme_bw()+
  theme(text=element_text(family="Times New Roman",
                          face="bold",
                          size=14),
        axis.text.x  = element_text(face="bold",colour="#000000"))

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

datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

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

# percentage of Quality of instruction ----
datos_pais %>% 
  dplyr::select(PROGRAMA, all_of(quality_of_instr)) %>%
  mutate(MFD005A_d = ifelse(MFD005A == 6 | MFD005A== 5,1,0),
         MFD005B_d = ifelse(MFD005B == 6 | MFD005B== 5,1,0),
         MFD005C_d = ifelse(MFD005C == 6 | MFD005C== 5,1,0),
         MFD005D_d = ifelse(MFD005D == 6 | MFD005D== 5,1,0),
         MFD005E_d = ifelse(MFD005E == 6 | MFD005E== 5,1,0),
         MFD005F_d = ifelse(MFD005F == 6 | MFD005F== 5,1,0)) -> datos_qi

datos_qi %>% 
  mutate(porc_qi = ((MFD005A_d+MFD005B_d+MFD005C_d+
                     MFD005D_d+MFD005E_d+MFD005F_d)/6)*100) -> datos_qi

datos_qi %>%
  group_by(PROGRAMA) %>%
  summarise(media = mean(porc_qi)) -> p_qi

# PROGRAMA   media
# Licenciado  49.6
# Normalista  49.8

# graph percentage of Quality of instruction ----
graf_porc_qi <- data.frame("Program" = c("Graduaded","Normalist"),
                             "M"=c("Quality of instruction",
                                   "Quality of instruction"),
                             "Porcentaje_promedio" = c(p_qi$media))

graf_porc_qi %>% 
  ggplot(., aes(x="", y=Porcentaje_promedio, fill=Program)) + 
  geom_col(position="dodge",colour="black",width=0.5)+
  labs(x="", y="Average percent of support")+
  #scale_fill_manual(values = c("#8cc482","#6a9cad"))+
  scale_fill_manual(values = c("#919191","#dedede"))+
  annotate("text", label = c("49.6%", "49.8%"),
           x = c(0.88,1.15),
           y = c(53,53.5),
           size = 5)+
  scale_y_continuous(limit=c(0,100))+
  facet_grid(.~ "Quality of instruction")+
  theme_bw()+
  theme(text=element_text(family="Times New Roman",
                          face="bold",
                          size=14),
        axis.text.x  = element_text(face="bold",colour="#000000"))
                      