# packages -----
library(tidyverse)
library(mirt)
library(readxl)
library(nortest)
library(effsize)
library(effectsize)
library(car)

# Data reading ----
datos_pais <- read_excel("Datos_TEDS_M_Colombia.xlsx",
                         sheet = "bd_Norm_Licen")

  
dic_creencias <- read_excel("Datos_TEDS_M_Colombia.xlsx",
                            sheet = "dic")

# subset data ----
datos_pais %>%
  filter(PROGRAMA=="Normalista") -> datos_normalista

datos_pais %>%
  filter(PROGRAMA=="Licenciado") -> datos_licenciado

# transformaciones de datos ----
dic_creencias %>% 
  filter(subdominio == "Rules and Procedures") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

creencias <- datos_pais %>%
  dplyr::select(PROGRAMA,MFD001A,MFD001B,MFD001E,MFD001G,MFD001K,MFD001L,
                MFD001C,MFD001D,MFD001F,MFD001H,MFD001I,MFD001J,
                MFD002A,MFD002B,MFD002C,MFD002D,MFD002E,MFD002F,MFD002I,
                MFD002J,MFD002G,MFD002H,MFD002K,MFD002L,MFD002M,MFD002N,
                MFD003A,MFD003B,MFD003C,MFD003D,MFD003E,MFD003F,MFD003G,
                MFD003H,MFD004A,MFD004B,MFD004C,MFD004D,MFD004E,MFD004F,MFD004G,
                MFD004H,MFD004I,MFD004J,MFD004K,MFD004L,MFD004M,
                MFD005A,MFD005B,MFD005C,MFD005D,MFD005E,MFD005F) %>%
  mutate(Rules_and_Procedures = ((MFD001A+MFD001B+MFD001E
                                  +MFD001G+MFD001K+MFD001L)/6),
         Process_of_Inquiry = ((MFD001C+MFD001D+MFD001F+
                        MFD001H+MFD001I+MFD001J)/6),
         Teacher_Direction = ((MFD002A+MFD002B+MFD002C+MFD002D+MFD002E+
                               MFD002F+MFD002I+MFD002J)/8),
         Active_Learning = ((MFD002G+MFD002H+MFD002K+MFD002L+MFD002M+MFD002N)/6),
         Fixed_Ability = ((MFD003A+MFD003B+MFD003C+MFD003D+MFD003E+MFD003F+MFD003G+
                           MFD003H)/8),
         Preparedness_for_Tea_Mat = ((MFD004A+MFD004B+MFD004C+MFD004D+MFD004E+MFD004F+MFD004G+
                                      MFD004H+MFD004I+MFD004J+MFD004K+MFD004L+MFD004M)/13),
         Quality_of_Instruction = ((MFD005A+MFD005B+MFD005C+MFD005D+MFD005E+MFD005F)/6)) 


# subset data ----
creencias %>%
  filter(PROGRAMA=="Normalista") -> c_normalista

creencias %>%
  filter(PROGRAMA=="Licenciado") -> c_licenciado

# Rules_and_Procedures total ----
creencias %>%
  summarise(n = length(Rules_and_Procedures),
            media= mean(Rules_and_Procedures),
            sd = sd(Rules_and_Procedures),
            median = median(Rules_and_Procedures),
            min = min(Rules_and_Procedures),
            max = max(Rules_and_Procedures),
            riq = IQR(Rules_and_Procedures))

creencias %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Rules_and_Procedures),
            media= mean(Rules_and_Procedures),
            sd = sd(Rules_and_Procedures),
            median = median(Rules_and_Procedures),
            min = min(Rules_and_Procedures),
            max = max(Rules_and_Procedures),
            riq = IQR(Rules_and_Procedures))

# non-parametric test
wilcox.test(c_normalista$Rules_and_Procedures,
            c_licenciado$Rules_and_Procedures,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(c_normalista$Rules_and_Procedures,
                     c_licenciado$Rules_and_Procedures)

r1A

# Process of Inquiry total ----
creencias %>%
  summarise(n = length(Process_of_Inquiry),
            media= mean(Process_of_Inquiry),
            sd = sd(Process_of_Inquiry),
            median = median(Process_of_Inquiry),
            min = min(Process_of_Inquiry),
            max = max(Process_of_Inquiry),
            riq = IQR(Process_of_Inquiry))

creencias %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Process_of_Inquiry),
            media= mean(Process_of_Inquiry),
            sd = sd(Process_of_Inquiry),
            median = median(Process_of_Inquiry),
            min = min(Process_of_Inquiry),
            max = max(Process_of_Inquiry),
            riq = IQR(Process_of_Inquiry))

# non-parametric test
wilcox.test(c_normalista$Process_of_Inquiry,
            c_licenciado$Process_of_Inquiry,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(c_normalista$Process_of_Inquiry,
                     c_licenciado$Process_of_Inquiry)

r1A

# Teacher Direction total ----
creencias %>%
  summarise(n = length(Teacher_Direction),
            media= mean(Teacher_Direction),
            sd = sd(Teacher_Direction),
            median = median(Teacher_Direction),
            min = min(Teacher_Direction),
            max = max(Teacher_Direction),
            riq = IQR(Teacher_Direction))

creencias %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Teacher_Direction),
            media= mean(Teacher_Direction),
            sd = sd(Teacher_Direction),
            median = median(Teacher_Direction),
            min = min(Teacher_Direction),
            max = max(Teacher_Direction),
            riq = IQR(Teacher_Direction))

# non-parametric test
wilcox.test(c_normalista$Teacher_Direction,
            c_licenciado$Teacher_Direction,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(c_normalista$Teacher_Direction,
                     c_licenciado$Teacher_Direction)

r1A

# Active Learning total ----
creencias %>%
  summarise(n = length(Active_Learning),
            media= mean(Active_Learning),
            sd = sd(Active_Learning),
            median = median(Active_Learning),
            min = min(Active_Learning),
            max = max(Active_Learning),
            riq = IQR(Active_Learning))

creencias %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Active_Learning),
            media= mean(Active_Learning),
            sd = sd(Active_Learning),
            median = median(Active_Learning),
            min = min(Active_Learning),
            max = max(Active_Learning),
            riq = IQR(Active_Learning))

# non-parametric test
wilcox.test(c_normalista$Active_Learning,
            c_licenciado$Active_Learning,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(c_normalista$Active_Learning,
                     c_licenciado$Active_Learning)

r1A

# Fixed Ability total ----
creencias %>%
  summarise(n = length(Fixed_Ability),
            media= mean(Fixed_Ability),
            sd = sd(Fixed_Ability),
            median = median(Fixed_Ability),
            min = min(Fixed_Ability),
            max = max(Fixed_Ability),
            riq = IQR(Fixed_Ability))

creencias %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Fixed_Ability),
            media= mean(Fixed_Ability),
            sd = sd(Fixed_Ability),
            median = median(Fixed_Ability),
            min = min(Fixed_Ability),
            max = max(Fixed_Ability),
            riq = IQR(Fixed_Ability))

# non-parametric test
wilcox.test(c_normalista$Fixed_Ability,
            c_licenciado$Fixed_Ability,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(c_normalista$Fixed_Ability,
                     c_licenciado$Fixed_Ability)

r1A

# Quality of Instruction total ----
creencias %>%
  summarise(n = length(Quality_of_Instruction),
            media= mean(Quality_of_Instruction),
            sd = sd(Quality_of_Instruction),
            median = median(Quality_of_Instruction),
            min = min(Quality_of_Instruction),
            max = max(Quality_of_Instruction),
            riq = IQR(Quality_of_Instruction))

creencias %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Quality_of_Instruction),
            media= mean(Quality_of_Instruction),
            sd = sd(Quality_of_Instruction),
            median = median(Quality_of_Instruction),
            min = min(Quality_of_Instruction),
            max = max(Quality_of_Instruction),
            riq = IQR(Quality_of_Instruction))

# non-parametric test
wilcox.test(c_normalista$Quality_of_Instruction,
            c_licenciado$Quality_of_Instruction,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(c_normalista$Quality_of_Instruction,
                     c_licenciado$Quality_of_Instruction)

r1A

# Preparedness for Tea Mat total ----
creencias %>%
  summarise(n = length(Preparedness_for_Tea_Mat),
            media= mean(Preparedness_for_Tea_Mat),
            sd = sd(Preparedness_for_Tea_Mat),
            median = median(Preparedness_for_Tea_Mat),
            min = min(Preparedness_for_Tea_Mat),
            max = max(Preparedness_for_Tea_Mat),
            riq = IQR(Preparedness_for_Tea_Mat))

creencias %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(Preparedness_for_Tea_Mat),
            media= mean(Preparedness_for_Tea_Mat),
            sd = sd(Preparedness_for_Tea_Mat),
            median = median(Preparedness_for_Tea_Mat),
            min = min(Preparedness_for_Tea_Mat),
            max = max(Preparedness_for_Tea_Mat),
            riq = IQR(Preparedness_for_Tea_Mat))

# non-parametric test
wilcox.test(c_normalista$Preparedness_for_Tea_Mat,
            c_licenciado$Preparedness_for_Tea_Mat,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(c_normalista$Preparedness_for_Tea_Mat,
                     c_licenciado$Preparedness_for_Tea_Mat)

r1A

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
       main="age of the graduated", font.main=1,
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

leveneTest(MFA001 ~ PROGRAMA, data = datos_pais, center = "median")

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

# "MFD001A" 
datos_pais %>%
  select(PROGRAMA,MFD001A) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001A),
            media = mean(MFD001A),
            sd = sd(MFD001A),
            median = median(MFD001A),
            minimo = min(MFD001A),
            maximo = max(MFD001A))

# non-parametric test
wilcox.test(datos_licenciado$MFD001A,
            datos_normalista$MFD001A,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1A <- rank_biserial(datos_normalista$MFD001A,
                    datos_licenciado$MFD001A)

r1A

interpret_r(r1A$r_rank_biserial, rules = "cohen")

# "MFD001B" 
datos_pais %>%
  select(PROGRAMA,MFD001B) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001B),
            media = mean(MFD001B),
            sd = sd(MFD001B),
            median = median(MFD001B),
            minimo = min(MFD001B),
            maximo = max(MFD001B))

# non-parametric test
wilcox.test(datos_licenciado$MFD001B,
            datos_normalista$MFD001B,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1B <- rank_biserial(datos_normalista$MFD001B,
                    datos_licenciado$MFD001B)

r1B

interpret_r(r1B$r_rank_biserial, rules = "cohen")

# "MFD001E" 
datos_pais %>%
  select(PROGRAMA,MFD001E) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001E),
            media = mean(MFD001E),
            sd = sd(MFD001E),
            median = median(MFD001E),
            minimo = min(MFD001E),
            maximo = max(MFD001E))

# non-parametric test
wilcox.test(datos_licenciado$MFD001E,
            datos_normalista$MFD001E,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1E <- rank_biserial(datos_normalista$MFD001E,
                    datos_licenciado$MFD001E)

r1E

interpret_r(r1E$r_rank_biserial, rules = "cohen")

# "MFD001G" 
datos_pais %>%
  select(PROGRAMA,MFD001G) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001G),
            media = mean(MFD001G),
            sd = sd(MFD001G),
            median = median(MFD001G),
            minimo = min(MFD001G),
            maximo = max(MFD001G))

# non-parametric test
wilcox.test(datos_licenciado$MFD001G,
            datos_normalista$MFD001G,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1G <- rank_biserial(datos_licenciado$MFD001G,
                    datos_normalista$MFD001G)

r1G

interpret_r(r1G$r_rank_biserial, rules = "cohen")

# "MFD001K" 
datos_pais %>%
  select(PROGRAMA,MFD001K) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001K),
            media = mean(MFD001K),
            sd = sd(MFD001K),
            median = median(MFD001K),
            minimo = min(MFD001K),
            maximo = max(MFD001K))

# non-parametric test
wilcox.test(datos_licenciado$MFD001K,
            datos_normalista$MFD001K,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1K <- rank_biserial(datos_licenciado$MFD001K,
                    datos_normalista$MFD001K)

r1K

interpret_r(r1K$r_rank_biserial, rules = "cohen")

# "MFD001L"
datos_pais %>%
  select(PROGRAMA,MFD001L) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001L),
            media = mean(MFD001L),
            sd = sd(MFD001L),
            median = median(MFD001L),
            minimo = min(MFD001L),
            maximo = max(MFD001L))

# non-parametric test
wilcox.test(datos_licenciado$MFD001L,
            datos_normalista$MFD001L,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1L <- rank_biserial(datos_licenciado$MFD001L,
                    datos_normalista$MFD001L)

r1L

interpret_r(r1L$r_rank_biserial, rules = "cohen")

# Process of Inquiry ----
process_of_inquiry <- dic_creencias %>% 
  filter(subdominio == "Process of Inquiry") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# "MFD001C" 
datos_pais %>%
  select(PROGRAMA,MFD001C) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001C),
            media = mean(MFD001C),
            sd = sd(MFD001C),
            median = median(MFD001C),
            minimo = min(MFD001C),
            maximo = max(MFD001C))

# non-parametric test
wilcox.test(datos_licenciado$MFD001C,
            datos_normalista$MFD001C,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1C <- rank_biserial(datos_licenciado$MFD001C,
                     datos_normalista$MFD001C)

r1C

interpret_r(r1C$r_rank_biserial, rules = "cohen")

# "MFD001D" 
datos_pais %>%
  select(PROGRAMA,MFD001D) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001D),
            media = mean(MFD001D),
            sd = sd(MFD001D),
            median = median(MFD001D),
            minimo = min(MFD001D),
            maximo = max(MFD001D))

# non-parametric test
wilcox.test(datos_licenciado$MFD001D,
            datos_normalista$MFD001D,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1D <- rank_biserial(datos_licenciado$MFD001D,
                     datos_normalista$MFD001D)

r1D

interpret_r(r1D$r_rank_biserial, rules = "cohen")

# "MFD001F"
datos_pais %>%
  select(PROGRAMA,MFD001F) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001F),
            media = mean(MFD001F),
            sd = sd(MFD001F),
            median = median(MFD001F),
            minimo = min(MFD001F),
            maximo = max(MFD001F))

# non-parametric test
wilcox.test(datos_licenciado$MFD001F,
            datos_normalista$MFD001F,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1F <- rank_biserial(datos_licenciado$MFD001F,
                     datos_normalista$MFD001F)

r1F

interpret_r(r1F$r_rank_biserial, rules = "cohen")

# "MFD001H" 
datos_pais %>%
  select(PROGRAMA,MFD001H) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001H),
            media = mean(MFD001H),
            sd = sd(MFD001H),
            median = median(MFD001H),
            minimo = min(MFD001H),
            maximo = max(MFD001H))

# non-parametric test
wilcox.test(datos_licenciado$MFD001H,
            datos_normalista$MFD001H,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1H <- rank_biserial(datos_licenciado$MFD001H,
                     datos_normalista$MFD001H)

r1H

interpret_r(r1H$r_rank_biserial, rules = "cohen")

# "MFD001I"
datos_pais %>%
  select(PROGRAMA,MFD001I) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001I),
            media = mean(MFD001I),
            sd = sd(MFD001I),
            median = median(MFD001I),
            minimo = min(MFD001I),
            maximo = max(MFD001I))

# non-parametric test
wilcox.test(datos_licenciado$MFD001I,
            datos_normalista$MFD001I,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1I <- rank_biserial(datos_licenciado$MFD001I,
                     datos_normalista$MFD001I)

r1I

interpret_r(r1I$r_rank_biserial, rules = "cohen")

# "MFD001J"
datos_pais %>%
  select(PROGRAMA,MFD001J) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD001J),
            media = mean(MFD001J),
            sd = sd(MFD001J),
            median = median(MFD001J),
            minimo = min(MFD001J),
            maximo = max(MFD001J))

# non-parametric test
wilcox.test(datos_licenciado$MFD001J,
            datos_normalista$MFD001J,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r1J <- rank_biserial(datos_licenciado$MFD001J,
                     datos_normalista$MFD001J)

r1J

interpret_r(r1J$r_rank_biserial, rules = "cohen")

# Teacher Direction ----
teacher_direction <- dic_creencias %>% 
  filter(subdominio == "Teacher Direction") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# "MFD002A" 
datos_pais %>%
  select(PROGRAMA,MFD002A) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002A),
            media = mean(MFD002A),
            sd = sd(MFD002A),
            median = median(MFD002A),
            minimo = min(MFD002A),
            maximo = max(MFD002A))

# non-parametric test
wilcox.test(datos_licenciado$MFD002A,
            datos_normalista$MFD002A,paired = F,
            exact = F,correct = T,conf.int = 0.95)

  # effect size non-parametric 
r2A <- rank_biserial(datos_licenciado$MFD002A,
                     datos_normalista$MFD002A)

r2A

interpret_r(r2A$r_rank_biserial, rules = "cohen")

# "MFD002B"
datos_pais %>%
  select(PROGRAMA,MFD002B) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002B),
            media = mean(MFD002B),
            sd = sd(MFD002B),
            median = median(MFD002B),
            minimo = min(MFD002B),
            maximo = max(MFD002B))

# non-parametric test
wilcox.test(datos_licenciado$MFD002B,
            datos_normalista$MFD002B,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2B <- rank_biserial(datos_licenciado$MFD002B,
                     datos_normalista$MFD002B)

r2B

interpret_r(r2B$r_rank_biserial, rules = "cohen")

# "MFD002C"
datos_pais %>%
  select(PROGRAMA,MFD002C) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002C),
            media = mean(MFD002C),
            sd = sd(MFD002C),
            median = median(MFD002C),
            minimo = min(MFD002C),
            maximo = max(MFD002C))

# non-parametric test
wilcox.test(datos_licenciado$MFD002C,
            datos_normalista$MFD002C,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2C <- rank_biserial(datos_licenciado$MFD002C,
                     datos_normalista$MFD002C)

r2C

interpret_r(r2C$r_rank_biserial, rules = "cohen")

# "MFD002D" 
datos_pais %>%
  select(PROGRAMA,MFD002D) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002D),
            media = mean(MFD002D),
            sd = sd(MFD002D),
            median = median(MFD002D),
            minimo = min(MFD002D),
            maximo = max(MFD002D))

# non-parametric test
wilcox.test(datos_licenciado$MFD002D,
            datos_normalista$MFD002D,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2D <- rank_biserial(datos_licenciado$MFD002D,
                     datos_normalista$MFD002D)

r2D

interpret_r(r2D$r_rank_biserial, rules = "cohen")

# "MFD002E"
datos_pais %>%
  select(PROGRAMA,MFD002E) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002E),
            media = mean(MFD002E),
            sd = sd(MFD002E),
            median = median(MFD002E),
            minimo = min(MFD002E),
            maximo = max(MFD002E))

# non-parametric test
wilcox.test(datos_licenciado$MFD002E,
            datos_normalista$MFD002E,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2E <- rank_biserial(datos_licenciado$MFD002E,
                     datos_normalista$MFD002E)

r2E

interpret_r(r2E$r_rank_biserial, rules = "cohen")

# "MFD002F"
datos_pais %>%
  select(PROGRAMA,MFD002F) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002F),
            media = mean(MFD002F),
            sd = sd(MFD002F),
            median = median(MFD002F),
            minimo = min(MFD002F),
            maximo = max(MFD002F))

# non-parametric test
wilcox.test(datos_licenciado$MFD002F,
            datos_normalista$MFD002F,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2F <- rank_biserial(datos_licenciado$MFD002F,
                     datos_normalista$MFD002F)

r2F

interpret_r(r2F$r_rank_biserial, rules = "cohen")

# "MFD002I"
datos_pais %>%
  select(PROGRAMA,MFD002I) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002I),
            media = mean(MFD002I),
            sd = sd(MFD002I),
            median = median(MFD002I),
            minimo = min(MFD002I),
            maximo = max(MFD002I))

# non-parametric test
wilcox.test(datos_licenciado$MFD002I,
            datos_normalista$MFD002I,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2I <- rank_biserial(datos_licenciado$MFD002I,
                     datos_normalista$MFD002I)

r2I

interpret_r(r2I$r_rank_biserial, rules = "cohen")

# "MFD002J"
datos_pais %>%
  select(PROGRAMA,MFD002J) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002J),
            media = mean(MFD002J),
            sd = sd(MFD002J),
            median = median(MFD002J),
            minimo = min(MFD002J),
            maximo = max(MFD002J))

# non-parametric test
wilcox.test(datos_licenciado$MFD002J,
            datos_normalista$MFD002J,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2J <- rank_biserial(datos_licenciado$MFD002J,
                     datos_normalista$MFD002J)

r2J

interpret_r(r2J$r_rank_biserial, rules = "cohen")

# Active Learning ----
active_learning <- dic_creencias %>% 
  filter(subdominio == "Active Learning") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# "MFD002G" 
datos_pais %>%
  select(PROGRAMA,MFD002G) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002G),
            media = mean(MFD002G),
            sd = sd(MFD002G),
            median = median(MFD002G),
            minimo = min(MFD002G),
            maximo = max(MFD002G))

# non-parametric test
wilcox.test(datos_licenciado$MFD002G,
            datos_normalista$MFD002G,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2G <- rank_biserial(datos_licenciado$MFD002G,
                     datos_normalista$MFD002G)

r2G

interpret_r(r2G$r_rank_biserial, rules = "cohen")

# "MFD002H" 
datos_pais %>%
  select(PROGRAMA,MFD002H) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002H),
            media = mean(MFD002H),
            sd = sd(MFD002H),
            median = median(MFD002H),
            minimo = min(MFD002H),
            maximo = max(MFD002H))

# non-parametric test
wilcox.test(datos_licenciado$MFD002H,
            datos_normalista$MFD002H,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2H <- rank_biserial(datos_licenciado$MFD002H,
                     datos_normalista$MFD002H)

r2H

interpret_r(r2H$r_rank_biserial, rules = "cohen")

# "MFD002K"
datos_pais %>%
  select(PROGRAMA,MFD002K) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002K),
            media = mean(MFD002K),
            sd = sd(MFD002K),
            median = median(MFD002K),
            minimo = min(MFD002K),
            maximo = max(MFD002K))

# non-parametric test
wilcox.test(datos_licenciado$MFD002K,
            datos_normalista$MFD002K,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2K <- rank_biserial(datos_licenciado$MFD002K,
                     datos_normalista$MFD002K)

r2K

interpret_r(r2K$r_rank_biserial, rules = "cohen")

# "MFD002L" 
datos_pais %>%
  select(PROGRAMA,MFD002L) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002L),
            media = mean(MFD002L),
            sd = sd(MFD002L),
            median = median(MFD002L),
            minimo = min(MFD002L),
            maximo = max(MFD002L))

# non-parametric test
wilcox.test(datos_licenciado$MFD002L,
            datos_normalista$MFD002L,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2L <- rank_biserial(datos_licenciado$MFD002L,
                     datos_normalista$MFD002L)

r2L

interpret_r(r2L$r_rank_biserial, rules = "cohen")

# "MFD002M" 
datos_pais %>%
  select(PROGRAMA,MFD002M) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002M),
            media = mean(MFD002M),
            sd = sd(MFD002M),
            median = median(MFD002M),
            minimo = min(MFD002M),
            maximo = max(MFD002M))

# non-parametric test
wilcox.test(datos_licenciado$MFD002M,
            datos_normalista$MFD002M,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2M <- rank_biserial(datos_licenciado$MFD002M,
                     datos_normalista$MFD002M)

r2M

interpret_r(r2M$r_rank_biserial, rules = "cohen")

# "MFD002N"
datos_pais %>%
  select(PROGRAMA,MFD002N) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD002N),
            media = mean(MFD002N),
            sd = sd(MFD002N),
            median = median(MFD002N),
            minimo = min(MFD002N),
            maximo = max(MFD002N))

# non-parametric test
wilcox.test(datos_licenciado$MFD002N,
            datos_normalista$MFD002N,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r2N <- rank_biserial(datos_licenciado$MFD002N,
                     datos_normalista$MFD002N)

r2N

interpret_r(r2N$r_rank_biserial, rules = "cohen")

# Fixed Ability ----
fixed_ability <- dic_creencias %>% 
  filter(subdominio == "Fixed Ability") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# "MFD003A" 
datos_pais %>%
  select(PROGRAMA,MFD003A) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003A),
            media = mean(MFD003A),
            sd = sd(MFD003A),
            median = median(MFD003A),
            minimo = min(MFD003A),
            maximo = max(MFD003A))

# non-parametric test
wilcox.test(datos_licenciado$MFD003A,
            datos_normalista$MFD003A,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3A <- rank_biserial(datos_licenciado$MFD003A,
                     datos_normalista$MFD003A)

r3A

interpret_r(r3A$r_rank_biserial, rules = "cohen")

# "MFD003B" 
datos_pais %>%
  select(PROGRAMA,MFD003B) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003B),
            media = mean(MFD003B),
            sd = sd(MFD003B),
            median = median(MFD003B),
            minimo = min(MFD003B),
            maximo = max(MFD003B))

# non-parametric test
wilcox.test(datos_licenciado$MFD003B,
            datos_normalista$MFD003B,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3B <- rank_biserial(datos_licenciado$MFD003B,
                     datos_normalista$MFD003B)

r3B

interpret_r(r3B$r_rank_biserial, rules = "cohen")

# "MFD003C" 
datos_pais %>%
  select(PROGRAMA,MFD003C) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003C),
            media = mean(MFD003C),
            sd = sd(MFD003C),
            median = median(MFD003C),
            minimo = min(MFD003C),
            maximo = max(MFD003C))

# non-parametric test
wilcox.test(datos_licenciado$MFD003C,
            datos_normalista$MFD003C,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3C <- rank_biserial(datos_licenciado$MFD003C,
                     datos_normalista$MFD003C)

r3C

interpret_r(r3C$r_rank_biserial, rules = "cohen")

# "MFD003D" 
datos_pais %>%
  select(PROGRAMA,MFD003D) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003D),
            media = mean(MFD003D),
            sd = sd(MFD003D),
            median = median(MFD003D),
            minimo = min(MFD003D),
            maximo = max(MFD003D))

# non-parametric test
wilcox.test(datos_licenciado$MFD003D,
            datos_normalista$MFD003D,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3D <- rank_biserial(datos_licenciado$MFD003D,
                     datos_normalista$MFD003D)

r3D

interpret_r(r3D$r_rank_biserial, rules = "cohen")

# "MFD003E" 
datos_pais %>%
  select(PROGRAMA,MFD003E) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003E),
            media = mean(MFD003E),
            sd = sd(MFD003E),
            median = median(MFD003E),
            minimo = min(MFD003E),
            maximo = max(MFD003E))

# non-parametric test
wilcox.test(datos_licenciado$MFD003E,
            datos_normalista$MFD003E,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3E <- rank_biserial(datos_licenciado$MFD003E,
                     datos_normalista$MFD003E)

r3E

interpret_r(r3E$r_rank_biserial, rules = "cohen")

# "MFD003F"
datos_pais %>%
  select(PROGRAMA,MFD003F) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003F),
            media = mean(MFD003F),
            sd = sd(MFD003F),
            median = median(MFD003F),
            minimo = min(MFD003F),
            maximo = max(MFD003F))

# non-parametric test
wilcox.test(datos_licenciado$MFD003F,
            datos_normalista$MFD003F,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3F <- rank_biserial(datos_licenciado$MFD003F,
                     datos_normalista$MFD003F)

r3F

interpret_r(r3F$r_rank_biserial, rules = "cohen")

# "MFD003G" 
datos_pais %>%
  select(PROGRAMA,MFD003G) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003G),
            media = mean(MFD003G),
            sd = sd(MFD003G),
            median = median(MFD003G),
            minimo = min(MFD003G),
            maximo = max(MFD003G))

# non-parametric test
wilcox.test(datos_licenciado$MFD003G,
            datos_normalista$MFD003G,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3G <- rank_biserial(datos_licenciado$MFD003G,
                     datos_normalista$MFD003G)

r3G

interpret_r(r3G$r_rank_biserial, rules = "cohen")

# "MFD003H"
datos_pais %>%
  select(PROGRAMA,MFD003H) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD003H),
            media = mean(MFD003H),
            sd = sd(MFD003H),
            median = median(MFD003H),
            minimo = min(MFD003H),
            maximo = max(MFD003H))

# non-parametric test
wilcox.test(datos_licenciado$MFD003H,
            datos_normalista$MFD003H,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r3H <- rank_biserial(datos_licenciado$MFD003H,
                     datos_normalista$MFD003H)

r3H

interpret_r(r3H$r_rank_biserial, rules = "cohen")

# Quality of Instruction ----
quality_of_instr <- dic_creencias %>% 
  filter(subdominio == "Quality of Instruction") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# "MFD005A" 
datos_pais %>%
  select(PROGRAMA,MFD005A) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD005A),
            media = mean(MFD005A),
            sd = sd(MFD005A),
            median = median(MFD005A),
            minimo = min(MFD005A),
            maximo = max(MFD005A))

# non-parametric test
wilcox.test(datos_licenciado$MFD005A,
            datos_normalista$MFD005A,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r5A <- rank_biserial(datos_licenciado$MFD005A,
                     datos_normalista$MFD005A)

r5A

interpret_r(r5A$r_rank_biserial, rules = "cohen")

# "MFD005B" 
datos_pais %>%
  select(PROGRAMA,MFD005B) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD005B),
            media = mean(MFD005B),
            sd = sd(MFD005B),
            median = median(MFD005B),
            minimo = min(MFD005B),
            maximo = max(MFD005B))

# non-parametric test
wilcox.test(datos_licenciado$MFD005B,
            datos_normalista$MFD005B,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r5B <- rank_biserial(datos_licenciado$MFD005B,
                     datos_normalista$MFD005B)

r5B

interpret_r(r5B$r_rank_biserial, rules = "cohen")

# "MFD005C" 
datos_pais %>%
  select(PROGRAMA,MFD005C) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD005C),
            media = mean(MFD005C),
            sd = sd(MFD005C),
            median = median(MFD005C),
            minimo = min(MFD005C),
            maximo = max(MFD005C))

# non-parametric test
wilcox.test(datos_licenciado$MFD005C,
            datos_normalista$MFD005C,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r5C <- rank_biserial(datos_licenciado$MFD005C,
                     datos_normalista$MFD005C)

r5C

interpret_r(r5C$r_rank_biserial, rules = "cohen")

# "MFD005D" 
datos_pais %>%
  select(PROGRAMA,MFD005D) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD005D),
            media = mean(MFD005D),
            sd = sd(MFD005D),
            median = median(MFD005D),
            minimo = min(MFD005D),
            maximo = max(MFD005D))

# non-parametric test
wilcox.test(datos_licenciado$MFD005D,
            datos_normalista$MFD005D,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r5D <- rank_biserial(datos_licenciado$MFD005D,
                     datos_normalista$MFD005D)

r5D

interpret_r(r5D$r_rank_biserial, rules = "cohen")

# "MFD005E" 
datos_pais %>%
  select(PROGRAMA,MFD005E) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD005E),
            media = mean(MFD005E),
            sd = sd(MFD005E),
            median = median(MFD005E),
            minimo = min(MFD005E),
            maximo = max(MFD005E))

# non-parametric test
wilcox.test(datos_licenciado$MFD005E,
            datos_normalista$MFD005E,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r5E <- rank_biserial(datos_licenciado$MFD005E,
                     datos_normalista$MFD005E)

r5E

interpret_r(r5E$r_rank_biserial, rules = "cohen")

# "MFD005F"
datos_pais %>%
  select(PROGRAMA,MFD005F) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD005F),
            media = mean(MFD005F),
            sd = sd(MFD005F),
            median = median(MFD005F),
            minimo = min(MFD005F),
            maximo = max(MFD005F))

# non-parametric test
wilcox.test(datos_licenciado$MFD005F,
            datos_normalista$MFD005F,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r5F <- rank_biserial(datos_licenciado$MFD005F,
                     datos_normalista$MFD005F)

r5F

interpret_r(r5F$r_rank_biserial, rules = "cohen")


# Preparedness for Teaching Mathematics ----
Prep_for_teach_math <- dic_creencias %>% 
  filter(subdominio == "Preparedness for Teaching Mathematics") %>% 
  dplyr::select(idi) %>% 
  pull(idi) %>% 
  as.character()

# "MFD004A" 
datos_pais %>%
  select(PROGRAMA,MFD004A) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004A),
            media = mean(MFD004A),
            sd = sd(MFD004A),
            median = median(MFD004A),
            minimo = min(MFD004A),
            maximo = max(MFD004A))

datos_pais %>%
  select(PROGRAMA,MFD004A)%>%
  group_by(MFD004A)%>%
  summarise(n = length(MFD004A),
            porc=(length(MFD004A)/618)*100)

mfd004a<- table(datos_pais$MFD004A,datos_pais$PROGRAMA)
chisq.test(mfd004a)
Pmfd004al<-(mfd004a[,1]/sum(mfd004a[,1]))*100
Pmfd004an <-(mfd004a[,2]/sum(mfd004a[,2]))*100
cont <- data.frame(n_n=mfd004a[,2],round(Pmfd004an,1),
                   n_l=mfd004a[,1],round(Pmfd004al,1))

# non-parametric test
wilcox.test(datos_licenciado$MFD004A,
            datos_normalista$MFD004A,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4A <- rank_biserial(datos_licenciado$MFD004A,
                     datos_normalista$MFD004A)

r4A

interpret_r(r4A$r_rank_biserial, rules = "cohen")

# "MFD004B" 
datos_pais %>%
  select(PROGRAMA,MFD004B) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004B),
            media = mean(MFD004B),
            sd = sd(MFD004B),
            median = median(MFD004B),
            minimo = min(MFD004B),
            maximo = max(MFD004B))

datos_pais %>%
  select(PROGRAMA,MFD004B)%>%
  group_by(MFD004B)%>%
  summarise(n = length(MFD004B),
            porc=(length(MFD004B)/618)*100)

mfd004b<- table(datos_pais$MFD004B,datos_pais$PROGRAMA)
chisq.test(mfd004b)
Pmfd004bl<-(mfd004b[,1]/sum(mfd004b[,1]))*100
Pmfd004bn <-(mfd004b[,2]/sum(mfd004b[,2]))*100
cont <- data.frame(n_n=mfd004b[,2],Pmfd004bn,
                   n_l=mfd004b[,1],Pmfd004bl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004B,
            datos_normalista$MFD004B,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4B <- rank_biserial(datos_licenciado$MFD004B,
                     datos_normalista$MFD004B)

r4B

interpret_r(r4B$r_rank_biserial, rules = "cohen")

# "MFD004C" 

datos_pais %>%
  select(PROGRAMA,MFD004C) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004C),
            media = mean(MFD004C),
            sd = sd(MFD004C),
            median = median(MFD004C),
            minimo = min(MFD004C),
            maximo = max(MFD004C))

datos_pais %>%
  select(PROGRAMA,MFD004C)%>%
  group_by(MFD004C)%>%
  summarise(n = length(MFD004C),
            porc=(length(MFD004C)/618)*100)

mfd004c<- table(datos_pais$MFD004C,datos_pais$PROGRAMA)
chisq.test(mfd004c)
Pmfd004cl<-(mfd004c[,1]/sum(mfd004c[,1]))*100
Pmfd004cn <-(mfd004c[,2]/sum(mfd004c[,2]))*100
cont <- data.frame(n_n=mfd004c[,2],Pmfd004cn,
                   n_l=mfd004c[,1],Pmfd004cl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004C,
            datos_normalista$MFD004C,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4C <- rank_biserial(datos_licenciado$MFD004C,
                     datos_normalista$MFD004C)

r4C

interpret_r(r4C$r_rank_biserial, rules = "cohen")

# "MFD004D"
datos_pais %>%
  select(PROGRAMA,MFD004D) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004D),
            media = mean(MFD004D),
            sd = sd(MFD004D),
            median = median(MFD004D),
            minimo = min(MFD004D),
            maximo = max(MFD004D))

datos_pais %>%
  select(PROGRAMA,MFD004D)%>%
  group_by(MFD004D)%>%
  summarise(n = length(MFD004D),
            porc=(length(MFD004D)/618)*100)

mfd004d<- table(datos_pais$MFD004D,datos_pais$PROGRAMA)
chisq.test(mfd004d)
Pmfd004dl<-(mfd004d[,1]/sum(mfd004d[,1]))*100
Pmfd004dn <-(mfd004d[,2]/sum(mfd004d[,2]))*100
cont <- data.frame(n_n=mfd004d[,2],Pmfd004dn,
                   n_l=mfd004d[,1],Pmfd004dl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004D,
            datos_normalista$MFD004D,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4D <- rank_biserial(datos_licenciado$MFD004D,
                     datos_normalista$MFD004D)

r4D

interpret_r(r4D$r_rank_biserial, rules = "cohen")

# "MFD004E" 
datos_pais %>%
  select(PROGRAMA,MFD004E) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004E),
            media = mean(MFD004E),
            sd = sd(MFD004E),
            median = median(MFD004E),
            minimo = min(MFD004E),
            maximo = max(MFD004E))

datos_pais %>%
  select(PROGRAMA,MFD004E)%>%
  group_by(MFD004E)%>%
  summarise(n = length(MFD004E),
            porc=(length(MFD004E)/618)*100)

mfd004e<- table(datos_pais$MFD004E,datos_pais$PROGRAMA)
chisq.test(mfd004e)
Pmfd004el<-(mfd004e[,1]/sum(mfd004e[,1]))*100
Pmfd004en <-(mfd004e[,2]/sum(mfd004e[,2]))*100
cont <- data.frame(n_n=mfd004e[,2],Pmfd004en,
                   n_l=mfd004e[,1],Pmfd004el)

# non-parametric test
wilcox.test(datos_licenciado$MFD004E,
            datos_normalista$MFD004E,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4E <- rank_biserial(datos_licenciado$MFD004E,
                     datos_normalista$MFD004E)

r4E

interpret_r(r4E$r_rank_biserial, rules = "cohen")

# "MFD004F"
datos_pais %>%
  select(PROGRAMA,MFD004F) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004F),
            media = mean(MFD004F),
            sd = sd(MFD004F),
            median = median(MFD004F),
            minimo = min(MFD004F),
            maximo = max(MFD004F))

datos_pais %>%
  select(PROGRAMA,MFD004F)%>%
  group_by(MFD004F)%>%
  summarise(n = length(MFD004F),
            porc=(length(MFD004F)/618)*100)

mfd004f<- table(datos_pais$MFD004F,datos_pais$PROGRAMA)
chisq.test(mfd004f)
Pmfd004fl<-(mfd004f[,1]/sum(mfd004f[,1]))*100
Pmfd004fn <-(mfd004f[,2]/sum(mfd004f[,2]))*100
cont <- data.frame(n_n=mfd004f[,2],Pmfd004fn,
                   n_l=mfd004f[,1],Pmfd004fl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004F,
            datos_normalista$MFD004F,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4F <- rank_biserial(datos_licenciado$MFD004F,
                     datos_normalista$MFD004F)

r4F

interpret_r(r4F$r_rank_biserial, rules = "cohen")

# "MFD004G" 
datos_pais %>%
  select(PROGRAMA,MFD004G) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004G),
            media = mean(MFD004G),
            sd = sd(MFD004G),
            median = median(MFD004G),
            minimo = min(MFD004G),
            maximo = max(MFD004G))

datos_pais %>%
  select(PROGRAMA,MFD004G)%>%
  group_by(MFD004G)%>%
  summarise(n = length(MFD004G),
            porc=(length(MFD004G)/618)*100)

mfd004g<- table(datos_pais$MFD004G,datos_pais$PROGRAMA)
chisq.test(mfd004g)
Pmfd004gl<-(mfd004g[,1]/sum(mfd004g[,1]))*100
Pmfd004gn <-(mfd004g[,2]/sum(mfd004g[,2]))*100
cont <- data.frame(n_n=mfd004g[,2],Pmfd004gn,
                   n_l=mfd004g[,1],Pmfd004gl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004G,
            datos_normalista$MFD004G,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4G <- rank_biserial(datos_licenciado$MFD004G,
                     datos_normalista$MFD004G)

r4G

interpret_r(r4G$r_rank_biserial, rules = "cohen")

# "MFD004H" 
datos_pais %>%
  select(PROGRAMA,MFD004H) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004H),
            media = mean(MFD004H),
            sd = sd(MFD004H),
            median = median(MFD004H),
            minimo = min(MFD004H),
            maximo = max(MFD004H))

datos_pais %>%
  select(PROGRAMA,MFD004H)%>%
  group_by(MFD004H)%>%
  summarise(n = length(MFD004H),
            porc=(length(MFD004H)/618)*100)

mfd004h<- table(datos_pais$MFD004H,datos_pais$PROGRAMA)
chisq.test(mfd004h)
Pmfd004hl<-(mfd004h[,1]/sum(mfd004h[,1]))*100
Pmfd004hn <-(mfd004h[,2]/sum(mfd004h[,2]))*100
cont <- data.frame(n_n=mfd004h[,2],Pmfd004hn,
                   n_l=mfd004h[,1],Pmfd004hl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004H,
            datos_normalista$MFD004H,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4H <- rank_biserial(datos_licenciado$MFD004H,
                     datos_normalista$MFD004H)

r4H

interpret_r(r4H$r_rank_biserial, rules = "cohen")

# "MFD004I" 
datos_pais %>%
  select(PROGRAMA,MFD004I) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004I),
            media = mean(MFD004I),
            sd = sd(MFD004I),
            median = median(MFD004I),
            minimo = min(MFD004I),
            maximo = max(MFD004I))

datos_pais %>%
  select(PROGRAMA,MFD004I)%>%
  group_by(MFD004I)%>%
  summarise(n = length(MFD004I),
            porc=(length(MFD004I)/618)*100)

mfd004i<- table(datos_pais$MFD004I,datos_pais$PROGRAMA)
chisq.test(mfd004i)
Pmfd004il<-(mfd004i[,1]/sum(mfd004i[,1]))*100
Pmfd004in <-(mfd004i[,2]/sum(mfd004i[,2]))*100
cont <- data.frame(n_n=mfd004i[,2],Pmfd004in,
                   n_l=mfd004i[,1],Pmfd004il)

# non-parametric test
wilcox.test(datos_licenciado$MFD004I,
            datos_normalista$MFD004I,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4I <- rank_biserial(datos_licenciado$MFD004I,
                     datos_normalista$MFD004I)

r4I

interpret_r(r4I$r_rank_biserial, rules = "cohen")

# "MFD004J" 
datos_pais %>%
  select(PROGRAMA,MFD004J) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004J),
            media = mean(MFD004J),
            sd = sd(MFD004J),
            median = median(MFD004J),
            minimo = min(MFD004J),
            maximo = max(MFD004J))

datos_pais %>%
  select(PROGRAMA,MFD004I)%>%
  group_by(MFD004I)%>%
  summarise(n = length(MFD004I),
            porc=(length(MFD004I)/618)*100)

mfd004j<- table(datos_pais$MFD004J,datos_pais$PROGRAMA)
chisq.test(mfd004j)
Pmfd004jl<-(mfd004j[,1]/sum(mfd004j[,1]))*100
Pmfd004jn <-(mfd004j[,2]/sum(mfd004j[,2]))*100
cont <- data.frame(n_n=mfd004j[,2],Pmfd004jn,
                   n_l=mfd004j[,1],Pmfd004jl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004J,
            datos_normalista$MFD004J,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4J <- rank_biserial(datos_licenciado$MFD004J,
                     datos_normalista$MFD004J)

r4J

interpret_r(r4J$r_rank_biserial, rules = "cohen")

# "MFD004K" 
datos_pais %>%
  select(PROGRAMA,MFD004K) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004K),
            media = mean(MFD004K),
            sd = sd(MFD004K),
            median = median(MFD004K),
            minimo = min(MFD004K),
            maximo = max(MFD004K))

datos_pais %>%
  select(PROGRAMA,MFD004K)%>%
  group_by(MFD004K)%>%
  summarise(n = length(MFD004K),
            porc=(length(MFD004K)/618)*100)

mfd004k<- table(datos_pais$MFD004K,datos_pais$PROGRAMA)
chisq.test(mfd004k)
Pmfd004kl<-(mfd004k[,1]/sum(mfd004k[,1]))*100
Pmfd004kn <-(mfd004k[,2]/sum(mfd004k[,2]))*100
cont <- data.frame(n_n=mfd004k[,2],Pmfd004kn ,
                   n_l=mfd004k[,1],Pmfd004kl)

# non-parametric test
wilcox.test(datos_licenciado$MFD004K,
            datos_normalista$MFD004K,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4K <- rank_biserial(datos_licenciado$MFD004K,
                     datos_normalista$MFD004K)

r4K

interpret_r(r4K$r_rank_biserial, rules = "cohen")

# "MFD004L"
datos_pais %>%
  select(PROGRAMA,MFD004L) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD004L),
            media = mean(MFD004L),
            sd = sd(MFD004L),
            median = median(MFD004L),
            minimo = min(MFD004L),
            maximo = max(MFD004L))

datos_pais %>%
  select(PROGRAMA,MFD004L)%>%
  group_by(MFD004L)%>%
  summarise(n = length(MFD004L),
            porc=(length(MFD004L)/618)*100)

mfd004l<- table(datos_pais$MFD004L,datos_pais$PROGRAMA)
chisq.test(mfd004l)
Pmfd004ll<-(mfd004l[,1]/sum(mfd004l[,1]))*100
Pmfd004ln <-(mfd004l[,2]/sum(mfd004l[,2]))*100
cont <- data.frame(n_n=mfd004l[,2],Pmfd004ln,
                   n_l=mfd004l[,1],Pmfd004ll)

# non-parametric test
wilcox.test(datos_licenciado$MFD004L,
            datos_normalista$MFD004L,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4L <- rank_biserial(datos_licenciado$MFD004L,
                     datos_normalista$MFD004L)

r4L

interpret_r(r4L$r_rank_biserial, rules = "cohen")

# MFD006 ----                      

datos_pais %>%
  select(PROGRAMA,MFD006) %>%
  group_by(PROGRAMA) %>%
  summarise(n = length(MFD006),
            media = mean(MFD006),
            sd = sd(MFD006),
            median = median(MFD006),
            minimo = min(MFD006),
            maximo = max(MFD006))

MFD006<- table(datos_pais$MFD006,datos_pais$PROGRAMA)
chisq.test(MFD006)
PMFD006l<-(MFD006[,1]/sum(MFD006[,1]))*100
PMFD006n <-(MFD006[,2]/sum(MFD006[,2]))*100
cont <- data.frame(n_n=MFD006[,2],PMFD006n,
                   n_l=MFD006[,1],PMFD006l)

# non-parametric test
wilcox.test(datos_licenciado$MFD006,
            datos_normalista$MFD006,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# effect size non-parametric 
r4L <- rank_biserial(datos_licenciado$MFD006,
                     datos_normalista$MFD006)

r4L

interpret_r(r4L$r_rank_biserial, rules = "cohen")

