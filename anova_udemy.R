
# Análises de Variância - ANOVA -------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data do script: 08/05/22 -----------------------------------------------------------------------------------------------------------------
# Referência: Curso Udemy ------------------------------------------------------------------------------------------------------------------

# ANOVA One Way - Unilateral ---------------------------------------------------------------------------------------------------------------

### Extensão do teste T de amostras independentes.

# Carregar pacote --------------------------------------------------------------------------------------------------------------------------

library(tidyverse)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dente <- ToothGrowth
dente$dose <- factor(dente$dose)

# Análise da distribuição dos dados --------------------------------------------------------------------------------------------------------

### Histogramas

qplot(x = len, data = dente, bins = 10) 
qplot(x = len, fill = dose, data = dente, binwidth = 3) 

# ANOVA One Way ----------------------------------------------------------------------------------------------------------------------------

a1 <- aov(len ~ dose, data = dente) # Modelo
summary(a1) # Resultado

TukeyHSD(a1) # Teste post hoc - Comparação entre grupos

pairwise.t.test(dente$len, dente$dose) # Vários teste t pareados

# ANOVA Two Way - Dois fatores -------------------------------------------------------------------------------------------------------------

### Análise de dois grupos - Suplemento e dose

ggplot(dente) +
  geom_boxplot(aes(x = dose, y = len, fill = supp)) +
  geom_jitter(aes(x = dose, y = len, group = supp)) +
  theme_bw()

a2 <- aov(len ~ dose + supp, data = dente) # Diferença entre grupos
summary(a2)

a3 <- aov(len ~ dose * supp, data = dente) # Diferença e interação entre grupos
summary(a3)

a4 <- aov(len ~ dose:supp, data = dente) # Somente interação entre grupos
summary(a4)

# ANOVA desbalanceada ----------------------------------------------------------------------------------------------------------------------

### Quando os grupos não apresentam o mesmo número de repetições.

library(car)

ad <- aov(len ~ dose * supp, data = dente)
Anova(ad, type = "III")
