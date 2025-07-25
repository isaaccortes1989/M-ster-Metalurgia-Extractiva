library(emmeans)
library(car)

reactivo <- factor(rep(paste0("R", 1:6), each = 6))
recuperacion <- c(82.18,82.78,80.65,83.98,83.24,83.20,
                  85.90,85.25,85.55,86.90,86.44,85.12,
                  87.37,86.73,87.77,87.74,86.43,89.75,
                  82.32,84.53,84.75,83.79,82.68,83.02,
                  86.64,86.81,87.08,87.17,88.09,89.01,
                  87.36,85.94,84.90,85.84,87.20,88.12)

datos3 <- data.frame(reactivo, recuperacion)

# ANOVA
modelo3 <- aov(recuperacion ~ reactivo, data = datos3)
summary(modelo3)

tapply( datos3$recuperacion, datos3$reactivo, mean)

#==supuestos del modelo

shapiro.test(residuals(modelo3))
leveneTest(recuperacion ~ reactivo, data = datos3)

#No hay evidencia estadística al nivel de significancia del 5% para rechazar la nor de los residuos
#No hay evidencia estadística al nivel de significancia del 5% para rechazar la homogeneidad de varianzas de los residuos

em1 <- emmeans(modelo3, ~ reactivo)
comparaciones1 <- contrast(em1, method = "pairwise", adjust = "tukey")
summary(comparaciones1, infer = c(TRUE, TRUE)) 

em2 <- emmeans(modelo3, ~ reactivo)
comparaciones2 <- contrast(em2, method = "pairwise", adjust = "bonferroni")
summary(comparaciones2, infer = c(TRUE, TRUE)) 

qqnorm(residuals(modelo3))
qqline(residuals(modelo3), col = "red")







# Comparación de medias
TukeyHSD(modelo3)

# Diagnóstico
par(mfrow = c(2, 2))
plot(modelo3)

(plot(em1, comparisons = TRUE))
