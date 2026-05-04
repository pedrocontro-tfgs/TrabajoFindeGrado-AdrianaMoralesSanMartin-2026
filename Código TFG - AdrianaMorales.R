# ══════════════════════════════════════════════════════════════════════════════
# TFG ADRIANA MORALES SAN MARTÍN
# Factores explicativos de la rentabilidad en startups tecnológicas españolas
# CÓDIGO DE IMPLEMENTACIÓN EN RSTUDIO (Depurado por Claude AI y Chat GPT)
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. LIBRERÍAS ──────────────────────────────────────────────────────────────

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(pROC)
library(caret)
library(readxl)
library(rpart)
library(rpart.plot)
library(car)

# ── 2. CARGA DE DATOS ─────────────────────────────────────────────────────────

# Sustituir por la ruta local correspondiente
ruta_2023 <- "Introduzca_RUTA_2023"
ruta_2024 <- "Introduzca_RUTA_2024"

df_2023 <- read.csv(
  ruta_2023,
  sep = ";", na.strings = c("NA", "n.d.", ""),
  stringsAsFactors = FALSE, fileEncoding = "latin1",
  check.names = FALSE
) %>% select(-1)

cols_numericas <- c("ROA23", "Total_Activo", "Ingresos_Explotacion", "Empleados",
                    "Ratio_Liquidez", "Ratio_Solvencia", "Ratio_Endeudamiento",
                    "EBITDA", "Resultado_Ejercicio", "Inmovilizado_Inmaterial")

df_2023 <- df_2023 %>%
  mutate(across(all_of(cols_numericas), ~ as.numeric(
    gsub(",", ".", gsub("\\.", "", .))
  )))

df_2024 <- read_excel(ruta_2024, na = c("NA", "n.d.", "")) %>%
  select(-`...1`)

# ── 3. OUTLIERS UNIVARIANTES ──────────────────────────────────────────────────

df_modelado <- df_2023 %>%
  mutate(across(-c("Nombre", "NIF", "Localidad", "Fecha_Constitucion"), 
                ~ as.numeric(.)))

vars_analisis <- c("ROA23", "Total_Activo", "Ingresos_Explotacion", "Empleados",
                   "Ratio_Liquidez", "Ratio_Endeudamiento", "EBITDA", "Antiguedad")

par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
for (v in vars_analisis) {
  boxplot(df_modelado[[v]], main = v, col = "lightblue", border = "darkblue",
          outcol = "red", outpch = 8, cex.main = 1.2, ylab = "Valor Real")
}
par(mfrow = c(1, 1))

# ── 4. OUTLIERS UNIVARIANTES POR GRUPO ───────────────────────────────────────

df_modelado$Grupo_Exito <- ifelse(df_modelado$ROA23 > 0, "Éxito", "Fracaso")

vars_comparar <- c("Total_Activo", "Ingresos_Explotacion", "Empleados",
                   "Ratio_Liquidez", "Ratio_Endeudamiento", "EBITDA",
                   "Resultado_Ejercicio", "Antiguedad")

par(mfrow = c(2, 4), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
for (v in vars_comparar) {
  boxplot(df_modelado[[v]] ~ df_modelado$Grupo_Exito,
          main = v, col = c("#69b3a2", "#e57373"),
          names = c("Éxito", "Fracaso"),
          outcol = "darkred", outpch = 18,
          ylab = "Valor Real", xlab = "")
}
title("Distribución por Condición de Rentabilidad", outer = TRUE, cex.main = 1.5)
par(mfrow = c(1, 1))

# ── 5. OUTLIERS MULTIVARIANTES (MAHALANOBIS) ──────────────────────────────────

df_mahalanobis <- df_modelado %>%
  select(Ratio_Endeudamiento, Ratio_Liquidez, Empleados, Antiguedad,
         EBITDA, Total_Activo, Ingresos_Explotacion) %>%
  drop_na()

centro    <- colMeans(df_mahalanobis)
covarianza <- cov(df_mahalanobis)
distancias <- mahalanobis(df_mahalanobis, centro, covarianza)
umbral     <- qchisq(1 - 0.001, df = ncol(df_mahalanobis))

df_mahalanobis$Distancia   <- distancias
df_mahalanobis$Es_Outlier_MV <- distancias > umbral
table(df_mahalanobis$Es_Outlier_MV)

plot(distancias, main = "Detección de Outliers Multivariantes",
     ylab = "Distancia de Mahalanobis", xlab = "Índice de la empresa",
     col = ifelse(distancias > umbral, "red", "black"), pch = 20)
abline(h = umbral, col = "blue", lty = 2)

# ── 6. ESTADÍSTICA DESCRIPTIVA ────────────────────────────────────────────────

summary(df_modelado)
# ── HISTOGRAMA DEL ROA 2023 ───────────────────────────────────────────────────

hist(df_modelado$ROA23,
     main   = "Distribución del ROA (2023)",
     xlab   = "ROA (%)",
     ylab   = "Frecuencia",
     col    = "steelblue",
     border = "white",
     breaks = 50)
abline(v = 0, col = "red", lty = 2, lwd = 2)
legend("topright",
       legend = "ROA = 0",
       col    = "red",
       lty    = 2,
       lwd    = 2,
       bty    = "n")

# ── BOXPLOT DEL ROA 2023 ──────────────────────────────────────────────────────

boxplot(df_modelado$ROA23,
        main   = "Distribución del ROA (2023)",
        ylab   = "ROA (%)",
        col    = "lightblue",
        border = "darkblue",
        outcol = "red",
        outpch = 8)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# ── 7. CONSTRUCCIÓN DEL TRAIN (2023) ─────────────────────────────────────────

df_train <- df_2023 %>%
  mutate(
    ROA_bin     = as.factor(ifelse(ROA23 > 0, 1, 0)),
    Antiguedad  = 2026 - as.numeric(substr(Fecha_Constitucion, 7, 10)),
    log_Activo  = log(Total_Activo),
    sector_cnae = as.factor(substr(as.character(CNAE), 1, 2))
  ) %>%
  select(ROA_bin, log_Activo, Ingresos_Explotacion, Empleados,
         Ratio_Liquidez, Ratio_Solvencia, Ratio_Endeudamiento,
         Antiguedad, sector_cnae) %>%
  na.omit()

# ── 8. CONSTRUCCIÓN DEL TEST (2024) ──────────────────────────────────────────

df_test <- df_2024 %>%
  rename(
    Empleados           = `Ultimo número empleados`,
    Ratio_Liquidez      = Liquidez,
    Ratio_Solvencia     = Solvencia,
    Ratio_Endeudamiento = Endeudamiento
  ) %>%
  mutate(
    Ratio_Liquidez  = as.numeric(Ratio_Liquidez),
    Ratio_Solvencia = as.numeric(Ratio_Solvencia),
    ROA_bin         = as.factor(ifelse(ROA2024 > 0, 1, 0)),
    Antiguedad      = 2026 - year(FechaConstitución),
    log_Activo      = log(Total_Activo),
    sector_cnae     = as.factor(substr(as.character(CNAE), 1, 2))
  ) %>%
  select(ROA_bin, log_Activo, Ingresos_Explotacion, Empleados,
         Ratio_Liquidez, Ratio_Solvencia, Ratio_Endeudamiento,
         Antiguedad, sector_cnae) %>%
  na.omit()

cat("=== TAMAÑOS ===\n")
cat("Train (2023):", nrow(df_train), "empresas\n")
cat("Test  (2024):", nrow(df_test),  "empresas\n")
cat("\nDistribución train:\n")
print(round(prop.table(table(df_train$ROA_bin)) * 100, 1))
cat("Distribución test:\n")
print(round(prop.table(table(df_test$ROA_bin)) * 100, 1))

# ── 9. REGRESIÓN LOGÍSTICA BINARIA ────────────────────────────────────────────

modelo_logit <- glm(
  ROA_bin ~ log_Activo + Ratio_Endeudamiento + Antiguedad,
  data   = df_train,
  family = binomial(link = "logit")
)

summary(modelo_logit)
cat("\nOdds Ratio:\n")
print(round(exp(coef(modelo_logit)), 3))
cat("\nVIF:\n")
print(vif(modelo_logit))

# Predicción sobre test
prob_logit <- predict(modelo_logit, newdata = df_test, type = "response")
roc_logit  <- pROC::roc(as.numeric(df_test$ROA_bin) - 1, prob_logit)
cat("\nAUC:", round(pROC::auc(roc_logit), 4), "\n")

# Umbral Youden
coords_youden <- pROC::coords(roc_logit, "best", best.method = "youden",
                              ret = c("threshold","sensitivity","specificity","accuracy"))
cat("Umbral Youden:", round(coords_youden$threshold, 3), "\n")

# Matrices de confusión
pred_05     <- as.factor(ifelse(prob_logit > 0.5, 1, 0))
pred_youden <- as.factor(ifelse(prob_logit > coords_youden$threshold, 1, 0))

cat("\n=== MATRIZ CONFUSIÓN (umbral 0.5) ===\n")
confusionMatrix(pred_05, df_test$ROA_bin, positive = "1")
cat("\n=== MATRIZ CONFUSIÓN (umbral Youden) ===\n")
confusionMatrix(pred_youden, df_test$ROA_bin, positive = "1")

# Curva ROC
plot(roc_logit, main = "Curva ROC — Regresión Logística (Test 2024)",
     col = "steelblue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
points(coords_youden$specificity, coords_youden$sensitivity,
       pch = 19, col = "red", cex = 1.5)
legend("bottomright",
       legend = c(paste0("AUC = ", round(pROC::auc(roc_logit), 4)),
                  paste0("Youden = ", round(coords_youden$threshold, 3))),
       col = c("steelblue","red"), lty = c(1,NA), pch = c(NA,19), bty = "n")

# ── 10. ÁRBOL DE CLASIFICACIÓN ────────────────────────────────────────────────

arbol_final <- rpart(
  ROA_bin ~ log_Activo + Ingresos_Explotacion + Empleados +
    Ratio_Liquidez + Ratio_Solvencia + Ratio_Endeudamiento +
    Antiguedad + sector_cnae,
  data    = df_train,
  method  = "class",
  control = rpart.control(cp = 0.02)
)

printcp(arbol_final)

cat("\n=== IMPORTANCIA DE VARIABLES ===\n")
data.frame(
  Variable    = names(arbol_final$variable.importance),
  Importancia = round(arbol_final$variable.importance, 2),
  Pct         = round(arbol_final$variable.importance /
                        sum(arbol_final$variable.importance) * 100, 1)
) %>% print()

# Visualización
rpart.plot(arbol_final, type = 4, extra = 104,
           main = "Árbol de clasificación — Train 2023",
           cex = 0.9, tweak = 1.4)

# Predicción sobre test
prob_arbol <- predict(arbol_final, newdata = df_test, type = "prob")[, 2]
pred_arbol <- predict(arbol_final, newdata = df_test, type = "class")
roc_arbol  <- pROC::roc(as.numeric(df_test$ROA_bin) - 1, prob_arbol)
cat("\nAUC árbol:", round(pROC::auc(roc_arbol), 4), "\n")

# Umbral Youden
coords_arbol <- pROC::coords(roc_arbol, "best", best.method = "youden",
                             ret = c("threshold","sensitivity","specificity","accuracy"))
cat("Umbral Youden:", round(coords_arbol$threshold, 3), "\n")

pred_arbol_you <- as.factor(ifelse(prob_arbol > coords_arbol$threshold, 1, 0))

cat("\n=== MATRIZ CONFUSIÓN ÁRBOL (umbral 0.5) ===\n")
confusionMatrix(pred_arbol, df_test$ROA_bin, positive = "1")
cat("\n=== MATRIZ CONFUSIÓN ÁRBOL (umbral Youden) ===\n")
confusionMatrix(pred_arbol_you, df_test$ROA_bin, positive = "1")
