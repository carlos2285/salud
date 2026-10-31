# =====================================================================
# Econometría 1 (ECO1) | UJMD - FEEN
# Paper corto: Ingreso, servicios básicos y esperanza de vida (2019)
# VERSIÓN DOCENTE (solución completa, no distribuir)
# Docente: Carlos Francisco Gómez Rodríguez
# Fuente: Banco Mundial, World Development Indicators
# =====================================================================

# --- 0. Paquetes -------------------------------------------------------
paquetes <- c("lmtest", "sandwich", "car", "ggplot2")
nuevos <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(nuevos) > 0) install.packages(nuevos)
library(lmtest); library(sandwich); library(car); library(ggplot2)

# --- 1. Datos ------------------------------------------------------------
datos <- read.csv("salud_desarrollo_paises_2019.csv", encoding = "UTF-8")
d <- na.omit(datos[, c("iso3", "pais", "region", "esperanza_vida", "pib_pc_ppa",
                       "gasto_salud_pib", "urbana_pct", "agua_basica_pct", "america_latina")])
nrow(d)                                   # 180 países con datos completos
d$ln_pib <- log(d$pib_pc_ppa)

# --- 2. Estadística descriptiva -----------------------------------------
summary(d[, c("esperanza_vida", "pib_pc_ppa", "ln_pib", "gasto_salud_pib",
              "urbana_pct", "agua_basica_pct")])
round(cor(d[, c("esperanza_vida", "ln_pib", "gasto_salud_pib", "urbana_pct", "agua_basica_pct")]), 2)
d[d$iso3 == "SLV", ]                      # posición de El Salvador

ggplot(d, aes(pib_pc_ppa, esperanza_vida)) + geom_point(alpha = .6) +
  labs(title = "Esperanza de vida y PIB per cápita, 2019", x = "PIB per cápita PPA (US$ int. 2021)", y = "Años")
ggplot(d, aes(ln_pib, esperanza_vida)) + geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(data = d[d$iso3 == "SLV", ], color = "red", size = 3) +
  labs(title = "Esperanza de vida y ln(PIB per cápita), 2019", x = "ln(PIB per cápita PPA)", y = "Años")

# --- 3. Modelo 1: regresión lineal simple --------------------------------
m1 <- lm(esperanza_vida ~ ln_pib, data = d)
summary(m1)                               # b1 ~ 6.08; R2 ~ 0.73
confint(m1, level = 0.95)
# Interpretación nivel-log: un aumento de 10% en el PIB pc se asocia con b1*ln(1.1) años
coef(m1)["ln_pib"] * log(1.10)            # ~0.58 años

# --- 4. Modelo 2: regresión múltiple -------------------------------------
m2 <- lm(esperanza_vida ~ ln_pib + gasto_salud_pib + urbana_pct + agua_basica_pct + america_latina, data = d)
summary(m2)                               # ln_pib y agua significativos; gasto y urbana no
confint(m2)

# Prueba F de significancia conjunta: gasto en salud y urbanización
linearHypothesis(m2, c("gasto_salud_pib = 0", "urbana_pct = 0"))   # p ~ 0.99

# Multicolinealidad
vif(m2)                                   # ln_pib y agua con VIF más altos

# --- 5. Supuestos: heterocedasticidad ------------------------------------
plot(fitted(m2), resid(m2), xlab = "Valores ajustados", ylab = "Residuos"); abline(h = 0, lty = 2)
bptest(m2)                                # Breusch-Pagan: rechaza homocedasticidad
bptest(m2, ~ fitted(m2) + I(fitted(m2)^2))  # versión especial de White
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))  # errores estándar robustos

# --- 6. Especificación y observaciones atípicas --------------------------
resettest(m2, power = 2, type = "fitted") # RESET: p ~ 0.03
d$residuo <- resid(m2)
head(d[order(d$residuo), c("pais", "esperanza_vida", "residuo")], 5)  # República Centroafricana
m3 <- update(m2, data = subset(d, iso3 != "CAF"))
summary(m3)                               # sensibilidad sin el atípico

# --- 7. El Salvador ----------------------------------------------------------
slv <- d[d$iso3 == "SLV", ]
predict(m2, newdata = slv, interval = "prediction")  # ~70.8 años vs. 71.7 observado
slv$esperanza_vida - predict(m2, newdata = slv)      # residuo ~ +0.9 años
