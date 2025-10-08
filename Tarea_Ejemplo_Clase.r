# Entregables 
# 1. Ejemplo Clase - El código normal
# 2. Ejemplo con Interacción - Código normal + la 
# última parte sobre el modelo con *
# 3- Ejemplo completo eliminando computadora 2



# Paquetes 
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}


# Ingresar los datos

Data = read.table("Datos_monofactorial_bloques.txt", header=TRUE)

# Filtrar para solo obtener los datos con la Computadora 1
library(dplyr)
Data <- Data %>%
        filter(Computadora == "Computadora 1")


# Ordenar los datos según se ingresan (Evitar que R no los ordene alfabéticamente)

Data$Algoritmo = factor(Data$Algoritmo,
                            levels = unique(Data$Algoritmo))

Data$Computador = factor(Data$Computadora,
                            levels = unique(Data$Computadora))


# Verificar que todo esté bien

library(psych)
headTail(Data)
str(Data)
summary(Data)
#rm(Data)

# Resumen de los datos por grupo

Summarize(Tiempo ~ Algoritmo,
            data = Data,
            digits = 3)


# Gráfico de Cajas

M = tapply(Data$Tiempo,
            INDEX = Data$Algoritmo,
            FUN = mean)

boxplot(Tiempo ~ Algoritmo,
            data = Data)

points(M,
        col = "red",
        pch = "+",
        cex = 2)

# Gráfcio de Cajas 2 - Separando los algortimos por cada computadora 

boxplot(Tiempo ~ Algoritmo + Computadora,
            data = Data)


# Gráfico de Promedios e Intervalos de Confianza

Sum = groupwiseMean(Tiempo ~ Algoritmo,
                    data = Data,
                    conf = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile = TRUE)
Sum

library(ggplot2)

ggplot(Sum,
        aes(x = Algoritmo,
            y = Mean)) + 
    geom_errorbar(aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),
                    width = 0.05,
                    size = 0.5) + 
    geom_point(shape = 15,
                size = 4) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    ylab("Tiempo promedio, s")


# Cambio para considerar la Computadora

Sum = groupwiseMean(Tiempo ~ Algoritmo + Computadora,
                    data = Data,
                    conf = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile = TRUE)
Sum

library(ggplot2)

ggplot(Sum,
        aes(x = Algoritmo,
            y = Mean)) + 
    geom_errorbar(aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),
                    width = 0.05,
                    size = 0.5) + 
    geom_point(shape = 15,
                size = 4) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    ylab("Tiempo promedio, s")

# Modelo Lineal
model = lm(Tiempo ~ Algoritmo + Computadora,
        data = Data)

summary(model)

## ANOVA = Análsis de Varianza

library(car)
Anova(model, # Tipo 2 es el por defecto
        type = "II") # Suma de cuadrados


# Modelo Lineal Con Interacción
model = lm(Tiempo ~ Algoritmo * Computadora,
        data = Data)

summary(model)

## ANOVA Multifactorial con 2 Factores

library(car)
Anova(model, # Tipo 2 es el por defecto
        type = "II") # Suma de cuadrados




######### Evaluación de Supuestos
x = residuals(model)

library(rcompanion)

plotNormalHistogram(x)

plot(fitted(model))
residuals(model)

plot(model)



## Análisis Post hoc (En caso de ser necesario)
library(multcompView)
library(lsmeans)

marginal = lsmeans(model,
                        ~ Algoritmo)

pairs(marginal,
        adjust = "tukey")

## Función CLD
library(multcomp)

CLD = cld(marginal,
        alpha = 0.05,
        Letters = letters,
        adjust = "tukey")
CLD

# Promedios, con los intervalos de confianza y las letras de separación
# entre grupos
# Ordenamos los niveles para imprimirlos
CLD$Algoritmo = factor(CLD$Algoritmo,
                        levels = c("Algoritmo A",
                                "Algoritmo B",
                                "Algoritmo C"))

# Removemos los espacios en blanco en CLD
CLD$.group = gsub(" ", "", CLD$.group)

# Crear el gráfico de CLD
library(ggplot2)

ggplot(CLD,
        aes(x = Algoritmo,
                y = lsmean,
                label = .group)) + 
        geom_point(shape = 15,
                        size = 4) +
                geom_errorbar(aes(ymin = lower.CL,
                                ymax = upper.CL),
                                width = 0.2,
                                size = 0.7) + 
                        theme_bw() +
                        theme(axis.title = element_text(face = "bold"),
                                axis.text = element_text(face="bold"),
                                plot.caption = element_text(hjust = 0)) +
                        ylab("Promedio del mínimo cuadrado\n
                                Tiempo de ejecución") + 
                        geom_text(nudge_x = c(0, 0, 0),
                                nudge_y = c(800, 800, 800),
                                color = "black")