"Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México
OBJETIVO
Realizar un análisis estadístico completo de un caso
Publicar en un repositorio de Github el análisis y el código empleado
REQUISITOS
Haber realizado los works y postworks previos
Tener una cuenta en Github o en RStudioCloud
DESARROLLO
Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente los patrones 
de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en su nivel socioeconómico,
en si el hogar tiene recursos financieros extras al ingreso y en si presenta o no inseguridad alimentaria. Además,
está interesado en un modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional 
de Salud Pública en México. La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a
gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros
determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:"
  
# nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
# area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
# numpeho (Número de persona en el hogar)
# refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
# edadjef (Edad del jefe/a de familia)
# sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
# añosedu (Años de educación del jefe de familia)
# ln_als (Logarítmo natural del gasto en alimentos saludables)
# ln_alns (Logarítmo natural del gasto en alimentos no saludables)
# IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"
library(dplyr)
library(DescTools)
library(ggplot2)
library(moments)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
df2 <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
summary(df)
View(df)

dfc <-  na.omit(df)
summary(dfc)  # datos sin NA,  20280 observaciones
View(dfc)

dfc$nse5f <- factor(dfc$nse5f, labels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"))
dfc$area <- factor(dfc$area, labels = c("Zona urbana", "Zona rural") )
dfc$refin <- factor(dfc$refin, labels=c("No","Si"))
dfc$sexojef <- factor(dfc$sexojef, labels=c("Hombre","Mujer") )
dfc$IA <- factor(dfc$IA, labels=c("No presenta IA","Presenta IA"))



#Plantea el problema del caso
#Realiza un análisis descriptivo de la información
summary(dfc)

boxplot( ln_als ~ nse5f,
        data = dfc)

boxplot( ln_alns ~ nse5f,
         data = dfc)

#media als : 6.192


muestra <- filter(dfc, nse5f == "Bajo")
meanbajoALs <- mean( muestra$ln_als ) #5.8  (-0.392 con respecto a la media general)

muestra <- filter(dfc, nse5f == "Medio bajo") 
meanMbajoALs  <- mean( muestra$ln_als )  #6.03

muestra <- filter(dfc, nse5f == "Medio") 
meanMedioALs  <- mean( muestra$ln_als ) #6.17

muestra <- filter(dfc, nse5f == "Medio alto")
meanMaltoALs  <- mean( muestra$ln_als ) #6.32

muestra <- filter(dfc, nse5f == "Alto")
meanAltoALS  <- mean( muestra$ln_als )  #6.53

#media alns : 4.119

meanbajoALns <- mean( muestra$ln_alns ) # 3.68  (-0.439 respecto a la media general)

meanMbajoALns  <- mean( muestra$ln_alns )  #3.909

meanMedioALns  <- mean( muestra$ln_alns ) #4.05

meanMaltoALns  <- mean( muestra$ln_alns ) #4.23

meanAltoALns  <- mean( muestra$ln_alns )  #4.61

meanbajoALs - meanbajoALns  # = 2.113189

meanMbajoALs - meanMbajoALns # = 2.122543

meanMedioALs - meanMedioALns # = 2.124319

meanMaltoALs - meanMaltoALns # = 2.094706

meanAltoALS - meanAltoALns # = 1.924641

# comparando las medias se puede decir que al no ser iguales indica que los gastos en alimentos saludables y
# no saludables Sí están relacionados al nivel socioeconómico.
# Y que a partir del nivel Alto lo que se gasta en alns aumenta con respecto a los als


freq <- table(dfc$nse5f)
transform(freq, 
          rel.freq=prop.table(Freq))
"       Var1 Freq  rel.freq
1       Bajo 3553 0.1751972
2 Medio bajo 3927 0.1936391
3      Medio 4119 0.2031065
4 Medio alto 4364 0.2151874
5       Alto 4317 0.2128698"

cuartiles <-  quantile(dfc$numpeho, probs = c(0.25,0.5,0.75))
cuartiles   #número de personas por hogar
"25% 50% 75% 
  3   4   5 "

cuartilesals <-  quantile(dfc$ln_als, probs = c(0.25,0.5,0.75), na.rm = TRUE)
cuartilesals
"  25%      50%      75% 
5.843544 6.273820 6.633318 "

cuartilesans <-  quantile(dfc$ln_alns, probs = c(0.25,0.5,0.75), na.rm = TRUE)
cuartilesans
" 25%      50%      75% 
3.401197 4.007333 4.867534"

data <- as.data.frame(dfc$numpeho)

ggplot(data, aes(dfc$numpeho)) +
  geom_histogram(colour = 'red', fill = 'pink',
                 alpha = 0.7, bins = 7) +
  labs(x = 'Numero de Personas x Hogar', y = 'Frecuencia')

#Calcula probabilidades que nos permitan entender el problema en México

#saludables vs nivel

as <- dfc$ln_als
nivel <- dfc$nse5f
pairs(exp(as) ~ nivel)

#no saludables vs nivel
ans <- dfc$ln_alns

pairs(exp(ans) ~ nivel)

cor(exp(df2$ln_als), df2$nse5f, use = "complete.obs")

cor(exp(df2$ln_alns), df2$nse5f, use = "complete.obs")

#relación entre refin e IA
"Recursos financieros distintos al ingreso laboral  : 0=no, 1=si
Inseguridad Alimentaria                             : 0= no tiene, 1: si tiene"

tabla_abs<-table(dfc$refin, dfc$IA, useNA = "no") #formato de variables (y - renglón, x - columna)
tabla_abs

addmargins(tabla_abs, c(1, 2))

"          No presenta IA Presenta IA   Sum
  No            5007 (30%) 11414 (70%)  16421 (100%)
  Si             846 (22%)  3013 (78%)   3859 (100%)
  Sum           5853       14427        20280"



rfinIA <- filter(dfc, refin =="Si" & IA == "Presenta IA" ) 
nivelMenorIgual3 <- filter(rfinIA, nse5f == "Bajo" | nse5f == "Medio bajo")

tabla_abs2<-table(dfc$nse5f, dfc$IA, useNA = "no") #formato de variables (y - renglón, x - columna)
tabla_abs2
addmargins(tabla_abs2, c(1, 2))
"                 No presenta IA Presenta IA   Sum
  Bajo                  499        3054  86%     3553
  Medio bajo            761        3166  81%     3927
  Medio                 989        3130  76%     4119
  Medio alto           1431        2933  67%     4364
  Alto                 2173        2144  50%     4317
  Sum                  5853       14427         20280"


#Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México
#Estima un modelo de regresión, lineal o logístico, para identificiar los determinanres de la inseguridad alimentaria en México
#Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github.
#NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e interpretar todos tus resultados 
#para poder dar una conclusión final al problema planteado.

# las familias que no tienen un ingreso adicional al alboral tienen mas probalibilidades
# de presentar inseguridad alimentaria (70%), entre las familias que Sí reciben un
# ingreso adicional al laboral aunque son una cantidad menor en porcentaje las probabilidades de
# caer en inseguridad alimentaria es mayor (78%) lo cual puede indicar que las ayudas a estas familias
# no son suficientes. 