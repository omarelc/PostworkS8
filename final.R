"Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente
los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en
su nivel socioeconómico, en si el hogar tiene recursos financieros extrar al ingreso y en si presenta
o no inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los
determinantes socioeconómicos de la inseguridad alimentaria.
La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por
el Instituto Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares
con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas
con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar
presente cierta inseguridad alimentaria.
La base de datos contiene las siguientes variables:"

#nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
#area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
#numpeho (Número de persona en el hogar)
#refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
#edadjef (Edad del jefe/a de familia)
#sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
#añosedu (Años de educación del jefe de familia)
#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)
#IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"

library(dplyr)
library(DescTools)
library(ggplot2)
library(zoo)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
head(df)



df$nse5f <- factor(df$nse5f, labels = c("Bajo", "Medio Bajo", "Medio", "Medio Alto", "Alto"))
df$area <- factor(df$area, labels = c("Zona Urbana", "Zona Rural"))
df$refin <- factor(df$refin, labels = c("No", "Si"))
df$sexojef <- factor(df$sexojef, labels = c("Hombre", "Mujer"))
df$IA <- factor(df$IA, labels = c("No Presenta IA", "Presenta IA"))

head(df)



"1.Plantea el problema del caso"
#Comprobar la relación entre el nivel socioeconómico de los hogares con los gastos en
#alimentos saludables y no saludables, así como si cuentan con recursos financieros extra,
#teniendo en cuenta si se presenta inseguridad alimentaria y los determinantes socioeconómicos asociados a ella


"2.Realiza un análisis descriptivo de la información"
summary(df)

#reemplazo de valores NA en als y alns por los promedios sin casos NA

 df$ln_als[is.na(df$ln_als)]<-mean(df$ln_als,na.rm=TRUE)
 df$ln_alns[is.na(df$ln_alns)]<-mean(df$ln_alns,na.rm=TRUE)


(freq <- table(df$nse5f))
ggplot(df, aes(x = nse5f, fill = nse5f)) +
  geom_bar(aes(color = nse5f, fill = nse5f), alpha = 0.4) +
  labs(title = "Nivel socioeconómico de los hogares",x = "Nivel Socioeconómico", y = "Frecuencia") + 
  theme_light()

" Bajo        Medio Bajo    Medio   Medio Alto   Alto 
      8858       8560       8323       7903       7165 "

(freq <- table(df$area))
ggplot(df, aes(x = area, fill = area)) +
  geom_bar(aes(color = area, fill = area), alpha = 0.4) +
  labs(title = "Zona de los hogares",x = "Zona", y = "Frecuencia") + 
  theme_light()
"Zona Urbana  Zona Rural 
      26591       14218 "

(freq <- table(df$refin))
ggplot(df, aes(x = refin, fill = refin)) +
  geom_bar(aes(color = refin, fill = refin), alpha = 0.4) +
  labs(title = "Recursos financieros extra en los hogares",x = "¿Cuenta con recursos financieros extra?", y = "Frecuencia") + 
  theme_light()
"No     Si 
33046  7763"

(freq <- table(df$sexojef))
ggplot(df, aes(x = sexojef, fill = sexojef)) +
  geom_bar(aes(color = sexojef), alpha = 0.4) +
  scale_color_manual(values = c("#97DB4F", "#E55381")) +
  scale_fill_manual(values = c("#97DB4F", "#E55381")) +
  labs(title = "Sexo del jefe del hogar",x = "Sexo", y = "Frecuencia") + 
  theme_light()
"Hombre  Mujer 
  26957   8861"

(freq <- table(df$IA))
ggplot(df, aes(x = IA, fill = IA)) +
  geom_bar(aes(color = IA, fill = IA), alpha = 0.4) +
  scale_color_manual(values = c("#97DB4F", "#E55381")) +
  scale_fill_manual(values = c("#97DB4F", "#E55381")) +
  labs(title = "Hogares con inseguridad alimentaria",x = "¿Presenta inseguridad alimentario?", y = "Frecuencia") + 
  theme_light()
"No Presenta IA    Presenta IA 
        10781         30028 "



#General
(mean.als <- mean(df$ln_als)) #6.066521
(sd.als <- sd(df$ln_als)) #0.7387856
(Mode(df$ln_als)[1]) # 6.066521
(mean.alns <- mean(df$ln_alns)) # 4.124941
(sd.alns <- sd(df$ln_alns)) # 0.7896813
(Mode(df$ln_alns)[1]) #4.124941

#Por nivel socioeconómico
(nivel.mean.sd <- df %>%
    select(nse5f, ln_als, ln_alns) %>%
    group_by(nse5f) %>%
    summarize(mean_ln_als = mean(ln_als),
              sd_ln_als = sd(ln_als),
              mean_ln_alns = mean(ln_alns),
              sd_ln_alns = sd(ln_alns)))
" nse5f      mean_ln_als sd_ln_als mean_ln_alns sd_ln_alns
1 Bajo              5.70     0.791         3.93      0.668
2 Medio Bajo        5.93     0.706         4.01      0.698
3 Medio             6.08     0.656         4.09      0.743
4 Medio Alto        6.24     0.644         4.20      0.817
5 Alto              6.47     0.627         4.46      0.925"

boxplot(ln_als ~ nse5f,data = df)
boxplot(ln_alns ~ nse5f,data = df)


"3.Calcula probabilidades que nos permitan entender el problema en México"
pairs(~ nse5f + ln_als + ln_alns, 
      data = df, gap = 0.4, cex.labels = 1.5)


sample <- df %>% 
  select(nse5f, ln_als, ln_alns)  %>%
 mutate(nse5f = as.numeric(nse5f))

round(cor(exp(sample)), 4)
"        nse5f ln_als ln_alns
nse5f   1.0000 0.3459  0.2175
ln_als  0.3459 1.0000  0.2742
ln_alns 0.2175 0.2742  1.0000"


prop.table(table(df$nse5f, df$refin),1)
"                  No        Si
  Bajo       0.7964552 0.2035448
  Medio Bajo 0.7929907 0.2070093
  Medio      0.8016340 0.1983660
  Medio Alto 0.8166519 0.1833481
  Alto       0.8481507 0.1518493"

#En el nivel Bajo existe un 79.6% de probabilidad que el hogar no cuente con recursos extra pero un 20.35% de que si exista
#En el nivel Medio Bajo existe un 79.3% de probabilidad que el hogar no cuente con recursos extra pero un 20.7% de que si exista
#En el nivel Medio existe un 80.16% de probabilidad que el hogar no cuente con recursos extra pero un 19.83% de que si exista
#En el nivel Medio Alto existe un 81.66% de probabilidad que el hogar no cuente con recursos extra pero un 18.33% de que si exista
#En el nivel Alto existe un 84.81% de probabilidad que el hogar no cuente con recursos extra pero un 15.18% de que si exista
transform(table(df$nse5f, df$refin),
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))


prop.table(table(df$nse5f, df$IA),1)
"              No Presenta IA Presenta IA
  Bajo            0.1290359   0.8709641
  Medio Bajo      0.1810748   0.8189252
  Medio           0.2255196   0.7744804
  Medio Alto      0.3246868   0.6753132
  Alto            0.5087230   0.4912770"
#En el nivel Bajo existe un 12.9% de probabilidad que no tengan Inseguridad Alimentaria pero un 87.09% de que si exista
#En el nivel Medio Bajo existe un 18.10% de probabilidad que no tengan Inseguridad Alimentaria pero un 81.89% de que si exista
#En el nivel Medio existe un 22.55% de probabilidad no tengan Inseguridad Alimentaria pero un 77.44% de que si exista
#En el nivel Medio Alto existe un 32.46% de probabilidad que no tengan Inseguridad Alimentaria pero un 67.53% de que si exista
#En el nivel Alto existe un 50.87% de probabilidad que no tengan Inseguridad Alimentaria pero un 49.12% de que si exista

transform(table(df$nse5f, df$IA),
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))

{curve(dnorm(x, mean = mean.als, sd = sd.als), from = 0, to = 10, 
       col='blue', main = "Densidad Normal:\nln_als y ln_alns",
       ylab = "f(x)", xlab = "X")
  legend(x = 8.5, y = 0.5, legend=c("ln_als", "ln_alns"),
         col=c("blue", "red"), lty = 1, bty = "n", cex=0.8)
  curve(dnorm(x, mean = mean.alns, sd = sd.alns), from = 0, to = 10, 
        col='red', add = TRUE)
}


"4.Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México"
df.b <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

"Existe EEE de que el logaritmo natural del gasto en alimentos saludables es mayor a 6.1834?"
"Planteamiento de hipótesis:"
#Ho: mu <= 6.1834
#Ha: mu > 6.1834
{
  ttest <- t.test(x = df$ln_als, alternative = "greater", mu = 6.1834)
  p.value <- ttest$p.value
  if(p.value < 0.05) {
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es mayor a 6.1834")
  } else{
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es menor a 6.1834")
  }
}
# "NC 95%: Existe evidencia para rechazar Ho, el gasto es menor a 6.1834"

"Con base en los datos, existe evidencia estadística para concluir que
el logaritmo natural en gastos de alimentos no saludables es mayor o igual 4.1298 ?"
#Ho: mu >= 4.1298
#Ha: mu < 4.1298
{
  ttest <- t.test(x = df$ln_alns, alternative = "less", mu = 4.1298)
  p.value <- ttest$p.value
  if(p.value < 0.05) {
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es mayor a 4.1298")
  } else{
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es menor a 4.1298")
  }
}
# "NC 95%: Existe evidencia para rechazar Ho, el gasto es menor a 4.1298"

"La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a
gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros
determinantes, lleva a que un hogar presente cierta inseguridad alimentaria"
var.test(df.b[df.b$nse5f > 3, "ln_alns"],
         df.b[df.b$nse5f < 3, "ln_alns"],
         ratio = 1, alternative = "two.sided")

"Planteamiento de hipótesis:
Ho: ln_alns_nse5f1-2 <= ln_alns_nse5f4-5
Ha: ln_alns_nse5f1-2 > ln_alns_nse5f4-5"

t.test(x = df.b[df.b$nse5f > 3, "ln_alns"],
       y = df.b[df.b$nse5f < 3, "ln_alns"],
       alternative = "greater", mu = 0, var.equal = FALSE) #p-value < 2.2e-16
#A nivel de confianza estándar, EEE para rechazar la Ho, el gasto en productos no saludables en los hogares
#con menor nivel socioeconómico es mayor al gasto de los hogares con mayor nivel socioeconómico


"5.Estima un modelo de regresión, lineal o logístico, para identificiar los determinantes de la inseguridad alimentaria en México"
attach(df.b)
#Regresión logística
#IA en relación con años de educación
y = df.b$IA
x = df.b$añosedu

logistic.1 <- glm(y ~ x, data = df.b, family = binomial)
plot(logistic.1)
summary(logistic.1)

par(mfrow = c(1, 1))
plot(IA ~ añosedu, data=df.b, xlim = c(0,50))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),add = TRUE)


#IA en relación con número de personas que viven en el hogar
x = df.b$numpeho
logistic.1 <- glm(y ~ x, data = df.b, family = binomial)

summary(logistic.1)

plot(IA ~ numpeho, data=df.b, xlim = c(0,20))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),add = TRUE)


"6.Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github."



" Comprobar : La mayoría de las personas afirman que los hogares
con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas
con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar
presente cierta inseguridad alimentaria"

dfc.summ <- df %>%
  select(nse5f, ln_als, ln_alns, IA) %>%
  mutate(sumaing = ln_als + ln_alns) %>%
  group_by(nse5f) %>%
  summarize(total_as = sum(ln_als),
            total_ans = sum(ln_alns),
            pctg_ans = (total_ans / ( total_as + total_ans )))
head(dfc.summ)
"nse5f      total_as total_ans pctg_ans
  <fct>         <dbl>     <dbl>    <dbl>
1 Bajo         50478.    34804.    0.408
2 Medio Bajo   50752.    34345.    0.404
3 Medio        50617.    34036.    0.402
4 Medio Alto   49330.    33169.    0.402
5 Alto         46391.    31981.    0.408"

# En esa zona del país la gente dedica dos quintas partes de su gasto en alimentos a comprar alimentos no saludables (pctg_ans).
# Y en proporción sobre el gasto toal (total_as + total_ans), se puede ver que no hay diferencia entre lo que se destina a la compra
# de Alimentos No Saludables



"NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e interpretar todos tus resultados
para poder dar una conclusión final al problema planteado."

