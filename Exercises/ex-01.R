run <- readRDS("RunningAgg.Rds")

head(run)
str(run)
summary(run)

# Informationen zu Runnern 
# pace: durschnittliche Pace (Kehrwert der Geschwindigkeit) in min/km
# HR: durchschnittliche Herzfrequenz in bpm also beats per minute 

library(ggplot2) 
library(patchwork)
library(dplyr)
library(tidyr)

# Definition eines ggplot-Themes:
theme <- theme_classic() +
  theme(text = element_text(size = 12), axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text.y = element_text(size = 12),
        strip.placement = "outside", strip.background = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))


# Streudiagramm für ersten Überblick 
run_dotplot <- 
  ggplot(run, aes(x = pace, y = HR)) + 
  geom_point() + 
  xlab("Pace (min/km)") + ylab("Heart Rate (bpm)") + 
  theme_minimal() 

run_dotplot

# Einfaches Lineares Modell 
lm_hr_pace <- lm(formula = HR ~ pace, data = run)

summary(lm_hr_pace)

coef(lm_hr_pace)
# Interpretation von β0^: 
# Bei einer Pace von 0 min/km (was ist das, quasi unendliche Geschwindigkeit??) ist 
# die geschätzte/erwartete/mittlere/durschnittliche Herzfrequenz 197.48 bpm.

# Interpretation von β1^: 
# Erhöht sich die Pace um 1 min/km, 
# so verringert sich die Herzfrequenz durchschnittlich um 8.67 bpm (gerundet).

# Grafische Regressionsgerade
plot_regr <- run_dotplot + geom_smooth(method = "lm", se = FALSE)
plot_regr

#---------------------------------------------------------------------------
##Warum könnte es sinnvoller sein, den Zusammenhang zwischen der Geschwindigkeit (in km/h)
##und der Herzfrequenz zu untersuchen?
# weil der Intercept im Model mit pace nicht sinnvoll interpretierbar ist 
# Mit Geschwindigkeit = 0 wäre ß0 einfach Herzfrequenz im Ruhezustand 

# Fügen Spalte mit "speed" zum Dataframe hinzu und modellieren neu 
# speed in km/h = 60*(1/pace) in min/km

run <- mutate(run, speed = 60/pace)
#alternativ: run$speed = 60 / run$pace

lm_hr_speed <- lm(data = run, formula = HR ~ speed)

summary(lm_hr_speed)

coef(lm_hr_speed)
# β0ˆspeed = 102.37bpm, β1ˆspeed = 4.30bpm

run_dotplot_transf <- 
  ggplot(run, aes(x = speed, y = HR)) + 
  geom_point() + 
  xlab("Speed (km/h)") + ylab("Heart Rate (bpm)") + 
  theme_minimal() 

plot_regr_transf <-
  run_dotplot_transf + geom_smooth(method = "lm", se = FALSE)

plot_regr_transf
# keine schöne exakte Gerade => exportieren Bild als PNG mit höherer Auflösung 
ggsave("plot-1.png", plot = plot_regr_transf, 
       dpi = 300, width = 10, height = 7, units = "in", bg = "white")

# beide Modelle grafisch nebeneinander
plots_regr_both <- plot_regr + plot_regr_transf

plots_regr_both

#Konfidenzintervalle der Parameter 
confint(lm_hr_speed, level = 0.95) 

##Varianzzerlegung im einfachen linearen Modell

# Empirische Varianz der beobachteten Y_i in Gesamtheit 
var(run$HR)

#Varianz-Komponenten des Modells 
var(fitted(lm_hr_speed))

var(residuals(lm_hr_speed))
#Tatsächlich ergibt deren Summe exakt die obige empirische Varianz

##Quadratsummenzerlegung
#Gesamtstreuung von HR 
SST_hr = sum((run$HR - mean(run$HR))**2)
SST_hr
#wobei var(hr) = 1/(nrow(run)-1) * sum((run$HR - mean(run$HR))**2)

#Durch Modell erklärte Streuung 
SSM_hr = sum((lm_hr_speed$fitted - mean(run$HR))**2)

#Streuung der Residuen
SSE_hr = ((run$HR - lm_hr_speed$fitted)**2)

#R2 manuell berechnen 
R2_hr = SSM_hr / SST_hr
R2_hr
#Stimmt überein mit Modelloutput (siehe summary)

#ca. 31 Prozent der Streuung der Herzfrequenz kann 
#durch das geschätzte lineare Modell erklärt werden 

predict(lm_hr_speed)
##-------------------------------------------------------------------------------
# Modell neu berechnen mit zentrierter Einflussgröße "speed" 
# Zentrierung: x_i_zentriert = x_i - MW(x)

mean(run$speed)
# 10.89935

run$speed_centralized <- (run$speed - mean(run$speed)) 

lm_hr_speed_c <- lm(data = run, formula = HR ~ speed_centralized)

summary(lm_hr_speed_c)
#Andere Koeffizienten 
#R² identisch zu vorher 

coef(lm_hr_speed_c) 
# ß0^speed_c = 149.24542, ß1^speed_c = 4.3008
# Standardfehler (ß0-Schätzer) ist deutlich geringer geworden 

# Intercept hat sich durch Zentrierung von "speed" etwas geändert, aber selbe Größenordnung
# Steigungsparameter bleibt gleich (evtl. noch Rundungsfehler)

#das sollte so sein: Der Einfuss von "speed" ist nicht beinflusst von linearer Transformation 
#(Zentrierung ist eine lineare Variblen-Transformation)
# Intercept ändert sich, damit Gerade weiterhin durch Punkte verläuft 
# ß0 entspricht nun erwarteter Herzfrequenz bei mittlerer Geschwindigkeit!!! 
# denn speed_c_i = 0 genau wenn speed_i = MW(speed)

# insgesamt: Herzfrequenz (Puls) steigt im Schnitt mit zunehmender mittlerer Laufgeschwindigkeit

run_dotplot_transf_c <- 
  ggplot(run, aes(x = speed_centralized, y = HR)) + 
  geom_point() + 
  xlab("Speed centralized (km/h)") + ylab("Heart Rate (bpm)") + 
  theme_minimal() 

plot_regr_transf_c <-
  run_dotplot_transf_c + geom_smooth(method = "lm", se = FALSE)

plot_regr_transf_c
#logischerweise hat sich x-Achse durch Zentrierung geändert 


## nun beide Variablen standardisieren (ergänzen Spalten dazu)
run <- 
  run |> mutate(HR_stand = scale(HR), speed_stand = scale(speed))

summary(select(run, HR_stand, speed_stand)) 

lm_hr_speed_standardized <- lm (run, formula = HR_stand ~ speed_stand)

summary(lm_hr_speed_standardized)

coef(lm_hr_speed_standardized)
#ß1 ist nun geschätzte Pearson-corr(x,y)
#ß0 sollte 0 sein, hier nun Näherungsweise durch Computerrechnung 

ggplot(run, aes(x = speed_stand, y = HR_stand)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
  xlab("Speed standardized (km/h)") + ylab("Heart Rate standardized (bpm)") + 
  theme_minimal() 

#Überprüfung der Gleichheit von Steigungskoeffizient und Pearson-Korrelation
coef(lm_hr_speed_standardized)[2] #bzw. ["speed_stand"]
cor(run$HR, run$speed, method = "pearson")

all.equal(cor(run$HR, run$speed), coef(lm_hr_speed_standardized)[2], 
          check.attributes = FALSE)
#-------------------------------------------------------------------------------

