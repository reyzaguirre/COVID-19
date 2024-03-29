###############################################################################
## Coronavirus - Datos Mundo por pais
###############################################################################

library(rvest)
library(dplyr)

## Datos

webpage_url <- "https://www.worldometers.info/coronavirus/"
webpage <- read_html(webpage_url)
cvdata <- rvest::html_table(webpage)[[1]] 
names(cvdata)[1] <- "Country"
names(cvdata)[9] <- "TotCases1M"
cvdata <- mutate(cvdata, TotalDeaths = as.numeric(gsub(",", "", cvdata$TotalDeaths)))
cvdata <- mutate(cvdata, TotCases1M = as.numeric(gsub(",", "", cvdata$TotCases1M)))
cvdata <- mutate(cvdata, TotalCases = as.numeric(gsub(",", "", cvdata$TotalCases)))
cvdata <- mutate(cvdata, TotalTests = as.numeric(gsub(",", "", cvdata$TotalTests)))
cvdata$DeathRateInfected <- cvdata$TotalDeaths / cvdata$TotalCases * 100

webpage_url <- "https://www.worldometers.info/world-population/population-by-country/"
webpage <- read_html(webpage_url)
popdata <- rvest::html_table(webpage)[[1]] 
names(popdata)[2] <- "Country"
names(popdata)[3] <- "Population"
popdata <- mutate(popdata, Population = as.numeric(gsub(",", "", popdata$Population)))

## Merge

cvdata[cvdata$Country == "S. Korea", "Country"] <- "South Korea"
cvdata[cvdata$Country == "USA", "Country"] <- "United States"
cvdata[cvdata$Country == "UK", "Country"] <- "United Kingdom"
cvdata[cvdata$Country == "Czechia", "Country"] <- "Czech Republic"
popdata[popdata$Country == "Czech Republic (Czechia)", "Country"] <- "Czech Republic"
cvdata[cvdata$Country == "UAE", "Country"] <- "United Arab Emirates"
popdata[popdata$Country == "State of Palestine", "Country"] <- "Palestine"
cvdata[cvdata$Country == "DRC", "Country"] <- "DR Congo"
popdata[popdata$Country == "Côte d'Ivoire", "Country"] <- "Ivory Coast"
cvdata[cvdata$Country == "CAR", "Country"] <- "Central African Republic"
cvdata[cvdata$Country == "CAR", "Country"] <- "Central African Republic"
cvdata[cvdata$Country == "St. Barth", "Country"] <- "Saint Barthelemy"
cvdata[cvdata$Country == "St. Vincent Grenadines", "Country"] <- "St. Vincent & Grenadines"

setdiff(popdata$Country, cvdata$Country)
setdiff(cvdata$Country, popdata$Country)

d <- merge(cvdata, popdata)

## Compute rates and first filter

d <- d[d$Population > 100000, ]
d$RateDeath <- d$TotalDeaths / d$Population * 1000000

## Groups of countries

# Sud America

sal <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
         "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")

# Sud America full

saf <- c(sal, "French Guiana", "Guyana", "Suriname")

# America Latina

al <- c(sal, "Costa Rica", "Cuba", "Dominican Republic", "Guatemala",
        "Honduras", "Mexico", "Panama") 

# America full

af <- unique(c(saf, al, "Bahamas", "Barbados", "Canada", "Jamaica",
               "Puerto Rico", "Trinidad and Tobago", "United States"))

# Calculos por grupo

temp <- d[d$Country %in% sal, ]

temp <- temp[sort.int(temp$TotCases1M, index.return = TRUE)$ix, ]
par(mar = c(4, 7, 2, 2) + 0.2)
barplot(height = temp$TotCases1M, names.arg = temp$Country, horiz = TRUE,
        las = 1, xlab = "Casos por cada millón de habitantes")

temp <- temp[!is.na(temp$DeathRateInfected), ]
temp <- temp[sort.int(temp$DeathRateInfected, index.return = TRUE)$ix, ]
par(mar = c(4, 7, 2, 2) + 0.2)
barplot(height = temp$DeathRateInfected, names.arg = temp$Country, horiz = TRUE,
        las = 1, xlab = "% de fallecidos sobre total de casos identificados")

temp <- d[d$Country %in% af, ]
temp <- temp[sort.int(temp$TotCases1M, index.return = TRUE)$ix, ]

png("plot.png", 470, 550)
par(mar = c(4, 9, 2, 2) + 0.2)
barplot(height = temp$TotCases1M, names.arg = temp$Country, horiz = TRUE,
        las = 1, xlab = "Casos por cada millón de habitantes")
dev.off()

temp <- temp[!is.na(temp$TotalDeaths), ]
temp <- temp[sort.int(temp$RateDeath, index.return = TRUE)$ix, ]

png("plot1.png", 470, 550)
par(mar = c(4, 9, 2, 2) + 0.2)
barplot(height = temp$RateDeath, names.arg = temp$Country, horiz = TRUE,
        las = 1, xlab = "Muertes por cada millón de habitantes")
dev.off()

## Relative number of deaths per country

temp <- d[!is.na(d$TotalDeaths), ]
temp <- temp[sort.int(temp$RateDeath, index.return = TRUE)$ix, ]

png("plot2.png", 800, 1400)
#png("plot.png", 450, 500)
par(mar = c(4, 11, 2, 2) + 0.2)
barplot(height = temp$RateDeath, names.arg = temp$Country, horiz = TRUE,
        las = 1, xlab = "Muertes por cada millón de habitantes")
dev.off()

## Graficos de dispersion

d <- cvdata[!(cvdata$Country %in% c("Total:", "World")), ]

d$TotalCases <- d$TotalCases / 1000
d$TotalTests <- d$TotalTests / 1000

temp <- d[d$Country %in% c("UK", "France", "Spain", "Italy",
                           "S. Korea", "China", "Germany", "USA"), ]

model <- lm(TotalCases ~ TotalTests, data = d)
pred <- data.frame(TotalTests = seq(0, 1900, 1))
pred$TotalCases <- predict(model, newdata = pred)

model.2 <- lm(TotalDeaths ~ TotalCases, data = d)
pred.2 <- data.frame(TotalCases = seq(0, 400, .1))
pred.2$TotalDeaths <- predict(model.2, newdata = pred.2)

ggplot(d, aes(TotalTests, TotalCases)) +
  geom_point() +
  geom_label(data = temp, aes(label = Country)) + 
  labs(x = "Pruebas totales (miles)", y = "Casos totales (miles)") +
  geom_line(data = pred, col = 4)

ggplot(d, aes(TotalCases, TotalDeaths)) +
  geom_point() +
  geom_label(data = temp, aes(label = Country)) + 
  labs(x = "Casos totales (miles)", y = "Muertes totales") +
  geom_line(data = pred.2, col = 4)

temp <- d[d$Country %in% c("Uruguay", "Panama", "Brazil", "Ecuador", "Chile",
                           "Bolivia", "Argentina", "Colombia", "Peru", "Mexico"), ]

ggplot(d, aes(TotalTests, TotalCases)) +
  geom_point() +
  xlim(0, 54) + 
  ylim(0, 4.7) + 
  geom_label(data = temp, aes(label = Country)) + 
  labs(x = "Pruebas totales (miles)", y = "Casos totales (miles)") +
  geom_line(data = pred, col = 4)

ggplot(d, aes(TotalCases, TotalDeaths)) +
  geom_point() +
  xlim(0, 5.8) + 
  ylim(0, 240) + 
  geom_label(data = temp, aes(label = Country)) + 
  labs(x = "Casos totales (miles)", y = "Muertes totales") +
  geom_line(data = pred.2, col = 4)

###############################################################################
## Coronavirus - Datos Mundo
###############################################################################

## Datos

webpage_url <- "https://www.worldometers.info/coronavirus/coronavirus-death-toll/"
webpage <- read_html(webpage_url)
d <- rvest::html_table(webpage)[[1]] 

colnames(d)[2] <- "y"
d <- mutate(d, y = as.numeric(gsub(",", "", d$y)))
d$y <- d$y/1000

origin <- as.Date("23/01/20", "%d/%m/%Y")
d$Fecha <- seq(as.Date.numeric(56, origin), as.Date.numeric(0, origin), -1)

model <- lm(log(y) ~ Fecha, data = d)
pred <- data.frame(Fecha = seq(as.Date.numeric(0, origin), as.Date.numeric(98, origin), 1))
pred.mod <- predict(model, newdata = pred, se.fit = TRUE)
pred$y <- exp(pred.mod$fit)
pred$ymin <- exp(pred.mod$fit - 1.96 * pred.mod$se.fit)
pred$ymax <- exp(pred.mod$fit + 1.96 * pred.mod$se.fit)

ggplot(d, aes(Fecha, y)) +
  geom_point() +
  geom_ribbon(data = pred, aes(ymin = ymin, ymax = ymax), alpha = 0.3) +
  labs(x = "Fecha", y = "Número de fallecidos (en miles)", title = "Número de fallecidos - Mundial") +
  geom_line(data = pred, col = 4)

ggplot(d, aes(Fecha, y)) +
  geom_point() + xlim(as.Date.numeric(0, d$Fecha[1]), as.Date.numeric(17, d$Fecha[1])) + 
  labs(x = "Fecha", y = "Número de casos identificados") +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), fullrange = TRUE)

###############################################################################
## Grafico relativo de causas de muerte
###############################################################################

item <- c("Hambre", "Cáncer", "Cigarrillo", "Alcohol", "Sida",
          "Acc. tráfico", "Suicidios", "Malaria", "Gripe", "COVID-19")

freq1 <- c(2769, 2033, 1237, 619, 416, 334, 265, 242, 120, 40)
date1 <- as.Date("31/03/20", "%d/%m/%Y")

freq2 <- c(2938, 2157, 1313, 657, 442, 354, 282, 258, 128, 69)
date2 <- as.Date("05/04/20", "%d/%m/%Y")

freq3 <- c(3215, 2361, 1437, 719, 483, 388, 308, 282, 140, 127)
date3 <- as.Date("14/04/20", "%d/%m/%Y")

freq4 <- c(3263, 2397, 1459, 730, 491, 394, 313, 286, 142, 143)
date4 <- as.Date("16/04/20", "%d/%m/%Y")

freq5 <- c(3461, 2541, 1547, 774, 520, 418, 332, 304, 151, 184)
date5 <- as.Date("22/04/20", "%d/%m/%Y")

freq6 <- c(3603, 2646, 1610, 806, 542, 434, 345, 316, 157, 210)
date6 <- as.Date("27/04/20", "%d/%m/%Y")

freq7 <- c(3920, 2878, 1752, 877, 589, 473, 376, 344, 171, 271)
date7 <- as.Date("07/05/20", "%d/%m/%Y")

freq8 <- c(4158, 3054, 1859, 930, 625, 502, 399, 365, 181, 308)
date8 <- as.Date("15/05/20", "%d/%m/%Y")

## Grafico de barras

d <- data.frame(item, freq = freq7)

ggplot(data = d, aes(x = reorder(item, 1/freq), y = freq)) +
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
  labs(x = "Causa de muerte", y = "Frecuencia en miles durante este año")

## Tendencia

d <- data.frame(item = rep(item, 8),
                freq = c(freq1, freq2, freq3, freq4, freq5, freq6, freq7,
                         freq8),
                dat = c(rep(date1, 10), rep(date2, 10), rep(date3, 10),
                        rep(date4, 10), rep(date5, 10), rep(date6, 10),
                        rep(date7, 10), rep(date8, 10)))

ggplot(d, aes(dat, freq, colour = reorder(item, 1/freq, min))) +
  geom_point() +
  labs(x = "Fecha", y = "Número de fallecidos durate este año (miles)", 
       fill = "Causa de muerte") +
  geom_smooth(method = "auto", se = FALSE)

##############################################################################
## Disminución de movilidad
###############################################################################

library(ggplot2)

item <- c("Retail & recreation", "Grocery & pharmacy", "Parks",
          "Transit stations", "Workplaces", "Residential")

pais <- c("Peru", "Chile", "Ecuador", "Argentina", "Colombia", "Bolivia")

df <- data.frame(Country = rep(pais, each = 6),
                 cate = rep(item, 6),
                 vari = c(-95, -95, -89, -93, -75, 34,
                          -69, -49, -70, -59, -42, 20,
                          -82, -64, -78, -78, -59, 25,
                          -83, -54, -87, -76, -52, 26,
                          -85, -66, -79, -81, -57, 26,
                          -93, -94, -89, -91, -71, 35))

ggplot(data = df, aes(x = cate, y = vari, fill = Country)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  scale_x_discrete(limits = item) +
  labs(x = "", y = "Change compared to baseline (%)",
       title = "Mobility changes")

###############################################################################
## Poisson regression
###############################################################################

library(MASS)

d <- d[13:37, ]
d$x <- 1:25

for (i in 5:25) {
  temp <- d[1:i, ]
  model <- glm(yd ~ x, family = poisson, temp)
  print(exp(coef(model)[2]))
}

model <- glm.nb(yd ~ x, temp)
summary(model)
exp(coef(model)[2])


temp <- d[13:35, ]

model <- glm(yd ~ x, family = poisson, temp)
summary(model)
exp(coef(model)[2])

##############################################################################
## Estimacion de muertos
###############################################################################

library(ggplot2)

mes <- c('abril', 'marzo', 'febrero', 'enero')
mes <- factor(rep(mes, 4), levels = c('enero', 'febrero', 'marzo', 'abril'),
              ordered = TRUE)
año = c(rep(2020, 4), rep(2019, 4), rep(2018, 4), rep(2017, 4))

d <- data.frame(y = c(11797, 9234, 9083, 9596, 
                       8961, 9950, 9325, 9769,
                       8615, 8720, 7936, 8785,
                       7670, 8683, 7408, 7749),
                año = año,
                mes = mes)

ggplot(d, aes(año, y, colour = mes)) +
  geom_point() +
  labs(x = "Año", y = "Número de fallecidos por mes") +
  geom_line()

ggplot(d, aes(año, y, colour = mes)) +
  geom_point() +
  labs(x = "Año", y = "Número de fallecidos por mes") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

###############################################################################
## Perú llegada vacunas
## https://es.wikipedia.org/wiki/Vacunaci%C3%B3n_contra_la_COVID-19_en_Per%C3%BA#Lotes_de_vacunas
## https://datosmacro.expansion.com/demografia/estructura-poblacion/peru
##  0 a mas: 32.517
###############################################################################

library(ggplot2)
library(reshape2)
library(st4gi)

laboratorios <- c("Sinopharm", "Pfizer", "AstraZeneca")
nl <- length(laboratorios)

# Sinopharm

sin.d <- c("02-07", "02-13", "06-02", "06-05", "07-10", "07-11", "08-01", "08-15",
           "08-21", "08-22", "08-29", "09-04", "09-10", "09-11", "09-12", "10-22",
           "10-23", "10-24", "10-29", "10-31", "11-10", "11-13")
sin.c <- c( .30000,  .70000,  .70000,  .30000,  .61440,  .38560, 1.00000, 1.00000,
            .61440,  .38560, 1.00000, 2.00000, 2.86250, 3.34260,  .79490, 1.90810,
           2.38760, 1.91465, 1.78965,  .23640, 1.25640, 1.00000)
sin.d <- paste0("2021-", sin.d)

# Pfizer

pfi.d <- c("03-03", "03-10", "03-10", "03-17", "03-24", "03-31", "04-07", "04-14",
           "04-21", "04-28", "05-06", "05-07", "05-13", "05-17", "05-19", "05-24",
           "05-26", "05-31", "06-03", "06-04", "06-10", "06-17", "06-24", "06-29",
           "07-01", "07-07", "07-15", "07-22", "07-26", "07-30", "08-05", "08-12",
           "08-18", "08-19", "08-20", "08-26", "09-03", "09-06", "09-13", "09-20",
           "09-27", "09-30", "10-03", "10-07", "10-11", "10-14", "10-18", "10-21",
           "10-26", "10-28", "11-01", "11-06", "11-13", "11-15")
pfi.c <- c( .05031,  .11750,  .05031,  .05031,  .05031,  .04914,  .20070,  .20070,
            .20070,  .20070,  .35000,  .35000,  .70000,  .39546,  .39546,  .39663,
            .39546,  .25155,  .71838,  .24219,  .49608,  .49608,  .49725, 1.00200,
            .49725,  .99800,  .28197,  .88686,  .93600,  .19656,  .47502,  .56979,
            .22230,  .22230,  .59436,  .54054,  .66222,  .74880,  .74880,  .65052,
            .64935,  .32994,  .70200,  .32877,  .70200,  .32874,  .70200,  .48040,
            .70200,  .32643,  .51480,  .51363,  .75933,  .76635)
pfi.d <- paste0("2021-", pfi.d)

# AstraZeneca

ast.d <- c("04-18", "05-29", "08-04", "09-02", "09-08", "09-13", "09-14", "09-25",
           "10-08", "10-22", "11-05", "11-06")
ast.c <- c( .27600,  .51120,  .10176,  .03510,  .36240,  .33600,  .14640,  .16320,
           1.24590,  .07920,  .86580,  .79190)
ast.d <- paste0("2021-", ast.d)

d <- data.frame(Fecha = c(rep(Sys.Date(), nl), rep("2021-02-06", nl),
                          sin.d, pfi.d, ast.d),
                Cantidad = c(rep(0, nl * 2), sin.c, pfi.c, ast.c),
                Laboratorio = c(rep(laboratorios, 2),
                                rep("Sinopharm", length(sin.d)),
                                rep("Pfizer", length(pfi.d)),
                                rep("AstraZeneca", length(ast.d))))

d$Fecha <- as.Date(d$Fecha, "%Y-%m-%d")

# Gráfico acumulado total

temp <- data.frame(tapply(d$Cantidad, d$Fecha, sum))
colnames(temp) <- "value"
temp <- data.frame(apply(temp, 2, cumsum))
temp$Fecha <- rownames(temp)
temp$Fecha <- as.Date(temp$Fecha, "%Y-%m-%d")

ggplot(temp, aes(Fecha, value)) +
  geom_step(color = "blue", lty = 1) +
  labs(y = "Cantidad acumulada en millones",
       title = "Llegada de vacunas y cantidad requerida según grupo de edad") +
  scale_y_continuous(expand = expansion(mult = c(0, 0), add = c(0, 2))) +
  geom_hline(yintercept = 0.565 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 0.565 * 2,
           label = "Mayores de 80", hjust = 0, vjust = -.5) +
  # geom_hline(yintercept = 1.066 * 2, linetype = 2, size = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 1.066 * 2,
  #          label = "Mayores de 75", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 1.755 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 1.754 * 2,
           label = "Mayores de 70", hjust = 0, vjust = -.5) +
  # geom_hline(yintercept = 2.729 * 2, linetype = 2, size = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 2.728 * 2,
  #          label = "Mayores de 65", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 3.927 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 3.927 * 2,
           label = "Mayores de 60", hjust = 0, vjust = -.5) +
  # geom_hline(yintercept = 5.386 * 2, linetype = 2, size = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 5.387 * 2,
  #          label = "Mayores de 55", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 7.112 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 7.112 * 2,
           label = "Mayores de 50", hjust = 0, vjust = -.5) +
  # geom_hline(yintercept = 9.088 * 2, linetype = 2, size = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 9.088 * 2,
  #          label = "Mayores de 45", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 11.31 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 11.31 * 2,
           label = "Mayores de 40", hjust = 0, vjust = -.5) +
  # geom_hline(yintercept = 13.758 * 2, linetype = 2, size = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 13.758 * 2,
  #          label = "Mayores de 35", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 16.379 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 16.379 * 2,
           label = "Mayores de 30", hjust = 0, vjust = -.5) +
  # geom_hline(yintercept = 19.134 * 2, linetype = 2, size = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 19.134 * 2,
  #          label = "Mayores de 25", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 21.802 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 21.802 * 2,
           label = "Mayores de 20", hjust = 0, vjust = -.5) +
  # geom_hline(yintercept = 24.307 * 2, linetype = 2, size = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 24.307 * 2,
  #          label = "Mayores de 15", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 27.027 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 27.027 * 2,
           label = "Mayores de 10", hjust = 0, vjust = -.5) +
  geom_hline(yintercept = 29.707 * 2, linetype = 2, size = 1) +
  annotate(geom = "text", x = as.Date("2021-02-06", "%Y-%m-%d"), y = 29.707 * 2,
           label = "Mayores de 5", hjust = 0, vjust = -.5)

# Gráfico acumulado por laboratorio

temp <- data.frame(tapply(d$Cantidad, list(d$Fecha, d$Laboratorio), sum))
temp[is.na(temp)] <- 0
temp <- data.frame(apply(temp, 2, cumsum))
temp$Fecha <- rownames(temp)

temp <- melt(temp, "Fecha")
temp$Fecha <- as.Date(temp$Fecha, "%Y-%m-%d")
colnames(temp)[2] <- "Laboratorio"

ggplot(temp, aes(Fecha, value, colour = Laboratorio)) +
  geom_step() +
  scale_y_continuous(expand = expansion(mult = c(0, 0), add = c(0, 1))) +
  labs(y = "Cantidad acumulada en millones",
       title = "Llegada de vacunas por laboratorio")
  # annotate(geom = "text", x = as.Date("2021-02-18", "%Y-%m-%d"), y = 1.17,
  #          label = "Efecto Bustamante / Willax / Beto Ortiz", size = 4.5,
  #          color = "darkorange", hjust = 0, vjust = 1) +
  # annotate(geom = "text", x = as.Date("2021-02-25", "%Y-%m-%d"), y = 0.97,
  #          label = "Fujimorismo nunca más", size = 4.5,
  #          color = "darkorange", hjust = 0, vjust = 1)

# Gráfico totales

temp <- docomp("sum", "Cantidad", "Laboratorio", dfr = d)

ggplot(temp, aes(Laboratorio, Cantidad)) +
  labs(y = "Cantidad en millones",
       title = "Total recibido por laboratorio") +
  geom_bar(stat = "identity", fill = "steelblue")

