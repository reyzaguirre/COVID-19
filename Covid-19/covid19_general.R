###############################################################################
## Coronavirus - Datos Peru
###############################################################################

setwd("D:/Users/reyzaguirre/Dropbox/RHEP-CIP")
setwd("~/Dropbox/RHEP-CIP")

library(readODS)
library(ggplot2)

d <- read_ods("MyBook.ods", 7, range = "A1:D45", col_names = TRUE)
d$Fecha <- as.Date(d$Fecha, "%d/%m/%Y")

## Modelo exponencial

temp <- d[1:15, ]
model <- lm(log(y) ~ Fecha, data = temp)
pred <- data.frame(Fecha = seq(as.Date.numeric(0, d$Fecha[1]),
                               as.Date.numeric(27, d$Fecha[1]), 1))
pred.mod <- predict(model, newdata = pred, se.fit = TRUE)
pred$y <- exp(pred.mod$fit)
pred$ymin <- exp(pred.mod$fit - 1.96 * pred.mod$se.fit)
pred$ymax <- exp(pred.mod$fit + 1.96 * pred.mod$se.fit)

ggplot(d, aes(Fecha, y)) +
  geom_point() +
  geom_ribbon(data = pred, aes(ymin = ymin, ymax = ymax), alpha = 0.3) +
  labs(x = "Fecha", y = "Número de casos identificados",
       title = "Número de casos - Perú") +
  geom_line(data = pred, col = 4)

## Efectos cuarentena

ggplot(d, aes(x = Fecha)) +
  geom_point(aes(y = y, colour = "Observado")) +
  geom_smooth(aes(y = y, colour = "Observado"),
              method = "auto", span = .75) +
  geom_smooth(data = pred, aes(y = y, colour = "Modelo exponencial"),
              method = "auto", span = .75) +
  labs(x = "Fecha", y = "Número de casos identificados", color = "") +
  theme(legend.position = "bottom",
        legend.box = "vertical")

## Pronosticos numero de casos identificados

ggplot(d, aes(Fecha, y)) +
  geom_point() +
  xlim(as.Date.numeric(0, d$Fecha[1]), as.Date.numeric(37, d$Fecha[1])) + 
  labs(x = "Fecha", y = "Número de casos identificados") +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), fullrange = TRUE)

ggplot(d, aes(Fecha, y)) +
  geom_point() +
  xlim(as.Date.numeric(0, d$Fecha[1]), as.Date.numeric(37, d$Fecha[1])) + 
  labs(x = "Fecha", y = "Número de casos identificados") +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'), fullrange = TRUE)

ggplot(d, aes(Fecha, y)) +
  geom_point() +
  labs(x = "Fecha", y = "Número de casos identificados") +
  geom_smooth(method = "auto", span = 0.9)

## Pronosticos numero de casos totales estimados

d$yt <- 0

tc <- c(rep(0.11, 14), seq(0.11, 0.08, -0.002), rep(0.08, 15))

for (i in 1:45)
  d$yt[i] <- foo(0.003, tc[i], d$yd[i])$infected_total / 1000

temp <- d[12:45, ]

ggplot(temp, aes(Fecha, yt)) +
  geom_point() +
  xlim(as.Date.numeric(0, temp$Fecha[1]), as.Date.numeric(25, temp$Fecha[1])) + 
  labs(x = "Fecha", y = "Número estimado de casos totales (en miles)") +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), fullrange = TRUE)

ggplot(temp, aes(Fecha, yt)) +
  geom_point() +
  xlim(as.Date.numeric(0, temp$Fecha[1]), as.Date.numeric(25, temp$Fecha[1])) + 
  labs(x = "Fecha", y = "Número estimado de casos totales (en miles)") +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'), fullrange = TRUE)

## Pronosticos numero de muertos

ggplot(temp, aes(Fecha, yd)) +
  geom_point() +
  xlim(as.Date.numeric(0, temp$Fecha[1]), as.Date.numeric(25, temp$Fecha[1])) + 
  labs(x = "Fecha", y = "Número de fallecidos") +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), fullrange = TRUE)

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

###############################################################################
## Coronavirus - Datos Peru - Estimacion de casos
###############################################################################

setwd("D:/Users/reyzaguirre/Dropbox/RHEP-CIP")
setwd("~/Dropbox/RHEP-CIP")

library(readODS)
library(ggplot2)

d <- read_ods("MyBook.ods", 7, range = "A1:D49", col_names = TRUE)
d$Fecha <- as.Date(d$Fecha, "%d/%m/%Y")

## Pronosticos numero de casos totales estimados

a <- 20
b <- 5000
a / b * 100
qgamma(0.005, a, b) * 100
qgamma(0.995, a, b) * 100

m <- 0.12
s <- 0.015
qnorm(0.005, m, s)
qnorm(0.995, m, s)
log(2) / log(1 + m)
log(2) / log(1 + qnorm(0.005, m, s))
log(2) / log(1 + qnorm(0.995, m, s))

tl <- 18

nsim <- 1000
dr <- rgamma(nsim, a, b)
ir <- rnorm(nsim, m, s)
nsd <- 0.15
alpha <- 0.01

temp <- d[14:49, ]
temp$yt <- 0

for (i in 1:36)
  temp$yt[i] <- foo(dr[1], ir[1], tl, temp$yd[i])$infected_now / 1000

model <- lm(yt ~ splines::bs(Fecha, 3), data = temp)
newd <- with(temp, data.frame(Fecha = seq(min(Fecha), max(Fecha), length = 40)))
pred <- predict(model, newd, se.fit = TRUE)
se.fit <- pred$se.fit

pred <- transform(cbind(data.frame(pred), newd),
                  upl = fit + nsd * se.fit * c(1:40),
                  lwl = fit - nsd * se.fit * c(1:40))

p <- ggplot(pred, aes(x = Fecha)) +
  labs(x = "Fecha", y = "Número estimado de casos totales (en miles)") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444")
        ,panel.background = element_rect(fill = '#444B5A')
        ,panel.grid.minor = element_line(color = '#4d5566')
        ,panel.grid.major = element_line(color = '#586174')) +
  geom_ribbon(aes(ymin = lwl, ymax = upl),
              alpha = alpha, fill = "cyan")

for (j in 2:nsim) {
  
  temp$yt <- 0
  
  for (i in 1:36)
    temp$yt[i] <- foo(dr[j], ir[j], tl, temp$yd[i])$infected_now / 1000
  
  model <- lm(yt ~ splines::bs(Fecha, 3), data = temp)
  newd <- with(temp, data.frame(Fecha = seq(min(Fecha), max(Fecha), length = 40)))
  pred <- predict(model, newd, se.fit = TRUE)
  se.fit <- pred$se.fit
  
  pred <- transform(cbind(data.frame(pred), newd),
                    upl = fit + nsd * se.fit * c(1:40),
                    lwl = fit - nsd * se.fit * c(1:40))
  
  p <- p +
    geom_ribbon(aes(ymin = lwl, ymax = upl), data = pred,
                alpha = alpha, fill = "cyan")
  
}

p

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
