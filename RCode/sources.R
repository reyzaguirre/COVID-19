###############################################################################
## Data Financial Times
###############################################################################

options(stringsAsFactors = FALSE)
options(digits = 4)

library(httr)

url.ft <- "https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/ft_excess_deaths.csv"
GET(url.ft, write_disk("temp.csv", overwrite = TRUE))
full <- read.csv("temp.csv")

## Solo Chile y Peru

full <- full[full$country %in% c("Chile", "Peru"), ]
full <- full[full$region %in% c("Chile", "Peru"), ]

## Solo 2018 en adelante

full <- full[full$year %in% 2018:2020, ]

tapply(full$deaths, list(full$year, full$country), sum, na.rm = TRUE)

## Limpiar

full <- full[!(full$country == "Peru" & full$region %in% c("Extranjero", "Sin Registro")), ]
