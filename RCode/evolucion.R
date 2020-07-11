###############################################################################
## Evolucion por ciudades
###############################################################################

options(stringsAsFactors = FALSE)
options(digits = 4)

library(ggplot2)
library(st4gi)
library(reshape)
library(readxl)

# Datos

full <- data.frame(read_excel("ReporteSinadef\\temp.xlsx", sheet = 1, skip = 3))
full <- full[, c("PAIS.DOMICILIO", "PROVINCIA.DOMICILIO", "DISTRITO.DOMICILIO",
                 "FECHA", "AÑO", "MES", "MUERTE.VIOLENTA")]
colnames(full) <- c("pais", "provincia", "distrito", "fecha", "ano", "mes", "violenta")

# Formato fecha

full$fecha <- as.Date(full$fecha, "%Y-%m-%d")
full$mes <- as.numeric(full$mes)
full$ano <- as.numeric(full$ano)

# Eliminar otros paises

full <- full[full$pais == "PERU", ]

# Identificar ultimo dia

ultimo.fecha <- max(full$fecha)
ultimo.dia <- as.numeric(substr(ultimo.fecha, 9, 10)) - 1 
ultimo.mes <- as.numeric(substr(ultimo.fecha, 6, 7))

# Datos hasta el último día de todos los años

full <- full[full$mes <= ultimo.mes, ]
full <- full[!(full$mes == ultimo.mes & as.numeric(substr(full$fecha, 9, 10)) > ultimo.dia), ]

# Juntar Lima y Callao

full$prov2 <- full$provincia
full$prov2[full$prov2 %in% c("CALLAO", "LIMA")] <- "LIMA Y CALLAO"

# Eliminar muerte violenta

full.nv <- full[full$violenta == "SIN REGISTRO", ]

# Totales nacionales por año

totales <- table(full.nv$ano)

# Solo año 2020

temp <- full.nv[full.nv$ano == 2020, ]

## Total nacional

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp, method = 'slow')

# Gráfico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios a nivel nacional por causa no violenta") +
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Lima y Callao

temp.p <- temp[temp$provincia %in% c("CALLAO", "LIMA"), ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en las provincias de Lima y Callao por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Arequipa

temp.p <- temp[temp$provincia == "AREQUIPA", ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la provincia de Arequipa por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Trujillo

temp.p <- temp[temp$provincia == "TRUJILLO", ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la provincia de Trujillo por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Piura

temp.p <- temp[temp$provincia == "PIURA", ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la provincia de Piura por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Maynas

temp.p <- temp[temp$provincia == "MAYNAS", ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la provincia de Maynas por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Santa

temp.p <- temp[temp$provincia == "SANTA", ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la provincia de Santa por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Coronel Portillo

temp.p <- temp[temp$provincia == "CORONEL PORTILLO", ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la provincia de Coronel Portillo por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))

## Sullana

temp.p <- temp[temp$provincia == "SULLANA", ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la provincia de Sullana por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr'))


# Conteo diario provincia y año

ds <- docomp("count", "pais", c("ano", "prov2"), dfr = full.nv, method = 'slow')

# Datos por año

dc <- ds[ds$ano == 2020, 2:3]
colnames(dc)[2] <- "total20"
temp.p <- ds[ds$ano == 2019, 2:3]
colnames(temp.p)[2] <- "total19"
dc <- merge(dc, temp.p)
temp.p <- ds[ds$ano == 2018, 2:3]
colnames(temp.p)[2] <- "total18"
dc <- merge(dc, temp.p)

# Calcular media 2018 2019

dc$media1819 <- (dc$total19 + dc$total18) / 2

# Diferencia

dc$dif <- dc$total20 - dc$media1819

#| Chiclayo         | `r dc[dc$prov2 == "CHICLAYO", "dif"] / 799675 * 1000` | 
#| Lambayeque       | `r dc[dc$prov2 == "LAMBAYEQUE", "dif"] / 300170 * 1000` | 
#| Ica              | `r dc[dc$prov2 == "ICA", "dif"] / 391519 * 1000` | 
#| Pasco            | `r dc[dc$prov2 == "PASCO", "dif"] /	123015 * 1000` | 
#| Sechura          | `r dc[dc$prov2 == "SECHURA", "dif"] /	79177 * 1000` | 
#| Huaral           | `r dc[dc$prov2 == "HUARAL", "dif"] / 183898 * 1000` | 
#| San Martin       | `r dc[dc$prov2 == "SAN MARTIN", "dif"] / 193095 * 1000` | 
#| Pisco            | `r dc[dc$prov2 == "PISCO", "dif"] / 150744 * 1000` | 
#| Leoncio Prado    | `r dc[dc$prov2 == "LEONCIO PRADO", "dif"] / 127793 * 1000` | 
#| Jaen             | `r dc[dc$prov2 == "JAEN", "dif"] / 185432 * 1000` | 
#| Huaura           | `r dc[dc$prov2 == "HUAURA", "dif"] / 227685 * 1000` | 
#| Requena          | `r dc[dc$prov2 == "REQUENA", "dif"] /	58511 * 1000` | 
#| Alto Amazonas    | `r dc[dc$prov2 == "ALTO AMAZONAS", "dif"] /	122725 * 1000` | 
#| Arequipa         | `r dc[dc$prov2 == "AREQUIPA", "dif"] / 1080635 * 1000` | 
#| Ica              | `r dc[dc$prov2 == "ICA", "dif"] / 391519 * 1000` | 
#| Cañete           | `r dc[dc$prov2 == "CAÑETE", "dif"] / 240013 * 1000` | 
#| Huaraz           | `r dc[dc$prov2 == "HUARAZ", "dif"] / 163936 * 1000` | 
#| Chanchamayo      | `r dc[dc$prov2 == "CHANCHAMAYO", "dif"] / 151489 * 1000` | 
#| Huánuco          | `r dc[dc$prov2 == "HUANUCO", "dif"] / 293397 * 1000` | 
#| Ascope           | `r dc[dc$prov2 == "ASCOPE", "dif"] / 115786 * 1000` | 


| Provincia        | Exceso de fallecidos por 1000 habitantes |
  |:-----------------|:----------------------------------------:|
  | Chincha          | `r dc[dc$prov2 == "CHINCHA", "dif"] / 226113 * 1000` | 
  | Lima y Callao    | `r dc[dc$prov2 == "LIMA Y CALLAO", "dif"] / 9569468 * 1000` |
  | Tumbes           | `r dc[dc$prov2 == "TUMBES", "dif"] / 154962 * 1000` |
  | Santa            | `r dc[dc$prov2 == "SANTA", "dif"] / 435807 * 1000` | 
  | Casma            | `r dc[dc$prov2 == "CASMA", "dif"] /	50989 * 1000` | 
  | Coronel Portillo | `r dc[dc$prov2 == "CORONEL PORTILLO", "dif"] / 384168 * 1000` |
  | Zarumilla        | `r dc[dc$prov2 == "ZARUMILLA", "dif"] / 48844 * 1000` | 
  | Trujillo         | `r dc[dc$prov2 == "TRUJILLO", "dif"] / 970016 * 1000` | 
  | Maynas           | `r dc[dc$prov2 == "MAYNAS", "dif"] / 479866 * 1000` |
  | Huarochiri       | `r dc[dc$prov2 == "HUAROCHIRI", "dif"] / 58145 * 1000` | 
  | Tambopata        | `r dc[dc$prov2 == "TAMBOPATA", "dif"] / 111474 * 1000` | 
  | Paita            | `r dc[dc$prov2 == "PAITA", "dif"] / 129892 * 1000` | 
  | Sullana          | `r dc[dc$prov2 == "SULLANA", "dif"] / 311454 * 1000` | 
  | Viru             | `r dc[dc$prov2 == "VIRU", "dif"] / 92324 * 1000` | 
  | Piura            | `r dc[dc$prov2 == "PIURA", "dif"] / 799321 * 1000` | 
  | Total nacional   | `r (totales[4] - totales[3]) / 31237385 * 1000` |
  