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

# Solo año 2020

full2020 <- full.nv[full.nv$ano == 2020, ]

###############################################################################
## Total nacional
# 1.364: 10-07
# 1.008: 18-06

temp <- full.nv
totales <- table(temp$ano)
(totales[4] - (totales[3] + totales[2]) / 2) / 31237385 * 1000

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = full2020, method = 'slow')

# Gráfico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios a nivel nacional por causa no violenta") +
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr')) +
  annotate(geom = "text", x = as.Date("2020-01-01", "%Y-%m-%d"), y = max(ds$pais),
           label = "Exceso en número de fallecidos por 1000 habitantes:\n- Línea verde: 1\n- Actual: 1.423",
           color = "blue", hjust = 0, vjust = 1) +
  geom_vline(xintercept = as.Date("2020-06-18", "%Y-%m-%d"), linetype = 2, 
           color = 3, size = 1.5)

###############################################################################
## Lima y Callao
# 3.006: 10-07
# 2.029: 08-06
# 1.004: 12-05

temp.p <- full2020[full2020$provincia %in% c("CALLAO", "LIMA"), ]

temp <- full.nv[full.nv$prov2 == "LIMA Y CALLAO", ]
totales <- table(temp$ano)
(totales[4] - (totales[3] + totales[2]) / 2) / 9569468 * 1000

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en las provincias de Lima y Callao por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr')) +
  annotate(geom = "text", x = as.Date("2020-01-01", "%Y-%m-%d"), y = max(ds$pais),
           label = "Exceso en número de fallecidos por 1000 habitantes:\n- Línea verde: 1\n- Línea naranja: 2\n- Línea roja: 3\n- Actual: 3.096",
           color = "blue", hjust = 0, vjust = 1) +
  geom_vline(xintercept = as.Date("2020-05-12", "%Y-%m-%d"), linetype = 2, 
           color = 3, size = 1.5) +
  geom_vline(xintercept = as.Date("2020-06-08", "%Y-%m-%d"), linetype = 2, 
             color = 7, size = 1.5) +
  geom_vline(xintercept = as.Date("2020-07-10", "%Y-%m-%d"), linetype = 2, 
             color = 2, size = 1.5)

###############################################################################
## Arequipa
# 1.022: 28-06

distritos <- c("AREQUIPA", "ALTO SELVA ALEGRE", "CAYMA", "CERRO COLORADO", "JACOBO HUNTER",
               "JOSE LUIS BUSTAMANTE Y RIVERO", "MARIANO MELGAR", "MIRAFLORES", "PAUCARPATA",
               "SABANDIA", "SACHACA", "SOCABAYA", "TIABAYA", "YANAHUARA")
poblacion <- c(55437, 85870, 91935, 197954, 50164, 81829, 59918, 60589, 131346, 4368,
               24225, 75351, 16191, 25417)

temp.p <- full2020[full2020$provincia == "AREQUIPA" & full2020$distrito %in% distritos, ]

temp <- full.nv[full.nv$provincia == "AREQUIPA" & full.nv$distrito %in% distritos, ]
totales <- table(temp$ano)
(totales[4] - (totales[3] + totales[2]) / 2) / sum(poblacion) * 1000

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la ciudad de Arequipa por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr')) +
  annotate(geom = "text", x = as.Date("2020-01-01", "%Y-%m-%d"), y = max(ds$pais),
           label = "Exceso en número de fallecidos por 1000 habitantes:\n- Línea verde: 1\n- Actual: 1.806",
           color = "blue", hjust = 0, vjust = 1) +
  geom_vline(xintercept = as.Date("2020-06-28", "%Y-%m-%d"), linetype = 2, 
             color = 3, size = 1.5)

###############################################################################
## Trujillo
# 2.026: 01-07
# 1.002: 13-06

distritos <- c("TRUJILLO", "VICTOR LARCO HERRERA", "FLORENCIA DE MORA",
               "EL PORVENIR", "LA ESPERANZA")
poblacion <- c(314939, 68506, 37262, 190461, 189206)

temp.p <- full2020[full2020$provincia == "TRUJILLO" & full2020$distrito %in% distritos, ]

temp <- full.nv[full.nv$provincia == "TRUJILLO" & full.nv$distrito %in% distritos, ]
totales <- table(temp$ano)
(totales[4] - (totales[3] + totales[2]) / 2) / sum(poblacion) * 1000

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la ciudad de Trujillo por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr')) +
  annotate(geom = "text", x = as.Date("2020-01-01", "%Y-%m-%d"), y = max(ds$pais),
           label = "Exceso en número de fallecidos por 1000 habitantes:\n- Línea verde: 1\n- Línea naranja: 2\n- Actual: 2.624",
           color = "blue", hjust = 0, vjust = 1) +
  geom_vline(xintercept = as.Date("2020-06-13", "%Y-%m-%d"), linetype = 2, 
             color = 3, size = 1.5) +
  geom_vline(xintercept = as.Date("2020-07-01", "%Y-%m-%d"), linetype = 2, 
             color = 7, size = 1.5)

###############################################################################
## Piura
# 1.021: 12-06

distritos <- c("PIURA", "VEINTISEIS DE OCTUBRE", "CASTILLA", "CATACAOS")
poblacion <- c(158495, 165779, 160201, 75870)

temp.p <- full2020[full2020$provincia == "PIURA" & full2020$distrito %in% distritos, ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la ciudad de Piura por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr')) +
  geom_vline(xintercept = as.Date("2020-06-13", "%Y-%m-%d"), linetype = 2, 
             color = 3, size = 1.5)

###############################################################################
## Iquitos
# 2.017: 20-05
# 1.094: 04-05

distritos <- c("IQUITOS", "BELEN", "PUNCHANA", "SAN JUAN BAUTISTA")
poblacion <- c(146853, 64488, 75210, 127005)

temp.p <- full2020[full2020$provincia == "MAYNAS" & full2020$distrito %in% distritos, ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la ciudad de Iquitos por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr')) +
  geom_vline(xintercept = as.Date("2020-05-04", "%Y-%m-%d"), linetype = 2, 
             color = 3, size = 1.5) +
  geom_vline(xintercept = as.Date("2020-05-20", "%Y-%m-%d"), linetype = 2, 
             color = 7, size = 1.5)

###############################################################################
## Pucallpa
# 2.992: 09-07
# 2.029: 14-05
# 1.024: 01-05

distritos <- c("CALLERIA", "YARINACOCHA", "MANANTAY")
poblacion <- c(149999, 103941, 87525)

temp.p <- full2020[full2020$provincia == "CORONEL PORTILLO" & full2020$distrito %in% distritos, ]

# Conteo diario

ds <- docomp("count", "pais", "fecha", dfr = temp.p, method = 'slow')

# Grafico

ggplot(ds, aes(fecha, pais)) +
  geom_point() +
  labs(x = "Fecha",
       y = "Número de fallecidos diarios",
       title = "Fallecidos diarios en la ciudad de Pucallpa por causa no violenta") + 
  ylim(0, NA) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cr')) +
  geom_vline(xintercept = as.Date("2020-05-01", "%Y-%m-%d"), linetype = 2, 
             color = 3, size = 1.5) +
  geom_vline(xintercept = as.Date("2020-05-14", "%Y-%m-%d"), linetype = 2, 
             color = 7, size = 1.5) +
  geom_vline(xintercept = as.Date("2020-07-09", "%Y-%m-%d"), linetype = 2, 
             color = 2, size = 1.5)

###########################################
temp <- full.nv[full.nv$provincia == "SAN ROMAN" & full.nv$distrito %in% distritos, ]
temp <- temp[temp$mes <= 6, ]
temp <- temp[!(temp$mes == 6 & as.numeric(substr(temp$fecha, 9, 10)) > 22), ]
totales <- table(temp$ano)
(totales[4] - (totales[3] + totales[2]) / 2) / sum(poblacion) * 1000
##############################################
temp <- full.nv[full.nv$provincia == "SAN ROMAN", ]
totales <- table(temp$ano)
(totales[4] - (totales[3] + totales[2]) / 2) / 307.417

temp <- full.nv[full.nv$provincia == "TACNA", ]
totales <- table(temp$ano)
(totales[4] - (totales[3] + totales[2]) / 2) / 306.363
