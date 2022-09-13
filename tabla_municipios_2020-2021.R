library(tidyverse)
library(lubridate)

epi2020 <- read.csv('data/epi_2020.txt', sep = '|', fileEncoding = 'latin1') %>%
  filter(ENTRESI == 'SONORA', CLASCOVID19 %in% c('CONF ASO', 'CONF ANT', 'CONF DIC', 'CONF LAB'))
epi2021 <- read.csv('data/epi_2021.txt', sep = '|', fileEncoding = 'latin1') %>%
  filter(ENTRESI == 'SONORA', CLASCOVID19 %in% c('CONF ASO', 'CONF ANT', 'CONF DIC', 'CONF LAB'))

epi <- rbind(epi2020, epi2021)

casost <- epi %>%
  mutate(municipio = str_to_title(MPIORESI, locale = 'es')) %>%
  mutate(municipio = case_when(
    FOLIO.SINAVE == 2621176381 ~ 'Nogales',
    FOLIO.SINAVE == 262756716 ~ 'San Luis Rio Colorado',
    TRUE ~ municipio
  )) %>%
  count(municipio) %>%
  rename(Municipio = municipio, 'Casos totales' = n)

casosd <- epi %>%
  filter(EVOLUCI == 'DEFUNCION') %>%
  mutate(municipio = str_to_title(MPIORESI, locale = 'es')) %>%
  mutate(municipio = case_when(
    FOLIO.SINAVE == 2621176381 ~ 'Nogales',
    FOLIO.SINAVE == 262756716 ~ 'San Luis Rio Colorado',
    TRUE ~ municipio
  )) %>%
  count(municipio) %>%
  rename(Municipio = municipio, 'Casos fatales' = n)

new <- full_join(casost, casosd)
new[is.na(new)] = 0

new <- new %>%
  mutate('Casos no fatales' = `Casos totales` - `Casos fatales`) %>%
  select(Municipio, `Casos no fatales`, `Casos fatales`, `Casos totales`)

saveRDS(new, 'tabla_municipios_2020-2021.rds')

#falta <- epi %>%
#  filter(MPIORESI == '')
#2621176381 ~ 'Nogales'
#262756716 ~ 'San Luis Rio Colorado'