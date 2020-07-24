junta_WP_acu <- function (list_acu, dados_WP) {
  library(stringr)
  library(dplyr)

# União das lng/lat dos WPs em saidas
list_acu$saidas$data_WP_I_saida <- str_c(as.character(list_acu$saidas$data),"-", list_acu$saidas$WP_I_saida)
list_acu$saidas$data_WP_F_saida <- str_c(as.character(list_acu$saidas$data),"-", list_acu$saidas$WP_F_saida)
dados_WP$data_WP_WP <- str_c(as.character(as.Date(dados_WP$datahora_WP)),"-", dados_WP$WP_WP)

list_acu$saidas <- list_acu$saidas %>%
  left_join(dados_WP, by= c("data_WP_I_saida" = "data_WP_WP"))

list_acu$saidas <- list_acu$saidas %>% 
  rename(
    datahora_I = datahora_WP,
    lng_I = lng_WP,
    lat_I = lat_WP,
    WP_I = WP_WP
  )

list_acu$saidas <- list_acu$saidas %>%
  dplyr::select(saida, data, barco, datahora_I, lng_I, lat_I, WP_I, area, equipe, litros_abastecidos, OBS, data_WP_F_saida)

# Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_acu$saidas <- list_acu$saidas %>%
  left_join(dados_WP, by= c("data_WP_F_saida" = "data_WP_WP"))

list_acu$saidas <- list_acu$saidas %>% 
  rename(
    datahora_F = datahora_WP,
    lng_F = lng_WP,
    lat_F = lat_WP,
    WP_F = WP_WP
  )

list_acu$saidas <- list_acu$saidas %>%
  dplyr::select(saida, data, barco, datahora_I, lng_I, lat_I, WP_I, datahora_F, lng_F, lat_F, WP_F, area, equipe, litros_abastecidos, OBS)

# Ajuste de dos tipos de variáveis de saidas
list_acu$saidas$datahora_I <- ymd_hms(list_acu$saidas$datahora_I)
list_acu$saidas$lng_I <- as.numeric(list_acu$saidas$lng_I)
list_acu$saidas$lat_I <- as.numeric(list_acu$saidas$lat_I)
list_acu$saidas$datahora_F <- ymd_hms(list_acu$saidas$datahora_F)
list_acu$saidas$lng_F <- as.numeric(list_acu$saidas$lng_F)
list_acu$saidas$lat_F <- as.numeric(list_acu$saidas$lat_F)

# O mesmo para WPs de clima
list_acu$clima$data_WP_I_clima <- str_c(as.character(list_acu$clima$data),"-", list_acu$clima$WP_I_clima)
list_acu$clima$data_WP_F_clima <- str_c(as.character(list_acu$clima$data),"-", list_acu$clima$WP_F_clima)

list_acu$clima <- list_acu$clima %>%
  left_join(dados_WP, by= c("data_WP_I_clima" = "data_WP_WP"))

list_acu$clima <- list_acu$clima %>%
  rename(
    datahora_I = datahora_WP,
    lng_I = lng_WP,
    lat_I = lat_WP,
    WP_I = WP_WP
  )

list_acu$clima <- list_acu$clima %>%
  dplyr::select(saida, datahora_I, lng_I, lat_I, WP_I, dir_vento, veloc_vento, beaufort, cobert_nuvens, visibilidade, reflexo, coloracao_agua, OBS, data_WP_F_clima)

# Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_acu$clima <- list_acu$clima %>%
  left_join(dados_WP, by= c("data_WP_F_clima" = "data_WP_WP"))

list_acu$clima <- list_acu$clima %>% 
  rename(
    datahora_F = datahora_WP,
    lng_F = lng_WP,
    lat_F = lat_WP,
    WP_F = WP_WP
  )

list_acu$clima <- list_acu$clima %>%
  dplyr::select(saida, datahora_I, lng_I, lat_I, WP_I, datahora_F, lng_F, lat_F, WP_F, dir_vento, veloc_vento, beaufort, cobert_nuvens, visibilidade, reflexo, coloracao_agua, OBS)

# Ajuste de dos tipos de variáveis de clima
list_acu$clima$datahora_I <- ymd_hms(list_acu$clima$datahora_I)
list_acu$clima$lng_I <- as.numeric(list_acu$clima$lng_I)
list_acu$clima$lat_I <- as.numeric(list_acu$clima$lat_I)
list_acu$clima$datahora_F <- ymd_hms(list_acu$clima$datahora_F)
list_acu$clima$lng_F <- as.numeric(list_acu$clima$lng_F)
list_acu$clima$lat_F <- as.numeric(list_acu$clima$lat_F)

# O mesmo para gravacoes
list_acu$gravacoes$data_WP_I <- str_c(as.character(list_acu$gravacoes$data),"-", list_acu$gravacoes$WP_I_grupo)
list_acu$gravacoes$data_WP_F <- str_c(as.character(list_acu$gravacoes$data),"-", list_acu$gravacoes$WP_F_grupo)

list_acu$gravacoes <- list_acu$gravacoes %>%
  left_join(dados_WP, by= c("data_WP_I" = "data_WP_WP"))

list_acu$gravacoes <- list_acu$gravacoes %>%
  rename(
    datahora_I = datahora_WP,
    lng_I = lng_WP,
    lat_I = lat_WP,
    WP_I = WP_WP
  )

list_acu$gravacoes <- list_acu$gravacoes %>%
  dplyr::select(saida, data, grupo, datahora_I, lng_I, lat_I, WP_I, arquivo, profund_m, estado, coesao, tam_grupo, tam_min, tam_max, n_neonatos, n_infantes, n_juvenis, n_adultos, quant_embarc, OBS, data_WP_F)

# Está funcionando, mas ficou confuso. Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_acu$gravacoes <- list_acu$gravacoes %>%
  left_join(dados_WP, by= c("data_WP_F" = "data_WP_WP"))

list_acu$gravacoes <- list_acu$gravacoes %>% 
  rename(
    datahora_F = datahora_WP,
    lng_F = lng_WP,
    lat_F = lat_WP,
    WP_F = WP_WP
  )

list_acu$gravacoes <- list_acu$gravacoes %>%
  dplyr::select(saida, data, grupo, datahora_I, lng_I, lat_I, WP_I, datahora_F, lng_F, lat_F, WP_F, arquivo, profund_m, estado, coesao, tam_grupo, tam_min, tam_max, n_neonatos, n_infantes, n_juvenis, n_adultos, quant_embarc, OBS)

# Ajuste de dos tipos de variáveis de gravacoes
list_acu$gravacoes$datahora_I <- ymd_hms(list_acu$gravacoes$datahora_I)
list_acu$gravacoes$lng_I <- as.numeric(list_acu$gravacoes$lng_I)
list_acu$gravacoes$lat_I <- as.numeric(list_acu$gravacoes$lat_I)
list_acu$gravacoes$datahora_F <- ymd_hms(list_acu$gravacoes$datahora_F)
list_acu$gravacoes$lng_F <- as.numeric(list_acu$gravacoes$lng_F)
list_acu$gravacoes$lat_F <- as.numeric(list_acu$gravacoes$lat_F)

return(list_acu)
}