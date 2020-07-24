# União das lng/lat dos WPs em saidas
junta_WP_com <- function (list_com, dados_WP) {
  library(stringr)
  library(dplyr)

list_com$saidas$data_WP_I_saida <- str_c(as.character(list_com$saidas$data),"-", list_com$saidas$WP_I_saida)
list_com$saidas$data_WP_F_saida <- str_c(as.character(list_com$saidas$data),"-", list_com$saidas$WP_F_saida)
dados_WP$data_WP_WP <- str_c(as.character(as.Date(dados_WP$datahora_WP)),"-", dados_WP$WP_WP)

list_com$saidas <- list_com$saidas %>%
  left_join(dados_WP, by= c("data_WP_I_saida" = "data_WP_WP"))

list_com$saidas <- list_com$saidas %>% 
  rename(
    datahora_I = datahora_WP,
    lng_I = lng_WP,
    lat_I = lat_WP,
    WP_I = WP_WP,
  )

list_com$saidas <- list_com$saidas %>%
  dplyr::select(saida, data, barco, datahora_I, lng_I, lat_I, WP_I, rota, equipe, litros_abastecidos, data_WP_F_saida)

# Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_com$saidas <- list_com$saidas %>%
  left_join(dados_WP, by= c("data_WP_F_saida" = "data_WP_WP"))

list_com$saidas <- list_com$saidas %>% 
  rename(
    datahora_F = datahora_WP,
    lng_F = lng_WP,
    lat_F = lat_WP,
    WP_F = WP_WP
  )

list_com$saidas <- list_com$saidas %>%
  dplyr::select(saida, data, barco, datahora_I, lng_I, lat_I, WP_I, datahora_F, lng_F, lat_F, WP_F, rota, equipe, litros_abastecidos)

# Ajuste de dos tipos de variáveis de saidas
list_com$saidas$datahora_I <- ymd_hms(list_com$saidas$datahora_I)
list_com$saidas$lng_I <- as.numeric(list_com$saidas$lng_I)
list_com$saidas$lat_I <- as.numeric(list_com$saidas$lat_I)
list_com$saidas$datahora_F <- ymd_hms(list_com$saidas$datahora_F)
list_com$saidas$lng_F <- as.numeric(list_com$saidas$lng_F)
list_com$saidas$lat_F <- as.numeric(list_com$saidas$lat_F)

# O mesmo para WPs de clima
list_com$clima$data_WP_I_clima <- str_c(as.character(list_com$clima$data),"-", list_com$clima$WP_I_clima)
list_com$clima$data_WP_F_clima <- str_c(as.character(list_com$clima$data),"-", list_com$clima$WP_F_clima)

list_com$clima <- list_com$clima %>%
  left_join(dados_WP, by= c("data_WP_I_clima" = "data_WP_WP"))

list_com$clima <- list_com$clima %>%
  rename(
    datahora_I = datahora_WP,
    lng_I = lng_WP,
    lat_I = lat_WP,
    WP_I = WP_WP
  )

list_com$clima <- list_com$clima %>%
  dplyr::select(saida, datahora_I, lng_I, lat_I, WP_I, dir_vento, veloc_vento, beaufort, cobert_nuvens, visibilidade, reflexo, data_WP_F_clima)

# Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_com$clima <- list_com$clima %>%
  left_join(dados_WP, by= c("data_WP_F_clima" = "data_WP_WP"))

list_com$clima <- list_com$clima %>% 
  rename(
    datahora_F = datahora_WP,
    lng_F = lng_WP,
    lat_F = lat_WP,
    WP_F = WP_WP
  )

list_com$clima <- list_com$clima %>%
  dplyr::select(saida, datahora_I, lng_I, lat_I, WP_I, datahora_F, lng_F, lat_F, WP_F, dir_vento, veloc_vento, beaufort, cobert_nuvens, visibilidade, reflexo)

# Ajuste de dos tipos de variáveis de clima
list_com$clima$datahora_I <- ymd_hms(list_com$clima$datahora_I)
list_com$clima$lng_I <- as.numeric(list_com$clima$lng_I)
list_com$clima$lat_I <- as.numeric(list_com$clima$lat_I)
list_com$clima$datahora_F <- ymd_hms(list_com$clima$datahora_F)
list_com$clima$lng_F <- as.numeric(list_com$clima$lng_F)
list_com$clima$lat_F <- as.numeric(list_com$clima$lat_F)

# O mesmo para avistagens
list_com$avistagens$data_WP_I <- str_c(as.character(list_com$avistagens$data),"-", list_com$avistagens$WP_I)
list_com$avistagens$data_WP_F <- str_c(as.character(list_com$avistagens$data),"-", list_com$avistagens$WP_F)

list_com$avistagens <- list_com$avistagens %>%
  left_join(dados_WP, by= c("data_WP_I" = "data_WP_WP"))

list_com$avistagens <- list_com$avistagens %>%
  rename(
    datahora_I = datahora_WP,
    lng_I = lng_WP,
    lat_I = lat_WP
  )

list_com$avistagens <- list_com$avistagens %>%
  dplyr::select(saida, data, grupo, agrupamento, embarcacao, agrupamento_embarcacao, datahora_I, lng_I, lat_I, WP_I, WP_F, motor, tipo, tempo, distancia, velocidade, fotoID, estado, coesao, tam_grupo, tam_min, tam_max, n_neonatos, n_infantes, n_juvenis, n_adultos, OBS, data_WP_F)

# Está funcionando, mas ficou confuso. Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_com$avistagens <- list_com$avistagens %>%
  left_join(dados_WP, by= c("data_WP_F" = "data_WP_WP"))

list_com$avistagens <- list_com$avistagens %>% 
  rename(
    datahora_F = datahora_WP,
    lng_F = lng_WP,
    lat_F = lat_WP
  )

list_com$avistagens <- list_com$avistagens %>%
  dplyr::select(saida, data, grupo, agrupamento, embarcacao, agrupamento_embarcacao, datahora_I, lng_I, lat_I, WP_I, datahora_F, lng_F, lat_F, WP_F, motor, tipo, tempo, distancia, velocidade, fotoID, estado, coesao, tam_grupo, tam_min, tam_max, n_neonatos, n_infantes, n_juvenis, n_adultos, OBS)

# Ajuste de dos tipos de variáveis de avistagens
list_com$avistagens$datahora_I <- ymd_hms(list_com$avistagens$datahora_I)
list_com$avistagens$lng_I <- as.numeric(list_com$avistagens$lng_I)
list_com$avistagens$lat_I <- as.numeric(list_com$avistagens$lat_I)
list_com$avistagens$datahora_F <- ymd_hms(list_com$avistagens$datahora_F)
list_com$avistagens$lng_F <- as.numeric(list_com$avistagens$lng_F)
list_com$avistagens$lat_F <- as.numeric(list_com$avistagens$lat_F)

return(list_com)
}
