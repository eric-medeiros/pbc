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
  dplyr::select(saida,
                data,
                barco,
                datahora_I,
                lng_I,
                lat_I,
                WP_I,
                periodo,
                equipe,
                litros_abastecidos,
                OBS,
                data_WP_F_saida)

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
  dplyr::select(saida,
                data,
                barco,
                datahora_I,
                lng_I,
                lat_I,
                WP_I,
                datahora_F,
                lng_F,
                lat_F,
                WP_F,
                periodo,
                equipe,
                litros_abastecidos,
                OBS)

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
  dplyr::select(saida,
                datahora_I,
                lng_I,
                lat_I,
                WP_I,
                dir_vento,
                veloc_vento,
                beaufort,
                cobert_nuvens,
                visibilidade,
                reflexo,
                coloracao_agua,
                OBS,
                data_WP_F_clima)

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
  dplyr::select(saida, 
                datahora_I, 
                lng_I, 
                lat_I, 
                WP_I, 
                datahora_F, 
                lng_F, 
                lat_F, 
                WP_F, 
                dir_vento, 
                veloc_vento, 
                beaufort, 
                cobert_nuvens,
                visibilidade,
                reflexo,
                coloracao_agua,
                OBS)

# Ajuste de dos tipos de variáveis de clima
list_acu$clima$datahora_I <- ymd_hms(list_acu$clima$datahora_I)
list_acu$clima$lng_I <- as.numeric(list_acu$clima$lng_I)
list_acu$clima$lat_I <- as.numeric(list_acu$clima$lat_I)
list_acu$clima$datahora_F <- ymd_hms(list_acu$clima$datahora_F)
list_acu$clima$lng_F <- as.numeric(list_acu$clima$lng_F)
list_acu$clima$lat_F <- as.numeric(list_acu$clima$lat_F)

# O mesmo para WPs de estacoes
list_acu$estacoes$data_WP_I_estacao <- str_c(as.character(list_acu$estacoes$data),"-", list_acu$estacoes$WP_I_estacao)
list_acu$estacoes$data_WP_F_estacao <- str_c(as.character(list_acu$estacoes$data),"-", list_acu$estacoes$WP_F_estacao)

list_acu$estacoes <- list_acu$estacoes %>%
  left_join(dados_WP, by= c("data_WP_I_estacao" = "data_WP_WP"))

list_acu$estacoes <- list_acu$estacoes %>%
  rename(
    datahora_I = datahora_WP,
    lng_I = lng_WP,
    lat_I = lat_WP,
    WP_I = WP_WP
  )

list_acu$estacoes <- list_acu$estacoes %>%
  dplyr::select(saida,
                estacao,
                datahora_I,
                lng_I,
                lat_I,
                WP_I,
                OBS,
                data_WP_F_estacao)

# Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_acu$estacoes <- list_acu$estacoes %>%
  left_join(dados_WP, by= c("data_WP_F_estacao" = "data_WP_WP"))

list_acu$estacoes <- list_acu$estacoes %>% 
  rename(
    datahora_F = datahora_WP,
    lng_F = lng_WP,
    lat_F = lat_WP,
    WP_F = WP_WP
  )

list_acu$estacoes <- list_acu$estacoes %>%
  dplyr::select(saida,
                estacao,
                datahora_I,
                lng_I,
                lat_I,
                WP_I,
                datahora_F,
                lng_F,
                lat_F,
                OBS)

# Ajuste de dos tipos de variáveis de estacoes
list_acu$estacoes$datahora_I <- ymd_hms(list_acu$estacoes$datahora_I)
list_acu$estacoes$lng_I <- as.numeric(list_acu$estacoes$lng_I)
list_acu$estacoes$lat_I <- as.numeric(list_acu$estacoes$lat_I)
list_acu$estacoes$datahora_F <- ymd_hms(list_acu$estacoes$datahora_F)
list_acu$estacoes$lng_F <- as.numeric(list_acu$estacoes$lng_F)
list_acu$estacoes$lat_F <- as.numeric(list_acu$estacoes$lat_F)

# O mesmo para gravacoes
list_acu$gravacoes$data_WP_arq <- str_c(as.character(list_acu$gravacoes$data),"-", list_acu$gravacoes$WP_arq)

list_acu$gravacoes <- list_acu$gravacoes %>%
  left_join(dados_WP, by= c("data_WP_arq" = "data_WP_WP"))

list_acu$gravacoes <- list_acu$gravacoes %>%
  rename(
    datahora = datahora_WP,
    lng = lng_WP,
    lat = lat_WP,
    WP = WP_WP
  )

list_acu$gravacoes <- list_acu$gravacoes %>%
  dplyr::select(saida,
                data,
                area,
                estacao,
                arq,
                datahora,
                lng,
                lat,
                WP,
                arq_wav,
                profund_m,
                OBS)

# Ajuste de dos tipos de variáveis de gravacoes
list_acu$gravacoes$datahora <- ymd_hms(list_acu$gravacoes$datahora)
list_acu$gravacoes$lng <- as.numeric(list_acu$gravacoes$lng)
list_acu$gravacoes$lat <- as.numeric(list_acu$gravacoes$lat)

# O mesmo para WPs de grupos
list_acu$grupos$data_WP_I_G <- str_c(as.character(list_acu$grupos$data),"-", list_acu$grupos$WP_I_G)
list_acu$grupos$data_WP_F_G <- str_c(as.character(list_acu$grupos$data),"-", list_acu$grupos$WP_F_G)

list_acu$grupos <- list_acu$grupos %>%
  left_join(dados_WP, by= c("data_WP_I_G" = "data_WP_WP"))

list_acu$grupos <- list_acu$grupos %>%
  rename(datahora_I = datahora_WP,
         lng_I = lng_WP,
         lat_I = lat_WP,
         WP_I = WP_WP)

list_acu$grupos <- list_acu$grupos %>%
  dplyr::select(saida, 
                data, 
                estacao, 
                grupo, 
                arq_wav, 
                datahora_I, 
                lng_I, 
                lat_I, 
                WP_I, 
                estado, 
                coesao, 
                tam_grupo, 
                tam_min, 
                tam_max, 
                n_neonatos, 
                n_infantes, 
                n_juvenis, 
                n_adultos, 
                OBS, 
                data_WP_F_G)

# Acima foi trocado os WPs iniciais e abaixo os WPs finais.

list_acu$grupos <- list_acu$grupos %>%
  left_join(dados_WP, by= c("data_WP_F_G" = "data_WP_WP"))

list_acu$grupos <- list_acu$grupos %>% 
  rename(datahora_F = datahora_WP,
         lng_F = lng_WP,
         lat_F = lat_WP,
         WP_F = WP_WP)

list_acu$grupos <- list_acu$grupos %>%
  dplyr::select(saida,
                data,
                estacao,
                grupo,
                arq_wav,
                datahora_I,
                lng_I,
                lat_I,
                WP_I,
                datahora_F,
                lng_F,
                lat_F,
                estado,
                coesao,
                tam_grupo,
                tam_min,
                tam_max,
                n_neonatos,
                n_infantes,
                n_juvenis,
                n_adultos,
                OBS)
   
# Ajuste de dos tipos de variáveis de grupos
list_acu$grupos$datahora_I <- ymd_hms(list_acu$grupos$datahora_I)
list_acu$grupos$lng_I <- as.numeric(list_acu$grupos$lng_I)
list_acu$grupos$lat_I <- as.numeric(list_acu$grupos$lat_I)
list_acu$grupos$datahora_F <- ymd_hms(list_acu$grupos$datahora_F)
list_acu$grupos$lng_F <- as.numeric(list_acu$grupos$lng_F)
list_acu$grupos$lat_F <- as.numeric(list_acu$grupos$lat_F)

# O mesmo para WPs de embarcacoes
list_acu$embarcacoes$data_WP_emb <- str_c(as.character(list_acu$embarcacoes$data),"-", list_acu$embarcacoes$WP_emb)

list_acu$embarcacoes <- list_acu$embarcacoes %>%
  left_join(dados_WP, by= c("data_WP_emb" = "data_WP_WP"))

list_acu$embarcacoes <- list_acu$embarcacoes %>%
  rename(datahora = datahora_WP,
         lng = lng_WP,
         lat = lat_WP,
         WP = WP_WP)

list_acu$embarcacoes <- list_acu$embarcacoes %>%
  dplyr::select(saida,
                data,
                estacao,
                arq,
                emb,
                datahora,
                lng,
                lat,
                WP,
                ID_emb,
                tipo_emb,
                finalid_emb,
                quant_emb,
                potenc_hp_motor,
                tipo_motor,
                registro_na_gravacao,
                distancia_m,
                OBS)

# Ajuste de dos tipos de variáveis de embarcacoes
list_acu$embarcacoes$datahora <- ymd_hms(list_acu$embarcacoes$datahora)
list_acu$embarcacoes$lng <- as.numeric(list_acu$embarcacoes$lng)
list_acu$embarcacoes$lat <- as.numeric(list_acu$embarcacoes$lat)

invisible(list_acu)

return(list_acu)
}
