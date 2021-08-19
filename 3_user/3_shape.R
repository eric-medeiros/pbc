### Criando um SHP das avistagens com dados das avistagens e sonda como tabela de atributo

library(magrittr)
library(dplyr)
library(purrr)
library(sf)
library(tibble)

# Linha_1 ----

pasta_proj <- rprojroot::find_rstudio_root_file()

bd_L1 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L1.rds"))

# Pegando os dados para shapefile
dados <- bd_L1$avistagens %>%
  mutate(tam_grupo = nafill(tam_grupo, fill = 0L),
         tam_min = nafill(tam_min, fill = 0L),
         tam_max = nafill(tam_max, fill = 0L)) %>%
  rowwise() %>%
  mutate(tam_est = round(sum(tam_grupo, mean(c(tam_min, tam_max))),0),
         int_avis = interval(datahora_I, datahora_F)) %>%
  ungroup() %>%
  dplyr::select(1,2,7,8,13,14,25,20:23,26) %>%
  rowid_to_column("n_grupo")

# Craindo uma lista vazia pra receber dados a seguir  
sonda <- list()

# Selecionando os dados da sonda do intervalo
for (i in 1:nrow(dados)) {
  sonda[[i]] <-  bd_L1$sonda[ymd_hms(bd_L1$sonda$datahora_SONDA) %within% dados$int_avis[[i]],]
}

# Juntando todos os dados
dados <- bind_rows(sonda, .id = "n_grupo") %>%
  mutate(n_grupo = as.numeric(n_grupo)) %>%
  group_by(n_grupo) %>%
  summarise(Temp_m = mean(Temp, na.rm = TRUE),
            Sal_m = mean(Sal, na.rm = TRUE),
            OD_m = mean(OD, na.rm = TRUE),
            Turb_m = mean(Turb, na.rm = TRUE),
            pH_m = mean(pH, na.rm = TRUE),
            Pres_m = mean(Pres, na.rm = TRUE),
            num_pts_s = n()) %>%
  right_join(dados, by = "n_grupo") %>%
  arrange(as.integer(saida)) %>%
  group_by(saida) %>%
  dplyr::select(9:19,2:8)

# Criando um objeto sf
grupos_sf <- st_as_sf(x = dados,
                      coords = c("lng_I", "lat_I"))

# Atribuindo o EPSG 4326 - geográficas/WGS84/padrão GPS
st_crs(grupos_sf) <- 4326


# Salvando o objeto sf como um Shapefile para fazer o mapa no QGIS
st_write(obj = grupos_sf,
         dsn = "4_export/3_shape",
         layer = "L1_grupos",
         driver = "ESRI Shapefile",
         append = FALSE)


# Linha_2 ----

pasta_proj <- rprojroot::find_rstudio_root_file()

bd_L2 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L2.rds"))

dados <- bd_L2$comportamento %>%
  mutate(vg = varios_grupos+98) %>%
  group_by(saida, grupo) %>%
  summarise(.groups = "keep",
            n_grupos = sum(dois_grupos, vg, 1, na.rm = TRUE),
            na_s = sum(na, na.rm = TRUE),
            cre_s = sum(cre, na.rm = TRUE),
            esc_s = sum(esc, na.rm = TRUE),
            cond_s = sum(cond, na.rm = TRUE),
            rvz_s = sum(rvz, na.rm = TRUE),
            int_s = sum(int, na.rm = TRUE),
            bar_s = sum(bar, na.rm = TRUE),
            mud_dir = sum(mud_dir, na.rm = TRUE),
            mud_coe = sum(mud_coe_af, na.rm = TRUE),
            mud_tam = sum(mud_tam_gru, na.rm = TRUE),
            mud_cmp = sum(mud_comp_gru, na.rm = TRUE)) %>%
  right_join(bd_L2$avistagens, by = c("saida", "grupo")) %>%
  ungroup() %>%
  dplyr::select(1,2,15,17,19,23,26:40,20,21) %>%
  mutate(tam_grupo = nafill(tam_grupo, fill = 0L),
         tam_min = nafill(tam_min, fill = 0L),
         tam_max = nafill(tam_max, fill = 0L)) %>%
  rename(embarc = agrupamento_embarcacao) %>%
  rowwise() %>%
  mutate(tam_est = round(sum(tam_grupo, mean(c(tam_min, tam_max))),0),
         int_avis = interval(datahora_I, datahora_F)) %>%
  ungroup() %>%
  dplyr::select(1:4,7:14,18:21,24,25,22,23) %>%
  rowid_to_column("n_grupo")

# Craindo uma lista vazia pra receber dados a seguir
sonda <- list()

# Selecionando os dados da sonda do intervalo
for (i in 1:nrow(dados)) {
  sonda[[i]] <-  bd_L2$sonda[ymd_hms(bd_L2$sonda$datahora_SONDA) %within% dados$int_avis[[i]],]
}

# Juntando todos os dados
dados <- bind_rows(sonda, .id = "n_grupo") %>%
  mutate(n_grupo = as.numeric(n_grupo)) %>%
  group_by(n_grupo) %>%
  summarise(Temp_m = mean(Temp, na.rm = TRUE),
            Sal_m = mean(Sal, na.rm = TRUE),
            OD_m = mean(OD, na.rm = TRUE),
            Turb_m = mean(Turb, na.rm = TRUE),
            pH_m = mean(pH, na.rm = TRUE),
            Pres_m = mean(Pres, na.rm = TRUE),
            num_pts_s = n()) %>%
  right_join(dados, by = "n_grupo") %>%
  arrange(as.integer(saida)) %>%
  group_by(saida) %>%
  dplyr::select(9:25,2:8,27,28)

# Criando um objeto sf para agrupamentos
agrup_sf <- st_as_sf(x = dados,
                     coords = c("lng_I", "lat_I"))

# Atribuindo o EPSG 4326 - geográficas/WGS84/padrão GPS para agrupamentos
st_crs(agrup_sf) <- 4326

# Salvando o objeto sf como um Shapefile para fazer o mapa no QGIS para agrupamentos
st_write(obj = agrup_sf,
         dsn = "4_export/3_shape",
         layer = "L2_agrup",
         driver = "ESRI Shapefile",
         append = FALSE)


# Linha_3 ----

pasta_proj <- rprojroot::find_rstudio_root_file()

bd_L3 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L3.rds"))

dados <- bd_L3$assobios %>%
  group_by(saida, arquivo_wav) %>%
  summarise(.groups = "keep",
            LF_m = mean(LF, na.rm = TRUE),
            HF_m = mean(HF, na.rm = TRUE),
            MF_m = mean(MF, na.rm = TRUE),
            CF_m = mean(CF, na.rm = TRUE),
            APD_m = mean(APD, na.rm = TRUE),
            DT_m = mean(DT, na.rm = TRUE),
            DF_m = mean(DF, na.rm = TRUE),
            FI_m = mean(FI, na.rm = TRUE),
            FF_m = mean(FF, na.rm = TRUE),
            num_ass = n()) %>%
  right_join(bd_L3$gravacoes, by = c("saida", "arquivo_wav")) %>%
  dplyr::select(1,15,14,2,12,16,3:11) %>%
  group_by(saida, estacao) %>%
  right_join(bd_L3$estacoes, by = c("saida", "estacao")) %>%
  rowwise() %>%
  mutate(int_est = interval(datahora_I, datahora_F)) %>%
  ungroup() %>%
  dplyr::select(1:3,16,4:15,18:20,26) %>%
  rowid_to_column("n_grupo")

# Craindo uma lista vazia pra receber dados a seguir
sonda <- list()

# Selecionando os dados da sonda do intervalo
for (i in 1:nrow(dados)) {
  sonda[[i]] <-  bd_L3$sonda[ymd_hms(bd_L3$sonda$datahora_SONDA) %within% dados$int_est[[i]],]
}

# Juntando todos os dados
dados <- bind_rows(sonda, .id = "n_grupo") %>%
  mutate(n_grupo = as.numeric(n_grupo)) %>%
  group_by(n_grupo) %>%
  summarise(Temp_m = mean(Temp, na.rm = TRUE),
            Sal_m = mean(Sal, na.rm = TRUE),
            OD_m = mean(OD, na.rm = TRUE),
            Turb_m = mean(Turb, na.rm = TRUE),
            pH_m = mean(pH, na.rm = TRUE),
            Pres_m = mean(Pres, na.rm = TRUE),
            num_pts_s = n()) %>%
  right_join(dados, by = "n_grupo") %>%
  arrange(as.integer(saida)) %>%
  group_by(saida) %>%
  dplyr::select(9:24,2:8,26,27)

dados <- dados[!is.na(dados[["lat_I"]]),]


# Criando um objeto sf
estacoes_sf <- st_as_sf(x = dados,
                        coords = c("lng_I", "lat_I"))

# Atribuindo o EPSG 4326 - geográficas/WGS84/padrão GPS
st_crs(estacoes_sf) <- 4326


# Salvando o objeto sf como um Shapefile para fazer o mapa no QGIS
st_write(obj = estacoes_sf,
         dsn = "4_export/3_shape",
         layer = "L3_estacoes",
         driver = "ESRI Shapefile",
         append = FALSE)

