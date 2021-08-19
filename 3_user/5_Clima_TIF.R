# Criação de um arquivo *.tif para cada um dos parâmetros do clima
# 6 rasters por dia de campo (vel_vento, beaufort, cobertura de nuvens, visibilidade, reflexo, velocidade do barco)


library(purrr)
library(dplyr)
library(forcats)
library(tidyr)
library(raster)
library(sf)
library(stringr)
library(lubridate)

# Linha 1 ----


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo arquivo RDS
bd_L1 <- readRDS(paste0(pasta_proj,"/results/1_RDS/bd_L1.rds"))

# Definindo dados climáticos com coordenadas para os pontos
clima <- bd_L1$clima %>% group_by(saida) %>% right_join(bd_L1$rotas, by = "saida")

# Criando um intervalo das observações de clima
clima$int_clima <- interval(clima$datahora_I, clima$datahora_F)

clima <- clima[ymd_hms(clima$datahora_ROTA) %within% clima$int_clima,]

clima <- clima %>% 
  mutate(vel_barco = 1000*dist_p_prox/period_to_seconds(as.period(tempo_p_prox))) %>%
  dplyr::select(1,2,19,11:16,27,20,21) %>%
  nest


# Abrindo shape da agua
agua_sf <- st_read("data/SHAPE/Agua_L1.shp")

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_proj <- st_transform(agua_sf, 32723)

# criando raster da agua transformada para projetada
agua_ras <- raster(agua_proj)

# definindo resolução de 250m
res(agua_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_ras)



# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(clima)) {
  
  # Pegar dados daquela saida  
  data <- clima[[2]][[i]]
  
  # Criar um objeto sf
  clima_sf <- st_as_sf(x = data, coords = c("lng", "lat"))
  
  # Definir o crs 4326 - geográfica/WGS 84 - padrão GPS
  st_crs(clima_sf) <- 4326
  
  # Transformapar para EPSG 32723 - projetadas/WGS 84/UTM 23S
  clima_proj <- st_transform(clima_sf, 32723)
  
  # Fazer um buffer de 250m em torno dos pontos da rota para pegar mais quadrantes
  clima_buff <- st_buffer(clima_proj, 250)
  
  # Fazer um raster para cada parâmetro de clima e depois nomeá-lo corretamente
  ras_vento <- rasterize(clima_buff, agua_ras, field = "veloc_vento", fun = "first")
  names(ras_vento) <- "veloc_vento"
  
  ras_beauf <- rasterize(clima_buff, agua_ras, field = "beaufort", fun = "first")
  names(ras_beauf) <- "beaufort"
  
  ras_nuven <- rasterize(clima_buff, agua_ras, field = "cobert_nuvens", fun = "first")
  names(ras_nuven) <- "cobert_nuvens"
  
  ras_visib <- rasterize(clima_buff, agua_ras, field = "visibilidade", fun = "first")
  names(ras_visib) <- "visibilidade"
  
  ras_refle <- rasterize(clima_buff, agua_ras, field = "reflexo", fun = "first")
  names(ras_refle) <- "reflexo"
  
  ras_vbarc <- rasterize(clima_buff, agua_ras, field = "vel_barco", fun = "mean")
  names(ras_vbarc) <- "vel_barco"
  
  # Criar um raster único empilhando todos 
  clima_ras <- raster::stack(ras_vento, ras_beauf, ras_nuven, ras_visib, ras_refle, ras_vbarc)
  
  # Definir o nome da pasta nova com o padrão de sempre
  novo_dir <- paste0(pasta_proj,
                     "/results/4_CLIMA_TIF/LINHA_1/",
                     str_pad(nest(clima)[[1]][[i]],width = 3, side = "left", pad = "0"),
                     "_",
                     str_sub(str_replace_all(data$datahora_ROTA[[1]], "-", "_"), 1, 10))
  
  # Criar a pasta propriamente dito
  dir.create(novo_dir, recursive = TRUE)
  
  # Nessa pasta, criar um arquivo tif, para cada camada, com um padrão de nome por arquivo.
  writeRaster(x = clima_ras,
              filename = paste0(novo_dir,"/", str_sub(novo_dir,-14,-1),".tif"),
              overwrite = TRUE,
              bylayer = TRUE,
              suffix = "names")
}



# Linha 2 ----


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo arquivo RDS
bd_L2 <- readRDS(paste0(pasta_proj,"/results/1_RDS/bd_L2.rds"))

# Definindo dados climáticos com coordenadas para os pontos
clima <- bd_L2$clima %>% group_by(saida) %>% right_join(bd_L2$rotas, by = "saida")

# Criando um intervalo das observações de clima
clima$int_clima <- interval(clima$datahora_I, clima$datahora_F)

clima <- clima[ymd_hms(clima$datahora_ROTA) %within% clima$int_clima,]

clima <- clima %>% 
  mutate(vel_barco = 1000*dist_p_prox/period_to_seconds(as.period(tempo_p_prox))) %>%
  dplyr::select(1,2,19,11:16,27,20,21) %>%
  nest


# Abrindo shape da agua
agua_sf <- st_read("data/SHAPE/Agua_L2.shp")

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_proj <- st_transform(agua_sf, 32723)

# criando raster da agua transformada para projetada
agua_ras <- raster(agua_proj)

# definindo resolução de 250m
res(agua_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_ras)



# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(clima)) {
  
  # Pegar dados daquela saida  
  data <- clima[[2]][[i]]
  
  # Criar um objeto sf
  clima_sf <- st_as_sf(x = data, coords = c("lng", "lat"))
  
  # Definir o crs 4326 - geográfica/WGS 84 - padrão GPS
  st_crs(clima_sf) <- 4326
  
  # Transformapar para EPSG 32723 - projetadas/WGS 84/UTM 23S
  clima_proj <- st_transform(clima_sf, 32723)
  
  # Fazer um buffer de 250m em torno dos pontos da rota para pegar mais quadrantes
  clima_buff <- st_buffer(clima_proj, 250)
  
  # Fazer um raster para cada parâmetro de clima e depois nomeá-lo corretamente
  ras_vento <- rasterize(clima_buff, agua_ras, field = "veloc_vento", fun = "first")
  names(ras_vento) <- "veloc_vento"
  
  ras_beauf <- rasterize(clima_buff, agua_ras, field = "beaufort", fun = "first")
  names(ras_beauf) <- "beaufort"
  
  ras_nuven <- rasterize(clima_buff, agua_ras, field = "cobert_nuvens", fun = "first")
  names(ras_nuven) <- "cobert_nuvens"
  
  ras_visib <- rasterize(clima_buff, agua_ras, field = "visibilidade", fun = "first")
  names(ras_visib) <- "visibilidade"
  
  ras_refle <- rasterize(clima_buff, agua_ras, field = "reflexo", fun = "first")
  names(ras_refle) <- "reflexo"
  
  ras_vbarc <- rasterize(clima_buff, agua_ras, field = "vel_barco", fun = "mean")
  names(ras_vbarc) <- "vel_barco"
  
  # Criar um raster único empilhando todos 
  clima_ras <- raster::stack(ras_vento, ras_beauf, ras_nuven, ras_visib, ras_refle, ras_vbarc)
  
  # Definir o nome da pasta nova com o padrão de sempre
  novo_dir <- paste0(pasta_proj,
                     "/results/4_CLIMA_TIF/LINHA_2/",
                     str_pad(nest(clima)[[1]][[i]],width = 3, side = "left", pad = "0"),
                     "_",
                     str_sub(str_replace_all(data$datahora_ROTA[[1]], "-", "_"), 1, 10))
  
  # Criar a pasta propriamente dito
  dir.create(novo_dir, recursive = TRUE)
  
  # Nessa pasta, criar um arquivo tif, para cada camada, com um padrão de nome por arquivo.
  writeRaster(x = clima_ras,
              filename = paste0(novo_dir,"/", str_sub(novo_dir,-14,-1),".tif"),
              overwrite = TRUE,
              bylayer = TRUE,
              suffix = "names")
}


# E pronto! veja results/4_CLIMA_TIF/...


