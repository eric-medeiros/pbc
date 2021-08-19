# Criação de um arquivo *.tif para cada um dos parâmetros da sonda
# 6 rasters por dia de campo (temp, sal, od, ph, turb, press)

# Linha 1 ----

library(sf)
library(spatstat)
library(raster)
library(dplyr)
library(stringr)
library(tidyr)

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo arquivo RDS
bd_L1 <- readRDS(paste0(pasta_proj,"/results/1_RDS/bd_L1.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L1$sonda %>% group_by(saida) %>% nest



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
for (i in 1:nrow(sonda)) {

  # Pegar dados daquela saida  
  data <- sonda[[2]][[i]]
  
  # Criar um objeto sf
  sonda_sf <- st_as_sf(x = data, coords = c("lng", "lat"))
  
  # Definir o crs 4326 - geográfica/WGS 84 - padrão GPS
  st_crs(sonda_sf) <- 4326
  
  # Transformapar para EPSG 32723 - projetadas/WGS 84/UTM 23S
  sonda_proj <- st_transform(sonda_sf, 32723)

  # Fazer um buffer de 250m em torno dos pontos da sonda para pegar mais quadrantes
  sonda_buff <- st_buffer(sonda_proj, 250)

  # Fazer um raster para cada parâmetro da sonda e depois nomeá-lo corretamente
  ras_temp <- rasterize(sonda_buff,
                        agua_ras,
                        field = "Temp",
                        fun = "mean")
  
  names(ras_temp) <- "Temp"
  
  ras_sal <- rasterize(sonda_buff, agua_ras, field = "Sal", fun = mean)
  
  names(ras_sal) <- "Sal"
  
  ras_od <- rasterize(sonda_buff, agua_ras, field = "OD", fun = mean)
  
  names(ras_od) <- "OD"
  
  ras_turb <- rasterize(sonda_buff, agua_ras, field = "Turb", fun = mean)
  
  names(ras_turb) <- "Turb"
  
  ras_ph <- rasterize(sonda_buff, agua_ras, field = "pH", fun = mean)
  
  names(ras_ph) <- "pH"
  
  ras_pres <- rasterize(sonda_buff, agua_ras, field = "Pres", fun = mean)
  
  names(ras_pres) <- "Pres"
  
  # Criar um raster único empilhando todos 
  sonda_ras <- raster::stack(ras_temp, ras_sal, ras_od, ras_turb, ras_ph, ras_pres)
  
  # Definir o nome da pasta nova com o padrão de sempre
  novo_dir <- paste0(pasta_proj,
                    "/results/3_SONDA_TIF/LINHA_1/",
                    sonda[[1]][[i]], "_",
                    str_sub(str_replace_all(data$datahora_SONDA[[1]], "-", "_"), 1, 10))
  
  # Criar a pasta propriamente dito
  dir.create(novo_dir, recursive = TRUE)
  
  # Nessa pasta, criar um arquivo tif, para cada camada, com um padrão de nome por arquivo.
  writeRaster(x = sonda_ras,
              filename = paste0(novo_dir,"/", str_sub(novo_dir,-14,-1),".tif"),
              overwrite = TRUE,
              bylayer = TRUE,
              suffix = "names")
}



# Linha 2 ----

library(sf)
library(spatstat)
library(raster)
library(dplyr)
library(stringr)
library(tidyr)

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo arquivo RDS
bd_L2 <- readRDS(paste0(pasta_proj,"/results/1_RDS/bd_L2.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L2$sonda %>% group_by(saida) %>% nest

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
for (i in 1:nrow(sonda)) {
  
  # Pegar dados daquela saida  
  data <- sonda[[2]][[i]]
  
  # Criar um objeto sf
  sonda_sf <- st_as_sf(x = data, coords = c("lng", "lat"))
  
  # Definir o crs 4326 - geográfica/WGS 84 - padrão GPS
  st_crs(sonda_sf) <- 4326
  
  # Transformapar para EPSG 32723 - projetadas/WGS 84/UTM 23S
  sonda_proj <- st_transform(sonda_sf, 32723)
  
  # Fazer um buffer de 250m em torno dos pontos da sonda para pegar mais quadrantes
  sonda_buff <- st_buffer(sonda_proj, 250)
  
  # Fazer um raster para cada parâmetro da sonda e depois nomeá-lo corretamente
  ras_temp <- rasterize(sonda_buff,
                        agua_ras,
                        field = "Temp",
                        fun = "mean")
  
  names(ras_temp) <- "Temp"
  
  ras_sal <- rasterize(sonda_buff, agua_ras, field = "Sal", fun = mean)
  
  names(ras_sal) <- "Sal"
  
  ras_od <- rasterize(sonda_buff, agua_ras, field = "OD", fun = mean)
  
  names(ras_od) <- "OD"
  
  ras_turb <- rasterize(sonda_buff, agua_ras, field = "Turb", fun = mean)
  
  names(ras_turb) <- "Turb"
  
  ras_ph <- rasterize(sonda_buff, agua_ras, field = "pH", fun = mean)
  
  names(ras_ph) <- "pH"
  
  ras_pres <- rasterize(sonda_buff, agua_ras, field = "Pres", fun = mean)
  
  names(ras_pres) <- "Pres"
  
  # Criar um raster único empilhando todos 
  sonda_ras <- raster::stack(ras_temp, ras_sal, ras_od, ras_turb, ras_ph, ras_pres)
  
  # Definir o nome da pasta nova com o padrão de sempre
  novo_dir <- paste0(pasta_proj,
                     "/results/3_SONDA_TIF/LINHA_2/",
                     sonda[[1]][[i]], "_",
                     str_sub(str_replace_all(data$datahora_SONDA[[1]], "-", "_"), 1, 10))
  
  # Criar a pasta propriamente dito
  dir.create(novo_dir, recursive = TRUE)
  
  # Nessa pasta, criar um arquivo tif, para cada camada, com um padrão de nome por arquivo.
  writeRaster(x = sonda_ras,
              filename = paste0(novo_dir,"/", str_sub(novo_dir,-14,-1),".tif"),
              overwrite = TRUE,
              bylayer = TRUE,
              suffix = "names")
}
