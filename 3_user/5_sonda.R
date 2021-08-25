# Criação de um arquivo *.tif para cada um dos parâmetros da sonda (temp, sal, od, ph, turb, press)

# Linha 1 ----

library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(stringr)

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo arquivo RDS
bd_L1 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L1.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L1$sonda %>% group_by(saida) %>% nest

# Abrindo shape da agua
agua_sf <- st_read("1_data/SHAPES_AUX/Agua_L1.shp")

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_proj <- st_transform(agua_sf, 32723)

# criando raster da agua transformada para projetada
agua_ras <- raster(agua_proj)

# definindo resolução de 250m
res(agua_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_ras)

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_1/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Temp"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Sal"), recursive = TRUE)
dir.create(paste0(novo_dir, "/OD"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Turb"), recursive = TRUE)
dir.create(paste0(novo_dir, "/pH"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Pres"), recursive = TRUE)

# Craindo um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
stack_Temp <- raster::stack()
stack_Turb <- raster::stack()
stack_Pres <- raster::stack()
stack_pH <- raster::stack()
stack_OD <- raster::stack()
stack_Sal <- raster::stack()


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
  
  # Fazer e salvar um raster / dia  de dados da sonda e anexar ao stack, para cada parâmetro.
  
  # Temperatura
  ras_temp <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Temp",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_temp,
              filename = paste0(novo_dir, "/Temp/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Temp <- addLayer(stack_Temp, ras_temp)
  
  
  # Salinidade
  ras_sal <- crop(rasterize(x = sonda_buff,
                            y = agua_ras,
                            field = "Sal",
                            fun = mean,
                            na.rm = TRUE),
                  agua_ras)
  
  writeRaster(x = ras_sal,
              filename = paste0(novo_dir, "/Sal/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Sal <- addLayer(stack_Sal, ras_sal)
  
  
  # Oxigênio Dissolvido                       
  ras_od <- crop(rasterize(x = sonda_buff,
                           y = agua_ras,
                           field = "OD",
                           fun = mean,
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = ras_od,
              filename = paste0(novo_dir, "/OD/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_OD <- addLayer(stack_OD, ras_od)
  
  
  # Turbidez
  ras_turb <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Turb",
                             fun = mean,
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_turb,
              filename = paste0(novo_dir, "/Turb/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Turb <- addLayer(stack_Turb, ras_turb)
  
  
  # pH
  ras_ph <- crop(rasterize(x = sonda_buff,
                           y = agua_ras,
                           field = "pH",
                           fun = mean,
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = ras_ph,
              filename = paste0(novo_dir, "/pH/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_pH <- addLayer(stack_pH, ras_ph)
  
  
  # Pressão atmosférica
  ras_pres <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Pres",
                             fun = mean,
                             na.rm = TRUE),
                   agua_ras)

  writeRaster(x = ras_pres,
              filename = paste0(novo_dir, "/Pres/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Pres <- addLayer(stack_Pres, ras_pres)
  
  pb <- txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}

# Fazendo as médias de cada parâmetro
rast_Temp <- mean(stack_Temp, na.rm = TRUE)

rast_Turb <- mean(stack_Turb, na.rm = TRUE)

rast_OD <- mean(stack_OD, na.rm = TRUE)

rast_pH <- mean(stack_pH, na.rm = TRUE)

rast_Pres <- mean(stack_Pres, na.rm = TRUE)

rast_Sal <- mean(stack_Sal, na.rm = TRUE)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/5_sonda/LINHA_1/MEDIA/")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Nessa pasta, criar um arquivo tif, para cada parâmetro.
writeRaster(x = rast_Temp,
            filename = paste0(novo_dir,"/sonda_Temp.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Turb,
            filename = paste0(novo_dir,"/sonda_Turb.tif"),
            overwrite = TRUE)

writeRaster(x = rast_OD,
            filename = paste0(novo_dir,"/sonda_OD.tif"),
            overwrite = TRUE)

writeRaster(x = rast_pH,
            filename = paste0(novo_dir,"/sonda_pH.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Pres,
            filename = paste0(novo_dir,"/sonda_Pres.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Sal,
            filename = paste0(novo_dir,"/sonda_Sal.tif"),
            overwrite = TRUE)



# Linha 2 ----

library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(stringr)

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo arquivo RDS
bd_L2 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L2.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L2$sonda %>% group_by(saida) %>% nest

# Abrindo shape da agua
agua_sf <- st_read("1_data/SHAPES_AUX/Agua_L2.shp")

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_proj <- st_transform(agua_sf, 32723)

# criando raster da agua transformada para projetada
agua_ras <- raster(agua_proj)

# definindo resolução de 250m
res(agua_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_ras)

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_2/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Temp"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Sal"), recursive = TRUE)
dir.create(paste0(novo_dir, "/OD"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Turb"), recursive = TRUE)
dir.create(paste0(novo_dir, "/pH"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Pres"), recursive = TRUE)

# Craindo um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
stack_Temp <- raster::stack()
stack_Turb <- raster::stack()
stack_Pres <- raster::stack()
stack_pH <- raster::stack()
stack_OD <- raster::stack()
stack_Sal <- raster::stack()


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
  
  # Fazer e salvar um raster / dia  de dados da sonda e anexar ao stack, para cada parâmetro.
  
  # Temperatura
  ras_temp <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Temp",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_temp,
              filename = paste0(novo_dir, "/Temp/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Temp <- addLayer(stack_Temp, ras_temp)
  
  
  # Salinidade
  ras_sal <- crop(rasterize(x = sonda_buff,
                            y = agua_ras,
                            field = "Sal",
                            fun = mean,
                            na.rm = TRUE),
                  agua_ras)
  
  writeRaster(x = ras_sal,
              filename = paste0(novo_dir, "/Sal/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Sal <- addLayer(stack_Sal, ras_sal)
  
  
  # Oxigênio Dissolvido                       
  ras_od <- crop(rasterize(x = sonda_buff,
                           y = agua_ras,
                           field = "OD",
                           fun = mean,
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = ras_od,
              filename = paste0(novo_dir, "/OD/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_OD <- addLayer(stack_OD, ras_od)
  
  
  # Turbidez
  ras_turb <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Turb",
                             fun = mean,
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_turb,
              filename = paste0(novo_dir, "/Turb/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Turb <- addLayer(stack_Turb, ras_turb)
  
  
  # pH
  ras_ph <- crop(rasterize(x = sonda_buff,
                           y = agua_ras,
                           field = "pH",
                           fun = mean,
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = ras_ph,
              filename = paste0(novo_dir, "/pH/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_pH <- addLayer(stack_pH, ras_ph)
  
  
  # Pressão atmosférica
  ras_pres <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Pres",
                             fun = mean,
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_pres,
              filename = paste0(novo_dir, "/Pres/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Pres <- addLayer(stack_Pres, ras_pres)
  
  pb <-  txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
}

# Fazendo as médias de cada parâmetro
rast_Temp <- mean(stack_Temp, na.rm = TRUE)

rast_Turb <- mean(stack_Turb, na.rm = TRUE)

rast_OD <- mean(stack_OD, na.rm = TRUE)

rast_pH <- mean(stack_pH, na.rm = TRUE)

rast_Pres <- mean(stack_Pres, na.rm = TRUE)

rast_Sal <- mean(stack_Sal, na.rm = TRUE)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/5_sonda/LINHA_2/MEDIA/")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Nessa pasta, criar um arquivo tif, para cada parâmetro.
writeRaster(x = rast_Temp,
            filename = paste0(novo_dir,"/sonda_Temp.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Turb,
            filename = paste0(novo_dir,"/sonda_Turb.tif"),
            overwrite = TRUE)

writeRaster(x = rast_OD,
            filename = paste0(novo_dir,"/sonda_OD.tif"),
            overwrite = TRUE)

writeRaster(x = rast_pH,
            filename = paste0(novo_dir,"/sonda_pH.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Pres,
            filename = paste0(novo_dir,"/sonda_Pres.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Sal,
            filename = paste0(novo_dir,"/sonda_Sal.tif"),
            overwrite = TRUE)


# Linha 3 ----

library(sf)
library(raster)
library(dplyr)
library(tidyr)

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo arquivo RDS
bd_L3 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L3.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L3$sonda %>% group_by(saida) %>% nest

# Abrindo shape da agua
agua_sf <- st_read("1_data/SHAPES_AUX/Agua_L3.shp")

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_proj <- st_transform(agua_sf, 32723)

# criando raster da agua transformada para projetada
agua_ras <- raster(agua_proj)

# definindo resolução de 250m
res(agua_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_ras)

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_3/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Temp"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Sal"), recursive = TRUE)
dir.create(paste0(novo_dir, "/OD"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Turb"), recursive = TRUE)
dir.create(paste0(novo_dir, "/pH"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Pres"), recursive = TRUE)

# Craindo um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
stack_Temp <- raster::stack()
stack_Turb <- raster::stack()
stack_Pres <- raster::stack()
stack_pH <- raster::stack()
stack_OD <- raster::stack()
stack_Sal <- raster::stack()

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
  sonda_buff <- st_buffer(sonda_proj, 1000)
  
  # Fazer e salvar um raster / dia  de dados da sonda e anexar ao stack, para cada parâmetro.
  
  # Temperatura
  ras_temp <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Temp",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_temp,
              filename = paste0(novo_dir, "/Temp/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Temp <- addLayer(stack_Temp, ras_temp)
  
  
  # Salinidade
  ras_sal <- crop(rasterize(x = sonda_buff,
                            y = agua_ras,
                            field = "Sal",
                            fun = mean,
                            na.rm = TRUE),
                  agua_ras)
  
  writeRaster(x = ras_sal,
              filename = paste0(novo_dir, "/Sal/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Sal <- addLayer(stack_Sal, ras_sal)
  
  
  # Oxigênio Dissolvido                       
  ras_od <- crop(rasterize(x = sonda_buff,
                           y = agua_ras,
                           field = "OD",
                           fun = mean,
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = ras_od,
              filename = paste0(novo_dir, "/OD/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_OD <- addLayer(stack_OD, ras_od)
  
  
  # Turbidez
  ras_turb <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Turb",
                             fun = mean,
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_turb,
              filename = paste0(novo_dir, "/Turb/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Turb <- addLayer(stack_Turb, ras_turb)
  
  
  # pH
  ras_ph <- crop(rasterize(x = sonda_buff,
                           y = agua_ras,
                           field = "pH",
                           fun = mean,
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = ras_ph,
              filename = paste0(novo_dir, "/pH/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_pH <- addLayer(stack_pH, ras_ph)
  
  
  # Pressão atmosférica
  ras_pres <- crop(rasterize(x = sonda_buff,
                             y = agua_ras,
                             field = "Pres",
                             fun = mean,
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = ras_pres,
              filename = paste0(novo_dir, "/Pres/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"), 1, 10), ".tif"),
              overwrite = TRUE) 
  
  stack_Pres <- addLayer(stack_Pres, ras_pres)
  
  pb <-  txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
}

# Fazendo as médias de cada parâmetro
rast_Temp <- mean(stack_Temp, na.rm = TRUE)

rast_Turb <- mean(stack_Turb, na.rm = TRUE)

rast_OD <- mean(stack_OD, na.rm = TRUE)

rast_pH <- mean(stack_pH, na.rm = TRUE)

rast_Pres <- mean(stack_Pres, na.rm = TRUE)

rast_Sal <- mean(stack_Sal, na.rm = TRUE)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/5_sonda/LINHA_3/MEDIA/")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Nessa pasta, criar um arquivo tif, para cada parâmetro.
writeRaster(x = rast_Temp,
            filename = paste0(novo_dir,"/sonda_Temp.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Turb,
            filename = paste0(novo_dir,"/sonda_Turb.tif"),
            overwrite = TRUE)

writeRaster(x = rast_OD,
            filename = paste0(novo_dir,"/sonda_OD.tif"),
            overwrite = TRUE)

writeRaster(x = rast_pH,
            filename = paste0(novo_dir,"/sonda_pH.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Pres,
            filename = paste0(novo_dir,"/sonda_Pres.tif"),
            overwrite = TRUE)

writeRaster(x = rast_Sal,
            filename = paste0(novo_dir,"/sonda_Sal.tif"),
            overwrite = TRUE)

