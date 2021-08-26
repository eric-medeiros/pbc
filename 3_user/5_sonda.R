# Criação de um arquivo *.tif para cada um dos parâmetros da sonda (temp, sal, od, ph, turb, press)

# Linha 1 ----

library(terra)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua transformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_sv <- vect("1_data/SHAPES_AUX/Agua_L1.shp")

# Mudando a projeção
agua_proj <- agua_sv %>%
  terra::project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L1 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L1.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L1$sonda %>% group_by(saida) %>% nest

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_1/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Temp"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Sal"), recursive = TRUE)
dir.create(paste0(novo_dir, "/OD"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Turb"), recursive = TRUE)
dir.create(paste0(novo_dir, "/pH"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Pres"), recursive = TRUE)

# Criando um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
Rast_Temp <- rast(agua_ras)
Rast_Sal <- rast(agua_ras)
Rast_OD <- rast(agua_ras)
Rast_Turb <- rast(agua_ras)
Rast_pH <- rast(agua_ras)
Rast_Pres <- rast(agua_ras)


# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(sonda)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  sonda_sv <- sonda[[2]][[i]] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    terra::project("epsg:32723") %>%
    terra::buffer(250)
  
  sonda_sv <- sonda_sv[,c(-9,-10)]
  
  # Temperatura
  ras_temp <- crop(rasterize(x = sonda_sv[,"Temp"],
                             y = agua_ras,
                             field = "Temp",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_temp,"epsg:4674"),
              filename = paste0(novo_dir, "/Temp/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_temp) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Temp) <-  ras_temp
  
  
  # Salinidade
  ras_sal <- crop(rasterize(x = sonda_sv[,"Sal"],
                            y = agua_ras,
                            field = "Sal",
                            fun = "mean",
                            na.rm = TRUE),
                  agua_ras)
  
  writeRaster(x = terra::project(ras_sal,"epsg:4674"),
              filename = paste0(novo_dir, "/Sal/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_sal) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Sal) <-  ras_sal
  
  
  # Oxigênio Dissolvido
  ras_od <- crop(rasterize(x = sonda_sv[,"OD"],
                           y = agua_ras,
                           field = "OD",
                           fun = "mean",
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = terra::project(ras_od,"epsg:4674"),
              filename = paste0(novo_dir, "/OD/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_od) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_OD) <-  ras_od
  
  
  # Turbidez
  ras_turb <- crop(rasterize(x = sonda_sv[,"Turb"],
                             y = agua_ras,
                             field = "Turb",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_turb,"epsg:4674"),
              filename = paste0(novo_dir, "/Turb/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_turb) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Turb) <-  ras_turb
  
  
  # pH
  ras_ph <- crop(rasterize(x = sonda_sv[,"pH"],
                           y = agua_ras,
                           field = "pH",
                           fun = "mean",
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = terra::project(ras_ph,"epsg:4674"),
              filename = paste0(novo_dir, "/pH/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_ph) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_pH) <-  ras_ph
  
  
  # Pressão Atmosférica
  ras_pres <- crop(rasterize(x = sonda_sv[,"Pres"],
                             y = agua_ras,
                             field = "Pres",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_pres,"epsg:4674"),
              filename = paste0(novo_dir, "/Pres/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_pres) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Pres) <-  ras_pres
  
  pb <- txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}

# Fazendo as médias de cada parâmetro
rast_Temp <- mean(Rast_Temp, na.rm = TRUE)
rast_Turb <- mean(Rast_Turb, na.rm = TRUE)
rast_OD <- mean(Rast_OD, na.rm = TRUE)
rast_pH <- mean(Rast_pH, na.rm = TRUE)
rast_Pres <- mean(Rast_Pres, na.rm = TRUE)
rast_Sal <- mean(Rast_Sal, na.rm = TRUE)


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

library(terra)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua transformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_sv <- vect("1_data/SHAPES_AUX/Agua_L2.shp")

# Mudando a projeção
agua_proj <- agua_sv %>%
  terra::project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L2 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L2.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L2$sonda %>% group_by(saida) %>% nest

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_2/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Temp"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Sal"), recursive = TRUE)
dir.create(paste0(novo_dir, "/OD"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Turb"), recursive = TRUE)
dir.create(paste0(novo_dir, "/pH"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Pres"), recursive = TRUE)

# Criando um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
Rast_Temp <- rast(agua_ras)
Rast_Sal <- rast(agua_ras)
Rast_OD <- rast(agua_ras)
Rast_Turb <- rast(agua_ras)
Rast_pH <- rast(agua_ras)
Rast_Pres <- rast(agua_ras)


# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(sonda)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  sonda_sv <- sonda[[2]][[i]] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    terra::project("epsg:32723") %>%
    terra::buffer(250)
  
  sonda_sv <- sonda_sv[,c(-9,-10)]
  
  # Temperatura
  ras_temp <- crop(rasterize(x = sonda_sv[,"Temp"],
                             y = agua_ras,
                             field = "Temp",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_temp,"epsg:4674"),
              filename = paste0(novo_dir, "/Temp/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_temp) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Temp) <-  ras_temp
  
  
  # Salinidade
  ras_sal <- crop(rasterize(x = sonda_sv[,"Sal"],
                            y = agua_ras,
                            field = "Sal",
                            fun = "mean",
                            na.rm = TRUE),
                  agua_ras)
  
  writeRaster(x = terra::project(ras_sal,"epsg:4674"),
              filename = paste0(novo_dir, "/Sal/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_sal) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Sal) <-  ras_sal
  
  
  # Oxigênio Dissolvido
  ras_od <- crop(rasterize(x = sonda_sv[,"OD"],
                           y = agua_ras,
                           field = "OD",
                           fun = "mean",
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = terra::project(ras_od,"epsg:4674"),
              filename = paste0(novo_dir, "/OD/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_od) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_OD) <-  ras_od
  
  
  # Turbidez
  ras_turb <- crop(rasterize(x = sonda_sv[,"Turb"],
                             y = agua_ras,
                             field = "Turb",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_turb,"epsg:4674"),
              filename = paste0(novo_dir, "/Turb/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_turb) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Turb) <-  ras_turb
  
  
  # pH
  ras_ph <- crop(rasterize(x = sonda_sv[,"pH"],
                           y = agua_ras,
                           field = "pH",
                           fun = "mean",
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = terra::project(ras_ph,"epsg:4674"),
              filename = paste0(novo_dir, "/pH/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_ph) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_pH) <-  ras_ph
  
  
  # Pressão Atmosférica
  ras_pres <- crop(rasterize(x = sonda_sv[,"Pres"],
                             y = agua_ras,
                             field = "Pres",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_pres,"epsg:4674"),
              filename = paste0(novo_dir, "/Pres/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_pres) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Pres) <-  ras_pres
  
  pb <- txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}

# Fazendo as médias de cada parâmetro
rast_Temp <- mean(Rast_Temp, na.rm = TRUE)
rast_Turb <- mean(Rast_Turb, na.rm = TRUE)
rast_OD <- mean(Rast_OD, na.rm = TRUE)
rast_pH <- mean(Rast_pH, na.rm = TRUE)
rast_Pres <- mean(Rast_Pres, na.rm = TRUE)
rast_Sal <- mean(Rast_Sal, na.rm = TRUE)


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

library(terra)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua transformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_sv <- vect("1_data/SHAPES_AUX/Agua_L3.shp")

# Mudando a projeção
agua_proj <- agua_sv %>%
  terra::project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L3 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L3.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L3$sonda %>% group_by(saida) %>% nest

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_3/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Temp"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Sal"), recursive = TRUE)
dir.create(paste0(novo_dir, "/OD"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Turb"), recursive = TRUE)
dir.create(paste0(novo_dir, "/pH"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Pres"), recursive = TRUE)

# Criando um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
Rast_Temp <- rast(agua_ras)
Rast_Sal <- rast(agua_ras)
Rast_OD <- rast(agua_ras)
Rast_Turb <- rast(agua_ras)
Rast_pH <- rast(agua_ras)
Rast_Pres <- rast(agua_ras)


# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(sonda)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  sonda_sv <- sonda[[2]][[i]] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    terra::project("epsg:32723") %>%
    terra::buffer(1000)
  
  sonda_sv <- sonda_sv[,c(-1,-9,-10)]
  
  # Temperatura
  ras_temp <- crop(rasterize(x = sonda_sv[,"Temp"],
                             y = agua_ras,
                             field = "Temp",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_temp,"epsg:4674"),
              filename = paste0(novo_dir, "/Temp/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_temp) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Temp) <-  ras_temp
  
  
  # Salinidade
  ras_sal <- crop(rasterize(x = sonda_sv[,"Sal"],
                            y = agua_ras,
                            field = "Sal",
                            fun = "mean",
                            na.rm = TRUE),
                  agua_ras)
  
  writeRaster(x = terra::project(ras_sal,"epsg:4674"),
              filename = paste0(novo_dir, "/Sal/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_sal) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Sal) <-  ras_sal
  
  
  # Oxigênio Dissolvido
  ras_od <- crop(rasterize(x = sonda_sv[,"OD"],
                           y = agua_ras,
                           field = "OD",
                           fun = "mean",
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = terra::project(ras_od,"epsg:4674"),
              filename = paste0(novo_dir, "/OD/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_od) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_OD) <-  ras_od
  
  
  # Turbidez
  ras_turb <- crop(rasterize(x = sonda_sv[,"Turb"],
                             y = agua_ras,
                             field = "Turb",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_turb,"epsg:4674"),
              filename = paste0(novo_dir, "/Turb/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_turb) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Turb) <-  ras_turb
  
  
  # pH
  ras_ph <- crop(rasterize(x = sonda_sv[,"pH"],
                           y = agua_ras,
                           field = "pH",
                           fun = "mean",
                           na.rm = TRUE),
                 agua_ras)
  
  writeRaster(x = terra::project(ras_ph,"epsg:4674"),
              filename = paste0(novo_dir, "/pH/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_ph) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_pH) <-  ras_ph
  
  
  # Pressão Atmosférica
  ras_pres <- crop(rasterize(x = sonda_sv[,"Pres"],
                             y = agua_ras,
                             field = "Pres",
                             fun = "mean",
                             na.rm = TRUE),
                   agua_ras)
  
  writeRaster(x = terra::project(ras_pres,"epsg:4674"),
              filename = paste0(novo_dir, "/Pres/", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif"),
              overwrite = TRUE)
  
  names(ras_pres) <- str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10) 
  add(Rast_Pres) <-  ras_pres
  
  pb <- txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}

# Fazendo as médias de cada parâmetro
rast_Temp <- mean(Rast_Temp, na.rm = TRUE)
rast_Turb <- mean(Rast_Turb, na.rm = TRUE)
rast_OD <- mean(Rast_OD, na.rm = TRUE)
rast_pH <- mean(Rast_pH, na.rm = TRUE)
rast_Pres <- mean(Rast_Pres, na.rm = TRUE)
rast_Sal <- mean(Rast_Sal, na.rm = TRUE)


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

