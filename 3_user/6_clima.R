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
  project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L1 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L1.rds"))

# Definindo dados de clima com coordenadas para os pontos
dados <- bd_L1$clima %>%
  group_by(saida) %>%
  mutate(int_clima = interval(datahora_I, datahora_F)) %>%
  dplyr::select(1,2,12:16,18) %>%
  drop_na() %>%
  ungroup() %>%
  rowid_to_column("n_clima") %>%
  nest_by(n_clima)

# Definindo dados de pontos das s
rotas <- bd_L1$rotas %>% 
  mutate(datahora_ROTA = ymd_hms(datahora_ROTA)) %>%
  ungroup() %>%
  dplyr::select(2:5)

pontos <- list()

for(i in 1:nrow(dados)) {
  
  pontos[[i]] <- rotas[rotas$datahora_ROTA %within% dados[[2]][[i]]$int_clima,c(3:4)]
  
  pontos[[i]]$n_clima <- dados[[1]][[i]]
  
}

# Arrumação dos dados
pontos <- drop_na(tibble(lng = abind::abind(pontos, along = 1)[,1],
                         lat = abind::abind(pontos, along = 1)[,2],
                         n_clima = abind::abind(pontos, along = 1)[,3]))

pontos <- pontos %>%
  left_join(unnest(dados, cols = data), by = "n_clima") %>%
  nest_by(saida)

  
# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/6_clima/LINHA_1/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Vento"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Beauf"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Cober"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Visib"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Refle"), recursive = TRUE)

# Criando um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
Rast_Vento <- rast(agua_ras)
Rast_Beauf <- rast(agua_ras)
Rast_Cober <- rast(agua_ras)
Rast_Visib <- rast(agua_ras)
Rast_Refle <- rast(agua_ras)

for(i in 1:nrow(pontos)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  clima_sv <- pontos[[2]][[i]][-10] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    project("epsg:32723") %>%
    buffer(250)
    
  clima_sv <- clima_sv[,c(-1,-2)]
  
  # Vento
  ras_vento <- crop(rasterize(x = clima_sv[,"veloc_vento"],
                              y = agua_ras,
                              field = "veloc_vento",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = paste0(novo_dir, "/Vento/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_vento) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Vento) <-  ras_vento
  
  
  # Beauf
  ras_beauf <- crop(rasterize(x = clima_sv[,"beaufort"],
                              y = agua_ras,
                              field = "beaufort",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_beauf,"epsg:4674"),
              filename = paste0(novo_dir, "/Beauf/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_beauf) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Beauf) <-  ras_beauf
  
  
  # Cober
  ras_cober <- crop(rasterize(x = clima_sv[,"cobert_nuvens"],
                              y = agua_ras,
                              field = "cobert_nuvens",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_cober,"epsg:4674"),
              filename = paste0(novo_dir, "/Cober/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_cober) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Cober) <-  ras_cober
  
  
  # Visib
  ras_visib <- crop(rasterize(x = clima_sv[,"visibilidade"],
                              y = agua_ras,
                              field = "visibilidade",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_visib,"epsg:4674"),
              filename = paste0(novo_dir, "/Visib/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_visib) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Visib) <-  ras_visib
  
  
  # Refle
  ras_refle <- crop(rasterize(x = clima_sv[,"reflexo"],
                              y = agua_ras,
                              field = "reflexo",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = paste0(novo_dir, "/Refle/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_refle) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Refle) <-  ras_refle
  
  
  # Barra de progresso
  pb <- txtProgressBar(min = 0, max = nrow(pontos), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}


# Fazendo as médias de cada parâmetro
med_Vento <- mean(Rast_Vento, na.rm = TRUE)
med_Beauf <- mean(Rast_Beauf, na.rm = TRUE)
med_Cober <- mean(Rast_Cober, na.rm = TRUE)
med_Visib <- mean(Rast_Visib, na.rm = TRUE)
med_Refle <- mean(Rast_Refle, na.rm = TRUE)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/6_clima/LINHA_1/MEDIA/")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Nessa pasta, criar um arquivo tif, para cada parâmetro.
writeRaster(x = med_Vento,
            filename = paste0(novo_dir,"/clima_Vent.tif"),
            overwrite = TRUE)

writeRaster(x = med_Beauf,
            filename = paste0(novo_dir,"/clima_Beauf.tif"),
            overwrite = TRUE)

writeRaster(x = med_Cober,
            filename = paste0(novo_dir,"/clima_Cober.tif"),
            overwrite = TRUE)

writeRaster(x = med_Visib,
            filename = paste0(novo_dir,"/clima_Visib.tif"),
            overwrite = TRUE)

writeRaster(x = med_Refle,
            filename = paste0(novo_dir,"/clima_Refle.tif"),
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
  project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L2 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L2.rds"))

# Definindo dados de clima com coordenadas para os pontos
dados <- bd_L2$clima %>%
  group_by(saida) %>%
  mutate(int_clima = interval(datahora_I, datahora_F)) %>%
  dplyr::select(1,2,12:16,18) %>%
  drop_na() %>%
  ungroup() %>%
  rowid_to_column("n_clima") %>%
  nest_by(n_clima)

# Definindo dados de pontos das s
rotas <- bd_L2$rotas %>% 
  mutate(datahora_ROTA = ymd_hms(datahora_ROTA)) %>%
  ungroup() %>%
  dplyr::select(2:5)

pontos <- list()

for(i in 1:nrow(dados)) {
  
  pontos[[i]] <- rotas[rotas$datahora_ROTA %within% dados[[2]][[i]]$int_clima,c(3:4)]
  
  pontos[[i]]$n_clima <- dados[[1]][[i]]
  
}

# Arrumação dos dados
pontos <- drop_na(tibble(lng = abind::abind(pontos, along = 1)[,1],
                         lat = abind::abind(pontos, along = 1)[,2],
                         n_clima = abind::abind(pontos, along = 1)[,3]))

pontos <- pontos %>%
  left_join(unnest(dados, cols = data), by = "n_clima") %>%
  nest_by(saida)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/6_clima/LINHA_2/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Vento"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Beauf"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Cober"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Visib"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Refle"), recursive = TRUE)

# Criando um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
Rast_Vento <- rast(agua_ras)
Rast_Beauf <- rast(agua_ras)
Rast_Cober <- rast(agua_ras)
Rast_Visib <- rast(agua_ras)
Rast_Refle <- rast(agua_ras)

for(i in 1:nrow(pontos)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  clima_sv <- pontos[[2]][[i]][-10] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    project("epsg:32723") %>%
    buffer(250)
  
  clima_sv <- clima_sv[,c(-1,-2)]
  
  # Vento
  ras_vento <- crop(rasterize(x = clima_sv[,"veloc_vento"],
                              y = agua_ras,
                              field = "veloc_vento",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = paste0(novo_dir, "/Vento/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_vento) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Vento) <-  ras_vento
  
  
  # Beauf
  ras_beauf <- crop(rasterize(x = clima_sv[,"beaufort"],
                              y = agua_ras,
                              field = "beaufort",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_beauf,"epsg:4674"),
              filename = paste0(novo_dir, "/Beauf/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_beauf) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Beauf) <-  ras_beauf
  
  
  # Cober
  ras_cober <- crop(rasterize(x = clima_sv[,"cobert_nuvens"],
                              y = agua_ras,
                              field = "cobert_nuvens",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_cober,"epsg:4674"),
              filename = paste0(novo_dir, "/Cober/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_cober) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Cober) <-  ras_cober
  
  
  # Visib
  ras_visib <- crop(rasterize(x = clima_sv[,"visibilidade"],
                              y = agua_ras,
                              field = "visibilidade",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_visib,"epsg:4674"),
              filename = paste0(novo_dir, "/Visib/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_visib) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Visib) <-  ras_visib
  
  
  # Refle
  ras_refle <- crop(rasterize(x = clima_sv[,"reflexo"],
                              y = agua_ras,
                              field = "reflexo",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = paste0(novo_dir, "/Refle/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_refle) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Refle) <-  ras_refle
  
  
  # Barra de progresso
  pb <- txtProgressBar(min = 0, max = nrow(pontos), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}


# Fazendo as médias de cada parâmetro
med_Vento <- mean(Rast_Vento, na.rm = TRUE)
med_Beauf <- mean(Rast_Beauf, na.rm = TRUE)
med_Cober <- mean(Rast_Cober, na.rm = TRUE)
med_Visib <- mean(Rast_Visib, na.rm = TRUE)
med_Refle <- mean(Rast_Refle, na.rm = TRUE)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/6_clima/LINHA_2/MEDIA/")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Nessa pasta, criar um arquivo tif, para cada parâmetro.
writeRaster(x = med_Vento,
            filename = paste0(novo_dir,"/clima_Vent.tif"),
            overwrite = TRUE)

writeRaster(x = med_Beauf,
            filename = paste0(novo_dir,"/clima_Beauf.tif"),
            overwrite = TRUE)

writeRaster(x = med_Cober,
            filename = paste0(novo_dir,"/clima_Cober.tif"),
            overwrite = TRUE)

writeRaster(x = med_Visib,
            filename = paste0(novo_dir,"/clima_Visib.tif"),
            overwrite = TRUE)

writeRaster(x = med_Refle,
            filename = paste0(novo_dir,"/clima_Refle.tif"),
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
  project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L3 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L3.rds"))

# Definindo dados de clima com coordenadas para os pontos
dados <- bd_L3$clima %>%
  group_by(saida) %>%
  mutate(int_clima = interval(datahora_I, datahora_F)) %>%
  dplyr::select(1,2,12:16,19) %>%
  drop_na() %>%
  ungroup() %>%
  rowid_to_column("n_clima") %>%
  nest_by(n_clima)

# Definindo dados de pontos das rotas
rotas <- bd_L3$rotas %>% 
  mutate(datahora_ROTA = ymd_hms(datahora_ROTA)) %>%
  ungroup() %>%
  dplyr::select(2:5)

pontos <- list()

for(i in 1:nrow(dados)) {
  
  pontos[[i]] <- rotas[rotas$datahora_ROTA %within% dados[[2]][[i]]$int_clima,c(3:4)]
  
  pontos[[i]]$n_clima <- dados[[1]][[i]]
  
}

# Arrumação dos dados
pontos <- drop_na(tibble(lng = abind::abind(pontos, along = 1)[,1],
                         lat = abind::abind(pontos, along = 1)[,2],
                         n_clima = abind::abind(pontos, along = 1)[,3]))

pontos <- pontos %>%
  left_join(unnest(dados, cols = data), by = "n_clima") %>%
  nest_by(saida)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/6_clima/LINHA_3/DIARIO")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(paste0(novo_dir, "/Vento"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Beauf"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Cober"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Visib"), recursive = TRUE)
dir.create(paste0(novo_dir, "/Refle"), recursive = TRUE)

# Criando um stack que receberá todos os dados de cada parâmetro, em cima destes que será feita a média
Rast_Vento <- rast(agua_ras)
Rast_Beauf <- rast(agua_ras)
Rast_Cober <- rast(agua_ras)
Rast_Visib <- rast(agua_ras)
Rast_Refle <- rast(agua_ras)

for(i in 1:nrow(pontos)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  clima_sv <- pontos[[2]][[1]][-10] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    project("epsg:32723") %>%
    buffer(250)
  
  clima_sv <- clima_sv[,c(-1,-2)]
  
  # Vento
  ras_vento <- crop(rasterize(x = clima_sv[,"veloc_vento"],
                              y = agua_ras,
                              field = "veloc_vento",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = paste0(novo_dir, "/Vento/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_vento) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Vento) <-  ras_vento
  
  
  # Beauf
  ras_beauf <- crop(rasterize(x = clima_sv[,"beaufort"],
                              y = agua_ras,
                              field = "beaufort",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_beauf,"epsg:4674"),
              filename = paste0(novo_dir, "/Beauf/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_beauf) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Beauf) <-  ras_beauf
  
  
  # Cober
  ras_cober <- crop(rasterize(x = clima_sv[,"cobert_nuvens"],
                              y = agua_ras,
                              field = "cobert_nuvens",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_cober,"epsg:4674"),
              filename = paste0(novo_dir, "/Cober/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_cober) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Cober) <-  ras_cober
  
  
  # Visib
  ras_visib <- crop(rasterize(x = clima_sv[,"visibilidade"],
                              y = agua_ras,
                              field = "visibilidade",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_visib,"epsg:4674"),
              filename = paste0(novo_dir, "/Visib/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_visib) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Visib) <-  ras_visib
  
  
  # Refle
  ras_refle <- crop(rasterize(x = clima_sv[,"reflexo"],
                              y = agua_ras,
                              field = "reflexo",
                              fun = "mode"),
                    agua_ras)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = paste0(novo_dir, "/Refle/", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                                str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif"),
              overwrite = TRUE)
  
  names(ras_refle) <- pontos[[2]][[1]]$data[[i]] 
  add(Rast_Refle) <-  ras_refle
  
  
  # Barra de progresso
  pb <- txtProgressBar(min = 0, max = nrow(pontos), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}


# Fazendo as médias de cada parâmetro
med_Vento <- mean(Rast_Vento, na.rm = TRUE)
med_Beauf <- mean(Rast_Beauf, na.rm = TRUE)
med_Cober <- mean(Rast_Cober, na.rm = TRUE)
med_Visib <- mean(Rast_Visib, na.rm = TRUE)
med_Refle <- mean(Rast_Refle, na.rm = TRUE)


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/6_clima/LINHA_3/MEDIA/")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Nessa pasta, criar um arquivo tif, para cada parâmetro.
writeRaster(x = med_Vento,
            filename = paste0(novo_dir,"/clima_Vent.tif"),
            overwrite = TRUE)

writeRaster(x = med_Beauf,
            filename = paste0(novo_dir,"/clima_Beauf.tif"),
            overwrite = TRUE)

writeRaster(x = med_Cober,
            filename = paste0(novo_dir,"/clima_Cober.tif"),
            overwrite = TRUE)

writeRaster(x = med_Visib,
            filename = paste0(novo_dir,"/clima_Visib.tif"),
            overwrite = TRUE)

writeRaster(x = med_Refle,
            filename = paste0(novo_dir,"/clima_Refle.tif"),
            overwrite = TRUE)

