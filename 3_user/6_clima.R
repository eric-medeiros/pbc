# Linha 1 ----

# Limpando o Global Environment 
rm(list = ls())

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
dados <- bd_L1$amostragens %>%
  group_by(saida) %>%
  transmute(int_amos = interval(datahora_I, datahora_F)) %>%
  right_join(bd_L1$clima, by = "saida") %>%
  mutate(int_clima = interval(datahora_I, datahora_F)) %>%
  dplyr::select(1,3,13:17,19,2) %>%
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
  
  pontos[[i]] <- rotas[rotas$datahora_ROTA %within% dados[[2]][[i]]$int_clima,]
  pontos[[i]] <- pontos[[i]][pontos[[i]]$datahora_ROTA %within% dados[[2]][[i]]$int_amos,c(3:4)]
  
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
novo_dir <- paste0(pasta_proj, "/4_export/6_clima/LINHA_1")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(novo_dir, recursive = TRUE)


for(i in 1:nrow(pontos)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  clima_sv <- pontos[[2]][[i]][-10] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    project("epsg:32723") %>%
    buffer(250) %>%
    crop(agua_proj)
    
  clima_sv <- clima_sv[,c(-1,-2)]
  
  
  # Criando os rasters
  ras_vento <- rasterize(x = clima_sv[,"veloc_vento"],
                         y = agua_ras,
                         field = "veloc_vento",
                         fun = "mode")
  
  ras_beauf <- rasterize(x = clima_sv[,"beaufort"],
                         y = agua_ras,
                         field = "beaufort",
                         fun = "mode")
  
  ras_cober <- rasterize(x = clima_sv[,"cobert_nuvens"],
                         y = agua_ras,
                         field = "cobert_nuvens",
                         fun = "mode")
  
  ras_visib <- rasterize(x = clima_sv[,"visibilidade"],
                         y = agua_ras,
                         field = "visibilidade",
                         fun = "mode")
  
  ras_refle <- rasterize(x = clima_sv[,"reflexo"],
                         y = agua_ras,
                         field = "reflexo",
                         fun = "mode")
  
  # Definindo o nomes dos arquivos
  arquivo_vt <- paste0(novo_dir, "/VT_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_bf <- paste0(novo_dir, "/BF_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_cb <- paste0(novo_dir, "/CB_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_vb <- paste0(novo_dir, "/VB_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_rf <- paste0(novo_dir, "/RF_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  
  
  # Salvando os arquivos
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = arquivo_vt,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_beauf,"epsg:4674"),
              filename = arquivo_bf,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_cober,"epsg:4674"),
              filename = arquivo_cb,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_visib,"epsg:4674"),
              filename = arquivo_vb,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = arquivo_rf,
              overwrite = TRUE)
  
  # Barra de progresso
  pb <- txtProgressBar(min = 0, max = nrow(pontos), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}



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
dados <- bd_L2$amostragens %>%
  group_by(saida) %>%
  transmute(int_amos = interval(datahora_I, datahora_F)) %>%
  right_join(bd_L2$clima, by = "saida") %>%
  mutate(int_clima = interval(datahora_I, datahora_F)) %>%
  dplyr::select(1,3,13:17,19,2) %>%
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
  
  pontos[[i]] <- rotas[rotas$datahora_ROTA %within% dados[[2]][[i]]$int_clima,]
  pontos[[i]] <- pontos[[i]][pontos[[i]]$datahora_ROTA %within% dados[[2]][[i]]$int_amos,c(3:4)]
  
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
novo_dir <- paste0(pasta_proj, "/4_export/6_clima/LINHA_2")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(novo_dir, recursive = TRUE)


for(i in 1:nrow(pontos)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  clima_sv <- pontos[[2]][[i]][-10] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    project("epsg:32723") %>%
    buffer(250) %>%
    crop(agua_proj)
  
  clima_sv <- clima_sv[,c(-1,-2)]
  
  # Criando os rasters
  ras_vento <- rasterize(x = clima_sv[,"veloc_vento"],
                         y = agua_ras,
                         field = "veloc_vento",
                         fun = "mode")
  
  ras_beauf <- rasterize(x = clima_sv[,"beaufort"],
                         y = agua_ras,
                         field = "beaufort",
                         fun = "mode")
  
  ras_cober <- rasterize(x = clima_sv[,"cobert_nuvens"],
                         y = agua_ras,
                         field = "cobert_nuvens",
                         fun = "mode")
  
  ras_visib <- rasterize(x = clima_sv[,"visibilidade"],
                         y = agua_ras,
                         field = "visibilidade",
                         fun = "mode")
  
  ras_refle <- rasterize(x = clima_sv[,"reflexo"],
                         y = agua_ras,
                         field = "reflexo",
                         fun = "mode")
  
  # Definindo o nomes dos arquivos
  arquivo_vt <- paste0(novo_dir, "/VT_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_bf <- paste0(novo_dir, "/BF_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_cb <- paste0(novo_dir, "/CB_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_vb <- paste0(novo_dir, "/VB_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_rf <- paste0(novo_dir, "/RF_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  
  # Salvando os arquivos
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = arquivo_vt,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_beauf,"epsg:4674"),
              filename = arquivo_bf,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_cober,"epsg:4674"),
              filename = arquivo_cb,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_visib,"epsg:4674"),
              filename = arquivo_vb,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = arquivo_rf,
              overwrite = TRUE)
  
  # Barra de progresso
  pb <- txtProgressBar(min = 0, max = nrow(pontos), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}




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
dados <- bd_L3$estacoes %>%
  group_by(saida) %>%
  transmute(int_esta = interval(datahora_I, datahora_F)) %>%
  right_join(bd_L3$clima, by = "saida") %>%
  mutate(int_clima = interval(datahora_I, datahora_F)) %>%
  dplyr::select(1,3,13:17,20,2) %>%
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
  
  pontos[[i]] <- rotas[rotas$datahora_ROTA %within% dados[[2]][[i]]$int_clima,]
  pontos[[i]] <- pontos[[i]][pontos[[i]]$datahora_ROTA %within% dados[[2]][[i]]$int_esta,c(3:4)]
  
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
novo_dir <- paste0(pasta_proj, "/4_export/6_clima/LINHA_3")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(novo_dir, recursive = TRUE)


for(i in 1:nrow(pontos)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  clima_sv <- pontos[[2]][[i]][-10] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    terra::project("epsg:32723") %>%
    terra::buffer(1000) %>%
    crop(agua_proj)
  
  clima_sv <- clima_sv[,c(-1,-2)]
  
  # Criando os rasters
  ras_vento <- rasterize(x = clima_sv[,"veloc_vento"],
                         y = agua_ras,
                         field = "veloc_vento",
                         fun = "mode")
  
  ras_beauf <- rasterize(x = clima_sv[,"beaufort"],
                         y = agua_ras,
                         field = "beaufort",
                         fun = "mode")
  
  ras_cober <- rasterize(x = clima_sv[,"cobert_nuvens"],
                         y = agua_ras,
                         field = "cobert_nuvens",
                         fun = "mode")
  
  ras_visib <- rasterize(x = clima_sv[,"visibilidade"],
                         y = agua_ras,
                         field = "visibilidade",
                         fun = "mode")
  
  ras_refle <- rasterize(x = clima_sv[,"reflexo"],
                         y = agua_ras,
                         field = "reflexo",
                         fun = "mode")
  
  # Definindo o nomes dos arquivos
  arquivo_vt <- paste0(novo_dir, "/VT_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_bf <- paste0(novo_dir, "/BF_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_cb <- paste0(novo_dir, "/CB_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_vb <- paste0(novo_dir, "/VB_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  arquivo_rf <- paste0(novo_dir, "/RF_", str_pad(pontos[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_replace_all(pontos[[2]][[i]]$data[[1]], "-", "_"), ".tif")
  
  
  # Salvando os arquivos
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = arquivo_vt,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_beauf,"epsg:4674"),
              filename = arquivo_bf,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_cober,"epsg:4674"),
              filename = arquivo_cb,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_visib,"epsg:4674"),
              filename = arquivo_vb,
              overwrite = TRUE)
  
  writeRaster(x = project(ras_vento,"epsg:4674"),
              filename = arquivo_rf,
              overwrite = TRUE)
  
  
  # Barra de progresso
  pb <- txtProgressBar(min = 0, max = nrow(pontos), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}



