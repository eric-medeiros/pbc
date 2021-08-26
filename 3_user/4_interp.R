# Mapas de Calor Kernel

library(sf)
library(spatstat)
library(maptools)
library(raster)
library(rgdal)
library(sp)
library(spatialEco)
library(data.table)
library(dplyr)


### Linha 1 ----

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua (tem que ter feito o "2_Gerando_SHP.R")
agua1_sf <- st_read(paste0(pasta_proj, "/1_data/SHAPES_AUX/Agua_L1.shp"))

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua1_proj <- st_transform(agua1_sf, 32723)

# criando raster da agua transformada para projetada
agua1_ras <- raster(agua1_proj)

# definindo resolução de 250m
res(agua1_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua1_ras <- rasterize(agua1_proj, agua1_ras)

# Abrindo shape dos grupos, tem que ter rodado o 2_Gerando_SHP.R"
grupos_sf <- st_read("4_export/3_shape/PONTOS/L1_grupos.shp")

# tranformando para EPSG 32723 - projetada/WGS 84/UTM zona23S - entre 48W e 42W
grupos_proj <- st_transform(grupos_sf, 32723)

# Transformando de classe "sf" para classe "ppp" com janela de agua - pacote spatstat
grupos_ppp <- as.ppp(grupos_proj)

# Atribuindo como marks
marks(grupos_ppp) <- as.data.frame(grupos_sf)[,-which(names(grupos_sf) == "geometry")]

# Atribuindo o Window da agua com resolução de 250m
Window(grupos_ppp) <- as.owin(as.im(agua1_ras))

# Atribuindo a unidade de metros, já que é UTM  
unitname(grupos_ppp) <- c("metro", "metros")

# Mudando unidade para km para que o kernel seja por km²
grupos_ppp <- rescale(grupos_ppp, 1000, "km")

# Removendo pontos duplicados
grupos_ppp <- unique.ppp(grupos_ppp)

# Kernel do pacote spatstat
kernel_L1 <- (density.ppp(grupos_ppp,
                          kernel = "gaussian",
                          sigma = bw.ppl(grupos_ppp),
                          edge = TRUE,
                          diggle = TRUE,
                          positive = TRUE,
                          weights = grupos_ppp$marks$tam_est))

# Voltando para m nos eixos X e Y, mas o Z continua como km²
kernel_L1 <- rescale(kernel_L1, .001, c("metro", "metros"))

# Rotulando o CRS no raster do kernel como WGS 84 projetada - pacote raster
rast_proj <- raster(kernel_L1, crs = CRS("+init=epsg:32723"))

# Fazendo o volume para cada p
p50_1 <- rasterToPolygons(raster.vol(rast_proj, p=0.50), dissolve = TRUE)[2,]
p95_1 <- rasterToPolygons(raster.vol(rast_proj, p=0.95), dissolve = TRUE)[2,]


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/4_interp/LINHA_1")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Salvando os raster do kernel
writeRaster(rast_proj, filename = "4_export/4_interp/LINHA_1/L1_kernel.tif", format = "GTiff", overwrite = TRUE)

# salvando os shapefiles de 0.50
writeOGR(obj = p50_1,
         dsn = "4_export/4_interp/LINHA_1",
         layer = "L1_p50",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# salvando o shapefile de 0.95
writeOGR(obj = p95_1,
         dsn = "4_export/4_interp/LINHA_1",
         layer = "L1_p95",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)



### Linha 2 (tem que ter rodados o kernel da linha 1) ----

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua (tem que ter feito o "2_Gerando_SHP.R")
agua2_sf <- st_read(paste0(pasta_proj,"/1_data/SHAPES_AUX/Agua_L2.shp"))

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua2_proj <- st_transform(agua2_sf, 32723)

# criando raster da agua transformada para projetada
agua2_ras <- raster(agua2_proj)

# definindo resolução de 250m
res(agua2_ras) <- 250


# Caminho do arquivo recém salvo pela linha 1
caminho_raster <- paste0(pasta_proj, "/4_export/4_interp/LINHA_1/L1_kernel.tif")

# Lendo o arquivo recém salvo pela linha 1
rast1_proj <- raster(caminho_raster)

# Cortando a imagem do kernel, com o contorno de agua da linha 2
rast2_proj <- crop(rast1_proj, agua2_ras)

# Fazendo o volume para cada p
p50_2 <- rasterToPolygons(raster.vol(rast2_proj, p=0.50), dissolve = TRUE)[2,]
p95_2 <- rasterToPolygons(raster.vol(rast2_proj, p=0.95), dissolve = TRUE)[2,]


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/4_interp/LINHA_2")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Salvando os raster do kernel
writeRaster(rast2_proj, filename = "4_export/4_interp/LINHA_2/L2_kernel.tif", format = "GTiff", overwrite = TRUE)

# salvando os shapefiles de 0.50
writeOGR(obj = p50_2,
         dsn = "4_export/4_interp/LINHA_2",
         layer = "L2_p50",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# salvando o shapefile de 0.95
writeOGR(obj = p95_2,
         dsn = "4_export/4_interp/LINHA_2",
         layer = "L2_p95",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


### Linha 3 ----

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua (tem que ter feito o "2_Gerando_SHP.R")
agua_sf <- st_read(paste0(pasta_proj, "/1_data/SHAPES_AUX/Agua_L3.shp"))

# tranformando para EPSG 32723 - projetada/WGS 84/UTM zona23S - entre 48W e 42W
agua_proj <- st_transform(agua_sf, 32723)

# criando raster da agua transformada para projetada
agua_ras <- raster(agua_proj)

# definindo resolução de 250m
res(agua_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_ras)

# Abrindo shape dos grupos, tem que ter rodado o 2_Gerando_SHP.R"
estacoes_sf <- st_read("4_export/3_shape/PONTOS/L3_estacoes.shp")

est_unif_sf <- as.data.frame(estacoes_sf) %>%
  group_by(estacao) %>%
  summarise(ASS_S = sum(num_ass, na.rm = TRUE),
            geometry = geometry[1]) %>%
  ungroup() %>%
  dplyr::select(c(2,3)) %>%
  st_as_sf()

# Menos estação 13  
est_unif_sf <- est_unif_sf[-13,]

# tranformando para EPSG 32723 - projetada/WGS 84/UTM zona23S - entre 48W e 42W
estacoes_proj <- st_transform(est_unif_sf, 32723)

# Transformando de classe "sf" para classe "ppp" com janela de agua - pacote spatstat
estacoes_ppp <- as.ppp(estacoes_proj)

# Atribuindo o Window
Window(estacoes_ppp) <- as.owin(as.im(agua_ras))

# Atribuindo a unidade de metros, já que é UTM  
unitname(estacoes_ppp) <- c("metro", "metros")

# Mudando unidade para km para que o kernel seja por km²
estacoes_ppp <- rescale(estacoes_ppp, 1000, "km")

# Fazendo uma superfície de tendência para os números de assobios
estacoes_im <- Smooth(estacoes_ppp, bw.smoothppp, sigma = c(hmin=0.5, hmax=0.8), kernel = "gaussian")

# Voltando para m em X e Y. Z continua como km²; CRS como WGS 84 projetada - pacote raster
assobios_im <- raster(rescale(estacoes_im, .001, c("metro", "metros")), crs = CRS("+init=epsg:32723"))
names(assobios_im) <- "ASS"

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/4_interp/LINHA_3")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Salvando os raster do kernel
writeRaster(assobios_im,
            filename = "4_export/4_interp/LINHA_3/L3_assobios.tif",
            format = "GTiff",
            overwrite = TRUE,
            bylayer = TRUE,
            suffix = "names")


