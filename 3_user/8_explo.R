
# Limpando o Global Environment 
rm(list = ls())

library(sf)
library(sp)
library(raster)
library(rgdal)
library(spatstat)


library(maptools)
library(spatialEco)
library(data.table)
library(dplyr)




# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua (tem que ter feito o "2_Gerando_SHP.R")
agua_noite_sf <- st_read(paste0(pasta_proj, "/1_data/SHAPES_AUX/Agua_L3_noite.shp"))

# tranformando para EPSG 32723 - projetada/WGS 84/UTM zona23S - entre 48W e 42W
agua_noite_proj <- st_transform(agua_noite_sf, 32723)

# criando raster da agua transformada para projetada
agua_noite_ras <- raster(agua_noite_proj)

# definindo resolução de 250m
res(agua_noite_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_noite_ras <- rasterize(agua_noite_proj, agua_noite_ras)

# Abrindo shape dos grupos, tem que ter rodado o 2_Gerando_SHP.R"
estacoes_sf <- st_read("4_export/3_shape/PONTOS/L3_estacoes.shp")

est_res <- as.data.frame(estacoes_sf) %>%
  group_by(estacao) %>%
  summarise(n_grav = n())

est_res <- as.data.frame(estacoes_sf) %>%
  filter(!is.na(estacoes_sf$num_ass)) %>%
  group_by(estacao) %>%
  summarise(n_grav_ass = n(),
            num_ass_S = sum(num_ass, na.rm = TRUE)) %>%
  right_join(est_res, by = "estacao")

est_res <- as.data.frame(estacoes_sf) %>%
  filter(!is.na(estacoes_sf$num_ass),
         periodo == "D") %>%
  group_by(estacao) %>%
  summarise(n_grav_ass_D = n(),
            num_ass_S_D = sum(num_ass, na.rm = TRUE)) %>%
  right_join(est_res, by = "estacao")
  
est_res <- as.data.frame(estacoes_sf) %>%
  filter(!is.na(estacoes_sf$num_ass),
         periodo == "N") %>%
  group_by(estacao) %>%
  summarise(n_grav_ass_N = n(),
            num_ass_S_N = sum(num_ass, na.rm = TRUE)) %>%
  right_join(est_res, by = "estacao") %>%
  arrange(estacao)


est_res

# RASTER PRINCIPAL
est_unif_sf <- as.data.frame(estacoes_sf) %>%
  filter(estacao == c("1","2","3","10","11","12","14")) %>%
  group_by(estacao)

View(est_unif_sf)
View(estacoes_sf)

pontos_com_gravacao <- bd_L3$gravacoes %>%
  group_by(estacao) %>%
  summarise(num_arquivos_grav = n())


pontos_com_assobio <- bd_L3$gravacoes[bd_L3$gravacoes$arquivo_wav %in% bd_L3$assobios$arquivo_wav,] %>%
  group_by(estacao) %>%
  summarise(num_arquivos_ass = n()) %>%
  right_join(pontos_com_gravacao, by = "estacao")

bd_L3$assobios %>%
  group_by(estacao) %>%
  summarise(ASS_S = n())

  filter(estacao == c(1,2,3,10,11,12,14))
  
  summarise()


  ,
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
estacoes_ppp <- rescale.ppp(estacoes_ppp, 1000, "km")

# Fazendo uma superfície de tendência para os números de assobios
estacoes_im <- Smooth(estacoes_ppp, bw.smoothppp, sigma = c(hmin=0.5, hmax=0.8), kernel = "gaussian")

# Voltando para m em X e Y. Z continua como km²; CRS como WGS 84 projetada - pacote raster
assobios_im <- raster(rescale.im(estacoes_im, .001, c("metro", "metros")), crs = CRS("+init=epsg:32723"))
names(assobios_im) <- "ASS"

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/4_interp/LINHA_3")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Salvando os raster PRINCIPAL do kernel
writeRaster(assobios_im,
            filename = "4_export/4_interp/LINHA_3/L3_assobios.tif",
            format = "GTiff",
            overwrite = TRUE,
            bylayer = TRUE,
            suffix = "names")
