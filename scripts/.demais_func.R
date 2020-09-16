# - Função para filtrar tudo por um dia só. Ainda criando. Depois fz uma pra filtrar por intervalo

filtra_dia  <- function(dia_em_dma, lista) {
  library(lubridate)
  
  saida_sel <- fdatabase$fsaidas[[ which(fdatabase$fsaidas$data == dmy(dia_em_dma)),"saida"]]
  
  
  fsaidas_sel <- fdatabase$fsaidas[which(fdatabase$fsaidas$saida==saida_sel),]
  
  fclima_sel <- fdatabase$fclima[which(fdatabase$fclima$saida==saida_sel),]
  
  favistagens_sel <- fdatabase$favistagens[which(fdatabase$favistagens$saida==saida_sel),]
  
  ffotos_sel <- fdatabase$ffotos[which(fdatabase$ffotos$saida==as.character.factor(saida_sel)),]
  
  fsonda_sel <- fdatabase$fsonda[which(fdatabase$fsonda$saida==saida_sel),]
  
  frota_sel <- fdatabase$frota[which(fdatabase$frota$saida==saida_sel),]
  
  
  
  lista_sel <- list(fsaidas_sel = fsaidas_sel,
                    fclima_sel = fclima_sel, 
                    favistagens_sel = favistagens_sel,
                    ffotos_sel = ffotos_sel,
                    fsonda_sel = fsonda_sel,
                    frota_sel = frota_sel)
                    
  dia_filtrado <<- dia_em_dma
  saida_filtrada <<- saida_sel
  
  return(lista_sel)
}
