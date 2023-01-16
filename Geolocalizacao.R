# Definido a rota e recebendo as bibliotecas
setwd(paste(getwd(), "/Dados", sep=""))
# setwd("Caminho")
getwd()

# Verificar se os pacotes foram instalados:
my_packages <- list(require ("rgdal"), require ("geojsonio"), require("htmlwidgets"), require("plotly"),
                    require ("dplyr"), require("RODBC"))
if(length(unique(my_packages)) != 1) {
  install.packages('rgdal')  
  install.packages('geojsonio')  
  install.packages('htmlwidgets')  
  install.packages('plotly') 
  install.packages('dplyr')  
  install.packages('RODBC')  
}
# Utilizar as bibliotecas
library(rgdal)
library(geojsonio)
library(htmlwidgets)
library(plotly)
library(sp)
library(dplyr)

# Importando o arquivo geojson
Regiao <- rgdal::readOGR("Arquivo.geojson", encoding = "UTF-8", use_iconv=TRUE, layer = "Bairros", require_geomType = "wkbPolygon")  
# Converter para dataframe
Regioes_DF <- fortify(Regiao, region = "name")
View(Regioes_DF)

# Tratar os dados,removendo dados sem coordenadas  
dataset<- na.omit(dataset[, c('long', 'lat')])
View(dataset)
# Importante forçar a conversão para numérico para o funcionamento do algoritmo
dataset[c("Long", "Lat")] <- sapply(dataset[c("Long", "Lat")],as.numeric)

# Criação da função para receber as coordenadas e retornar rotulada a ragião pertecente de acordo com a área dentro do arquivo geojson 
geolocalizacao_get = function (dataset_geojson,dataset_alvo) {
  
  # Criação do dataframe final que vai receber o dataset origonal junto com as regiões rotuladas
  tabela_final <- data.frame(matrix(nrow = 0, ncol = 0))
  # Laço de repetição para percorrer por todos os polígonos (regiões) do arquivo geojson
  for (Name_Regiao in as.list(dataset_geojson %>%  distinct(id))[[1]]) {
    # Primeiro, procurar a regiao presente no dataset_geojson normalmente na ordem alfabética
    dataset_det_regiao<- subset(dataset_geojson, dataset_geojson$id == Name_Regiao)
    
    # Criar uma tabela onde as coordenadas de uma determinada região corresponda próximadamente ao polígono baseado em long e lat
    Position_Coord <- which(
      dataset_alvo$Long >= min(dataset_det_regiao$long) & # oeste
      dataset_alvo$Long <= max(dataset_det_regiao$long) & # leste
      dataset_alvo$Lat <= max(dataset_det_regiao$lat) & # norte
      dataset_alvo$Lat >= min(dataset_det_regiao$lat) )# sul
    # Fazer o relacionamento das coordenadas que se encaixe dentro do polígono, assim sugindo uma nova tabela
    Position_Scale_P <- dataset_alvo[Position_Coord, 1:ncol(dataset_alvo) ]
    # Logo criar o polígono da regiao (dataset_det_regiao), bibliotec sp
    Polygon_Regiao <<- SpatialPolygonsDataFrame(
      SpatialPolygons(list(Polygons(list(Polygon(dataset_det_regiao[, c('long', 'lat')])), 1)
      )), data = data.frame(ID = 1), match.ID = FALSE)
    
    # Finalmente fazer a consulta de achar os pontos que estão dentro ou não do polígono área da região
    # 1 para sim , NA para não
    Geolocalizado <- over(SpatialPoints(Position_Scale_P[, c('Long', 'Lat')]), Polygon_Regiao)
    # Há alguma região (polígono) não encontrada, vai para próxima
    if (nrow(na.omit(Geolocalizado)) == 0) {
      next  
    }
    
    # Adicionar a coluna Localizado na tabela  
    Position_Scale_P[, "Localizado"] <- c(Geolocalizado$ID)

    # Remover linha com NA, ou seja, aqueles  não localizados na área da região abragente do arquivo geojson
    Position_Scale_P <- Position_Scale_P %>% drop_na(Localizado)
    
    # Criar a coluna Regiao e preenchê-la com os registros localizados no Polígono
    Position_Scale_P[, "Regiao"] <- Name_Regiao
    
    # Já que se localizou, então excluir a coluna Localizado
    Position_Scale_P["Localizado"] <- NULL
    
    # Juntando as linhas até o final do laço
     tabela_final <- bind_rows(tabela_final, Position_Scale_P)
  }
  # Retornar com o dataset geolocalizado e rotulada a área com base nas coordenadas
  return(data.frame(tabela_final))
}
dataset_final <- geolocalizacao_get(Regioes_DF, dataset)
View(dataset_final)
