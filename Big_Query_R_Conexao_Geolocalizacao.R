# Caminho da pasta do arquivo geojson
setwd("")
getwd()

# conexão Big Query para adquirir dados do Waze for City
# Verificar se os pacotes foram instalados:
my_packages <- list(require("rgdal"), require("geojsonio"), require("stringr"), require("sp"),require("ggplot2"), require ("dplyr"), 
                    require("tidyr"), require("RODBC"), require("bigrquery"))
if(length(unique(my_packages)) != 1) {
  install.packages('rgdal')  
  install.packages('geojsonio')  
  install.packages('stringr')  
  install.packages('sp')
  install.packages('ggplot2') 
  install.packages('dplyr')  
  install.packages('tidyr')
  install.packages('RODBC')
  install.packages('bigrquery') # necesssário para a conexão do bigquery
  
}

library(rgdal)
library(geojsonio)
library(stringr)
library(sp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RODBC) 
library(bigrquery) # necesssário para a conexão do bigquery

tryCatch(
  expr = {
    projectid = "idprojeto" # colocar o id do projeto encontrado no Big Query
    bq_auth(path = "arquivo.json") # o arquivo em json com as credencias do usuário de big query
    # Consulta
    sql <- "" # fazer a consulta em SQL
     
    # Conexão com o banco
    tb <- bq_project_query(projectid, sql)
    # Fazer o downloard da tabela vindo do Big Query
    sample <-bq_table_download(tb)
    View(sample)
    # Tratamento dos dados
    # remover point na coluna geo
    sample$geo = str_sub(sample$geo, 6)
    
    # remover parentêses
    sample$geo = gsub("([()])","", sample$geo)
    
    # dividir a coluna com as localizações em duas: Long e Lat 
    sample <- sample %>% extract(geo, c("Long", "Lat"), "([^,]+) ([^)]+)")
    View(sample)
    
    # deletar uma outra coluna repetida 
    sample$geoWKT <- NULL
    # converter de string para numerica
    sample[c("Long", "Lat")] <- sapply(sample[c("Long", "Lat")],as.numeric)
    
    # Importar o shape do mapa para coordenar
    Regiao <- rgdal::readOGR("Arquivo.geojson", encoding = "UTF-8", use_iconv=TRUE, layer = "Bairros", require_geomType = "wkbPolygon")  
    
    #Converter para dataframe
    Regioes_DF <- fortify(Regiao, region = "name")
    
    
    geolocalizacao_get = function (dataset_regiao,dataset_bgq) {
      
      # Criação do dataframe final que vai receber o dataset origonal junto com as regiões rotuladas
      tabela_final <- data.frame(matrix(nrow = 0, ncol = 0))
      for (Name_Regiao in as.list(Coord_SdeP %>%  distinct(id))[[1]]) {
        # Primeiro, procurar a regiao presente no dataset_geojson normalmente na ordem alfabética
        dataset_regiao<- subset(Coord_SdeP, Coord_SdeP$id == Name_Regiao)
        # Criar uma tabela onde as coordenadas de uma determinada região corresponda próximadamente ao polígono baseado em long e lat
        Position_Coord <- which(
          dataset_bgq$Long >= min(dataset_regiao$long) & # oeste
            dataset_bgq$Long <= max(dataset_regiao$long) & # leste
            dataset_bgq$Lat <= max(dataset_regiao$lat) & # norte
            dataset_bgq$Lat >= min(dataset_regiao$lat) )# sul
        
        # Fazer o relacionamento das coordenadas que se encaixe dentro do polígono, assim sugindo uma nova tabela
        Position_Scale_P <- dataset_bgq[Position_Coord, 1:ncol(dataset_bgq) ]
        # Há alguma região (polígono) não encontrada, vai para próxima
        if (nrow(na.omit(Position_Scale_P)) == 0) {
          next  
        }
        # Logo criar o polígono da regiao (dataset_det_regiao), bibliotec sp
        Polygon_Bairro <<- SpatialPolygonsDataFrame(
          SpatialPolygons(list(Polygons(list(Polygon(dataset_regiao[, c('long', 'lat')])), 1)
          )), data = data.frame(ID = 1), match.ID = FALSE)
        # Finalmente fazer a consulta de achar os pontos que estão dentro ou não do polígono área da região
        # 1 para sim , NA para não
        Geolocalizado <- over(SpatialPoints(Position_Scale_P[, c('Long', 'Lat')]), Polygon_Bairro)
        # Mais uma verificação
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
        # Juntando as linhas aos poucos
        tabela_final <- bind_rows(tabela_final, Position_Scale_P)
      }
      
      # Juntar Tabela final
      # Excluir aqueles que não foram localizados para colocar Outros Municípios 
      dataset_bgq <- filter(dataset_bgq, !(uuid %in% tabela_final$uuid))
      
      # Colocar Bairro como Outros Municípios
      dataset_bgq[,"Regiao"] <- "Outros Municípios"
      
      # Juntar Tabela final
      tabela_final <- bind_rows(dataset_bgq, tabela_final)
      return(data.frame(tabela_final))
    }
    dataset_final <- geolocalizacao_get(Regioes_DF, sample)
    View(dataset_final)
  },
  error = function(e){ 
    
  }
)

