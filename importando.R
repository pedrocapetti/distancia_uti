library(tidyverse)
library(jsonlite)
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(httr)
library(geobr)
library(tidyverse)
library(magrittr)
library(httr)
library(geosphere)

#Subindo as bases de dados de coordenadas dos centros das cidades e também a base de leitos de UTI do SUS com base no referencial de abril
coordenadas_fonte <- fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vRIEaR6A-i5WGtFbGFMRPmTdj5MXt3ulncrvxCihzE_kMDrz89Swjkdc9IZLQgIjN-cqzRHsoUTb216/pub?output=csv", encoding = "UTF-8")
leitos <- fread("D:/Documentos/covid/datasus/s_leito_abr.csv", encoding = "UTF-8")

#Filtrando apenas as cidades que possuem leito SUS
leitos <- leitos %>% filter(Quantidade_SUS > 0)

#Juntando as duas bases
coordenada_leito <- left_join(leitos, coordenadas_fonte, by = c("cod_ibge" = "codigo"))

#Salvando uma versão da tabela com coordenada
write.csv(coordenada_leito, "coordenadas_leitos_sus_abril.csv", row.names = FALSE)

#Vamos agora fazer os pares para calcular a distância
#Lembrando que qualquer alteração que precisar ser feita nos modelos de distância, como atualização da nova distribuição de leitos, devem ser feitos com filtro na tabela DESTINO, uma vez que nela se encontram as cidades onde essas pessoas serão atendidas

origem <- fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vRIEaR6A-i5WGtFbGFMRPmTdj5MXt3ulncrvxCihzE_kMDrz89Swjkdc9IZLQgIjN-cqzRHsoUTb216/pub?output=csv", encoding = "UTF-8")

#Lembre-se de alterar essa base aqui, com o conjunto de possibilidades para os municípios de destino
destino <- fread("coordenadas_leitos_sus_abril.csv")

#Criando uma tabela para armazenarmos os pares em seguida
final <- tribble(
  ~origem, ~destino, ~distancia
)

#Vamos fazer uma requisição para retornar a distância euclidiana mínima entre os dois pontos. Isso facilitará na hora da identificação dos pares, trocando um cálculo de 5570! para 5570. São três meses por apenas 20 minutos!!!!!!

for(i in 1:nrow(origem)) {
  dist = 10000000000000
  tmp = NULL
  for(j in 1:nrow(destino)) {
    km <- distHaversine(c(origem$xcoord[i],origem$ycoord[i]),c(destino$xcoord[j],destino$ycoord[j]))
    if(km < dist){
      dist <- km
      tmp <- j
    }
  }
  final %<>%  add_row(origem = origem$codigo[i],
                      destino = destino$cod_ibge[tmp],
                      distancia = dist)
}

#Vamos agora colocar latitude e longitude nas coordenadas de origem e destino
#Trocando os nomes das colunas para padronizar
#Primeiro vamos cruzar a origem, em seguida o destino
final <- left_join(final, coordenadas_fonte, by = c("origem" = "codigo"))
colnames(final)[5] <- "long_o"
colnames(final)[6] <- "lat_o"

final <- left_join(final, coordenadas_fonte, by = c("destino" = "codigo"))
final$nome.y <- NULL

colnames(final)[7] <- "long_d"
colnames(final)[8] <- "lat_d"
colnames(final)[4] <- "nome"

write.csv(final, "final_abril.csv", row.names = FALSE)
