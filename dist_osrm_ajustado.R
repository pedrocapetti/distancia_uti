library(tidyverse)
library(magrittr)
library(httr)

coord <- read_csv("final.csv")

final <- tribble(
  ~origem, ~destino, ~distancia, ~tempo
)

for(i in 1:nrow(coord)) {
  print(i)
  result <- content(GET(paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=",coord$lat_o[i],",",coord$long_o[i],"&destinations=",coord$lat_d[i],",",coord$long_d[i],"&mode=driving&key=")))
  print(result)
    if(result$rows[[1]]$elements[[1]]$status != "OK"){
      final %<>%  add_row(origem = coord$origem[i],
                          destino = coord$destino[i],
                          distancia = NaN,
                          tempo = NaN)
      } 
    else {
      final %<>%  add_row(origem = coord$origem[i],
                          destino = coord$destino[i],
                          distancia = result$rows[[1]]$elements[[1]]$distance$value,
                          tempo = result$rows[[1]]$elements[[1]]$duration$value)
    }
  }

summary(final)

write.csv(final, "mapeamento_google.csv", row.names = FALSE)

---
  final <- tribble(
    ~origem, ~destino, ~distancia, ~tempo
  )
  
  for(i in 1:nrow(coord)) {
    print(i)  
    result <- content(GET(paste0("http://router.project-osrm.org/route/v1/driving/",coord$long_o[i],",",coord$lat_o[i],";",coord$long_d[i],",",coord$lat_d[i],"?overview=false")))
    if("code" %in% names(result)) {
      if(result$code != "Ok"){
        final %<>%  add_row(origem = coord$origem[i],
                            destino = coord$destino[i],
                            distancia = NaN,
                            tempo = NaN)      
      } else {
        final %<>%  add_row(origem = coord$origem[i],
                            destino = coord$destino[i],
                            distancia = result$routes[[1]]$distance,
                            tempo = result$routes[[1]]$duration)      
      }
    }
  }
  write.csv(final,"teste_final_v4.csv", row.names = FALSE)
  