
library(tidyverse)




library(tidyverse) #librerias que se necesitan
library(lubridate)




x < -https://github.com/owid/covid-19-data/blob/master/public/data/jhu/full_data.csv




#Hacer un dataframe del archivo recuperado de la base de datos
datosmundialess <- read_csv("1614")

# cambiar a formato timestamp a fecha
worlddata <- datosmundialess %>% 
  mutate(
    date = as.POSIXct(timestamp, origin = "1970-01-01")
  ) 

#hace un tidy con solo los datos que se requieren
worlddata_tidy <- select (worlddata, 
                          "confirmed", 
                          "country_region", 
                          "date", 
                          "deaths", 
                          "recovered")

#vector para la selección de paises con población similar
poblacionsimiliar <- c( "Mexico",
                        "Japan", 
                        "Russia", 
                        "Bangladesh", 
                        "Philippines")

#filtrado para la comparación
comparison <- filter (worlddata_tidy, 
                      country_region %in% poblacionsimiliar
)
#grafica comparativa
p <- ggplot(data = comparison) +
  geom_line(mapping = aes(x = date, y = confirmed, color = country_region) )

plotly::ggplotly(p)