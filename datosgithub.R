
library(tidyverse)




library(tidyverse) #librerias que se necesitan
library(lubridate)




datos_mundiales <- read_csv("https://raw.github.com/owid/covid-19-data/master/public/data/jhu/full_data.csv")




#Hacer un dataframe del archivo recuperado de la base de datos
# datosmundialess <- read_csv("1614")

#hace un tidy con solo los datos que se requieren
# worlddata_tidy <- select (worlddata, 
#                           "confirmed", 
#                           "country_region", 
#                           "date", 
#                           "deaths", 
#                           "recovered")

#vector para la selección de paises con población similar
poblacion_similiar <- c("Mexico",
                        "Japan", 
                        "Russia", 
                        "Bangladesh", 
                        "Philippines")


p <- datos_mundiales %>%
  filter(location %in% poblacion_similiar) %>% 
  ggplot(aes(x = date, y = new_cases, color = location)) +
  geom_line()

plotly::ggplotly(p)

datos_mundiales %>%
  ggplot(aes(x = date, y = new_cases, group = location)) +
  geom_line(color = "grey") +
  geom_line(data = datos_mundiales %>% filter(location %in% poblacion_similiar),
            aes(color = location), size = 1) +
  scale_y_log10()
