library(tidyverse)

delitos <- read_csv("delitos2015-2021.csv")

feminicidios <- delitos %>%
  select( Ano, Entidad, Tipo_de_delito, Enero:Diciembre ) %>%
  filter( Tipo_de_delito == "Feminicidio") %>%
  
  group_by(Entidad)
  
#feminicidios
t <- mutate( feminicidios, sum(Enero:Diciembre))

#sumademeses <-feminicidios %>%
 # all <- transmut

feminicidios %>% pivot_longer(
  cols = Enero:Diciembre ,
  names_to = "Meses",
  values_to = "count"
)