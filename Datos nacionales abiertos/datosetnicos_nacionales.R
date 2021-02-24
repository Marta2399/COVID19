library(tidyverse)

data_raw <- read_csv("Datos nacionales abiertos/200430COVID19MEXICO.csv")

data_raw %>% glimpse()

data_raw %>% 
  mutate_at(
    .vars = c("HABLA_LENGUA_INDIG", "INTUBADO"),
    .funs = as_factor
  ) %>% 
  ggplot(aes(x = HABLA_LENGUA_INDIG, fill = INTUBADO)) +
  geom_bar()
