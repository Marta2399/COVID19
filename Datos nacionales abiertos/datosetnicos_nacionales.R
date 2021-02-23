library(tidyverse)
data_raw <- read_csv("200430COVID19MEXICO.csv")

ggplot(data = data_raw, mapping = aes(x = HABLA_LENGUA_INDIG, fill = INTUBADO)) +
  geom_bar()

data_raw %>% glimpse()
data_raw %>% 
  mutate_at(
    HABLA_LENGUA_INDIG = as_factor(HABLA_LENGUA_INDIG)
  ) %>% 
  ggplot(aes(x = HABLA_LENGUA_INDIG, fill = INTUBADO))