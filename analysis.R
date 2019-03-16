library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
df_actions  <- read_csv("db_actions.csv")
df_types  <- read_csv("db_types.csv")


glimpse(df_actions)

ggplot(df_actions,  aes(y = proportion, x = generation, color = as.factor(action))) +
geom_line(size=0.5)


glimpse(df_types)

ggplot(df_types,  aes(y = proportion, x = generation, color = as.factor(type))) +
geom_line(size=0.5)

df_types %>% filter(generation == 7500) %>%  select(type, proportion) %>% arrange(desc(proportion))

get_type_strategy(56)
