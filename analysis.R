library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
df_actions  <- read_csv("test_actions.csv")
glimpse(df_actions)

ggplot(df_actions,  aes(y = proportion, x = generation, color = as.factor(action))) +
geom_line(size=0.05)
