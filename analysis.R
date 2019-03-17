library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

num_actions  <<- 3
source("./functions.R")

#df_actions  <- read_csv("./output/db_actions.csv")
#df_types  <- read_csv("./output/db_types.csv")

df_actions  <- read_csv("./output/db_actions-2019-03-17.csv")
df_types  <- read_csv("./output/db_types-2019-03-17.csv")

glimpse(df_actions)
glimpse(df_types)

# For each delta, each simulation 
#ggplot(df_actions,  aes(y = proportion, x = generation, color = as.factor(action))) +
#geom_line(size=0.5) + facet_grid(delta~simulation)


# Averaging according to simulation
df_actions_agg  <- df_actions  %>% filter(simulation == 1)  %>% group_by(delta, efficiency_rate, mistake_rate, mutation_rate, num_agents, generation, action)  %>%
  summarize(proportion = mean(proportion))

  ggplot(df_actions_agg, aes(y = proportion, x = generation, color = as.factor(action))) +
  geom_line(size=0.5) + facet_grid(delta ~ .)

# This is the same as above but it groups by everyting except simulation and proportion, which give the same result
df_actions  %>% group_by_at(vars(-simulation, -proportion))  %>%
  summarize(proportion = mean(proportion))  %>%
  ggplot(aes(y = proportion, x = generation, color = as.factor(action))) +
  geom_line(size=0.5)  + facet_grid(delta)





#ggplot(df_types,  aes(y = proportion, x = generation, color = as.factor(type))) +
#geom_line(size=0.5) + facet_grid(simulation~delta)


df_types_agg <- df_types  %>%  filter(simulation == 1)  %>% 
  group_by_at(vars(-simulation, -proportion))  %>%
  summarize(proportion = mean(proportion))

  ggplot(df_types_agg, aes(y = proportion, x = generation, color = as.factor(type))) +
  geom_line(size=0.5) + facet_grid(delta ~ .)



df_types_agg  %>% ungroup()  %>% filter(delta == 0.25 & proportion > 0.90)  %>% select(type) %>% unique()  %>% print(n = 30)


df_types %>% filter(generation == 4000 & delta == 0.25) %>%  select(type, proportion) %>% arrange(desc(proportion))

get_type_strategy(3)


df_types_agg  %>%
    filter(delta == 0.25) %>%
    select(generation, type, proportion) %>%
    group_by(type)  %>%
    summarize(mean_prop = mean(proportion))  %>%
    arrange(desc(mean_prop))
