library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)


df_actions  <- read_csv("db_actions.csv")
df_types  <- read_csv("db_types.csv")


glimpse(df_actions)

# For each delta, each simulation 
ggplot(df_actions,  aes(y = proportion, x = generation, color = as.factor(action))) +
geom_line(size=0.5) + facet_grid(delta~simulation)


# Averaging according to simulation
df_actions  %>% group_by(delta, efficiency_rate, mistake_rate, mutation_rate, num_agents, generation, action)  %>%
    summarize(proportion = mean(proportion))  %>%
    ggplot(aes(y = proportion, x = generation, color = as.factor(action))) +
geom_line(size=0.5) + facet_grid(~delta)

# This is the same as above but it groups by everyting except simulation and proportion, which give the same result
df_actions  %>% group_by_at(vars(-simulation, -proportion))  %>%
    summarize(proportion = mean(proportion))  %>%
    ggplot(aes(y = proportion, x = generation, color = as.factor(action))) +
geom_line(size=0.5)




    
#ggplot(df_types,  aes(y = proportion, x = generation, color = as.factor(type))) +
#geom_line(size=0.5) + facet_grid(simulation~delta)


df_types  %>%
    group_by_at(vars(-simulation, -proportion))  %>%
     summarize(proportion = mean(proportion))  %>%
    ggplot(aes(y = proportion, x = generation, color = as.factor(type))) +
geom_line(size=0.5) + facet_grid(~delta)



df_types %>% filter(generation == 500) %>%  select(type, proportion) %>% arrange(desc(proportion))

get_type_strategy(31)
