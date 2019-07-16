library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
setwd(here::here())
num_actions  <<- 3
source("./functions.R")




#df_actions  <- read_csv("./output/db_actions.csv")
#df_types  <- read_csv("./output/db_types.csv")

df_actions  <- read_csv("./output/db_actions_three_single.csv")
df_types  <- read_csv("./output/db_types_three_single.csv")

glimpse(df_actions)
glimpse(df_types)  

# For each delta, each simulation 
#ggplot(df_actions,  aes(y = proportion, x = generation, color = as.factor(action))) +
#geom_line(size=0.5) + facet_grid(delta~simulation)



## PLOT-1 Single simulation types
df_actions  %>% filter(simulation == 3)  %>% group_by(delta, efficiency_rate, mistake_rate, mutation_rate, num_agents, generation, action)  %>% 
  filter(delta %in% c(0.5,0.75,0.9))  %>%
  ggplot(aes(y = proportion, x = generation, color = as.factor(action))) +
  geom_line(size=0.25) +
  facet_grid(delta ~ .,labeller = labeller(delta=facet_labeller_delta)) +
  scale_x_continuous(limits = c(0,15000), expand = c(0.01,0.01)) +
  scale_color_manual("Action", values = c("red","turquoise2","blue"),labels = get_action_labels() )+
  ylab("Fraction of Action") +
  xlab("Generation") +
  theme_bw() +
  theme(legend.position="bottom") -> plot_actions_singlerun
  
ggsave("./images/actions_singlerun.pdf")

ggsave("./images/actions_singlerun2.jpg", plot = plot_actions_singlerun, height=5, width=8, units="in", dpi=300)




df_types  %>% filter(simulation == 3)  %>%
  filter(delta >= 0.5)  %>%
   filter(delta %in% c(0.5,0.75,0.9))  %>%
#  filter(generation <=5000)  %>%
  mutate(class = get_class(type)) -> df_types_small


rm(df_types) # Too much memory usage so we better get rid of the big data

# PLOT-2 Single simulation agents
df_types_small %>%
ggplot(aes(y = proportion, x = generation, color = class)) +
  geom_line(aes(group = type), size=0.50) +
  facet_grid(delta ~ .,labeller = labeller(delta=facet_labeller_delta)) +
  scale_color_manual(values = get_color_vector()) +
    scale_x_continuous(limits = c(0,15000), expand = c(0.01,0.01)) +
  theme_bw() +
  xlab("Generation") +
  ylab("Fraction in population") +
  theme(legend.position="bottom") ->  plot_types_singlerun

ggsave("./images/types_singlerun.pdf")

ggsave("./images/types_singlerun2.jpg", plot = plot_types_singlerun, height=5, width=8, units="in", dpi=300)


# scale_x_discrete(labels = name_labeler) +
 # scale_fill_manual(values  = c("red","blue","green","pink")) +
  scale_color_manual(values  = get_color_vector()) +
  facet_wrap(delta ~ ., labeller = labeller(delta=facet_labeller_delta)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Average fraction")



# This is the same as above but it groups by everyting except simulation and proportion, which give the same result
df_actions  %>% group_by_at(vars(-simulation, -proportion))  %>%
  filter(delta >= 0.5)  %>% 
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


df_types_small %>% filter(generation == 500 & delta == 0.5) %>%  select(type, proportion) %>% arrange(desc(proportion))

get_type_strategy(30)


df_types_agg  %>%
#    filter(delta == 0.25) %>%
    filter(generation == 10000)  %>% 
    select(generation, type, proportion) %>%
    group_by(type)  %>%
    summarize(mean_prop = mean(proportion))  %>%
    arrange(desc(mean_prop))




### Already aggregated
df_actions_agg  <- read_csv("./output/cloudoutput/db_actions_agg_delta06_cloud.csv")
df_types_agg  <- read_csv("./output/cloudoutput/db_types_agg_delta06_cloud.csv")


  ggplot(df_actions_agg, aes(y = proportion, x = generation, color = as.factor(action))) +
  geom_line(size=0.5) + facet_grid(delta ~ .)

 ggplot(df_types_agg, aes(y = proportion, x = generation, color = as.factor(type))) +
  geom_line(size=0.5) + facet_grid(delta ~ .)



output_folder = ("./output/cloudoutput500/")

db_actions_agg_data  <- bind_multiple_files(output_folder, pattern = "^db_actions_agg")
db_types_agg_data  <- bind_multiple_files(output_folder, pattern = "^db_types_agg")

db_actions_agg_data%>%
    filter(mistake_rate == 0.005)  %>%
  filter(delta >= 0.5)  %>% 
#          filter(delta == 0.55 | delta == 0.6 | delta == 0.65)  %>% 
    ggplot(aes(y = proportion, x = generation, color = as.factor(action))) +
  geom_line(size=0.5) + facet_wrap(~ delta, labeller = labeller(delta=facet_labeller_delta)) +
  scale_color_discrete(labels = get_action_labels() )

# mistake comparision
db_types_agg_data  %>%
  filter(delta == 0.95)  %>% 
  ggplot(aes(y= proportion, x = generation, color = as.factor(type))) +
  geom_line(size = 0.5, aes(linetype = as.factor(mistake_rate))) +
  facet_grid(delta ~ .)

db_actions_agg_data  %>%
  filter(delta >= 0.5)  %>% 
  ggplot(aes(y= proportion, x = generation, color = as.factor(action))) +
  geom_line(size = 0.5, aes(linetype = as.factor(mistake_rate))) +
  facet_wrap(delta ~ .) 





db_types_agg_data%>%
        filter (delta == 0.95)  %>% 
    ggplot(aes(y = proportion, x = generation, color = as.factor(type))) +
  geom_line(size=0.5) +
  facet_grid(delta ~ mistake_rate) -> testgraph

db_types_agg_data %>% filter(generation == 5000 & delta == 0.95) %>%  select(type, proportion) %>% arrange(desc(proportion))

get_type_strategy(65)


db_types_agg_data  %>% filter(generation == 5000)  %>%
  ggplot(aes(y = proportion, x = delta, color = as.factor((type)))) +
  geom_line() +
    facet_grid(. ~ mistake_rate)



# Actions evolution
# Plot 3 - Actions over delta
db_actions_agg_data  %>% filter(generation == 5000)  %>% filter(delta >= 0.5)  %>%
  filter(mistake_rate == 0.005)  %>% 
  ggplot(aes(y = proportion, x = delta, color = as.factor((action)))) +
  geom_line()   +
  geom_point() +
  scale_x_continuous(breaks = seq(0.5,1,0.05)) +
  scale_color_manual("Action", values = c("red","turquoise2","blue"),labels = get_action_labels() )+
  ylab("Average fraction") +
  xlab("Continuation probability (delta)") +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(panel.grid.minor = element_blank())

ggsave("./images/actions_over_delta.pdf")


db_types_agg_data  %>% filter(generation == 5000)  %>%  filter(mistake_rate == 0)  %>%
  filter( delta >= 0.5 )  %>%
  group_by(type, delta)  %>%
  summarise(mean_prop = mean(proportion))  %>%
  filter(mean_prop > 0.1)  %>% select(type)  %>%
  unique() -> successful_types

db_types_agg_data  %>% filter(generation == 5000) %>%
  filter(mistake_rate == 0.005)  %>%
    filter( delta >= 0.5 )  %>% 
  filter( type %in% successful_types$type)  %>% 
  ggplot(aes(y = proportion, x = as.factor(type), fill = as.factor((type)))) +
  geom_bar(stat = "identity") +
  facet_wrap(delta ~ .)

# Note to self: it makes sense to filter delta > 0.5 


# Below analysis averages last 1000 generations to reduce instant changes
db_types_agg_data  %>% filter(generation >= 3000)  %>%  filter(mistake_rate == 0.005)  %>%
  filter( delta >= 0.5 )  %>% group_by_at(vars(-proportion,-generation))  %>% summarise(proportion = mean(proportion)) -> db_types_agg_data_averaged_with_mistakes

db_types_agg_data_averaged_with_mistakes  %>%  group_by(type, delta)  %>%
  summarise(mean_prop = mean(proportion))  %>%
  filter(mean_prop > 0.01)  %>%
  select(type)  %>%
  unique() -> successful_types_with_mistakes


# Plot 3 - Types Over Delta
db_types_agg_data_averaged_with_mistakes   %>%  filter( type %in% successful_types_with_mistakes$type)  %>% mutate(class = get_class(type))  %>%
  ggplot(aes(y = proportion, x = as.factor(type), fill = class)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = name_labeler) +
 # scale_fill_manual(values  = c("red","blue","green","pink")) +
  scale_fill_manual(values  = get_color_vector()) +
  facet_wrap(delta ~ ., labeller = labeller(delta=facet_labeller_delta), ncol = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Average fraction") +
  theme(legend.position="bottom")


ggsave("./images/types_average_t05.pdf")

db_types_agg_data_averaged_with_mistakes  %>% 
  ggplot(aes(y = proportion, x = delta, color = as.factor((type)))) +
  geom_line() +
  scale_color_discrete("Types",labels = name_labeler)




# Mistakestuff

# Actions evolution
# Plot 3 - Actions over delta
db_actions_agg_data  %>% filter(generation == 5000)  %>% filter(delta >= 0.5)  %>%
  #filter(mistake_rate == 0.005)  %>% 
  ggplot(aes(y = proportion, x = delta, color = as.factor((action)))) +
  geom_line(aes(linetype = as.factor(mistake_rate)))   +
  geom_point() +
  scale_x_continuous(breaks = seq(0.5,1,0.05)) +
  scale_color_manual("Action", values = c("red","turquoise2","blue"),labels = get_action_labels() )+
  ylab("Average fraction") +
  xlab("Continuation probability (delta)") +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(panel.grid.minor = element_blank())+
  scale_linetype_discrete("Mistake rate")


ggsave("./images/actions_over_delta_with_mistakes.pdf")


## # this is the picture where x is delta and y is average fraction
## # it looks quite messy though. i will use another one below
## db_types_agg_data  %>% filter(generation == 5000)  %>% filter(delta >= 0.5)  %>%
##   #filter(mistake_rate == 0.005)  %>% 
##   ggplot(aes(y = proportion, x = delta, color = as.factor((type)))) +
##   geom_line(aes(linetype = as.factor(mistake_rate)))   +
##   geom_point() +
##   scale_x_continuous(breaks = seq(0.5,1,0.05)) +
##   ylab("Average fraction") +
##   xlab("Continuation probability (delta)") +
##   theme_bw() +
##   theme(legend.position="bottom") +
##   theme(panel.grid.minor = element_blank())+
##   scale_linetype_discrete("Mistake rate")


## Experimenting with mistake representation
# Below analysis averages last 1000 generations to reduce instant changes
db_types_agg_data  %>% filter(generation >= 3000)  %>%  #filter(mistake_rate == 0.005)  %>%
  filter( delta >= 0.5 )  %>% group_by_at(vars(-proportion,-generation))  %>% summarise(proportion = mean(proportion)) -> db_types_agg_data_averaged_both

db_types_agg_data_averaged_both  %>%  group_by(type, delta, mistake_rate)  %>%
  summarise(mean_prop = mean(proportion))  %>%
  filter(mean_prop > 0.05)  %>%
  ungroup() %>% 
  select(type)  %>%
  unique() -> successful_types_both


# Plot 3 - Types Over Delta mistake comparision
db_types_agg_data_averaged_both   %>%  filter( type %in% successful_types_both$type)  %>% mutate(class = get_class(type))  %>%
  filter(delta %in% c(0.5,0.7,0.9)) %>% 
  ggplot(aes(y = proportion, x = as.factor(type), fill = class)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = name_labeler) +
 # scale_fill_manual(values  = c("red","blue","green","pink")) +
  scale_fill_manual(values  = get_color_vector()) +
  facet_wrap(delta ~ mistake_rate , labeller = labeller(delta=facet_labeller_delta, mistake_rate = facet_labeller_mistake_rate), ncol = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Average fraction") +
  theme(legend.position="bottom")
ggsave("./images/types_average_mistake_comparision.pdf")

