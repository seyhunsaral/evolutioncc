library(here)
options(scipen=999)
setwd(here::here())
set.seed(18090212)

# Number of possible actions. This is to generalize nth-level linear prisoners dilemma
# Please note that the complexity drastically increases as the num_actions increase
# As I wanted the simulation to be easily generalizable, most of the functions depend on this value
# Therefore it needs to be assigned globally (with super-assignment operator <<) before the load of `functions.R`

num_actions  <<- 3


source('./functions.R')
types  <- get_type_names()
actions  <- get_actions()
num_types  <- length(types)
num_actions  <- length(actions)
delta_range  <- c(0.8)
output_types = "./output/db_types_agg.csv"
output_actions = "./output/db_actions_agg.csv"

initiate_output_files_agg(types_filename = output_types, actions_filename = output_actions)

# number of agents should be even
num_agents  <- 200
mistake_rate  <- 0.005
mutation_rate  <- 0.01
num_generations  <- 15000 
num_simulations  <- 10
efficiency_rate  <- 3

#delta_range  <- c(0.25, 0.33, 0.50, 0.66, 0.75, 0.85, 0.90, 0.95)

simulation_weight  <- 1 / num_simulations
# These are for warnings etc. 
message_percent  <- 25
message_steps  <- num_generations * message_percent / 100


# Inititate data files


for (delta in delta_range ) {
message("delta:", delta, " in range ", delta_range)
   # Data files for simulation
  aggregated_actions  <- replicate(num_generations, rep(0, num_actions), simplify = FALSE)  
  aggregated_types  <- replicate(num_generations, rep(0, num_types), simplify = FALSE)  


  
  for (simulation in 1:num_simulations) {
    message("simulation:", simulation, " out of ", num_simulations, "  Time ",Sys.time())

#    message(simulation)
    agents = NULL # NULL indicates the generation function that it is the first generation

    for (generation in 1:num_generations) {
 
      agents  <- generate_agents(num_agents = num_agents, all_types = types, agent_table = agents, mutation_prob = mutation_rate)
      matchings  <- create_matching(num_agents)
      num_matchings  <- dim(matchings)[1]
  
      actions_prop_generation <- rep(0,num_actions)
      
      for (current_matching_line in 1:num_matchings) {
        current_matching <-matchings[current_matching_line,]
        num_interactions  <- draw_num_interactions(delta)

        # reset action frequencies
        action_frequencies  <- rep(0,num_actions)

        #Setting The Initial Reaction

        previous_action<--1

        for (intr in 1:num_interactions) {  
          # random selection of the first mover
          mover<- sample(current_matching,1)
          receiver  <- current_matching[current_matching!=mover]
          #for (i in 1:number_of_interactions) { 
          # action of the current player
          action <- react(agents[mover,"type"],opponent_action = previous_action, mistake_rate = mistake_rate)
          
          action_frequencies[action+1]  <- action_frequencies[action+1] + 1 # +1 is the usual 0,1,2
          
          #recording the moves
          #todo mcmovefreq[move+1,gen]=mcmovefreq[move+1,gen]+1
          #assigning payoffs

          current_payoffs<-get_payoffs(action, efficiency_rate)
          #movers payoff

          agents[mover, "payoff"]  <- agents[mover, "payoff"] + current_payoffs["mover"]
          #counterparts payoff
          agents[receiver,"payoff"] <- agents[receiver,"payoff"] + current_payoffs["receiver"]

          
          #some verbose summary
          #        print(intr)
          #        print(paste0("mover: ", as.character(mover),"(",as.character(agents[mover,"type"]), ") | receiver: ", as.character(receiver),"(",as.character(agents[receiver,"type"]), ")"))
          #        print(paste0("mover plays ", as.character(action)))
          #        print(agents[current_matching,])

          #Switches the Mover here
          mover<- receiver
          receiver  <- current_matching[current_matching!=mover]
          previous_action <- action


        }

        # Action frequencies stands for each 
        actions_prop_generation  <- actions_prop_generation + action_frequencies/(num_interactions * num_matchings)
      }
 


      # Writing to data
      
      types_prop_generation <- table(factor(agents[,"type"], levels= types)) / num_agents


      # Adding to aggregated
      aggregated_actions[[generation]]  <- aggregated_actions[[generation]] + actions_prop_generation * simulation_weight
      aggregated_types[[generation]]  <- aggregated_types[[generation]] + types_prop_generation * simulation_weight

    
    }
  }




write_to_file(file_name = output_actions,
              delta = delta,
              efficiency_rate = efficiency_rate,
              mistake_rate = mistake_rate,
              mutation_rate = mutation_rate,
              num_agents = num_agents,
              category_vector = actions,
              proportion_list = aggregated_actions
              )

write_to_file(file_name = output_types,
              delta = delta,
              efficiency_rate = efficiency_rate,
              mistake_rate = mistake_rate,
              mutation_rate = mutation_rate,
              num_agents = num_agents,
              category_vector = types,
              proportion_list = aggregated_types
              )





}
message('done!')


# TODO: Create two empty lists with number of generations: one for actions, one for types
# TODO: Weight actions_prop_generation and types_prop_generation according to the number of simulations
# TODO: Add to the current list
