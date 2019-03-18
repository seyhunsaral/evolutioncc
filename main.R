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

initiate_output_files()

# number of agents should be even
num_agents  <- 200
mistake_rate  <- 0.005
mutation_rate  <- 0.01
num_generations  <- 50000
num_simulations  <- 300
efficiency_rate  <- 3



delta_range  <- c(0.25, 0.33, 0.50, 0.66, 0.75, 0.85, 0.90, 0.95)



message_percent  <- 25
message_steps  <- num_generations * message_percent / 100


for (simulation in 1:num_simulations) {

  message('=========== ',Sys.time(), ' =============')
  
  message('Simulation ',simulation, ' out of ', num_simulations)

  if (simulation == 1) {
    first_time_measure  <- proc.time()
  }
  else {
    time_elapsed  <-  proc.time()[3] - first_time_measure[3]
    simulation_left  <- num_simulations - simulation + 1
    average_time_per_simulation  <-  time_elapsed / (simulation - 1) 
    expected_remaining_time  <- ceiling((simulation_left * average_time_per_simulation)/60)
    message('      expected remaining time:', expected_remaining_time, ' mins')
    }
  
  for (delta in delta_range ) {

#    message('    -> current delta ', delta)

    time  <- proc.time()
    # Enable plot here
    #    plot(NA,NA,xlim=range(0:num_generations), ylim = range(0:1))

    agents = NULL # NULL indicates the generation function that it is the first generation


    for (generation in 1:num_generations) {
      
#      if (generation %% message_steps  == 0) {message('    -> (',(generation/num_generations)*100,'% current generation: ', generation, ' out of ', num_generations)}

      
      agents  <- generate_agents(num_agents = num_agents, all_types = types, agent_table = agents, mutation_prob = mutation_rate)
      matchings  <- create_matching(num_agents)
      num_matchings  <- dim(matchings)[1]
      # Tables



      # Recording history
      action_prop_generation <- rep(0,num_actions)
      


      
      
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
        action_prop_generation  <- action_prop_generation + action_frequencies/(num_interactions * num_matchings)
      }
      #print(c(generation, action_prop_generation))
      #print(head(agents[order(agents[,"payoff"], decreasing = TRUE),]))


      # ENABLE PLOT HERE
      #points(generation, action_prop_generation[1],col = "red", cex = 0.4)
      #points(generation, action_prop_generation[2],col = "yellow", cex = 0.4)
      #points(generation, action_prop_generation[3],col = "green", cex = 0.4)



      
      ## for (act in 1:num_actions){
      ##   tbl_actions_current_line  <- matrix(c(delta, efficiency_rate, mistake_rate, mutation_rate, num_agents, simulation, generation, act-1, action_prop_generation[act]),nrow = 1)

      ##   write.table(tbl_actions_current_line, "db_actions.csv", append = TRUE, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 
      ## }


      # Writing to data
      types_prop_generation <- table(factor(agents[,"type"], levels= types))

      tbl_types_current_gen  <- data.frame(delta = delta, efficiency_rate = efficiency_rate, mistake_rate = mistake_rate, mutation_rate = mutation_rate, num_agents = num_agents, simulation = simulation, generation = generation, type = names(types_prop_generation), prop = as.numeric(types_prop_generation)/num_agents)

      write.table(tbl_types_current_gen, "./output/db_types.csv", append = TRUE, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 



      tbl_actions_current_gen  <- data.frame(delta = delta, efficiency_rate = efficiency_rate, mistake_rate = mistake_rate, mutation_rate = mutation_rate, num_agents = num_agents, simulation = simulation, generation = generation, action = actions, prop = action_prop_generation)

      write.table(tbl_actions_current_gen, "./output/db_actions.csv", append = TRUE, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 



      
    }
  }
}
message('done!')
