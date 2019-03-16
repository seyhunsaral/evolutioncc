
# DONE : WHICH VARS TO RECORD - Actions over Simulation, Generation with parameters
# DONE : WHAT TYPE OF DATA STRUCTURE - Tidy Data
# DONE : ITERATION OVER INTERACTIONS 
# DONE : ITERATION OVER MATCHINGS
# DONE : ITERATION OVER GENERATIONS
# TODO : ITERATION OVER SIMULATIONS

# The function is written in a generalizable manner in terms of number of actions
library(here)
options(scipen=999)
setwd(here::here())
set.seed(05091946)

# Number of possible actions. This is to generalize nth-level linear prisoners dilemma
# Please note that the complexity drastically increases as the num_actions increase
# As I wanted the simulation to be easily generalizable, most of the functions depend on this value
# Therefore it needs to be assigned globally (with super-assignment operator <<) before the load of `functions.R`
num_actions  <<- 3


source('./functions.R')
efficiency_rate  <- 2
delta  <- 0.90

# number of agents should be even
num_agents  <- 500
mistake_rate  <- 0.005
mutation_rate  <- 0.01
num_generations  <- 500000

types  <- get_type_names()
num_types  <- length(types)



plot(0,0,xlim=range(0:num_generations), ylim = range(0:1))


# Creating population

agents = NULL # First generation


# Creating tables
simulation  <- 1

tbl_actions_header  <- matrix(c("delta", "efficiency_rate", "mistake_rate", "mutation_rate", "num_agents", "simulation", "generation", "action", "proportion"),nrow = 1)
write.table(tbl_actions_header, "db_actions.csv", row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 


for (generation in 1:num_generations) {

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

  points(generation, action_prop_generation[1],col = "red", cex = 0.4)
  points(generation, action_prop_generation[2],col = "yellow", cex = 0.4)
  points(generation, action_prop_generation[3],col = "green", cex = 0.4)



    
for (act in 1:num_actions){
    tbl_actions_current_line  <- matrix(c(delta, efficiency_rate, mistake_rate, mutation_rate, num_agents, simulation, generation, act-1, action_prop_generation[act]),nrow = 1)

    write.table(tbl_actions_current_line, "db_actions.csv", append = TRUE, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 




}
    
}



