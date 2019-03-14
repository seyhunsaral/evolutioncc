# The function is written in a generalizable manner in terms of number of actions
library(here)
setwd(here::here())

# These are special variables which the functions take input from
num_actions  <<- 3
efficiency_rate  <<- 2
source('./functions.R')

delta  <- 0.8

# number of agents should be even
num_agents  <- 1000
types  <- get_type_names(num_actions)
num_types  <- length(types)

# Creating population
agents  <- generate_agents(num_agents = num_agents, all_types = types)
matchings  <- create_matching(1000)
# print(agents)
round  <- 1

current_matching_line  <- 1
current_matching <-matchings[current_matching_line,]

#Setting The Initial Reaction
previous_action<--1

num_interactions  <- draw_num_interactions(delta)

num_interactions  <- 10 # THIS IS TEMPORARY

for (intr in 1:num_interactions) {  
      # random selection of the first mover
      mover<- sample(current_matching,1)
      receiver  <- current_matching[current_matching!=mover]
      #for (i in 1:number_of_interactions) { 
        # action of the current player
        action <- react(agents[mover,"type"],opponent_action = previous_action)
        #recording the moves
  #todo mcmovefreq[move+1,gen]=mcmovefreq[move+1,gen]+1
        #assigning payoffs
        current_payoffs<-get_payoffs(action)
        #movers payoff
        agents[mover, "payoff"]  <- agents[mover, "payoff"] + current_payoffs["mover"]
        #counterparts payoff
        agents[receiver,"payoff"] <- agents[receiver,"payoff"] + current_payoffs["receiver"]
        #some verbose summary
        print(intr)
        print(paste0("mover: ", as.character(mover),"(",as.character(agents[mover,"type"]), ") | receiver: ", as.character(receiver),"(",as.character(agents[receiver,"type"]), ")"))
        print(paste0("mover plays ", as.character(action)))
        print(agents[current_matching,])
        #Switches the Mover here
        mover<- receiver
        receiver  <- current_matching[current_matching!=mover]
        previous_action <- action
}




# TODO : WHICH VARS TO RECORD
# TODO : WHAT TYPE OF DATA STRUCTURE
# TODO : ITERATION OVER INTERACTIONS
# TODO : ITERATION OVER MATCHINGS
# TODO : ITERATION OVER GENERATIONS
      #}





### I STARTED THIS PART THAT IT WAS SOLVED IN A MORE ELEGANT WAY IN THE PREVIOUS CODE
## 

## current_matching  <- matchings[1,]

## first_mover  <- current_matching[1]
## second_mover  <- current_matching[2]

## type_first_mover  <- agents[first_mover, "type"]
## type_second_mover  <- agents[second_mover, "type"]

## type_first_mover  <- 80
## type_second_mover  <- 0

## num_interactions  <- draw_num_interactions(delta)                       

### THAT WILL PROBABLY GO
## # initial reaction 
## reaction_second  <- -1

## # for (i in 1:num_interactions) {
## interaction  <- 1

## reaction_first  <- react(type = type_first_mover, opponent_action = reaction_second)
## print(reaction_first)
## payoffs_round  <- get_payoffs(reaction_first)
## print(payoffs_first_move)
## agents[first_mover, "payoff"]  <- agents[first_mover, "payoff"] + payoffs_round[1]
## agents[second_mover, "payoff"]  <- agents[second_mover, "payoff"] + payoffs_round[2]
## agents[first_mover,]
## agents[second_mover,]


## reaction_second  <- react(type = type_second_mover, opponent_action = reaction_first)
## print(reaction_second)
## payoffs_round  <- get_payoffs(reaction_second)
## print(payoffs_second_move)
## agents[first_mover, "payoff"]  <- agents[first_mover, "payoff"] + payoffs_round[2]
## agents[second_mover, "payoff"]  <- agents[second_mover, "payoff"] + payoffs_round[1]

## agents[first_mover,]
## agents[second_mover,]


## #}

# HERE IS THE ADAPTATION OF THE LEGACY CODE
     
 #   for (CurrMatchLine in 1:(NumAgents/2)) {