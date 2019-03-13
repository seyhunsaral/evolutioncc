# ==== Reaction Function ====
domain_size  <- num_actions + 1 # +1 is the first move

## L=0, M=1, H=2
possible_actions  <- list(0:(num_actions-1))

## First column refers to the first move, the rest are conditional responses
## In case three possible actions the rest are reactions to L,M, and H respectively
# The last part belove is to sort 
possible_types  <- expand.grid(rep(possible_actions, domain_size))
possible_types  <- possible_types[,domain_size:1]  # trick to sort in a numeric way

number_of_types<-dim(possible_types)[1]
type_names  <- 0:(number_of_types-1)
colnames(possible_types) <- seq(-1,num_actions-1)
rownames(possible_types) <- type_names

possible_types<-as.matrix(possible_types)

get_type_names  <- function(number_of_actions) {
return(0:(number_of_actions^(number_of_actions+1)-1))
    }

draw_num_interactions  <-  function(delta) {
    if(delta <=0) { stop('hey... this may take forever')}
    return(rgeom(1, 1-delta) + 1)   
    }

react<-function(type, opponent_action, error_rate)  {
    # Reaction function:
    #    takes type no and opponent action as input and reacts according to type
    #    possible to add noise
    #    note that types start from 0 (type+1)
    #

    # finds the reaction according to the name of columns, not the intex
    reaction_deterministic  <- possible_types[as.character(type),as.character(opponent_action)]
    
    if (missing(error_rate)) {
    return(reaction_deterministic)
  }
  else
  {
     if (error_rate>runif(1)) {
      return(round(runif(n=1,min=0,max=num_actions-1)))
    }
    else
      return(reaction_deterministic)
      }
}

# ==== ==== ====

# ==== Payoff Function ====
# Linear Definition 
# Assume the actions are {0,1,...,n}
# The action represents the fraction of the given amount out of n
# cost is c (normalized to 1) b is b 


# The payoff of the individual 1 is 
# 1- (c_i/n) + b (c_j/n)

# The idea is generalizability of the number of actions
## |     | C_0      | C_1 | ... | C_k               |   | C_n   |
## | C_0 | (1,1)    |     |     |                   |   |       |
## | C_1 |          |     |     |                   |   |       |
## | ... |          |     |     |                   |   |       |
## | C_k |          |     |     | (1-(k/n) + b(k/n) |   |       |
## | ... |          |     |     |                   |   |       |
## | C_n | (0, 1+b) |     |     |                   |   | (b,b) |


# Specific case of 3 actions with b = 2
# |   |    L     |    M       |    H     |
# |---+----------+------------+----------|
# | L | (1, 1)   | (2, 0.5)   | (3, 0)   |
# | M | (0.5, 2) | (1.5, 1.5) | (2.5, 1) |
# | H | (0, 3)   | (1, 2.5)   | (2, 2)   |


# payoff of the first mover, payoff of the second mover
# c and ie is normalized to 1
# therefore b is also b/c

get_payoffs <- function(action) {
    if (action > (num_actions-1)) {
        stop('Action outside of defined range')
    }
       
    first_player_payoff  <- 1 - (action /(num_actions-1))
    second_player_payoff  <- efficiency_rate * (action/(num_actions-1))
    payoffs <- c(first_player_payoff, second_player_payoff)
    names(payoffs)  <- c("mover", "receiver")
    return(payoffs)
    }


 mutate_from_vector  <- function(mutators, mutants, mutation_prob) {
     # Mutators get mutated from mutant vector according to the probablility
     # This is a computation efficient way to handle mutations
     mutator_size  <- length(mutators)
     if(mutator_size != length(mutants)) {
         stop("Mutator and mutant vectors should have the same length")
         }
     mutation_happens  <- runif(mutator_size) < mutation_prob
     mutators[mutation_happens]  <- mutants[mutation_happens]
     return(mutators)
 }


       
generate_agents  <- function(num_agents, all_types, agent_table = NULL, method = "uniform", mutation_prob = 0) {
    if (length(all_types) == 1) { stop("We need more than one types")} 
    if (is.null(agent_table)) {
        # Initial generation
        if (method == "uniform"){
            type  <-  sample(all_types,
                             size=num_agents,
                             replace = TRUE
                             )   
        }        
        }
    else {
            if (method == "uniform") {
                agent_table_size  <- dim(agent_table)
                agents_no_mutation  <- sample(agent_table[,1], 
                                              size = num_agents, # Here we gave a little flexibility to changing population size
                                              prob = agent_table[,2]/sum(agent_table[,2]),
                                              replace = TRUE
                                              )
                agents_all_mutation  <- sample(all_types, size = num_agents, replace = TRUE)
                type  <- mutate_from_vector(agents_no_mutation, agents_all_mutation, mutation_prob)
            }
        }
    agent_no  <- 1:num_agents 
    payoff  <- rep(0, times=num_agents)
    return(cbind(agent_no, type, payoff)) 
           }
 

create_matching<-function(num_agents, method = "random") {
    if(method == "random"){
      agents_shuffled  <- sample(1:num_agents)
      matching_matrix  <- cbind(agents_shuffled[1:(num_agents/2)],agents_shuffled[((num_agents/2)+1):num_agents])
    }
return(matching_matrix)
}
