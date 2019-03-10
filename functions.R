# ==== Reaction Function ====

# The function is written in a generalizeable manners in terms of number of actions
num_actions  <- 3
efficiency_rate  <- 2


domain_size  <- num_actions + 1 # +1 is the first move

## L=0, M=1, H=2
possible_actions  <- list(0:(num_actions-1))

## First column refers to the first move, the rest are conditional responses
## In case three possible actions the rest are reactions to L,M, and H respectively
# The last part belove is to sort 
possible_types  <- expand.grid(rep(possible_actions, domain_size))
possible_types  <- possible_types[,domain_size:1]  # trick to sort in a numeric way

number_of_types<-dim(possible_types)[1]

colnames(possible_types) <- seq(-1,num_actions-1)
rownames(possible_types) <-0:(number_of_types-1)

possible_types<-as.matrix(possible_types)

react<-function(type_no, opponent_action, error_rate)  {
    # Reaction function:
    #    takes type no and opponent action as input and reacts according to type
    #    possible to add noise
    #    note that types start from 0 (type_no+1)
    #

    # finds the reaction according to the name of columns, not the intex
    reaction_deterministic  <- possible_types[as.character(type_no),as.character(opponent_action)]
    
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

# Specific case of 3 actions with b = 2
# |   |    L     |    M       |    H     |
# |---+----------+------------+----------|
# | L | (1, 1)   | (2, 0.5)   | (3, 0)   |
# | M | (0.5, 2) | (1.5, 1.5) | (2.5, 1) |
# | H | (0, 3)   | (1, 2.5)   | (2, 2)   |


# payoff of the first mover, payoff of the second mover
# c and ie is normalized to 1
# therefore b is also b/c

get_payoffs <- function(first_player_action) {
    if (first_player_action > (num_actions-1)) {
        stop('Action outside of defined range')
    }
       
    first_player_payoff  <- 1 - (first_player_action /(num_actions-1))
    second_player_payoff  <- efficiency_rate * (first_player_action/(num_actions-1))

    return(c(first_player_payoff, second_player_payoff))
    }
    
       
 


