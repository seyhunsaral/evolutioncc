# Reaction Function

# The function is written in a generalizeable manners in terms of number of actions
number_of_possible_actions  <- 3

domain_size  <- number_of_possible_actions + 1 # +1 is the first move

## L=0, M=1, H=2
possible_actions  <- list(0:(number_of_possible_actions-1))

## First column refers to the first move, the rest are conditional responses
## In case three possible actions the rest are reactions to L,M, and H respectively
# The last part belove is to sort 
possible_types  <- expand.grid(rep(possible_actions, domain_size))
possible_types  <- possible_types[,domain_size:1]  # trick to sort in a numeric way

number_of_types<-dim(possible_types)[1]

colnames(possible_types) <- NULL
rownames(possible_types) <-NULL

possible_types<-as.matrix(possible_types)

reaction<-function(type_no, action, error_rate)  {
    # Reaction function:
  if (missing(error_rate)) {
    return(possible_types[type_no+1,action+1])
  }
  else
  {
    if (error_rate>runif(1)) {
      return(round(runif(1,0,2)))
    }
    else
      return(possible_types[type_no+1,action+1])
      }
}

