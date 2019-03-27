# ==== Reaction Function ====
domain_size  <- num_actions + 1 # +1 is the first move

## L=0, M=1, H=2
possible_actions_list  <- list(0:(num_actions-1))
get_actions  <- function() {return(possible_actions_list[[1]])}
possible_actions  <- get_actions()
## First column refers to the first move, the rest are conditional responses
## In case three possible actions the rest are reactions to L,M, and H respectively
# The last part belove is to sort 
possible_types  <- expand.grid(rep(possible_actions_list, domain_size))
possible_types  <- possible_types[,domain_size:1]  # trick to sort in a numeric way

get_type_strategies  <- function() {return(possible_types)}


number_of_types<-dim(possible_types)[1]
type_names  <- 0:(number_of_types-1)
colnames(possible_types) <- seq(-1,num_actions-1)
rownames(possible_types) <- type_names

possible_types<-as.matrix(possible_types)

get_type_names  <- function() {
  return(0:(num_actions^(num_actions+1)-1))
}

draw_num_interactions  <-  function(delta) {
  if(delta <=0 | delta >=1 ) { stop('wrong delta range')}
  return(rgeom(1, 1-delta) + 1)   
}

react<-function(type, opponent_action, mistake_rate)  {
  if (mistake_rate < 0 | mistake_rate > 1) {
    stop("mistake rate should be between 0 and 1")
  }
  # Reaction function:
  #    takes type no and opponent action as input and reacts according to type
  #    possible to add noise
  #    note that types start from 0 (type+1)
  #

  # finds the reaction according to the name of columns, not the intex
  reaction_deterministic  <- possible_types[as.character(type),as.character(opponent_action)]
  
  if (missing(mistake_rate) | mistake_rate == 0) {
    return(reaction_deterministic)
  }
  else
  {
    if (mistake_rate>runif(1)) {
      return(sample(possible_actions,1))
    }
    else
      return(reaction_deterministic)
  }
}


initiate_output_files  <- function(types_filename, actions_filename) {
  tbl_actions_header  <- matrix(c("delta",
                                  "efficiency_rate",
                                  "mistake_rate",
                                  "mutation_rate",
                                  "num_agents",
                                  "simulation",
                                  "generation",
                                  "action",
                                  "proportion"
                                  )
                               ,nrow = 1)


  write.table(tbl_actions_header, actions_filename, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 

  tbl_types_header  <- matrix(c("delta",
                                "efficiency_rate",
                                "mistake_rate",
                                "mutation_rate",
                                "num_agents",
                                "simulation",
                                "generation",
                                "type",
                                "proportion"
                                )
                             ,nrow = 1)


  write.table(tbl_types_header, types_filename, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 

  message("initiated output files")

}


initiate_output_files_agg  <- function(types_filename, actions_filename) {
  tbl_actions_header  <- matrix(c("delta",
                                  "efficiency_rate",
                                  "mistake_rate",
                                  "mutation_rate",
                                  "num_agents",
                                  #"simulation",
                                  "generation",
                                  "action",
                                  "proportion"
                                  )
                               ,nrow = 1)


  write.table(tbl_actions_header, actions_filename, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 

  tbl_types_header  <- matrix(c("delta",
                                "efficiency_rate",
                                "mistake_rate",
                                "mutation_rate",
                                "num_agents",
                                #"simulation",
                                "generation",
                                "type",
                                "proportion"
                                )
                             ,nrow = 1)


  write.table(tbl_types_header, types_filename, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 

  message("initiated output files,", types_filename, actions_filename)

}


write_to_file  <- function(file_name, delta, efficiency_rate, mistake_rate, mutation_rate, num_agents, class_vector, proportion_list) {
  num_list_entries  <- length(proportion_list)

  # Stripping names to avoid them to be written in the db

for (entry in 1:num_list_entries) {
#  names(proportion_list[[entry]])  <- NULL
table_to_write  <- data.frame(delta = delta, efficiency_rate = efficiency_rate, mistake_rate = mistake_rate, mutation_rate = mutation_rate, num_agents = num_agents, generation = entry, class  = class_vector, prop = as.vector(proportion_list[[entry]]))

write.table(table_to_write, file = file_name, append = TRUE, row.names = FALSE, na = "NA", sep=",", col.names = FALSE) 
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

get_payoffs <- function(action, efficiency_rate) {
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
      agents_no_mutation  <- sample(x = agent_table[,"type"], 
                                    size = num_agents, # Here we gave a little flexibility to changing population size
                                    prob = agent_table[,"payoff"]/sum(agent_table[,"payoff"]),
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


# -----------------------------------------------------------------------------------------
#                             Analysis Functions
# -----------------------------------------------------------------------------------------

get_type_strategy  <- function(typeno) {
  strategy  <- possible_types[as.character(typeno),]
  names(strategy)  <- NULL
  return(strategy)
}

bind_multiple_files  <- function(output_folder, pattern) {

file_list  <- list.files(output_folder, pattern = pattern)
  
filepath_list <- paste0(output_folder,file_list)
message("gathered following files:")
cat(file_list, sep="\n")
return(suppressMessages(bind_rows(lapply(filepath_list,read_csv))))
  }



  
#==================NAMING FUNCTIONS==================================


# this part is not generalized as naming is specific to three actions
possible_actions_letter<-as.vector(c("L","M","H"))
#Input: Reaction Functions as column vector ex: c(0,1,2,2) 
#Output: nametypev: L17, initial action and 13 base-3 type
#        nametypef: LMHH initial action,response to L,M,H respectively

#nametypev<-function(typev) paste(possible_actions_letter[typev[1]+1],typev[4]+typev[3]*3+typev[2]*9,sep = "")
name_from_vector <- function(type_vector) paste(possible_actions_letter[type_vector[1]+1],"-",possible_actions_letter[type_vector[2]+1],possible_actions_letter[type_vector[3]+1],possible_actions_letter[type_vector[4]+1],sep = "")

name_from_type  <- function(type_number) {
name_from_vector(as.vector(possible_types[as.character(type_number),]))
  }


name_labeler  <- function(type_list) {
return(sapply(type_list,name_from_type))
  }

get_action_labels  <- function(){
return(c(`0` = "L", `1` = "M", `2` = "H"))
  }
#===================================================================





# ======================= Type labeler ==========================
class_vector  <- c("selfish",#1
                 "conditional",#2
                 "conditional",#3
                 "humped",#4
                 "conditional",#5
                 "perf-conditional",#6
                 "humped",#7
                 "humped",#8
                 "conditional",#9
                 "other",#10
                 "other",#11
                 "other",#12
                 "other",#13
                 "unconditional",#14
                 "conditional",#15
                 "humped",#16
                 "humped",#17
                 "conditional",#18
                 "other",#19
                 "other",#20
                 "other",#21
                 "other",#22
                 "other",#23
                 "other",#24
                 "other",#25
                 "unconditional",#26
                 "unconditional",#27
                 "selfish",#28
                 "conditional",#29
                 "conditional",#30
                 "humped",#31
                 "conditional",#32
                 "perf-conditional",#33
                 "humped",#34
                 "humped",#35
                 "conditional",#36
                 "other",#37
                 "other",#38
                 "other",#39
                 "other",#40
                 "unconditional",#41
                 "conditional",#42
                 "humped",#43
                 "humped",#44
                 "conditional",#45
                 "other",#46
                 "other",#47
                 "other",#48
                 "other",#49
                 "other",#50
                 "other",#51
                 "other",#52
                 "unconditional",#53
                 "unconditional",#54
                 "selfish",#55
                 "conditional",#56
                 "conditional",#57
                 "humped",#58
                 "conditional",#59
                 "perf-conditional",#60
                 "humped",#61
                 "humped",#62
                 "conditional",#63
                 "other",#64
                 "other",#65
                 "other",#66
                 "other",#67
                 "unconditional",#68
                 "conditional",#69
                 "humped",#70
                 "humped",#71
                 "conditional",#72
                 "other",#73
                 "other",#74
                 "other",#75
                 "other",#76
                 "other",#77
                 "other",#78
                 "other",#79
                 "unconditional",#80
                 "unconditional"#81
                )



class_from_type  <- function(type_number) {
  if (type_number == -999) {return("other")}
    return(class_vector[[type_number+1]])
  }

get_class  <- function(type_list) {
    return(sapply(type_list,class_from_type))
  }


  pal_red<-"#C0392B"
  pal_blue<-"#2980B9"
  pal_dblue<-"#1e6391"
  pal_purple<-"#9B59B6"
  pal_green<-"#27AE60"
  pal_dgreen<-"#085b2b"
  pal_yellow<-"#F1C40F"
  pal_orange<-"#E67E22"
  pal_pink <- "#ef39a3"
  pal_gray  <- "#888888"




color_vector  <- c("conditional"= pal_green,
                             "selfish" = pal_red,
                             "perf-conditional" = pal_dgreen,
                             "other" = pal_gray,
                                                "unconditional" = pal_orange,

                             "humped"=  pal_yellow)

get_color_vector  <- function(){return(color_vector)}

get_color_type_vector  <- function(){
  return(as.vector(colorize_class(get_class(0:80))))}

color_from_class  <- function(classname) {
 return(color_vector[classname])   
  }

colorize_class  <- function(class_list){
    return(sapply(class_list,color_from_class))
}

facet_labeller_delta  <- function(input) {
return(as.list(paste("delta:", input)))
  }
