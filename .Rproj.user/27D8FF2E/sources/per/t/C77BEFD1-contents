# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(filename, p = 0.5, random_actions = FALSE) {
  problem <- list() # Default value is an empty list.
  
  #P = minimum customers who must like pizza
  problem$p <- p
  
  #Read all the file
  lines <- readLines(filename)
  
  #Number of clients
  problem$customers <- as.numeric(lines[1])
  
  #Remove first element of lines
  lines <- lines[-1]
  #Remove the number and blank from each line
  lines <- substr(lines, start = 3, stop = nchar(lines))
  
  #Get like ingredients (odd lines)
  problem$like <- lines[seq(1, length(lines), by = 2)]

  # Action is "Add an ingredient". There are as many actions as ingredients that customers like.
  
  #Randomize actions
  if (random_actions) {
    problem$actions_possible <- data.frame(action = sample(unique(unlist(strsplit(problem$like, " ")))), stringsAsFactors = FALSE)
  } else {
    problem$actions_possible <- data.frame(action = unique(unlist(strsplit(problem$like, " "))), stringsAsFactors = FALSE)
  }

  #Split like ingredients into a vector of string vectors
  problem$like <- strsplit(problem$like, " ")
    
  #Get dislike ingredients (even lines)
  problem$dislike <- lines[seq(2, length(lines), by = 2)]
  #Split dislike ingredients into a vector of string vectors
  problem$dislike <- strsplit(problem$dislike, " ")
  
  # Initial state is an empty vector
  problem$state_initial <- c()
  # There is no final state
  problem$state_final   <- NULL

  problem$name  <- paste0("One-pizza [filename = ", filename, 
                          " - Clients = ", length(problem$like), 
                          " - Diff. ingredients = ", nrow(problem$actions_possible),
                          " - Satisfied = ", problem$p*100, "%]")
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  #Check if the ingredient (action) is already in the state.
  return(!action %in% state)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  #The ingredient (action) is added to the state. The result is sorted.
  result <- sort(c(state, action))
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  satisfied <- satisfied.clients(state, problem)
  
  return(satisfied/problem$customers >= problem$p)
}

# Transforms a state into a string
to.string = function (state, problem) {
  return(paste0(state, collapse = ", "))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms.
get.evaluation <- function(state, problem) {
  unsatisfied <- unsatisfied.clients(state, problem)
  
  return(unsatisfied)
}

# Counts satisfied customers
satisfied.clients <- function(state, problem) {
  satisfied <- 0;
  
  for (i in 1:length(problem$like)) {
    #All liked and none disliked ingredients are in the state.
    if (all(problem$like[[i]] %in% state) && 
        (length(problem$dislike[[i]]) == 0 || !all(problem$dislike[[i]] %in% state))) {
      satisfied <- satisfied + 1;
    }
  }
  
  return(satisfied)
}

# Counts unsatisfied customers
unsatisfied.clients <- function(state, problem) {
  unsatisfied <- 0;
  
  for (i in 1:length(problem$like)) {
    # Not all liked ingredients or at least one disliked ingredient is in the state.
    if (!all(problem$like[[i]] %in% state) || any(problem$dislike[[i]] %in% state)) {
      unsatisfied <- unsatisfied + 1;
    }
  }
  
  return(unsatisfied)
}