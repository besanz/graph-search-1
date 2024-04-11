# =======================================================================
# Group Name: Group 1
# Students: Benat SANZ, Jon Ander ARANA
# =======================================================================
# Implementation of functions for the multimodal urban route finder problem.

initialize.problem <- function(file_path) {
  problem <- list()
  lines <- readLines(file_path)
  
  if (length(lines) < 5) { 
    stop("El formato del archivo es incorrecto. No hay suficientes líneas.")
  }
  
  problem$grid_size <- as.numeric(strsplit(lines[1], ",")[[1]])
  problem$state_initial <- list(
    position = as.numeric(strsplit(lines[2], ",")[[1]]),
    mode = "walking",
    tickets = list(metro = 0, bus = 0, tram = 0)
  )
  problem$state_final <- list(position = as.numeric(strsplit(lines[3], ",")[[1]]))
  
  problem$costs <- list(
    walk = as.numeric(strsplit(gsub("W:", "", lines[4]), ";")[[1]][1]),
    change = as.numeric(strsplit(gsub("E:", "", lines[5]), ";")[[1]][1]),
    walking = 0
  )
  
  if(!"walking" %in% names(problem$costs)) {
    problem$costs$walking <- 0
  }
  
  problem$transport_modes <- list()
  
  problem$actions_possible <- data.frame(
    actions = c("up", "down", "left", "right", "up_left", "up_right", "down_left", "down_right",
                "change_to_metro", "change_to_bus", "change_to_tram",
                "purchase_metro_ticket", "purchase_bus_ticket", "purchase_tram_ticket"), 
    stringsAsFactors = FALSE
  )
  
  return(problem)
}

# Helper function to convert stop information from string to list of coordinates
convertStops <- function(stops_string) {
  # Split the stops string by ";"
  stop_pairs <- strsplit(stops_string, ";")[[1]]
  # Convert each pair to numeric coordinates
  lapply(stop_pairs, function(pair) {
    as.numeric(strsplit(pair, ",")[[1]])
  })
}

is.applicable <- function(state, action, problem) {
  current_position <- state$position
  grid_size <- problem$grid_size
  new_position <- current_position
  
  switch(action,
         'up' = new_position[1] <- current_position[1] - 1,
         'down' = new_position[1] <- current_position[1] + 1,
         'left' = new_position[2] <- current_position[2] - 1,
         'right' = new_position[2] <- current_position[2] + 1,
         'up_left' = new_position <- c(current_position[1] - 1, current_position[2] - 1),
         'up_right' = new_position <- c(current_position[1] - 1, current_position[2] + 1),
         'down_left' = new_position <- c(current_position[1] + 1, current_position[2] - 1),
         'down_right' = new_position <- c(current_position[1] + 1, current_position[2] + 1)
  )
  
  if(all(new_position >= 1) && new_position[1] <= grid_size[1] && new_position[2] <= grid_size[2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

effect <- function(state, action, problem) {
  # Initialize a copy of the state to modify.
  result <- list(position = state$position, mode = state$mode, tickets = state$tickets)
  
  if (is.applicable(state, action, problem)) {
    switch(action,
           up = {
             result$position[1] <- max(1, result$position[1] - 1)
           },
           down = {
             result$position[1] <- min(problem$grid_size[1], result$position[1] + 1)
           },
           left = {
             result$position[2] <- max(1, result$position[2] - 1)
           },
           right = {
             result$position[2] <- min(problem$grid_size[2], result$position[2] + 1)
           },
           up_left = {
             result$position <- c(max(1, result$position[1] - 1), max(1, result$position[2] - 1))
           },
           up_right = {
             result$position <- c(max(1, result$position[1] - 1), min(problem$grid_size[2], result$position[2] + 1))
           },
           down_left = {
             result$position <- c(min(problem$grid_size[1], result$position[1] + 1), max(1, result$position[2] - 1))
           },
           down_right = {
             result$position <- c(min(problem$grid_size[1], result$position[1] + 1), min(problem$grid_size[2], result$position[2] + 1))
           },
           {
             if (startsWith(action, "change_to_")) {
               mode <- gsub("change_to_", "", action)
               if (is.station(result$position, mode, problem)) {
                 result$mode <- mode
                 result$tickets[[mode]] <- max(0, result$tickets[[mode]] - 1)
               }
             } else if (startsWith(action, "purchase_")) {
               ticket_type <- gsub("purchase_", "", action)
               result$tickets[[ticket_type]] <- result$tickets[[ticket_type]] + 1
             }
           }
    )
  }
  
  return(result)
}

# Check if the current state is the final state.
is.final.state <- function (state, final_state, problem) {
  result <- identical(state$position, final_state$position)
  return(result)
}

# Convert a state into a string representation.
to.string <- function (state, problem) {
  state_str <- sprintf("Position: (%s), Mode: %s, Tickets: %s",
                       paste(state$position, collapse = ","),
                       state$mode,
                       paste(sapply(state$tickets, toString), collapse = ", "))
  return(state_str)
}

# Return the cost of an action from one state to another.
get.cost <- function(action, state, problem) {
  cost <- NA
  
  if (startsWith(action, "change_to_") || action %in% c("up", "down", "left", "right")) {
    mode_action <- gsub("change_to_", "", action)
    mode_action <- ifelse(mode_action %in% c("up", "down", "left", "right"), "walk", mode_action)
    cost <- problem$costs[[mode_action]]
  } else if (startsWith(action, "purchase_")) {
    ticket_type <- sub("purchase_", "", action)
    cost <- problem$prices[[ticket_type]]
  }
  
  if(is.na(cost) || !is.numeric(cost)) {
    cost <- 0  # Ensuring that the cost is always numeric and valid.
  }
  
  return(cost)
}

# Evaluate a state using a heuristic for informed search algorithms.
get.evaluation <- function(state, problem) {
  # Verificar si el modo actual está definido en los costos, si no, usar 'walking'
  if (!state$mode %in% names(problem$costs)) {
    state$mode <- "walk"
  }
  
  # Calcular la distancia de Manhattan como heurística
  evaluation <- abs(problem$state_final$position[1] - state$position[1]) +
    abs(problem$state_final$position[2] - state$position[2])
  
  # Ajustar la evaluación con el costo del modo de transporte actual
  # Usar un valor predeterminado si el costo no es numérico
  mode_cost <- if(is.numeric(problem$costs[[state$mode]])) {
    problem$costs[[state$mode]]
  } else {
    1  # Valor predeterminado si el costo no es numérico
  }
  
  evaluation <- evaluation * mode_cost
  
  # Asegurar que la evaluación sea numérica y no NA
  if (!is.numeric(evaluation) || is.na(evaluation)) {
    evaluation <- Inf  # Usar Infinito como valor de evaluación si hay problemas
  }
  
  return(evaluation)
}

# =======================================================================
# Helper function to check if the current position is a station.
# =======================================================================
is.station <- function(position, type, problem) {
  if (type %in% names(problem$transport_modes)) {
    stations <- problem$transport_modes[[type]]
    return(any(apply(stations, 1, function(station) all(station == position))))
  }
  return(FALSE)
}

