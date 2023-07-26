################################################################################ 
#################### Statistical Programming Languages 2023 ####################
###################             Take-home Exam              ####################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Bartari
# Name: Amirhossein
# Student ID (Matrikelnummer): 629940
#-------------------------------------------------------------------------------

### Exercise 5 -----------------------------------------------------------------



memory <- function(width, height) {
  is_odd <- (width * height) %% 2 == 1
  
  # Check if the product of width and height exceeds 208
  if (width * height > 208) {
    stop("The product of width and height cannot exceed 208.")
  }
  
  # 1. Initiate the game board
  
  # Calculate the maximum number of shape-color combinations
  max_combinations <- min((width * height) / 2, 13 * 8)
 
  target_score <- width*height/2
  if (is_odd == TRUE) {
    target_score <- target_score - 1
    
  }
  total_scores <- 0
  matched_indices <- c()
  matched_players <- c()
  
  # function for the computer player: ###########################################
  computer_play <- function() {
    remaining_indices <- setdiff(1:(width * height), matched_indices)
    
    if (length(remaining_indices) < 2) {
      # No more available cards to select from
      return(NULL)
    }
    
    # Generate random indices from the remaining pool
    indices <- sample(remaining_indices, 2)
    
    # Get the corresponding x and y positions
    x_pos <- x_box[indices]
    y_pos <- y_box[indices]

    # Return the selected positions
    return(list(x1 = x_pos[1], y1 = y_pos[1], x2 = x_pos[2], y2 = y_pos[2], indices1 = indices[1], indices2 = indices[2]))
  }
  
  ##############################################################################
  
  # Generate unique combinations of shapes and colors
  shape_color_combinations <- expand.grid(shape = 1:13, color = 1:8)
  shape_color_combinations <- shape_color_combinations[sample(nrow(shape_color_combinations)), ] # Shuffle the combinations
  
  # Take the required number of combinations based on the maximum allowed
  shape_color_combinations <- shape_color_combinations[1:max_combinations, ]
  
  shapes <- shape_color_combinations$shape
  shapes <- c(shapes, shapes) # Repeat the shapes to create pairs
  

  
  colors <- shape_color_combinations$color
  colors <- c(colors, colors) # Repeat the colors to match the shape pairs
  
  # assign shape and color for the leftover card
  if (is_odd == TRUE) {
    available_shape <- setdiff(1:13, shape_color_combinations$color)  # remaining
    random_shape <- sample(available_shape, 1)  # Select a random color
    random_color <- sample(1:8, 1)  # Select a random color
    colors <- c(colors, random_color)  # Append the random color to the colors vector
    shapes <- c(shapes, random_shape)
    
    matching_indices <- which(shapes == random_shape & colors == random_color)  # Get the indices of points with matching shape and color
    x_leftover <- x_box[matching_indices]  # Get the x positions of the matching points
    y_leftover <- y_box[matching_indices]  # Get the y positions of the matching points
    
  }
  
  
  # Set up an empty plot
  plot(1, 1, type = "n", xlim = c(0, width), ylim = c(0, height),
       xlab = "X-axis", ylab = "Y-axis", main = "MEMORY")
  
  # Randomize box positions
  box_positions <- sample(1:(width * height))
  x_box <- rep(0:width, each = height)[box_positions] + 0.5
  y_box <- rep(height:1, width)[box_positions] - 0.5
  
  # Add scatter points with specific shapes and colors inside the boxes
  for (i in 1:(width * height)) {
    x <- x_box[i]
    y <- y_box[i]
    point_color <- colors[i]
    point_shape <- shapes[i]
    #points(x, y, pch = point_shape, col = point_color, cex = 4) # game board key
  }
  
  # Add dashed lines for x points
  abline(v = 0:width, lty = "dashed")
  
  # Add dashed lines for y points
  abline(h = 1:height, lty = "dashed")
  
  # enter player numbers
  
  real_players <- as.numeric(readline("ENTER NUMBER OF REAL PLAYERS: "))
  pc_players <- as.numeric(readline("ENTER NUMBER OF PC PLAYERS: "))
  
  total_players <- real_players + pc_players
  # leader board
  scores <- vector("list", total_players)  # Create an empty list
  for (i in 1:length(scores)) {
    scores[[i]] <- 0
  }
  


  # while loop to continue the for loop while all pairs found
  while (sum(as.numeric(unlist(scores))) < target_score) {
  
  
    for (i in 1:total_players) {                 # for loop so every player plays
    print(paste("Player", i, "is playing the game."))
    
      if (i <= real_players) {
        prompt1 <- readline("Press [Y] to move on: ")
        while (prompt1 != "y") {
          prompt1 <- readline("Wrong key! - Press [Y] to move on: ")
          
        }
  
      is_matched <- FALSE
      
      
      
      x1_pos <- as.numeric(readline("x1: "))
      y1_pos <- as.numeric(readline("Y1: "))
      
      while (x1_pos > width | y1_pos > height) { # check if the inputs are valid
        print("invalid card positions! please enter agian: ")
        # ask for inputs again
        x1_pos <- as.numeric(readline("x1: "))
        y1_pos <- as.numeric(readline("Y1: "))
        
      }
  
     indices1 <- which(x_box == x1_pos - 0.5 & y_box == y1_pos - 0.5)
     points(x_box[indices1], y_box[indices1], pch = shapes[indices1], col = colors[indices1], cex = 4) 
  
     x2_pos <- as.numeric(readline("x2: "))
     y2_pos <- as.numeric(readline("Y2: "))
     while (x1_pos > width | y1_pos > height) { # check if the inputs are valid
       print("invalid card positions! please enter agian: ")
       x2_pos <- as.numeric(readline("x2: "))
       y2_pos <- as.numeric(readline("Y2: "))
     }
  
      indices2 <- which(x_box == x2_pos - 0.5 & y_box == y2_pos - 0.5)
      points(x_box[indices2], y_box[indices2], pch = shapes[indices2], col = colors[indices2], cex = 4)
      }
      else {
        selected_cards <- computer_play()
        
        if (is.null(selected_cards)) {
          # No more available cards for the computer
          next  # Skip to the next player
        }
        
        x1_pos <- selected_cards$x1
        y1_pos <- selected_cards$y1
        x2_pos <- selected_cards$x2
        y2_pos <- selected_cards$y2
        indices1 <- selected_cards$indices1
        indices2 <- selected_cards$indices2
        
        points(x_box[indices1], y_box[indices1], pch = shapes[indices1], col = colors[indices1], cex = 4)
        points(x_box[indices2], y_box[indices2], pch = shapes[indices2], col = colors[indices2], cex = 4)
        print(paste("Player", i, "chose", "(", x1_pos+0.5, ",", y1_pos+0.5, ")", "and", "(", x2_pos+0.5, ",", y2_pos+0.5, ")"))
        
      }
  
    # check if pairs match
      if (shapes[indices2] == shapes[indices1] & colors[indices2] == colors[indices1]) {
        print("MATCH!")
        is_matched <- TRUE
        
        scores[[i]] <- scores[[i]] + 1
        matched_indices <- c(matched_indices, indices1, indices2)
        matched_players <- c(matched_players, i, i)
    
      }
      else{  
        print("NOT MATCH!")
        is_matched <- FALSE
        Sys.sleep(2)
        # Plot the player number for the matched pairs
        plot(1, 1, type = "n", xlim = c(0, width), ylim = c(0, height),
             xlab = "X-axis", ylab = "Y-axis", main = "MEMORY")
        abline(v = 0:width, lty = "dashed")
        abline(h = 1:height, lty = "dashed")
        for (j in seq_along(matched_indices)) {
          x <- x_box[matched_indices[j]]
          y <- y_box[matched_indices[j]]
          player_number <- matched_players[j]
          text(x, y, player_number, col = "black", cex = 4)
        
      
        }
      }
  
    }
  
  }
  for (j in seq_along(matched_indices)) {
    x <- x_box[matched_indices[j]]
    y <- y_box[matched_indices[j]]
    player_number <- matched_players[j]
    text(x, y, player_number, col = "black", cex = 4)
    if (is_odd == TRUE) {
    points(x_leftover, y_leftover, pch = random_shape, col = random_color, cex = 4)
    }
  }
  # Find the highest score
  highest_score <- max(unlist(scores))
  
  # Find the players with the highest score
  winners <- which(unlist(scores) == highest_score)
  
  # Print the winners
  if (length(winners) == 1) {
    cat("Winner: Player", winners, "\n")
  } else {
    cat("Tied Winners: Players", paste(winners, collapse = ", "), "\n")
  }
  
  
  
  cat("---- FINAL LEADERBOARD ----\n")
  for (i in 1:length(scores)) {
    cat("Player", i)
    for (score in scores[[i]]) {
      cat("\t", score)
    }
    cat("\n")
  }
}

memory(4, 4)
