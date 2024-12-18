simulate_guard <- function(input) {
  # Transform the input into a matrix
  rows <- unlist(strsplit(input, "\n"))
  grid <- do.call(rbind, lapply(rows, function(row) unlist(strsplit(row, NULL))))

  # Find the starting position and initial direction
  start_pos <- which(grid %in% c("^", ">", "v", "<"), arr.ind = TRUE)
  if (length(start_pos) == 0) {
    stop("No starting position found in the input grid.")
  }
  if (is.vector(start_pos)) {
    start_pos <- matrix(start_pos, nrow = 1)
  }
  directions <- list("^" = c(-1, 0), ">" = c(0, 1), "v" = c(1, 0), "<" = c(0, -1))
  turns <- c("^" = ">", ">" = "v", "v" = "<", "<" = "^")

  direction <- grid[start_pos[1, 1], start_pos[1, 2]]
  visited <- list()  # List of visited positions

  # Initialize the position
  x <- start_pos[1, 1]
  print(x)
  y <- start_pos[1, 2]

  # Function to check the bounds of the grid
  in_bounds <- function(x, y, grid) {
    x > 0 && x <= nrow(grid) && y > 0 && y <= ncol(grid)
  }

  # Mark the initial position as visited
  visited[[paste(x, y, sep = ",")]] <- TRUE

  # Simulate the movement
  repeat {
    # Calculate the next position in the current direction
    dx <- directions[[direction]][1]
    dy <- directions[[direction]][2]
    nx <- x + dx
    ny <- y + dy

    # If out of bounds or obstacle ahead, turn right
    if (!in_bounds(nx, ny, grid) || grid[nx, ny] == "#") {
      direction <- turns[direction]
    } else {
      # Move forward
      x <- nx
      y <- ny

      # Mark the new position as visited
      visited[[paste(x, y, sep = ",")]] <- TRUE

      # If out of bounds, stop the simulation
      if (!in_bounds(x, y, grid)) break
    }
  }

  # Return the number of distinct positions visited
  length(visited)
}

# Input as a single string (provided)
input <- "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

# Call the function
distinct_positions <- simulate_guard(input)
cat("Distinct positions visited:", distinct_positions, "\n")