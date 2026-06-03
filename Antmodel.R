# set seed for pseudorandomness

set.seed(1)

GRID_SIZE = 100

MAX_ANTS = 50
N_STEPS = 1000
SPAWN_INTERVAL = 5
DRAW_INTERVAL = 2
STEP_SIZE = 1
DETECT_RADIUS = 6
REPULSION_STRENGTH = 0.6 #ranging from 0 - 1

nest <- c(50, 25)
food_spawn <- c(50, 95)

ants <- data.frame(
  x = numeric(),
  y = numeric(),
  hx = numeric(),
  hy = numeric(),
  state = character() # searching | returning
)

pheromone <- matrix(0, GRID_SIZE, GRID_SIZE)

par(mar = c(1,1,2,1))


unit_vec <- function(v) {
  
  n <- sqrt(sum(v^2))
  
  if(n < 1e-10)
    return(c(0,0))
  
  v / n
}


for(step in 1:N_STEPS) {
  
  # ---------
  # Spawn ants
  # ---------
  
  if(step %% SPAWN_INTERVAL == 0 && nrow(ants) < MAX_ANTS) { # 
    # Loaded ant starts at foodspawn
    ants <- rbind(
      ants,
      data.frame(
        x = food_spawn[1],
        y = food_spawn[2],
        hx = 0,
        hy = -1,
        state = "returning"
      )
    )
    
    # unloaded ant starts at nest
    ants <- rbind(
      ants,
      data.frame(
        x = nest[1],
        y = nest[2],
        hx = 0,
        hy = 1,
        state = "searching"
      )
    )
  }
  
  # ----
  # Move ants
  # ----
  for(i in seq_len(nrow(ants))) {
    
    pos <- c(ants$x[i], ants$y[i])
    noise <- unit_vec(rnorm(2))
    
    
    from_nest <- unit_vec(
      pos - nest
    )
    
    to_nest <- unit_vec(
      nest - pos
    )
    
    # Cause no pheromone for now; ants 'know' location of food for now
    
    to_food <- unit_vec(
      food_spawn - pos
    )
    
    if(ants$state[i] == "searching") {
      dir <-
        # 0.70 * from_nest +
        0.80 * to_food +
        0.20 * noise
      
      # traffic rule: move out of way for returning ants
      returners <- which(ants$state == "returning")
      
      if(length(returners) > 0) {
        diffs <- cbind(ants$x[returners] - pos[1],
                       ants$y[returners] - pos[2])
        dists <- sqrt(rowSums(diffs^2))
        near  <- which(dists > 0 & dists < DETECT_RADIUS)
        if(length(near) > 0) {
          # weighted average push-away vector (the closer the stronger)
          weights  <- 1 / dists[near]
          away_vec <- colSums(-diffs[near, , drop = FALSE] * weights)
          away_vec <- unit_vec(away_vec)
          # step sideways: keep only the perpendicular component
          fwd      <- unit_vec(dir)
          side     <- away_vec - sum(away_vec * fwd) * fwd
          side     <- unit_vec(side)
          dir <- unit_vec((1 - REPULSION_STRENGTH) * dir +
                            REPULSION_STRENGTH      * side)
        }
      }
    }
    
    if(ants$state[i] == "returning") {
      dir <-
        0.80 * to_nest +
        0.20 * noise
    }
    
    dir <- unit_vec(dir)
    
    ants$x[i] <- ants$x[i] +
      STEP_SIZE * dir[1]
    
    ants$y[i] <- ants$y[i] +
      STEP_SIZE * dir[2]
}
  
  # -----
  # Draw
  # -----
  
  if(step %% DRAW_INTERVAL == 0) {
    image(
      1:GRID_SIZE,
      1:GRID_SIZE,
      pheromone,
      col = gray.colors(
        100,
        start = 1,
        end = 0
      ),
      axes = FALSE,
      main = paste(
        "Step",
        step,
        "| ants:",
        nrow(ants)
      )
    )
    
    # Returning ants (carrying food)
    points(
      ants$x[ants$state == "returning"],
      ants$y[ants$state == "returning"],
      col = "darkgreen",
      pch = 16,
      cex = 0.7
    )
    
    # Searching ants
    points(
      ants$x[ants$state == "searching"],
      ants$y[ants$state == "searching"],
      col = "orange",
      pch = 16,
      cex = 0.7
    )
    
    points(
      nest[1],
      nest[2],
      pch = 15,
      col = "blue",
      cex = 2
    )
    
    points(
      food_spawn[1],
      food_spawn[2],
      pch = 15,
      col = "red",
      cex = 2
    )
    
    Sys.sleep(0.1)
  }
}