# set seed for pseudorandomness

set.seed(2)

GRID_SIZE = 100

N_STEPS = 300
DRAW_INTERVAL = 50

PRIORITY_DETECT_RADIUS = 5
PRIORITY_REPULSION_STRENGTH = 0.7 #ranging from 0 - 1

LOAD_SPEED_MULTIPLIER = 0.75

MAX_ANTS = 110

FOOD_RADIUS = 3
NEST_RADIUS = 3
SPAWN_INTERVAL = 3

SELF_REPULSION_STRENGTH = 3
SELF_DETECT_RADIUS = 2
STEP_SIZE = 1

food_count = 0

nest <- c(50, 5)
food_spawn <- c(50, 95)

ants <- data.frame(
  x = numeric(),
  y = numeric(),
  hx = numeric(),
  hy = numeric(),
  startTime = numeric(),
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
    # ants <- rbind(
    #   ants,
    #   data.frame(
    #     x = food_spawn[1],
    #     y = food_spawn[2],
    #     hx = 0,
    #     hy = -1,
    #     state = "returning",
    #     startTime = step
    #   )
    # )
    
    # unloaded ant starts at nest
    ants <- rbind(
      ants,
      data.frame(
        x = nest[1],
        y = nest[2],
        hx = 0,
        hy = 1,
        state = "searching",
        startTime = step
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
        near  <- which(dists > 0 & dists < PRIORITY_DETECT_RADIUS)
        if(length(near) > 0) {
          # weighted average push-away vector (the closer the stronger)
          weights  <- 1 / dists[near]
          away_vec <- colSums(-diffs[near, , drop = FALSE] * weights)
          away_vec <- unit_vec(away_vec)
          # step sideways: keep only the perpendicular component
          fwd      <- unit_vec(dir)
          side     <- away_vec - sum(away_vec * fwd) * fwd
          side     <- unit_vec(side)
          dir <- unit_vec((1 - PRIORITY_REPULSION_STRENGTH) * dir +
                            PRIORITY_REPULSION_STRENGTH      * side)
        }
      }
      
      speed = STEP_SIZE
      }
    
    if(ants$state[i] == "returning") {
      dir <-
        0.80 * to_nest +
        0.20 * noise
      
      speed = STEP_SIZE * LOAD_SPEED_MULTIPLIER
    }
    
      # collision avoidance
      diffs2 <- cbind(ants$x - pos[1],
                      ants$y - pos[2])
      dists2 <- sqrt(rowSums(diffs2^2))
      near2  <- which(dists2 > 0 & dists2 < SELF_DETECT_RADIUS)
      
      if(length(near2) > 0) {
        weights2  <- 1 / dists2[near2]
        repel_vec <- unit_vec(colSums(-diffs2[near2, , drop = FALSE] * weights2))
        dir <- unit_vec(dir + SELF_REPULSION_STRENGTH * repel_vec)
      }
    
    dir <- unit_vec(dir)
    
    ants$x[i] <- ants$x[i] +
      speed * dir[1]
    
    ants$y[i] <- ants$y[i] +
      speed * dir[2]
}
  
  
  
  # ---------
  # State flips
  # ---------
  searchers <- which(ants$state == "searching")
  if (length(searchers) > 0) {
    dists_to_food <- sqrt((ants$x[searchers] - food_spawn[1])^2 +
                            (ants$y[searchers] - food_spawn[2])^2)
    ants$state[searchers[dists_to_food < FOOD_RADIUS]] <- "returning"
  }

  returners <- which(ants$state == "returning")
  
  if (length(returners) > 0) {
    
    dists_to_nest <- sqrt(
      (ants$x[returners] - nest[1])^2 +
        (ants$y[returners] - nest[2])^2
    )
    
    arrived <- returners[dists_to_nest < NEST_RADIUS]
    
    food_count <- food_count + length(arrived)
    
    ants$state[arrived] <- "searching"
  }
  
  
  # -----
  # Draw
  # -----
  
  if(step %% DRAW_INTERVAL == 0 | step == N_STEPS) {
    plot(
      NA,
      xlim = c(1, GRID_SIZE),
      ylim = c(1, GRID_SIZE),
      xlab = "",
      ylab = "",
      axes = FALSE,
      asp = 1,
      main = paste(
        "Step",
        step,
        "| ants:",
        nrow(ants),
        "| food:",
        food_count
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
      cex = 3
    )
    
    points(
      food_spawn[1],
      food_spawn[2],
      pch = 15,
      col = "red",
      cex = 3
    )
    
    Sys.sleep(0.1)
  }
}