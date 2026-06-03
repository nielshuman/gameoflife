# set seed for pseudorandomness

set.seed(1)

GRID_SIZE = 100

MAX_ANTS = 50
N_STEPS = 1000
SPAWN_INTERVAL = 5

#step size?

nest <- c(50, 25)
food_spawn <- c(50, 95)

ants <- data.frame(
  x = numeric(),
  y = numeric(),
  hx = numeric(),
  hy = numeric(),
  state = character() # searching | returning
)

par(mar = c(1,1,2,1))

for(step in 1:N_STEPS) {
  if(step %% SPAWN_INTERVAL == 0 && nrow(ants) < max_ants) { # 
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
}