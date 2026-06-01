ROWS = 20
COLS = 10

# We create 2 matrices, the arena and the one to calculate nb of living neighbours 
arena        <- matrix(data = 0, nrow = ROWS, ncol = COLS, byrow = FALSE, dimnames = NULL)
living_neigh <- matrix(data = 0, nrow = nrow(arena), ncol = ncol(arena))

arena[2:7, 3:6] = 1 # Init cell block of 2-7 x 3-6 as alive?

# Put the nb of living neighbours for each cell in the corresponding one in that matrix
for (R in 2:11) { # why 2:11??
  for (C in 2:11) {
    living_neigh[R,C] <- (
      as.numeric(arena[R-1,C-1])
      + as.numeric(arena[R-1, C])
      + as.numeric(arena[R-1, C+1])
      + as.numeric(arena[R, C-1]) 
      + as.numeric(arena[R, C+1]) 
      + as.numeric(arena[R+1, C-1]) 
      + as.numeric(arena[R+1, C]) 
      + as.numeric(arena[R+1, C+1])
    )
  }
}

# calculate if alive or dead for next gen
for (R in 2:11) {
  for (C in 2:11) {
    if (arena[R,C] == 1) { # Cell is alive
      if (living_neigh[R,C] > 1 && living_neigh[R,C] < 4) {
        arena[R,C] <- 1
      } else {
        arena[R,C] <- 0 
      }
    } else { # Cell is dead
      if (living_neigh[R,C] == 3) {
        arena[R,C] <- 1
      } else {
        arena[R,C] <- 0
      }
    }
  }
}

