
#We create 2 matrices, the arena and the one to calculate nb of living neighbors 
arena <- matrix(data = 0, nrow = 20, ncol = 20, byrow = FALSE, dimnames = NULL)
living_neigh <- matrix(data = 0, nrow = nrow(arena), ncol = ncol(arena))

arena[2:7,3:6]=1
#Put the nb of living neighbors for each cell in the corresponding one in that matrix
for (R in 2:11) {
  for (C in 2:11) {
    living_neigh[R,C] <- as.numeric(arena[R-1,C-1]) + as.numeric(arena[R-1, C]) + as.numeric(arena[R-1, C+1]) + as.numeric(arena[R,C-1]) + as.numeric(arena[R, C+1]) + as.numeric(arena[R+1,C-1]) + as.numeric(arena[R+1, C]) + as.numeric(arena[R+1, C+1])
  }
}
#calculate if alive or dead for next gen
for (R in 2:11) {
  for (C in 2:11) {
    if (arena[R,C]==1) {
      if (4>living_neigh[R,C] && living_neigh[R,C]>1) {
        arena[R,C] <- 1
      } else {
        arena[R,C] <- 0 
      }
    } else {
      if (living_neigh[R,C]==3) {
        arena[R,C] <- 1
      } else {
        arena[R,C] <- 0
      }
    }
  }
}
