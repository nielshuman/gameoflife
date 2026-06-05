library(parallel)
library(pbapply)
source("Antmodel.R")

param  <- "PRIORITY_DETECT_RADIUS"
values <- seq(0, 20, by = 0.25)

out_dir <- file.path("sweep_results", param)
png_dir <- file.path(out_dir, "snapshots")
dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)

n_runs <- 3

n_cores <- max(1, detectCores() - 1)
cl <- makeCluster(n_cores)
clusterEvalQ(cl, source("Antmodel.R"))
clusterExport(cl, c("param", "values", "png_dir"))

all_foods <- matrix(NA, nrow = length(values), ncol = n_runs)

for (run in 1:n_runs) {
  
  clusterExport(cl, "run")
  
  foods <- pbsapply(seq_along(values), function(i) {
    args <- list()
    args[[param]] <- values[i]
    args[["seed"]] <- run
    
    args[["png_out"]] <- file.path(
      png_dir,
      sprintf("run%d_%s_%.4g.png", run, param, values[i])
    )
    
    food <- do.call(run_sim, args)
    
    cat(sprintf("Run %d  [%d/%d] %s = %.4g  ->  food = %d\n",
                run, i, length(values), param, values[i], food))
    food
  }, cl = cl)
  
  all_foods[, run] <- foods
}

stopCluster(cl)

results <- data.frame(
  param_value = values,
  run1 = all_foods[, 1],
  run2 = all_foods[, 2],
  run3 = all_foods[, 3]
)

results$average <- rowMeans(all_foods)

write.csv(
  results,
  file.path(out_dir, paste0(param, "_3runs.csv")),
  row.names = FALSE
)

plot(values, results$average, type = "l", lwd = 2,
     xlab = param, ylab = "Average Food Collected",
     main = paste("Sweep:", param))
points(values, results$average, pch = 16, cex = 0.5)