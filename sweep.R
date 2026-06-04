library(parallel)
library(pbapply)
source("Antmodel.R")

param  <- "PRIORITY_DETECT_RADIUS"
values <- seq(0, 20, by = 0.25)
out_dir <- file.path("sweep_results", param)
png_dir <- file.path(out_dir, "snapshots")
dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)

n_cores <- max(1, detectCores() - 1)
cl <- makeCluster(n_cores)
clusterEvalQ(cl, source("Antmodel.R"))
clusterExport(cl, c("param", "values", "png_dir"))

foods <- pbsapply(seq_along(values), function(i) {
  args <- list()
  args[[param]] <- values[i]
  args[["png_out"]] <- file.path(png_dir, sprintf("%s_%.4g.png", param, values[i]))
  food <- do.call(run_sim, args)
  cat(sprintf("  [%d/%d] %s = %.4g  ->  food = %d\n",
              i, length(values), param, values[i], food))
  food
}, cl = cl)

stopCluster(cl)

results <- data.frame(param_value = values, food = foods)
write.csv(results, file.path(out_dir, paste0(param, ".csv")), row.names = FALSE)

plot(values, foods, type = "l", lwd = 2,
     xlab = param, ylab = "Food Collected",
     main = paste("Sweep:", param))
points(values, foods, pch = 16, cex = 0.5)