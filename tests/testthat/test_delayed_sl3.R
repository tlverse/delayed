library(delayed)
library(sl3)
library(testthat)
library(SuperLearner)
library(rlang)
library(future)
library(uuid)
library(assertthat)
context("Delayed sl3")

plan(sequential)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
# cpp <- cpp[sample(nrow(cpp),10000,replace=T),]
outcome <- "haz"
cpp <- cpp[1:150, ]

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)



sl_random_forest <- Lrnr_pkg_SuperLearner$new("SL.randomForest")
delayed_sl_random_forest <- delayed:::Lrnr_delayed$new(sl_random_forest)

# SuperLearner(task$Y,task$X, SL.library="SL.randomForest")

rf_fits <- lapply(1:10,function(x){delayed_sl_random_forest$train(task)})

rf_preds <- lapply(rf_fits,function(rf_fit){rf_fit$predict()})
rf_preds[[1]]$compute()


mean_preds <- function(...){
  mat=cbind(...)
  rowMeans(mat)
}

delayed_mp <- delayed_fun(mean_preds)
# debugonce(delayed_mp)


result <- do.call(delayed_mp,rf_preds)
system.time({
agg_preds=result$compute()
})

graph <- result$make_graph()
plot(graph, label.cex=0.2)

