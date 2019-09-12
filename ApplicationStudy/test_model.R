rm(list = ls(all.names = T))
source("pdPlots.R") 
source("libraries.R") 
data(BostonHousing, package = "mlbench")
task = makeRegrTask(id = "bh", data = BostonHousing, target = "medv")
learner =regr.lrn = makeLearner("regr.gbm", par.vals = list(n.trees = 500, interaction.depth = 3))
mod.mlr = train(learner, task)
PrediObj = Predictor$new(mod.mlr, data = BostonHousing)
saveRDS(PrediObj, file = "PrediObj.RDS")
X = as.data.frame(PrediObj$data$get.x())
x.interest = X[6,]
model_data = Predictor$new(PrediObj$model, data = X)
set.seed(seed = seed)
shapley = Shapley$new(predictor = model_data, x.interest = x.interest, sample.size = 100)
plot(shapley)
shapley$results[,c(1, 4, 2)]

