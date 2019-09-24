rm(list = ls(all.names = T))

library(iml)
library(mlr)
lrn = makeLearner("regr.rpart")
tsk = bh.task
dat = getTaskData(bh.task)
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)
saveRDS(pred, file = "example/pred.RDS")
