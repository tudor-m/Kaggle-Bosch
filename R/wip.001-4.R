if (1 == 1)
{
  rm(list=ls())
  gc()
}

library(data.table)
library(xgboost)
source("futil.R")

# Variation of wip.001.R
# This time we'll make 4 models, splitting out the train data in 2 equal sizes.
# at the end we'll combine the models
# We'll work with only numerical input


VERBOSE = 1
sink(file="output.R.txt",append = TRUE, split=TRUE)
timestamp()

# Load all the input file:
train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = -1)
nr = nrow(train.num)
nr1 = round(0.25*nr)

set.seed(100)
rIndex = sample(nr,nr)
rIndex1 = rIndex[1:nr1]
rIndex2 = rIndex[(nr1+1):2*nr1]
rIndex3 = rIndex[(2*nr1+1):3*nr1]
rIndex4 = rIndex[(3*nr1+1):4*nr1]

fit.dev.xgb.model=list()
remove(train.num);
gc()


for (i in 1:4)
{
# Model1 XGB:
# Data:
  if (i==1)
  {
    train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = -1)
    dtrain <- xgb.DMatrix(data = as.matrix(train.num[rIndex1,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex1], missing = NA)
    dtest <- xgb.DMatrix(data = as.matrix(train.num[rIndex2,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex2], missing = NA)
    remove(train.num);
    gc()
  }
  if (i==2)
  {
    train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = -1)
    dtrain <- xgb.DMatrix(data = as.matrix(train.num[rIndex2,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex1], missing = NA)
    dtest <- xgb.DMatrix(data = as.matrix(train.num[rIndex3,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex2], missing = NA)
    remove(train.num);
    gc()
  }
  if (i==3)
  {
    train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = -1)
    dtrain <- xgb.DMatrix(data = as.matrix(train.num[rIndex3,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex1], missing = NA)
    dtest <- xgb.DMatrix(data = as.matrix(train.num[rIndex4,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex2], missing = NA)
    remove(train.num);
    gc()
  }
  if (i==4)
  {
    train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = -1)
    dtrain <- xgb.DMatrix(data = as.matrix(train.num[rIndex4,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex1], missing = NA)
    dtest <- xgb.DMatrix(data = as.matrix(train.num[rIndex1,][,-c("Id","Response"),with=F]), label=train.num$Response[rIndex2], missing = NA)
    remove(train.num);
    gc()
  }
  
  # Fit:
  watchlist <- list(train = dtrain, test = dtest)
  mccEval <- function(preds, dtrain)
  {
    labels = getinfo(dtrain, "label")
    err = as.numeric(errMeasure4(preds,labels,0.25))
    return(list(metric="error",value=err))
  }
  for (min_child_w in 13:13) {
    for (max_d in 11:11) {
      print(c("max_d: ",max_d))
      print(c("min_child_weight: ",min_child_w))
      nround = 120
      param <- list(  
        #objective           = "multi:softprob", num_class = 4,
        objective           = "binary:logistic",
        #objective           = "reg:linear",
        booster             = "gbtree",
        #booster             = "gblinear",
        base_score          = 0.5,
        eta                 = 0.01,#0.05, #0.02, # 0.06, #0.01,
        max_depth           = max_d, #changed from default of 8
        subsample           = 0.5, #0.9, # 0.7
        colsample_bytree    = 0.5, # 0.7
        #num_parallel_tree   = 2,
        nthread = 4,
        alpha = 0,    #0.0001,
        lambda = 0,
        gamma = 0,
        scale_pos_weight = 1,
        min_child_weight    = min_child_w, #4, #4
        eval_metric         = mccEval,
        #eval_metric         = "rmse",
        early_stopping_rounds    = 2,
        maximize = TRUE
      )
    set.seed(100)
    fit.dev = xgb.train(params=param,dtrain,nrounds=nround,print.every.n = 2,maximize = FALSE,watchlist)
  }
}
# Model:
fit.dev.xgb.model[[i]] = fit.dev
}

remove(dtrain)
remove(dtest)
remove(fit.dev)
gc()


if ( 1==0)
{
# Use the model to produce 2 submissions:
test.num = fread('../data/test_numeric.csv',header = TRUE)
pred_test = list()

for (i in 1:length(fit.dev.xgb.model))
{
  pred_test[[i]] = predict(fit.dev.xgb.model[[i]],as.matrix(test.num[,-c("Id"),with=F]),missing = NA)
}
test.num_Id = test.num$Id
remove(test.num)
gc()

# prepare the submission file
# Model:
predData = list()
submitData = list()
for (i in 1:length(pred_test))
{
  thr = 0.25
  ed_test_submission = pred_test[[i]]
  pred_test_submission[which(pred_test[[i]] <= thr)] = 0
  pred_test_submission[which(pred_test[[i]] >  thr)] = 1

  predData[[i]]   = as.data.table(cbind(test.num_Id,pred_test[[i]]))
  setnames(predData[[i]],c("Id","Response"))
  submitData[[i]] = as.data.table(cbind(test.num_Id,pred_test_submission))
  setnames(submitData[[i]],c("Id","Response"))
  options(scipen = 999)

  write.csv(predData[[i]][,.(Id,Response)],paste(c("pred.wip.001-4.",i,".csv"),sep="",collapse = ""), row.names = FALSE)
  write.csv(submitData[[i]][,.(Id,Response)],paste(c("submit.wip.001-4.",i,".csv"),sep="",collapse = ""), row.names = FALSE)

  options(scipen = 0)
}

# Mean:
cCoef = c(0.25,0.25,0.25,0.25)
combinedPred = 0
for (j in 1:length(cCoef))
  combinedPred = combinedPred + cCoef[j]*predData[[j]][,2,with=F]

combinedSubmission = combinedPred
combinedSubmission[which(combinedPred<=thr)] = 0
combinedSubmission[which(combinedPred>thr)] = 1

combinedSubmitData = as.data.table(cbind(test.num_Id,combinedSubmission))
setnames(combinedSubmitData,c("Id","Response"))
options(scipen = 999)
write.csv(combinedSubmitData[,.(Id,Response)],paste(c("submit.wip.001-2.","MEAN",".csv"),sep="",collapse = ""), row.names = FALSE)
options(scipen = 0)

}

sink()

