if (1 == 1)
{
  rm(list=ls())
  gc()
}

library(data.table)
library(xgboost)
library(stringi)
source("futil.R")

VERBOSE = 1
sink(file="output.R.txt",append = FALSE, split=TRUE)
timestamp()

train.num.plant = getDataT("train","train.num.plant")
train.num.response = getDataT("train","train.num.response")
train.cat.plant = getDataT("train","train.cat.plant")

# Modelling
numrows = -1;
# Load all the input file:
train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = numrows)
#train.cat = fread('../data/train_categorical.csv',header = TRUE,nrows = numrows)
#train.dat = fread('../data/train_date.csv',header = TRUE,nrows = numrows)
#test.num = fread('../data/test_numeric.csv',header = TRUE,nrows = numrows)
#test.cat = fread('../data/test_categorical.csv',header = TRUE,nrows = numrows)
#test.dat = fread('../data/test_date.csv',header = TRUE,nrows = numrows)

idxrows1 = 1:200000
idxrows2 = subSample(train.num$Response[idxrows1],5,1000)
idxrows3 = 200001:400000
#idxrows = which(train.num.plant$L1==1 & train.num.plant$L0==0 & train.num.plant$L2==0 & train.num.plant$L4==0)
#idxcols = 169:681 #L1 related

train.num1 = train.num[idxrows1,]
train.num2 = train.num[idxrows2,] # subsampled
train.num3 = train.num[idxrows3,] # cv

train.num1$Response = train.num.response[idxrows1]
train.num2$Response = train.num.response[idxrows2]
train.num3$Response = train.num.response[idxrows3]

remove(train.num);
gc()

set.seed(100)

fit.dev.xgb.model=list()
# remove(train.num);
#gc()

for (thr in seq(0.25,0.75,0.1))
for (i in 1:1)
{
# Model1 XGB:
# Data:
  if (i==1)
  {
    #train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = numrows)
    dtrain <- xgb.DMatrix(data = as.matrix(train.num2[,-c("Id","Response"),with=F]), label=train.num2$Response, missing = NA)
    dtest  <- xgb.DMatrix(data = as.matrix(train.num3[,-c("Id","Response"),with=F]), label=train.num3$Response, missing = NA)
    #remove(train.num);
    #gc()
  }

  # Fit:
  watchlist <- list(train = dtrain, test = dtest)
  mccEval <- function(preds, dtrain)
  {
    labels = getinfo(dtrain, "label")
    err = as.numeric(errMeasure4(preds,labels,thr))
    return(list(metric="error",value=err))
  }
  for (min_child_w in seq(21,29,4)) {
    for (max_d in seq(21,29,4)) {
      print(c("max_d: ",max_d))
      print(c("min_child_weight: ",min_child_w))
      print(thr)
      nround = 80
      param <- list(  
        #objective           = "multi:softprob", num_class = 4,
        objective           = "binary:logistic",
        #objective           = "reg:linear",
        booster             = "gbtree",
        #booster             = "gblinear",
        base_score          = 0.5,
        eta                 = 0.05,#0.05, #0.02, # 0.06, #0.01,
        max_depth           = max_d, #changed from default of 8
        subsample           = 0.9, #0.9, # 0.7
        colsample_bytree    = 0.9, # 0.7
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
  pred_test_submission = pred_test[[i]]
  pred_test_submission[which(pred_test[[i]] <= thr)] = 0
  pred_test_submission[which(pred_test[[i]] >  thr)] = 1

  predData[[i]]   = as.data.table(cbind(test.num_Id,pred_test[[i]]))
  setnames(predData[[i]],c("Id","Response"))
  submitData[[i]] = as.data.table(cbind(test.num_Id,pred_test_submission))
  setnames(submitData[[i]],c("Id","Response"))
  options(scipen = 999)

  write.csv(predData[[i]][,.(Id,Response)],paste(c("pred.wip.001-4-4.",i,".csv"),sep="",collapse = ""), row.names = FALSE)
  write.csv(submitData[[i]][,.(Id,Response)],paste(c("submit.wip.001-4-4.",i,".csv"),sep="",collapse = ""), row.names = FALSE)

  options(scipen = 0)
}

# Mean:
cCoef = c(0.4,0.1,0.4,0.1)
combinedPred = 0
for (j in 1:length(cCoef))
  combinedPred = combinedPred + cCoef[j]*predData[[j]][,2,with=F]

combinedSubmission = combinedPred
combinedSubmission[which(combinedPred<=thr)] = 0
combinedSubmission[which(combinedPred>thr)] = 1

combinedSubmitData = as.data.table(cbind(test.num_Id,combinedSubmission))
setnames(combinedSubmitData,c("Id","Response"))
options(scipen = 999)
write.csv(combinedSubmitData[,.(Id,Response)],paste(c("submit.wip.001-4-4.","MEAN",".csv"),sep="",collapse = ""), row.names = FALSE)
options(scipen = 0)

}

sink()

