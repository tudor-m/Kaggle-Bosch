if (1 == 1)
{
  rm(list=ls())
  gc()
}

library(data.table)
library(xgboost)
source("futil.R")


if (1 == 0) #run only once
{
  train.num = fread('../data/train_numeric.csv',header = TRUE)
  trainTargetData = as.data.table(cbind(train.num$Id,train.num$Response))
  setnames(trainTargetData,c("Id","Response"))
  write.csv(trainTargetData[,.(Id,Response)],"../data/train_response.csv", row.names = FALSE)
  remove(train.num)
  remove(trainTargetData)
}

sink(file="output.R.txt",append = TRUE, split=TRUE)
timestamp()

#"QnD" solution : ("quick and dirty")
# replace NA with 0 - not needed because xboost handles NAs
# arrind = which(is.na(train.num),arr.ind=T)
# train.num[arrind] = 0

nLoadRows = 100000
VERBOSE = 1
if (1 == 0)
{
  train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = nLoadRows)
  train.cat = fread('../data/train_categorical.csv',header = TRUE,nrows = nLoadRows)
  train.dat = fread('../data/train_date.csv',header = TRUE,nrows = nLoadRows)

  test.num = fread('../data/test_numeric.csv',header = TRUE,nrows = nLoadRows)
  test.cat = fread('../data/test_categorical.csv',header = TRUE,nrows = nLoadRows)
  test.dat = fread('../data/test_date.csv',header = TRUE,nrows = nLoadRows)
}


# ========== Initialize the features =================

train.target = fread('../data/train_response.csv',header = TRUE,nrows = nLoadRows)
train.dat = fread('../data/train_date.csv',header = TRUE,nrows = nLoadRows)

#SPLIT train in development (50%) and cross-validation (20%)
devInd = 1:round(0.5*nrow(train.target))
cvInd = (1+last(devInd)):nrow(train.target)

dev.feat   = train.dat[devInd,-"Response",with=F]
cv.feat    = train.dat[cvInd,-"Response",with=F]
dev.target = train.target[devInd]
cv.target  = train.target[cvInd]


# ========== FIT and PREDICT on CrossValidation=================

# FIT on dev ...
dtrain <- xgb.DMatrix(data = as.matrix(dev.feat), label=dev.target$Response, missing = NA)
dtest <- xgb.DMatrix(data = as.matrix(cv.feat), label=cv.target$Response , missing = NA)

watchlist <- list(train = dtrain, test = dtest)

for (thr in c(0.25)) {
  print(c("threshold: ",thr ))
mccEval <- function(preds, dtrain)
{
  labels = getinfo(dtrain, "label")
  err = as.numeric(errMeasure4(preds,labels,thr))
  return(list(metric="error",value=err))
}

for (min_child_w in 11:11) {
  for (max_d in 11:11) {
    print(c("max_d: ",max_d))
    #print(c("fmla= ",fmla_c))
    print(c("min_child_weight: ",min_child_w))

    nround = 150
    param <- list(  
      #objective           = "multi:softprob", num_class = 4,
      #objective           = "reg:linear",
      objective           = "binary:logistic",
      booster             = "gbtree",
      #booster             = "gblinear",
      base_score          = 0.8,
      eta                 = 0.01,#0.05, #0.02, # 0.06, #0.01,
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
    
    if (1==0) {
      set.seed(100)
      fit.cv.res = xgb.cv(param, dtrain,nrounds = nround,nfold = 5,metrics = "error",showsd = FALSE,prediction = TRUE)
    }
    
    set.seed(100)
    fit.dev = xgb.train(params=param,dtrain,nrounds=nround,print.every.n = 2,maximize = FALSE,watchlist)
    if (1==0) {
      xgb.plot.importance(xgb.importance(model=fit.train))
      head(xgb.importance(model=fit.train))
    }
    # saveDataT(fit.train,DATA_SET,paste(as.character(c("fit.train",".",jBin,".",jj)),collapse = ""))
    # PREDICT on cv ...
    pred_cv = predict(fit.dev, as.matrix(cv.feat),missing = NA)
    #pred_test[which(pred_test<0)] = 0
    err_pred_cv = errMeasure4(pred_cv,cv.target$Response,thr)
    if (VERBOSE == 1){
      print(err_pred_cv)
    }
    #pred_test_xgb[[jj]] = pred_cv
    #err_pred_test_xgb[[jj]] = err_pred_cv
  }
}
}

if (1 == 0)
{
  rm(list=ls())
  gc()
}



# ========== PREDICT for Submission=================


if ( 1==0)
{
# Use the model to produce a dirty submission:
test.feat = fread('../data/test_date.csv',header = TRUE)

# predict on test ...
pred_test = predict(fit.dev,as.matrix(test.feat),missing = NA)

# prepare the submission file
thr = 0.25
pred_test_submission = pred_test
pred_test_submission[which(pred_test <= thr)] = 0
pred_test_submission[which(pred_test >  thr)] = 1

predData   = as.data.table(cbind(test.feat$Id,pred_test))
setnames(predData,c("Id","Response"))
submitData = as.data.table(cbind(test.feat$Id,pred_test_submission))
setnames(submitData,c("Id","Response"))
options(scipen = 999)
if (1==1)
{
  write.csv(predData[,.(Id,Response)],"pred.qnd.002-1.csv", row.names = FALSE)
  write.csv(submitData[,.(Id,Response)],"submit.qnd.002-1.csv", row.names = FALSE)
}
options(scipen = 0)
}

sink()

