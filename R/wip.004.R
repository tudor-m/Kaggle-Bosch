for (rat in seq(5,5,1))
{
  rat = 5
  sink()
  

if (1 == 1)
{
  rm(list=setdiff(ls(),"rat"))
  gc()
}

kod = "simple"
#kod = "std"
#kod = "simple+std"
kod1 = "non-cluster"

library(data.table)
library(xgboost)
library(stringi)
source("futil.R")

VERBOSE = 1
sink(file="output.R.txt",append = TRUE, split=TRUE)
timestamp()

train.num.plant = getDataT("train","train.num.plant")
train.num.response = getDataT("train","train.num.response")
#train.cat.plant = getDataT("train","train.cat.plant")

# Modelling
numrows = -1;
#numrows = 1000;
numrows = 500000;
# Load all the input file:
if (kod == "simple")
{
train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = numrows)
#train.cat = fread('../data/train_categorical.csv',header = TRUE,nrows = numrows)
#train.dat = fread('../data/train_date.csv',header = TRUE,nrows = numrows)
#test.num = fread('../data/test_numeric.csv',header = TRUE,nrows = numrows)
#test.cat = fread('../data/test_categorical.csv',header = TRUE,nrows = numrows)
#test.dat = fread('../data/test_date.csv',header = TRUE,nrows = numrows)
}
idxrows1 = 1:300000
#idxrows2 = subSample(train.num$Response[idxrows1],2,1000)
#idxrows2 = subSample(train.num$Response[idxrows1],2,2000)
#idxrows2 = subSample(train.num$Response[idxrows1],2,3000)
train.num.response = getDataT("train","train.num.response")
idxrows2 = subSample(train.num.response[idxrows1],rat,3000)
print(c("rat = ",rat))
#idxrows2 = subSample(train.num$Response[idxrows1],5,1000)
#idxrows3 = 500001:nrow(train.num)
#idxrows3 = 200001:700000
idxrows3 = 300001:500000
#idxrows = which(train.num.plant$L1==1 & train.num.plant$L0==0 & train.num.plant$L2==0 & train.num.plant$L4==0)
#idxcols = 169:681 #L1 related
idxrows4 = 500001:700000

train.num1.plant = train.num.plant[idxrows1]
train.num2.plant = train.num.plant[idxrows2]
train.num3.plant = train.num.plant[idxrows3]
if (kod=="simple")
{
  train.num1 = train.num[idxrows1,]
  train.num2 = train.num[idxrows2,] # subsampled
  train.num3 = train.num[idxrows3,] # cv

  train.num1$Response = train.num.response[idxrows1]
  train.num2$Response = train.num.response[idxrows2]
  train.num3$Response = train.num.response[idxrows3]
}

if (kod == "std")
{
  
  #train.num1.std = train.num1; for (j in 2:(ncol(train.num1)-1)) {train.num1.std[[j]] = std3T(unlist(train.num1[,j,with=FALSE]))}
  #train.num2.std = train.num2; for (j in 2:(ncol(train.num2)-1)) {train.num2.std[[j]] = std3T(unlist(train.num2[,j,with=FALSE]))}
  #train.num3.std = train.num3; for (j in 2:(ncol(train.num3)-1)) {train.num3.std[[j]] = std3T(unlist(train.num3[,j,with=FALSE]))}
  
  train.num.std = getDataT("train","train.num.std")

  train.num1.std = train.num.std[idxrows1]
  train.num2.std = train.num.std[idxrows2]
  train.num3.std = train.num.std[idxrows3]
  from1 = -10
  to1 = -100
  from2 = 10
  to2 = 100
  for (j in 2:(ncol(train.num.std)-1))
  {
    print(j)
    idxtmp = which(train.num.std[[j]] == from1)
    train.num.std[[j]][idxtmp] = to1
    idxtmp = which(train.num.std[[j]] == from2)
    train.num.std[[j]][idxtmp] = to2
  }
  remove(train.num.std)
  gc()
  
  # for now change some values:
  from1 = -10
  to1 = -100
  from2 = 10
  to2 = 100
  for (j in 2:(ncol(train.num1.std)-1))
  {
    print(j)
    idxtmp = which(train.num1.std[[j]] == from1)
    train.num1.std[[j]][idxtmp] = to1
    idxtmp = which(train.num1.std[[j]] == from2)
    train.num1.std[[j]][idxtmp] = to2
  }
  for (j in 2:(ncol(train.num2.std)-1))
  {
    print(j)
    idxtmp = which(train.num2.std[[j]] == from1)
    train.num2.std[[j]][idxtmp] = to1
    idxtmp = which(train.num2.std[[j]] == from2)
    train.num2.std[[j]][idxtmp] = to2
  }
  for (j in 2:(ncol(train.num3.std)-1))
  {
    print(j)
    idxtmp = which(train.num3.std[[j]] == from1)
    train.num3.std[[j]][idxtmp] = to1
    idxtmp = which(train.num3.std[[j]] == from2)
    train.num3.std[[j]][idxtmp] = to2
  }
  
  remove(train.num.std)
  gc()
}
remove(train.num.plant)
remove(train.num);
gc()


if (kod1 == "cluster")
{
  #km=kmeans(train.num1.plant[,-c("Id","Response"),with=FALSE],centers = 10,algorithm="Lloyd",iter.max=100)
  #km=kmeans(train.num1.plant[,2:59,with=FALSE],centers = 10,algorithm="Lloyd",iter.max=100)
  km=kmeans(train.num1.plant[,2:6,with=FALSE],centers = 10,algorithm="Lloyd",iter.max=100)
  plot(km$cluster,rnorm(nrow(train.num1.plant),0,0.1)+train.num1.std$Response)

  km=kmeans(train.num.plant[,2:6,with=FALSE],centers = 10,algorithm="Lloyd",iter.max=100)
  plot(km$cluster,rnorm(nrow(train.num.plant),0,0.1)+train.num.response)
  
  for (jj in 1:length(km$size))
  {
    idxc = which(km$cluster == jj)
    centrec = as.data.frame(colMeans(train.num1.std[idxc,-c("Id","Response"),with=FALSE],na.rm = TRUE ))
    dlist = list()
    for (ii in idxc)
    {
      r = train.num1.std[ii,-c("Id","Response"),with=FALSE]
      d = dist(t(cbind(centrec,t(r))),method = "euclidian")
      dlist = append(dlist,d[[1]])
    }
  }
  plot(unlist(dlist),rnorm(length(idxc),0,0.1)+train.num1.std[idxc,]$Response)
  
  train.num1.plant[which(km$cluster==13),2:40,with=FALSE]
  plot(km$cluster,rnorm(nrow(train.num3.plant),0,0.1)+train.num3.std$Response)
  plot(km$cluster,rnorm(nrow(train.num3.plant),0,0.1)+train.num3.std$Response)
  idxc = which(km$cluster==1)
}
set.seed(100)

fit.dev.xgb.model=list()
# remove(train.num);
#gc()

for (thr in seq(0.6,0.6,0.05))
for (i in 1:1)
{
# Model1 XGB:
# Data:
  if (i==1)
  {
    #train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = numrows)
    set.seed(100)
    if (kod=="simple")
    {
      dtrain <- xgb.DMatrix(data = as.matrix(train.num2[,-c("Id","Response"),with=F]), label=train.num2$Response, missing = NA)
      dtest  <- xgb.DMatrix(data = as.matrix(train.num3[,-c("Id","Response"),with=F]), label=train.num3$Response, missing = NA)
    }
    if (kod == "std")
    {
      dtrain <- xgb.DMatrix(data = as.matrix(train.num2.std[,-c("Id","Response"),with=F]), label=train.num2.std$Response, missing = NA)
      dtest  <- xgb.DMatrix(data = as.matrix(train.num3.std[,-c("Id","Response"),with=F]), label=train.num3.std$Response, missing = NA)
      dtrain <- xgb.DMatrix(data = as.matrix(train.num2.std[,-c("Response"),with=F]), label=train.num2.std$Response, missing = NA)
      dtest  <- xgb.DMatrix(data = as.matrix(train.num3.std[,-c("Response"),with=F]), label=train.num3.std$Response, missing = NA)
    }
    if (kod=="simple+std")
    {
      dtrain <- xgb.DMatrix(data = cbind(as.matrix(train.num2[,-c("Id","Response"),with=F]),as.matrix(train.num2.std[,-c("Id","Response"),with=F])), label=train.num2$Response, missing = NA)
      dtest  <- xgb.DMatrix(data = cbind(as.matrix(train.num3[,-c("Id","Response"),with=F]),as.matrix(train.num3.std[,-c("Id","Response"),with=F])), label=train.num3$Response, missing = NA)
    }
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
  for (min_child_w in seq(10,10,2)) {
    for (max_d in seq(50,50,2)) {
      print(c("max_d: ",max_d))
      print(c("min_child_weight: ",min_child_w))
      print(thr)
      nround = 100
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


# Use the model to split data in 2 (positives and negatives), then further refine each half:
for (i in 1:1)
{
  mdl = fit.dev.xgb.model[[i]]
  pred_dev_mdl = predict(mdl,as.matrix(train.num3.std[,-c("Id","Response"),with=F]),missing = NA)
  
  idx_0 = which(pred_dev_mdl <= 0.65)
  idx_1 = which(pred_dev_mdl >  0.65)
  
  train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = numrows)
  train.num3 = train.num[idxrows3]
  remove(train.num)
  gc()
  train.num3.0 = train.num3[idx_0,]
  train.num3.1 = train.num3[idx_1,]
  
  for (thr in seq(0.6,0.6,0.05))
    for (i in 1:1)
    {
      # Model1 XGB:
      # Data:
      kod = "simple"
      if (i==1)
      {
        #train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = numrows)
        set.seed(100)
        if (kod=="simple")
        {
          dtrain <- xgb.DMatrix(data = as.matrix(train.num3.0[,-c("Id","Response"),with=F]), label=train.num3.0$Response, missing = NA)
          dtest  <- xgb.DMatrix(data = as.matrix(train.num3[,-c("Id","Response"),with=F]), label=train.num3$Response, missing = NA)
        }
        if (kod == "std")
        {
          dtrain <- xgb.DMatrix(data = as.matrix(train.num3.0.std[,-c("Id","Response"),with=F]), label=train.num2.std$Response, missing = NA)
          dtest  <- xgb.DMatrix(data = as.matrix(train.num3.std[,-c("Id","Response"),with=F]), label=train.num3.std$Response, missing = NA)
        }
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
      for (min_child_w in seq(10,10,2)) {
        for (max_d in seq(50,50,2)) {
          print(c("max_d: ",max_d))
          print(c("min_child_weight: ",min_child_w))
          print(thr)
          nround = 100
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
  
  
  
  
  
}

# Predict:
if (kod == "simple")
{
  pred_dev = list()
  for (i in 1:length(fit.dev.xgb.model))
  {
    pred_dev[[i]] = predict(fit.dev.xgb.model[[i]],as.matrix(train.num3[,-c("Id","Response"),with=F]),missing = NA)
  }
}


# Predict (std) :
if (kod == "std")
{
  pred_dev = list()
  for (i in 1:length(fit.dev.xgb.model))
  {
    pred_dev[[i]] = predict(fit.dev.xgb.model[[i]],as.matrix(train.num3.std[,-c("Id","Response"),with=F]),missing = NA)
  }
}

if (kod == "simple")
{
  print(c("thr = ",thr))
  print(errMeasure4(pred_dev[[1]],train.num3$Response,thr))
  idxtmp0 = 1:nrow(train.num3)
  idxtmp1 = which(train.num3$Response[idxtmp0] > 0.5) # responses
  idxtmp2 = which(pred_dev[[1]][idxtmp0] > thr)      # predicted
  if (1==0)
  {
    print(c("positives: ",train.num3$Id[idxtmp1]))
    print(c("predicted: ",train.num3$Id[idxtmp2]))
    print("intersect: ");print(intersect(idxtmp1,idxtmp2))
  }
  print("length of intersect: ");print(length(intersect(idxtmp1,idxtmp2)))
  print(c("error:     ",errMeasure4(pred_dev[[1]][idxtmp0],train.num3$Response[idxtmp0],thr)))
}

if (kod == "std")
for (thr in seq(0.6,0.6,0.05))
{
  print(c("thr = ",thr))
  print(errMeasure4(pred_dev[[1]],train.num3.std$Response,thr))
  idxtmp0 = 1:nrow(train.num3.std)
  idxtmp1 = which(train.num3.std$Response[idxtmp0] > 0.5) # responses
  idxtmp2 = which(pred_dev[[1]][idxtmp0] > thr)      # predicted
  if (1==0)
  {
  print(c("positives: ",train.num3.std$Id[idxtmp1]))
  print(c("predicted: ",train.num3.std$Id[idxtmp2]))
  print("intersect: ");print(intersect(idxtmp1,idxtmp2))
  }
  print("length of intersect: ");print(length(intersect(idxtmp1,idxtmp2)))
  print(c("error:     ",errMeasure4(pred_dev[[1]][idxtmp0],train.num3.std$Response[idxtmp0],thr)))
}

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

