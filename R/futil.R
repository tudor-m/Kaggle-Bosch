library(moments)

errMeasureRMSE <- function(v1,v2)
{
  sqrt(mean((v1 - v2)^2))
}

errMeasure <- function (vPred, vTarget)
{
  if (length(vPred) != length(vTarget))
    return(-1);
  s1 = log(1+vPred)
  s2 = log(1+vTarget)
  s12 = (s1-s2)^2
  return(list(sqrt(mean(s12,na.rm = TRUE)),(s1-s2),s12))
}

errMeasure3 <- function (vPred, vTarget)
{
  if (length(vPred) != length(vTarget))
    return(-1);
  s12 = (log1p(vPred)-log1p(vTarget))^2
  return(sqrt(mean(s12,na.rm = TRUE)))
}

errMeasure4 <- function (vPred, vTarget, threshold)
{
  # Mathew Correlation Coefficient
  vPred[which(vPred <= threshold)] = 0
  vPred[which(vPred > threshold)] = 1
  
  TP = sum((vTarget == 1 & vPred == 1))
  TN = sum((vTarget == 0 & vPred == 0))
  FP = sum((vTarget == 0 & vPred == 1))
  FN = sum((vTarget == 1 & vPred == 0))
  return(TP*(TN-FP/TP)/(sqrt(TP+FP)*sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TN+FN)))
}

errMeasure5 <- function (vPred, vTarget, threshold)
{
  vPred[which(vPred <= threshold)] = 0
  vPred[which(vPred > threshold)] = 1
  
  TP = sum((vTarget == 1 & vPred == 1))
  TN = sum((vTarget == 0 & vPred == 0))
  FP = sum((vTarget == 0 & vPred == 1))
  FN = sum((vTarget == 1 & vPred == 0))
  #return(TP/(FN+TP))
  return(TP)
}

errMeasure6 <- function (vPred, vTarget, threshold)
{
  vPred[which(vPred <= threshold)] = 0
  vPred[which(vPred > threshold)] = 1
  
  TP = sum((vTarget == 1 & vPred == 1))
  TN = sum((vTarget == 0 & vPred == 0))
  FP = sum((vTarget == 0 & vPred == 1))
  FN = sum((vTarget == 1 & vPred == 0))
  #return(abs((TP-TN)/TN))
  return(TP+FP)
}


saveDataT <- function(object,data.base.name,object.name,compress = FALSE)
{
  if (FALSE == dir.exists(paste("Rdatabase//",data.base.name,sep = "")))
    dir.create(path = paste("Rdatabase//",data.base.name,sep = ""),recursive = TRUE)
  saveRDS(object, file = paste("Rdatabase//",data.base.name,"//",object.name,".rds",sep = ""),compress)
}

getDataT <- function(data.base.name,object.name)
{
  object = readRDS(file = paste("Rdatabase//",data.base.name,"//",object.name,".rds",sep = ""))
} 
  
lines_and_stations <- function(object)
{
  #Extract all the line and station names
  allNames = names(object)
  lines = list()
  stations = list()
  for (i in 1:length(allNames))
  {
    s = allNames[i]
    undsc = stri_locate_all_fixed(s,"_")
    undsc1 = undsc[[1]][1,1]
    if (is.na(undsc1))
    {
      station = "*";
      line = "*";
    }
    else
    {
      undsc2 = undsc[[1]][2,1]
      line = stri_sub(s,from=1,to=undsc1-1)
      line = stri_replace(line,fixed="L",replacement = "")
      station = stri_sub(s,from=undsc1+1,to=undsc2-1)
      station = stri_replace(station,fixed="S",replacement = "")
    }
    lines = append(lines,line)
    stations = append(stations,station)
  }
  lns = cbind(allNames,lines,stations)
  names(lns) = c("names","lines","stations")
  return(as.data.frame(lns))
}

subSample <- function(response,ratio,seed)
{
  # eliminate negative (=0) resp elements
  # resulted ratio 0 to 1
  # started with random seed
  # returns the index of remaining elements in response
  p_index = which(response==1)
  n_index = which(response==0)
  p_count = sum(response == 1)
  n_count_final = round(p_count*ratio)
  set.seed(seed=seed)
  n_index_final = sample(n_index,n_count_final)
  return(sort(c(n_index_final,p_index)))
}

std3T <- function(inval)
{
  # Returns for each element of inval:
  # 0 if N/A
  # 1 if it's in the 1st standard deviation
  # 2 if it's in the 2nd standard deviation
  # 3 if it's in the 3rd standard deviation
  s = sd(inval,na.rm = TRUE)
  m = mean(inval,na.rm = TRUE)
  outval = -10+0*inval
  idx = which(inval >= m-3*s)
  outval[idx] = -10
  idx = which(inval >= m-2*s)
  outval[idx] = -2
  idx = which(inval >= m-s)
  outval[idx] = -1
  idx = which(inval >= m)
  outval[idx] = 1
  idx = which(inval >= m+s)
  outval[idx] = 2
  idx = which(inval >= m+2*s)
  outval[idx] = 10
  idx = which(inval >= m+3*s)
  outval[idx] = 10
  idx = which(is.na(inval))
  outval[idx] = 0
  return(outval)
}

dist1 <- function(v1,v2)
{
  v1[is.na(v1)] = 100
  v2[is.na(v2)] = 100
  sum(v1 != v2,na.rm = TRUE)
}
