library(moments)


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
  
  
  

errMeasure2 <- function (vPred, vTarget)
{
  if (length(vPred) != length(vTarget))
    return(-1);
  s1 = vPred
  s2 = vTarget
  s12 = (s1-s2)^2
  return(list(sqrt(mean(s12,na.rm = TRUE)),(s1-s2),s12))
}

errMeasure1 <- function (vPred, vTarget)
{
  vPred = as.data.frame(vPred)
  vTarget = as.data.frame(vTarget)
  vPred$V1 <- NULL
  s1 = sum(vPred[,1]==vTarget[,1])
  s2 = sum(vPred[,2]==vTarget[,1])
  s3 = sum(vPred[,3]==vTarget[,1])
  ret = (s1+0.5*s2+1/3*s3)/nrow(vPred)
  return(ret)
  #vPred = cbind(vPred,vTarget);
  #ret = apply(vPred,MARGIN=1,function(x) {ix = which(x[1:3]==x[4]);v=0; if(length(ix) != 0) v=1/ix; return(v)});
  #return(mean(ret))
}


makeSubmissionFromForecastFile <- function(csvFile,s)
{
  options(scipen = 999)
  tmp = read.csv(file=csvFile,header=FALSE)
  nr = nrow(tmp)
  nc = ncol(tmp)
  predMatrix = as.data.frame(matrix(data=tmp,nrow=nr,ncol=nc))
  makeSubmission(predMatrix,s)
  options(scipen = 0)
  return(predMatrix)
}

makeSubmission <- function(predMatrix,s)
{
  options(scipen = 999)
  fname.csv = paste("submission",s,".csv",sep="")
  fname.rda = paste("submission",s,".rda",sep="")
  predFinal <- NULL
  predFinal <- predMatrix

  save(predFinal,file=fname.rda)
  write.table(predFinal,file=fname.csv,sep=",",row.names=FALSE,col.names=c("id","relevance"))
  options(scipen = 0)
}

sumForecastFiles <- function(inFileList,outFile,nr,sumW=NA)
{
  # in File List, list with forecast files to be combined
  # format: csvFile, csv format: Id, 60 intraday returns, 2 daily returns
  # You should assign nr = 120000 for the contest
  # sumW are the sum weight. default, 1, the result is the sum. Can be tune to compute the average, etc
  options(scipen = 999)
  nc = 63
  nf = length(inFileList)
  if (is.na(sumW[1]))
    wcf = 1+vector(mode="numeric",length=nf)
  if (!is.na(sumW[1]))
    wcf = as.vector(sumW,mode="numeric")
  predMatrix = as.data.frame(matrix(nrow=nr,ncol=nc))
  predMatrix[is.na(predMatrix)] <- 0
  predMatrix[,1]=1:nr
  predMatrixTmp = as.data.frame(matrix(nrow=nr,ncol=nc))
  predMatrixTmp[is.na(predMatrixTmp)] <- 0
  predMatrixTmp[,1]=1:nr

  for (i in 1:nf)
  {
    tmp = read.csv(file=inFileList[[i]],header=TRUE)
    predMatrixTmp[tmp[,1],1:ncol(tmp)] = tmp
    predMatrixTmp[is.na(predMatrixTmp)] <- 0
    
    predMatrix[,2:nc] = predMatrix[,2:nc] + wcf[[i]]*predMatrixTmp[,2:nc]
  }
  write.table(predMatrix,file=outFile,row.names = FALSE,col.names = FALSE,sep=",")  
  return(predMatrix);
}





