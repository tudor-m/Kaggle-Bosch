i=2;j=3;plot(dev.num[[i]],dev.num[[j]]);points(dev.num[[i]][idx1],dev.num[[j]][idx1],col='red')


for (j in 2:ncol(train.num3))
{
  coltmp = unlist(train.num3[,j,with=FALSE])
  print(c(j,min(coltmp,na.rm = TRUE),max(coltmp,na.rm = TRUE)))
}

a = c(1,2,3,4,8:20,seq(20,-5,-1))

std3T <- function(inval)
{
  # Returns for each element of inval:
  # 0 if N/A
  # 1 if it's in the 1st standard deviation
  # 2 if it's in the 2nd standard deviation
  # 3 if it's in the 3rd standard deviation
  s = sd(inval,na.rm = TRUE)
  m = mean(inval,na.rm = TRUE)
  outval = 3+0*inval
  idx = which(inval >= m-2*s)
  outval[idx] = 2
  idx = which(inval >= m-s)
  outval[idx] = 1
  idx = which(inval >= m+s)
  outval[idx] = 2
  idx = which(inval >= m+2*s)
  outval[idx] = 3
  idx = which(is.na(inval))
  outval[idx] = 0
  return(outval)
}

b=std3T(a)

for (j in 1:(ncol(train.num3)-1)) {train.num3.std[[j]] = std3T(unlist(train.num3[,j,with=FALSE]))}


plot(rowMeans(train.num3.std,na.rm = TRUE)[1:1000],train.num3.std$Response)
Error in xy.coords(x, y, xlabel, ylabel, log) : 
  'x' and 'y' lengths differ
> plot(rowMeans(train.num3.std,na.rm = TRUE)[1:1000],train.num3.std[1:1000,]$Response)
> plot(rowSums(train.num3.std,na.rm = TRUE)[1:1000],train.num3.std[1:1000,]$Response)
> plot(apply(train.num3.std[1:1000],MARGIN = 1,function(x) {sum(x==3)}),train.num3.std[1:1000,]$Response)
> plot(apply(train.num3.std[1:5000],MARGIN = 1,function(x) {sum(x==3)}),train.num3.std[1:5000,]$Response)
> c(1,2,3,4)*rnorm(4,1,0.2)
[1] 0.9216191 2.0484107 3.4164664 3.6084780
> plot(apply(train.num3.std[1:5000],MARGIN = 1,function(x) {sum(x==3)}),rnorm(5000,1,0.2)*train.num3.std[1:5000,]$Response)
plot(apply(train.num3.std[1:5000],MARGIN = 1,function(x) {sum(x==3)}),rnorm(5000,1,0.1)*(1+train.num3.std[1:5000,]$Response))
plot(apply(train.num2.std,MARGIN = 1,function(x) {sum(x==3)}),rnorm(nrow(train.num2.std),1,0.2)*(1+train.num2.std$Response))

     