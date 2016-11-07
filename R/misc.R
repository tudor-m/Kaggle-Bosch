i=2;j=3;plot(dev.num[[i]],dev.num[[j]]);points(dev.num[[i]][idx1],dev.num[[j]][idx1],col='red')


for (j in 2:ncol(train.num3))
{
  coltmp = unlist(train.num3[,j,with=FALSE])
  print(c(j,min(coltmp,na.rm = TRUE),max(coltmp,na.rm = TRUE)))
}

a = c(1,2,3,4,8:20,seq(20,-5,-1))



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

km=kmeans(train.num3.plant[,-c("Id"),with=FALSE],centers = 20,algorithm="Lloyd",iter.max=100)
train.num3.plant[which(km$cluster==13),2:40,with=FALSE]
plot(km$cluster,rnorm(nrow(train.num3.plant),0,0.1)+train.num3.std$Response)
plot(km$cluster,rnorm(nrow(train.num3.plant),0,0.1)+train.num3.std$Response)
plot(km$cluster,rnorm(nrow(train.num3.plant),0,0.1)+train.num3.std$Response)
idxc = which(km$cluster==1)


idxclist = list()
  for (i3 in 0:1)
    for (i2 in 0:1)
      for (i1 in 0:1)
        for (i0 in 0:1)
        {
          idxc = which(
                        train.num.plant[,2,with=FALSE]==i0 & 
                        train.num.plant[,3,with=FALSE]==i1 & 
                        train.num.plant[,4,with=FALSE]==i2 & 
                        train.num.plant[,5,with=FALSE]==i3)
          if (length(idxc) != 0)
          {
            cname = as.character(i0+2*i1+4*i2+8*i3)
            idxclist[[cname]] = idxc
            print(c(i0,i1,i2,i3))
            print(cname)
            print(c(length(idxc),sum(train.num.response[idxc]),sum(train.num.response[idxc])/length(idxc)))
          }
        }

# num columns:
l0numCols = 2:169
l1numCols = 170:682
l2numCols = 683:724
l3numCols = 725:969

# L3 products:
rows_c = idxclist[["8"]]
cols_c = l3numCols




km = kmeans(train.num[idxclist[["15"]],-c("Id","Response"),with=FALSE],centers = 4,algorithm="Lloyd",iter.max=100)

Major breakout I would say, the plots show almost 0 presence of positives for x>1500
> plot(rowSums(abs(train.num3.std[,2:969,with=FALSE])),train.num3.std$Response+rnorm(nrow(train.num3.std),mean=0,sd=0.1))
> plot(rowSums(abs(train.num1.std[,2:969,with=FALSE])),train.num1.std$Response+rnorm(nrow(train.num1.std),mean=0,sd=0.1))

importance <- xgb.importance(feature_names = names(train.num1), model = mdl)
head(importance)
ft = importance[1:10]$Feature
