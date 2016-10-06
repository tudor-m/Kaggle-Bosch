if (1 == 1)
{
  rm(list=ls())
  gc()
}

library(data.table)
source("futil.R")
library(stringi)


#Extract all the line and station names in lns
numrows = -1
train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = numrows)
test.num = fread('../data/test_numeric.csv',header = TRUE,nrows = numrows)


ntn = ncol(train.num)-1
for (j in 2:ntn)
{
  #tmp = std3T(rbind(unlist(train.num[,j,with=FALSE]),unlist(test.num[,j,with=FALSE])))
  tmpCol = c(train.num[[j]],test.num[[j]])
  tmp = std3T((tmpCol))
  #tmp = std3T(c(unlist(train.num[,j,with=FALSE]),unlist(test.num[,j,with=FALSE])))
  train.num[[j]] = tmp[1:nrow(train.num)]
  test.num[[j]] = tmp[(1+nrow(train.num)):(nrow(train.num)+nrow(test.num))]
  print(j)
}

saveDataT(train.num,"train","train.num.std")
saveDataT(test.num,"test","test.num.std")

rm(train.num)
rm(test.num)
gc()
