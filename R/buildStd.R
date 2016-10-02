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


for (j in 2:(ncol(train.num)-1))
{
  tmp = std3T(rbind(unlist(train.num[,j,with=FALSE]),unlist(test.num[,j,with=FALSE])))
  train.num[[j]] = tmp[1:nrow(train.num)]
  test.num[[j]] = tmp[(1+nrow(train.num)):(nrow(train.num)+nrow(test.num))]
}






lines_seq = 0:3
stations_seq = 0:51

# Map the train numerical ids to the plant elements (lines and stations):
line.group.train = list()
train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = -1)
train.num.plant = data.table(Id=train.num$Id)
for (L in lines_seq)
{
  Lname = paste(c("L",L),collapse="")
  cols = which(L == lns[["train.num"]]$lines)
  rows = which(apply(train.num[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
  train.num.plant[[Lname]] = 0
  train.num.plant[[Lname]][rows] = 1
}
for (S in stations_seq)
{
  Sname = paste(c("S",S),collapse="")
  cols = which(S == lns[["train.num"]]$stations)
  rows = which(apply(train.num[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
  train.num.plant[[Sname]] = 0
  train.num.plant[[Sname]][rows] = 1
}
for (L in lines_seq)
  for (S in stations_seq)
  {
    Lname = paste(c("L",L),collapse="")
    Sname = paste(c("S",S),collapse="")
    LSname = paste(c(Lname,"_",Sname),collapse ="")
    cols = which(S == lns[["train.num"]]$stations & L == lns[["train.num"]]$lines)
    print(LSname)
    rows = which(apply(train.num[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
    train.num.plant[[LSname]] = 0
    train.num.plant[[LSname]][rows] = 1
  }
saveDataT(train.num.plant,"train","train.num.plant")
train.num.response = train.num$Response
saveDataT(train.num.response,"train","train.num.response")
train.num.Id = train.num$Id
saveDataT(train.num.Id,"train","train.num.id")
remove(train.num)
remove(line.group.train)
remove(train.num.plant)
remove(train.num.response)
remove(train.num.Id)
gc()

# Map the train categorical ids to the plant elements (lines and stations):
line.group.train = list()
train.cat = fread('../data/train_categorical.csv',header = TRUE,nrows = -1,data.table=TRUE)
gc()
train.cat.plant = data.table(Id=train.cat$Id)
for (L in lines_seq)
{
  Lname = paste(c("L",L),collapse="")
  cols = which(L == lns[["train.cat"]]$lines)
  rows = 0*vector(length=nrow(train.cat))
  for ( j in 1:nrow(train.cat))
  {
    tmp = train.cat[j,cols,with=FALSE]
    if (length(cols) != sum(is.na(tmp)))
      rows[j] = 1
  }
  train.cat.plant[[Lname]] = 0
  train.cat.plant[[Lname]][rows] = 1
  gc()
}
for (S in stations_seq)
{
  Sname = paste(c("S",S),collapse="")
  cols = which(S == lns[["train.cat"]]$stations)
  rows = which(apply(train.cat[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
  train.cat.plant[[Sname]] = 0
  train.cat.plant[[Sname]][rows] = 1
  gc()
}
for (L in lines_seq)
  for (S in stations_seq)
  {
    Lname = paste(c("L",L),collapse="")
    Sname = paste(c("S",S),collapse="")
    LSname = paste(c(Lname,"_",Sname),collapse ="")
    cols = which(S == lns[["train.cat"]]$stations & L == lns[["train.cat"]]$lines)
    print(LSname)
    rows = which(apply(train.cat[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
    train.cat.plant[[LSname]] = 0
    train.cat.plant[[LSname]][rows] = 1
    gc()
  }
saveDataT(train.cat.plant,"train","train.cat.plant")
remove(train.cat)
remove(line.group.train)
gc()

line.group.test = list()
test.num = fread('../data/test_numeric.csv',header = TRUE,nrows = -1)
test.num.plant = data.table(Id=test.num$Id)
for (L in lines_seq)
{
  Lname = paste(c("L",L),collapse="")
  cols = which(L == lns[["test.num"]]$lines)
  rows = which(apply(test.num[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
  test.num.plant[[Lname]] = 0
  test.num.plant[[Lname]][rows] = 1
}
for (S in stations_seq)
{
  Sname = paste(c("S",S),collapse="")
  cols = which(S == lns[["test.num"]]$stations)
  rows = which(apply(test.num[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
  test.num.plant[[Sname]] = 0
  test.num.plant[[Sname]][rows] = 1
}
for (L in lines_seq)
  for (S in stations_seq)
  {
    Lname = paste(c("L",L),collapse="")
    Sname = paste(c("S",S),collapse="")
    LSname = paste(c(Lname,"_",Sname),collapse ="")
    cols = which(S == lns[["test.num"]]$stations & L == lns[["test.num"]]$lines)
    print(LSname)
    rows = which(apply(test.num[,cols,with=F],MARGIN = 1, function(x) length(x)!=sum(is.na(x))))
    test.num.plant[[LSname]] = 0
    test.num.plant[[LSname]][rows] = 1
  }

saveDataT(test.num.plant,"test","test.num.plant")
remove(test.num)
remove(line.group.test)
gc()

if (1 == 1)
{
  rm(list=ls())
  gc()
}

lineClasses = rowSums(t(t(train.num.plant[,2:5,with=F])*c(1,2,4,8)))
for ( j in sort(unique(lineClasses))) { idx=which(lineClasses==j);tmp1=sum(train.num.response[idx]);tmp2=length(idx);print(c(j,tmp1,tmp2,tmp1/tmp2))}
lineClasses = rowSums(t(t(test.num.plant[,2:5,with=F])*c(1,2,4,8)))
for ( j in sort(unique(lineClasses))) { idx=which(lineClasses==j);tmp2=length(idx);print(c(j,tmp2))}