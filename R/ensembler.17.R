library(data.table)
source("futil.R")

file_list = vector();
file_list = append(file_list,"submission-script-001-94.3.csv")   # PB: 0.39319
file_list = append(file_list,"submission-4-001-91.csv")          # PB: 0.39419
#file_list = append(file_list,"submission-script-4-001-93.csv")
#file_list = append(file_list,"submit.wip.001-2.MEAN.csv")
#file_list = append(file_list,"./public/xgbsubmission08.csv")
#file_list = append(file_list,"./public/xgbsubmission09.csv")
#file_list = append(file_list,"./public/xgbsubmission10.csv")
#file_list = append(file_list,"./public/xgbsubmission20.csv")
file_list = append(file_list,"./public/0.40681/xgbsubmission30.csv")
file_list = append(file_list,"./public/0.41798/xgbsubmission30.csv")
file_list = append(file_list,"./public/0.41984/xgbsubmission30.csv")
file_list = append(file_list,"./public/0.41984/xgbsubmission40.csv")
file_list = append(file_list,"./public/0.40367/xgbsubmission30.csv")
file_list = append(file_list,"./public/0.42082/xgbsubmission30.csv")

pred_file_list = list()
for ( i in 1:length(file_list))
  pred_file_list = append(pred_file_list,fread(file_list[i],select = "Response"))

cff = c(0.5,   0.5,   0.,   0.,   1,   0.,   0.,   1)
#cff = cff/sum(cff)
pred_file_total = 0
for (i in 1:length(pred_file_list))
{
  pred_file_total = pred_file_total + cff[i]*pred_file_list[[i]]
}

for (i in 1:length(pred_file_list))
{
  print(sum(pred_file_list[[i]]))
}

#for (ii in seq(0.2,1,0.05))
for (ii in seq(1.5,1.5,0.05))
{

dt_ens = 0*pred_file_total
dt_ens[which(pred_file_total>=ii)] = 1

print(sum(dt_ens))

Id = fread(file_list[i],select = "Id")
Id = as.vector(Id[[1]])
dt <- data.table(Id=Id,Response = dt_ens)

print("Store prediction")
write.csv(dt, "submission.csv", row.names = F)

# Load some predictions for analysis:
some_pred = list()

some_pred[[1]] = fread("submission-ensembler.10.csv",select = "Response") # 0.4199
some_pred[[2]] = fread("submission-ensembler.11.csv",select = "Response") # 0.41700
some_pred[[3]] = fread("submission-ensembler.12.csv",select = "Response") # 0.41955
some_pred[[4]] = fread("submission-ensembler.13.csv",select = "Response") # 0.41955
some_pred[[5]] = fread("submission-ensembler.14.csv",select = "Response") # 0.41964
some_pred[[6]] = fread("submission.csv",select = "Response") #

print(ii)
for (i in 1:5)
  for(j in 6:6)
  {
    err = errMeasure4(some_pred[[i]],some_pred[[j]],0.5);
    print(c(i,j,err));
  }
}


