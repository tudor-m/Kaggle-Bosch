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
#file_list = append(file_list,"./public/0.40367/xgbsubmission30.csv")

pred_file_list = list()
for ( i in 1:length(file_list))
  pred_file_list = append(pred_file_list,fread(file_list[i],select = "Response"))

coef = c(0.3,0.3,0.6,0.9,0.9,0.6)
pred_file_total = 0
for (i in 1:length(pred_file_list))
{
  pred_file_total = pred_file_total + coef[i]*pred_file_list[[i]]
}

for (i in 1:length(pred_file_list))
{
  print(sum(pred_file_list[[i]]))
}

dt_ens = 0*pred_file_total
dt_ens[which(pred_file_total>=1.6)] = 1

print(sum(dt_ens))

Id = fread(file_list[i],select = "Id")
Id = as.vector(Id[[1]])
dt <- data.table(Id=Id,Response = dt_ens)

print("Store prediction")

write.csv(dt, "submission.csv", row.names = F)


# Load some predictions for analysis:
some_pred = list()

some_pred[[1]] = fread("submission-ensembler.10.csv",select = "Response")
some_pred[[2]] = fread("submission-ensembler.11.csv",select = "Response")
some_pred[[3]] = fread("submission-ensembler.12.csv",select = "Response")
some_pred[[4]] = fread("submission-ensembler.13.csv",select = "Response")

