library(data.table)
source("futil.R")

file_list = vector();
file_list = append(file_list,"submission-script-001-94.3.csv")
file_list = append(file_list,"submission-script-4-001-93.csv")
file_list = append(file_list,"submit.wip.001-2.MEAN.csv")
file_list = append(file_list,"./public/xgbsubmission08.csv")
file_list = append(file_list,"./public/xgbsubmission09.csv")
file_list = append(file_list,"./public/xgbsubmission10.csv")
file_list = append(file_list,"./public/xgbsubmission20.csv")
file_list = append(file_list,"./public/xgbsubmission30.csv")

pred_file_list = list()
for ( i in 1:length(file_list))
  pred_file_list = append(pred_file_list,fread(file_list[i],select = "Response"))

pred_file_total = 0
for (i in 1:length(pred_file_list))
{
  pred_file_total = pred_file_total + pred_file_list[[i]]
}

for (i in 1:length(pred_file_list))
{
  print(sum(pred_file_list[[i]]))
}

dt_ens = 0*pred_file_total
dt_ens[which(pred_file_total>=2)] = 1

print(sum(dt_ens))

dt <- data.table(Id=Id,Response = dt_ens)

print("Store prediction")

write.csv(dt, "submission.csv", row.names = F)

