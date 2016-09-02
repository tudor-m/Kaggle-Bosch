library(data.table)

nLoadRows = 200

train.num = fread('../data/train_numeric.csv',header = TRUE,nrows = nLoadRows)
train.cat = fread('../data/train_categorical.csv',header = TRUE,nrows = nLoadRows)
train.dat = fread('../data/train_date.csv',header = TRUE,nrows = nLoadRows)

test.num = fread('../data/test_numeric.csv',header = TRUE,nrows = nLoadRows)
test.cat = fread('../data/test_categorical.csv',header = TRUE,nrows = nLoadRows)
test.dat = fread('../data/test_date.csv',header = TRUE,nrows = nLoadRows)
