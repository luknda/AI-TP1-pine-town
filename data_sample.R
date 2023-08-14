data = read.csv("data\\neo.csv")
data_sample1 =  data[sample(nrow(data), size = 10000, replace = FALSE), ]
