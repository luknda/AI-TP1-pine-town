data = read.csv("data\\neo.csv")
data_sample =  data[sample(nrow(data), size = 10000, replace = FALSE), ]
data_sample_700 =  data[sample(nrow(data), size = 700, replace = FALSE), ]
