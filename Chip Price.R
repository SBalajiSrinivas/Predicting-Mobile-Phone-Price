path <- "/Users/balajisrinivas/Desktop/Msc BADS/Sem2/BADS Dissertation/Data/GEEKBENCH.csv"
df <- read.csv(path)
model <- lm(Price~Single_core_score+Multi_core_score+Number_of_cores, data = df)
print(model)
a <- coef(model)[1]
print(a)

single_core_score <- coef(model)[2]
multi_core_score <- coef(model)[3]
no_of_cores <- coef(model)[4]

print(single_core_score)
print(multi_core_score)
print(no_of_cores)

#Y = a+single_core_score*x1+multi_core_score*x2+no_of_cores*x3
x1 <- readline(prompt = "Enter the single core score : ");
x2 <- readline(prompt = "Enter the multi core score : ");
x3 <- readline(prompt = "Enter the number of cores : ");
x1 = as.integer(x1)

x2 = as.integer(x2)

x3 = as.integer(x3)
Price_Predicted <- a+single_core_score*x1+multi_core_score*x2+no_of_cores*x3
if(Price_Predicted < 0)
  Price_Predicted = -1*Price_Predicted
Price_Predicted
