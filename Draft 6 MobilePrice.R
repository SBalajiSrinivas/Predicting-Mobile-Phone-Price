#Libraries
library(keras)
library(dplyr)
library(magrittr)
library(neuralnet)
path <- "/Users/balajisrinivas/Desktop/Msc BADS/Sem2/BADS Dissertation/Data/Final Data for phone .csv"
df<-read.csv(path)


summary(df)


df <- mutate_if(df, is.character, as.factor)

df %<>% mutate_if(is.factor,as.numeric)


#using multiple imputation
library(mice)
my_imp <- mice(df, m = 5, method = c("","","","","","","","","","","rf"),maxit = 20)

final_df<-complete(my_imp)
summary(final_df)
library(mice)
md.pattern(final_df)

# Visualisation

library(ggplot2)

ggplot(data = final_df) + 
  geom_point(mapping = aes(x = nfc, y = Price)) 



ggplot(data = final_df) + 
  geom_bar(mapping = aes(x = nfc, fill = "Red"))
p <- tablePrepare(final_df)
library(Rcmdr)
c<-cor(final_df,c("Screen_size","RAM","Battery","Weight","Internal_Memory","Cam1MP","Cam2MP","Price"))
library(corrplot)
library(RColorBrewer)
n <- neuralnet(Price ~ Screen_size+RAM+Battery+weight+memoryslot+Internal_Memory+Cam1MP+Cam2MP+gps+nfc,
               data = final_df,
               hidden = c(10,5),
               linear.output = F,
               lifesign = 'full',
               rep=1)
plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')
#Matrix
data<-as.matrix(final_df)
dimnames(data) <- NULL

#Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1,1:10]
test <- data[ind==2, 1:10]
trainingtarget <- data[ind==1, 11]
testtarget <- data[ind==2, 11]

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)


# Create Model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(10)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# Compile
model %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(lr = 0.002),
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      
      validation_split = 0.2)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)

