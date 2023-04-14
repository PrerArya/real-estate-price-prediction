#importing the datasets
train <- read.csv("housing_train.csv")
test <- read.csv("housing_test.csv")

                    #######     exploratory data analysis   #########

#summary of the data train
summary(train)

#getting the head values
head(train)

#getting data type of each attribute of the data set
str(train)

#shape of data
dim(train)  # data has 7536 col and 16 row

# Find the attributes that contain missing values
na_variable <- names(train)[sapply(train, function(var) mean(is.na(var)) > 0)]
na_variable 

# Calculate the proportion of missing values in the columns with missing values
data_na <- sapply(train[na_variable], function(var) mean(is.na(var)))
data_na

#data has more than 5% of missing values
#using median imputation for the missing values
for (i in 1:ncol(train)) {
  if (is.numeric(train[[i]])) {
    train[[i]] <- ifelse(is.na(train[[i]]), median(train[[i]], na.rm = TRUE), train[[i]])
  }
}
sum(is.na(train))

#data has more than 5% of missing values
#using median imputation for the missing values
for (i in 1:ncol(test)) {
  if (is.numeric(test[[i]])) {
    test[[i]] <- ifelse(is.na(test[[i]]), median(test[[i]], na.rm = TRUE), test[[i]])
  }
}
sum(is.na(test))


                      #########    DATA PREPARATION    ##########

#combining both train and test data sets
test$Price = NA
train$data ='train'
test$data  ='test'
net  = rbind(train,test)
apply(net,2,function(x)sum(is.na(x)))#check the null values

#Deleting variable address as it is unique.
net = net %>% select(-Address)

Dummy_variables = function(data,var,freq_count=0){
  t = table(data[,var])#selecting the col
  t = t[t > freq_count]
  t = sort(t)
  categories = names(t)[-1]#extract the category names
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)#replaces all occurrences of spaces in the name string with empty string
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)#create a new col with name of cat
  }
  #finally droping the original var col
  data[,var]=NULL
  return(data)
}

#getting our categorical variables:
names(net)[sapply(net,function(x) is.character(x))]
#check cardinality of categorical variables:
length(unique(net$SellerG))#198
length(unique(net$CouncilArea))#120

rest_col= c("SellerG","CouncilArea","Method","Suburb","Type")

for(cat in rest_col){
  net=Dummy_variables(net,cat,100) 
}

#Separating test and train:
train_new = net %>%  filter(data=='train') %>%  select(-data)
test_new =  net %>% filter(data=='test') %>%  select(-data,-Price)
#dividing train data to 75:25 manner
set.seed(123)
s=sample(1:nrow(train_new),0.75*nrow(train_new))
train_1=train_new[s,] 
test_2=train_new[-s,]


                  ########       MODEL BUILDING       #########

#we will be using 75% data for train and rest for performance
library(car)

first_lr = lm(Price ~ .,data=train_1)
summary(first_lr)

#removing data values with p>0.05:
# Get the summary of the linear regression model
lm_summary <- summary(first_lr)
# Extract the p-values for each predictor variable
p_values <- lm_summary$coefficients[,4]
# Identify the predictor variables whose p-values are greater than 0.05
values = lm_summary$coefficients[p_values > 0.05, ]

first_lr=lm(Price ~ .-SellerG_Hodges-SellerG_McGrath-SellerG_Noel-SellerG_Gary-SellerG_Jas-SellerG_Greg-SellerG_Sweeney-SellerG_Fletchers-SellerG_Woodards-SellerG_Brad-SellerG_Ray-SellerG_Buxton-SellerG_Barry-SellerG_Nelson-CouncilArea_Manningham-CouncilArea_Brimbank-CouncilArea_Bayside-CouncilArea_Melbourne-CouncilArea_Banyule-CouncilArea_PortPhillip-CouncilArea_Yarra-CouncilArea_GlenEira-CouncilArea_MooneeValley-CouncilArea_Moreland-Method_SP-Method_PI-Suburb_AscotVale-Suburb_Footscray-Suburb_MooneePonds-Suburb_MooneePonds-Suburb_Yarraville-Suburb_MalvernEast-Suburb_Carnegie-Suburb_Bentleigh-Suburb_PascoeVale-Suburb_Coburg-Suburb_Northcote-Suburb_Glenroy-Suburb_GlenIris-Suburb_Brunswick-Suburb_BentleighEast ,data=train_1)
summary(first_lr)


#performance check:
pred_25 = predict(first_lr,newdata =test_2)
pred_25 = round(pred_25,1)#predicted valuesn for test_2

#plot the visualization:
plot(test_2$Price,pred_25)

#FINAL MODEL BUILDING :
pred_price = predict(first_lr,newdata = test_new)
test_new$Predicted_Price = pred_price




