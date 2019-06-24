### Introductory Code
library(forecast)

library(ade4)

library(e1071)

library(ggplot2)

library("rpart")

library("rpart.plot")
library(corrplot)
library(plotly)
library(dplyr)
library(reshape2)
#install.packages("nFactors")
library(nFactors)
library(nnet)
#install.packages("stargazer")
library(stargazer)
library(Metrics)
#install.packages("rattle")
#library(rattle)
library(rpart)
library(randomForest)
library(ggplot2)
library(xgboost)
library(DT)
#install.packages("pROC")
library(pROC)
library(MASS)
#install.packages("performanceEstimation")
library(performanceEstimation)
#require(rattle)
train <- read.csv('train.csv')
colnames(train)
str(train)
test <- read.csv("test.csv")
View(var_kind)
var_kind<-c("Product_Info_", "Ins_Age", "Ht", "Wt","BMI","Employment_Info_","InsuredInfo_",
            "Insurance_History_", "Family_Hist_","Medical_History_", "Medical_Keyword_")

########## remove variables with excess NAs in both test and train#######

#rmNAvars<-function(dat,threshold){
 # dat<-dat[, -which(colMeans(is.na(dat)) > threshold)]
#}
#?colMeans
#train_clean<-rmNAvars(train,0.3)
#?intersect
test_clean<-test[,intersect(colnames(test), colnames(train_clean))]
#View(test_clean)
train_clean<-train
################### replacing Missing value with median ###########
sort(colSums(is.na(train_clean)),decreasing = TRUE)
#str(train_clean)
manage_na <- function(datafra)
{
  for(i in 1:ncol(datafra))
  {
    if(is.numeric(datafra[,i]))
    {
      datafra[is.na(datafra[,i]),i] <- median(datafra[!is.na(datafra[,i]),i])
    }
  }
  datafra
}

#####################Function and linear model



train_clean <- manage_na(train_clean)
test_clean <- manage_na(test)
#View(train_clean)
str(train_clean)
train_conti<-train_clean[,c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI",
                            "Employment_Info_1", "Employment_Info_4", "Employment_Info_6")] 

#View(train_conti)
#levels(train_clean$Medical_Keyword_46)
####### converting nonnumeric column to numeric ####
train_clean[,!(sapply(train_clean,class) == "numeric" | sapply(train_clean, class) == "integer")]<-
as.numeric(train_clean[,
            !(sapply(train_clean, class) == "numeric" | sapply(train_clean, class) == "integer")])
str(train_clean)
#?sapply
test_clean[,!(sapply(train_clean,class) == "numeric" | sapply(test_clean, class) == "integer")]<-
  as.numeric(test_clean[,
                         !(sapply(test_clean, class) == "numeric" | sapply(test_clean, class) == "integer")])
## **1.	Introduction**

#In a one-click shopping world, the life insurance application process is antiquated. 
#Customers provide extensive information to identify risk classification and eligibility, 
#including scheduling medical exams, a process that takes an average of 30 days for the purchase of 
#an Insurance product. Hence, only 40% of U.S. households own individual life insurance. 
#We aim to make the process of issuing life insurance quicker and less labor intensive for new and existing
#customers to get a quote while maintaining privacy boundaries. The model aims to automate the process
#of risk assessment for issue of various insurance products. This model will help the firm to generate
#more revenues by optimally targeting more profitable and less risky customers. The data set used includes
#the insurance company's earlier data-set that contains various parameters of an insurance application along
#with a risk score computed by the company's earlier internal model. The results will to better understand
#the predictive power of the data points in the existing assessment, enabling streamlining the process.


## **2.	Data used**

#The model data set is pre-separated among training and test sample in the ratio of 3:1 and
#have a random sampling being done. The train data set is a transactional (low level) data consists of 
#59,381 customers and 126 variables as predictors. The test dataset contains the same variables for another
#set of 19,766 customers. The variables are categorized based on the type of information they provide. 
#Due to proprietary reasons the variable names have been masked, however they have been numbered within 
#the category type for identification. The categories of variables present are: 
 

#* Product Information
#* Insurance Age
#* Height
#* Weight 
#* BMI
#* Employment Information
#* Insured Information
#* Insurance History
#* Family History
#* Medical History
#* Medical Keyword

#Each of these variable categories contain multiple variables they represent different items under 
#that variable class.

## **3. Explanatory Data Analysis**

### **3.1	Variable Types**

#A primary data analysis was performed through visual inspection of the training data-set to identify
#the different types of variables among Continuous, Categorical (Nominal and Ordinal) and
#Dummy (Binary/Indicator) variables. This analysis helps us identify the choice of variable selection
#and reduction algorithms in the next stage of modelling. 

temp1<- data.frame(Variable_Type = c("Product Information",
                                     "Insurance Age",
                                     "Height",
                                     "Weight", 
                                     "BMI",
                                     "Employment Information",
                                     "Insured Information",
                                     "Insurance History",
                                     "Family History",
                                     "Medical History",
                                     "Medical Keyword"))
#str(train_clean)
temp1$Continous<-c(1,1,1,1,1,3,0,1,4,0,0)
temp1$Categorical<-c(6,0,0,0,0,3,7,8,1,41,0)
temp1$Dummy<-c(0,0,0,0,0,0,0,0,0,0,48)
temp1$Total<-rowSums(temp1[,-1])
temp1[12,2:5]<-colSums(temp1[,-1])
temp1$Variable_Type[12]<-"Total"
temp1$Continous
temp1$Variable_Type
temp1[,2:5]
#View(temp1)
#str(temp1)
library(data.table)
data.table(temp1, 
          options = list(pageLength = 13,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}")))


##Continuous variables are analyzed using summary statistics, box plots and density plots. 
##The categorical variables are analyzed using event rate chart to track the variation to the response. 


### **3.2 Histogram of Response plot**


#The response is a nominal variable with levels from 1 to 8 and associates to the risk level of a customer.


p<-ggplot(train, aes(x=Response))+ geom_histogram(fill="Red", alpha=0.3)
p
ggplotly(p, color=~Response, width = 800, height = 400)%>%
            layout(title="Distribution of Response Variable", 
                   plot_bgcolor= "white", 
                   xaxis=list(gridcolor="lightgrey", opacity=0.5), 
                   yaxis=list(gridcolor="lightgrey",opacity = 0.5),
                   autosize = T, width = 800, height = 400)



#While it is not mentioned whether the scale is in increasing order of riskiness or otherwise, 
#from the distribution of the response variable we can infer that 8 could possibly refer to less 
#risky customers

missing_prct<-data.frame(variable=colnames(train),
                         missing=sapply(train,
                                        function(x){
                                          sum(is.na(x))
                                          }/nrow(train)
                                        )
                         )
missing_prct_test<-data.frame(variable=colnames(test),
                              missing=sapply(test,
                                             function(x){
                                            sum(is.na(x))
                                               }/nrow(test)
                                )
                              )

#missing_prct
#missing_prct_test
### **3.3	Summary Statistics**

#To allow for easier convergence of machine learning algorithms variables are normalized to the 
#range of [0, 1]. The most common normalizing function used is given below:
#X_{norm} = frac{x_{i}-x_{min}}{x_{max}-x_{min}}

#The same function had been applied to the continuous variables in the input data-set. 
#The summary statistics help understand the distribution of the underlying dataset,
#the box plots and density plots enable visualizing the data-set
## Generating Summary Table

summ_conti<-data.frame(Variables =  colnames(train_conti))
summ_conti$Min<-apply(train_conti,2,function(x){min(x, na.rm = T)})
summ_conti$Max<-apply(train_conti,2,function(x){max(x, na.rm = T)})
summ_conti$Mean<-apply(train_conti,2,function(x){mean(x, na.rm = T)})
summ_conti$Median<-apply(train_conti,2,function(x){median(x, na.rm = T)})
datatable(summ_conti, options = list(initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")))




### **3.4 Continuous Variable Analysis** {.tabset .tabset-fade}

#### Box Plots
#The box plots enable visualization of the data-set especially in relation to outliers. 
#However considering the large number of data


#plt<-htmltools::tagList()
#temp1<-train_conti[1:5]
p<-plot_ly( data=melt(temp1), type = "box",
            split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")
p
#plt[[1]] <- as_widget(p)
p<-plot_ly( data=melt(train_conti[,6:length(train_conti)]), type = "box",
            split = ~variable,y = ~value)%>%
  layout( title = "Box-Plots of variables")
p
#plt[[2]] <- as_widget(p)
#plt
#### Density Plot
#View(train_conti$Employment_Info_4)
#temp1$Ht


#The density plots help visualize the characteristics of the distribution 
#including statistical metrics such as mean, standard deviation and kurtosis. 
#It also enables us to visually identify if any relationship exists with the response variable.
#For example: The density plot of variable Employment_Info_6 is similar to the histogram of the 
#response variable, this probably indicated that this variable could be a good predictor of the 
#response variable
#train_conti[,1:2]
temp_melt<-melt(train_conti[,1:2])
#str(train_conti)
p1<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
p1
ggplotly(p1, height= 800, width = 1000)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent",autosize = F, width = 1000, height = 800)
temp_melt<-melt(train_conti[,c(3,4,5)])

p2<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
p2
ggplotly(p2, height= 800, width = 1000)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent",autosize = F, width = 1000, height = 800)
temp_melt<-melt(train_conti[,c(6,8)])
p3<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
p3
ggplotly(p3, height= 800, width = 1000)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent",autosize = F, width = 1000, height = 800)
temp_melt<-melt(train_conti[,7])
temp_melt$variable<-"Employment_Info_4"
p4<-ggplot(temp_melt,aes(value, fill = variable ))+geom_density(alpha = 0.5)+ggtitle("Density Plots")
p4
ggplotly(p4, height= 800, width = 1000)%>%
  layout(plot_bgcolor="transparent",paper_bgcolor= "transparent",autosize = F, width = 1000, height = 800)

### **3.5 Missing Value Analysis** {.tabset .tabset-fade}


#### Missing Value  Plots {.tabset #missing1}

#Missing value percentage charts evaluate whether the variable has sufficient 
#number of data records for predictions. The plot presents the percentage of observations missing for each variable



par(mfrow=c(2,2))
for(i in var_kind){
  plot(x=as.factor(as.character(missing_prct[grep(i, row.names(missing_prct)),1])),
       y=missing_prct$missing[grep(i, row.names(missing_prct))], ylim = c(0,1),
       main =gsub("_"," ",i))
  
}



#### Missing vs Response Chart


#This chart enables us to identify whether variables with a 
#high percentage of missing values actually help in predicting the response variable. 
#The distribution missing values for each response category has been retained within the 
#variable and hence the missing values are random in nature.


train_na_response <- sapply(sort(unique(train$Response)), function(x) { 
  apply(train[train$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train_na_response<-data.frame(train_na_response)
train_na_response<-train_na_response[which(rowSums(train_na_response)>0),]
train_na_response$ID<-rownames(train_na_response)
train_na_response_melt<-melt(train_na_response)
plot_ly(train_na_response_melt, x = ~ID, y =~value , color = ~variable)%>%
  layout(title ="Missing vs Response Chart")


### **3.6 Event Rate Chart**
#In an attempt to capture the conditional probability of the response given a specific bin 
#of the categorical variable 
#P(y=1|ProdInfo_2= A_1)=\frac{P(y=1  \cap  ProdInfo_2= A_1  )}{P(ProdInfo_2= A_1)}


#### Product Information
train_categ<-train_clean[,-which(colnames(train_clean) %in% colnames(train_conti))]
i="Product_Info"
train_temp<-train_categ[,grep(i,colnames(train_categ))]
index<-1
plt<-htmltools::tagList()
for (i in colnames(train_temp)){
  data_freq<-as.data.frame(table(train_temp[,i],train_clean$Response)/(as.data.frame(table(train_temp[,i]))[,2]))
  p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~Var2, type="bar")%>%
    layout(title = paste0("Event Rate Chart- ",gsub("_"," ",i)),
           xaxis = list(title = gsub("_"," ",i),showgrid = T))
  plt[[index]] <- as_widget(p)
  index <- index + 1
}
#plt



#### Employment Information



i="Employment_Info"
train_temp<-train_categ[,grep(i,colnames(train_categ))]
index<-1
plt<-htmltools::tagList()
for (i in colnames(train_temp)){
  data_freq<-as.data.frame(table(train_temp[,i],train_clean$Response)/(as.data.frame(table(train_temp[,i]))[,2]))
  p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~Var2, type="bar")%>%
    layout(title = paste0("Event Rate Chart- ",gsub("_"," ",i)),
           xaxis = list(title = gsub("_"," ",i),showgrid = T))
  plt[[index]] <- as_widget(p)
  index <- index + 1
}
plt


#### Insured Information

i="InsuredInfo"
train_temp<-train_categ[,grep(i,colnames(train_categ))]
index<-1
plt<-htmltools::tagList()
for (i in colnames(train_temp)){
  data_freq<-as.data.frame(table(train_temp[,i],train_clean$Response)/(as.data.frame(table(train_temp[,i]))[,2]))
  p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~Var2, type="bar")%>%
    layout(title = paste0("Event Rate Chart- ",gsub("_"," ",i)),
           xaxis = list(title = gsub("_"," ",i),showgrid = T))
  plt[[index]] <- as_widget(p)
  index <- index + 1
}
plt



#### Insurance History

i="Insurance_History"
train_temp<-train_categ[,grep(i,colnames(train_categ))]
index<-1
plt<-htmltools::tagList()
for (i in colnames(train_temp)){
  data_freq<-as.data.frame(table(train_temp[,i],train_clean$Response)/(as.data.frame(table(train_temp[,i]))[,2]))
  p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~Var2, type="bar")%>%
    layout(title = paste0("Event Rate Chart- ",gsub("_"," ",i)),
           xaxis = list(title = gsub("_"," ",i),showgrid = T))
  plt[[index]] <- as_widget(p)
  index <- index + 1
}
plt



#### Medical History

par(mfrow=c(2,2))  
i="Medical_History"
train_temp<-train_categ[,grep(i,colnames(train_categ))]
index<-1
plt<-htmltools::tagList()
for (i in colnames(train_temp)){
  data_freq<-as.data.frame(table(train_temp[,i],train_clean$Response)/(as.data.frame(table(train_temp[,i]))[,2]))
  p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~Var2, type="bar")%>%
    layout(title = paste0("Event Rate Chart- ",gsub("_"," ",i)),
           xaxis = list(title = gsub("_"," ",i),showgrid = T))
  plt[[index]] <- as_widget(p)
  index <- index + 1
}
plt



#### Medical Keyword


i="Medical_Keyword"
train_temp<-train_categ[,grep(i,colnames(train_categ))]
index<-1
plt<-htmltools::tagList()
for (i in colnames(train_temp)){
  data_freq<-as.data.frame(table(train_temp[,i],train_clean$Response)/(as.data.frame(table(train_temp[,i]))[,2]))
  p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~Var2, type="bar")%>%
    layout(title = paste0("Event Rate Chart- ",gsub("_"," ",i)),
           xaxis = list(title = gsub("_"," ",i),showgrid = T))
  plt[[index]] <- as_widget(p)
  index <- index + 1
}
plt


### **3.7 Correlation Plots** {.tabset .tabset-fade #corr}
#After data analysis and data treatment, the next stage of model development would be variable reduction. 
#### Product Information

i="Product_Info"
if(class(train_clean[,grep(i, colnames(train_clean))])=="data.frame"){
  m<-cor(train_clean[,c(grep(i, colnames(train_clean)),119)])
  corrplot(m, method = "number", type="lower")
}
#?corrplot
#corrplot(train_conti,method = "circle")
#### Employment Information

i="Employment_Info"
if(class(train_clean[,grep(i, colnames(train_clean))])=="data.frame"){
  m<-cor(train_clean[,c(grep(i, colnames(train_clean)),119)])
  corrplot(m, method = "number", type="lower")
}



#### Insured Information

i="InsuredInfo_"
if(class(train_clean[,grep(i, colnames(train_clean))])=="data.frame"){
  m<-cor(train_clean[,c(grep(i, colnames(train_clean)),119)])
  corrplot(m, method = "number", type="lower")
}




#### Insurance History

i="Insurance_History"
if(class(train_clean[,grep(i, colnames(train_clean))])=="data.frame"){
  m<-cor(train_clean[,c(grep(i, colnames(train_clean)),119)])
  corrplot(m, method = "number", type="lower")
}


#### Medical History

i="Medical_History"
if(class(train_clean[,grep(i, colnames(train_clean))])=="data.frame"){
  m<-cor(train_clean[,c(grep(i, colnames(train_clean)),119)])
  corrplot(m, method = "circle", type="lower")
}

  
  ### Medical Keyword
  
i="Medical_Keyword"
if(class(train_clean[,grep(i, colnames(train_clean))])=="data.frame"){
  m<-cor(train_clean[,c(grep(i, colnames(train_clean)),118)])
  corrplot(m, method = "circle", type="lower")
}
  ## **4. Variable treatment**
  
  ### **4.1 Missing data treatment**
  
  #As a preliminary step in data treatment, variables that have a high percentage
#of missing values are removed. While the threshold for removal is user determined, 
#for this exercise the threshold was 30%.

### **4.2 Missing Value treatment**

#For the variables that are not dropped at the previous step of modeling,
#variables that have missing values in lesser percentages are imputed. 
#The methodology used for imputation is using median of the remaining data series. 
#This is a commonly used industry practice and is efficient as the missing data for all variables 
#is randomly distributed over the response variable.

View(train)
### **4.3 PCA** {.tabset .tabset-fade #PCA}




#####################Tried by Ganesh #########################
#### Product Information
train_apca<-data.frame(ID=train_clean$Id)
#View(train_apca)
i="Product_Info"
mydata<-train_clean[,grep(i, colnames(train_clean))]
pc <- princomp(mydata,cor='TRUE')
summary(pc)
plot(pc)
View(pc$scores)
loadings(pc)
var(mydata)
pc$loadings###95 % data 
optimal_PCA<-6
#View(pc$scores[,1:6])
#mydata_hat<-predict(pc, as.data.frame(mydata))
#View(mydata_hat)
train_apca1<-cbind(train_apca,pc$scores[,1:optimal_PCA])
colnames(train_apca1)[grepl("PC",colnames(train_apca1))]<-paste0(i,1:optimal_PCA)
View(train_apca1)
#plotnScree(nS)


#### Employment Information
#train_apca<-data.frame(ID=train_clean$Id)
#View(train_apca)
i="Employment_Info"
mydata<-train_clean[,grep(i, colnames(train_clean))]
pc <- princomp(mydata,cor='TRUE')
#summary(pc)

#pc$scores
plot(pc)
#loadings(pc)
#var(mydata)
#pc$loadings
optimal_PCA<-5
#mydata_hat<-predict(pc, as.data.frame(mydata))
train_apca2<-cbind(train_apca1,pc$scores[,1:optimal_PCA])
#View(train_apca)
colnames(train_apca2)[grepl("PC",colnames(train_apca2))]<-paste0(i,1:optimal_PCA)


#### Insured Information
#train_apca<-data.frame(ID=train_clean$Id)
#View(train_apca)
i="InsuredInfo_"
mydata<-train_clean[,grep(i, colnames(train_clean))]
pc <- princomp(mydata,cor='TRUE')
#summary(pc)
plot(pc)
#loadings(pc)
#var(mydata)
#pc$loadings
optimal_PCA<-6
#mydata_hat<-predict(pc, as.data.frame(mydata))
train_apca3<-cbind(train_apca2,pc$scores[,1:optimal_PCA])
#View(train_apca)
colnames(train_apca3)[grepl("PC",colnames(train_apca3))]<-paste0(i,1:optimal_PCA)

#### Insurance History
#train_apca<-data.frame(ID=train_clean$Id)
#View(train_apca)
i="Insurance_History"
mydata<-train_clean[,grep(i, colnames(train_clean))]
pc <- princomp(mydata,cor='TRUE')
#summary(pc)
plot(pc)
#loadings(pc)
#var(mydata)
#pc$loadings
optimal_PCA<-6
#mydata_hat<-predict(pc, as.data.frame(mydata))
train_apca4<-cbind(train_apca3,pc$scores[,1:optimal_PCA])
#View(train_apca)
colnames(train_apca4)[grepl("PC",colnames(train_apca4))]<-paste0(i,1:optimal_PCA)

#### Medical History
i="Medical_History"
#train_apca<-data.frame(ID=train_clean$Id)
#View(train_apca)

mydata<-train_clean[,grep(i, colnames(train_clean))]
pc <- princomp(mydata,cor='TRUE')
#summary(pc)
#plot(pc)
#loadings(pc)
#var(mydata)
#pc$loadings
optimal_PCA<-35
#mydata_hat<-predict(pc, as.data.frame(mydata))
train_apca5<-cbind(train_apca4,pc$scores[,1:optimal_PCA])
#View(train_apca)
colnames(train_apca5)[grepl("PC",colnames(train_apca5))]<-paste0(i,1:optimal_PCA)

#### Medical Keyword
i="Medical_Keyword"
#train_apca<-data.frame(ID=train_clean$Id)
#View(train_apca)
mydata<-train_clean[,grep(i, colnames(train_clean))]
pc <- princomp(mydata,cor='TRUE')
#summary(pc)
#plot(pc)
#loadings(pc)
#var(mydata)
#pc$loadings
optimal_PCA<-46
#mydata_hat<-predict(pc, as.data.frame(mydata))
train_apca6<-cbind(train_apca5,pc$scores[,1:optimal_PCA])
#View(train_apca)
colnames(train_apca6)[grepl("PC",colnames(train_apca6))]<-paste0(i,1:optimal_PCA)
####
View(train_apca6)
Response<-train_clean$Response
train_apca6<-cbind(train_apca6,Response)
train_apca6$random <- runif(nrow(train_apca6))
train_apca6_70 <- train_apca6[train_apca6$random <= 0.7,] 
train_apca6_30 <- train_apca6[train_apca6$random > 0.7,]
train_apca6<-train_apca6[,-c(106,107)]

temp1<- data.frame(Variable_Type = c("Product Information",
                                     "Insurance Age",
                                     "Height",
                                     "Weight", 
                                     "BMI",
                                     "Employment Information",
                                     "Insured Information",
                                     "Insurance History",
                                     "Family History",
                                     "Medical History",
                                     "Medical Keyword"))
names(train_apca6)
temp1$Prior_to_PCA<-c(7,1,1,1,1,6,7,7,1,37,48)
temp1$After_PCA<-c(6,1,1,1,1,5,6,6,1,35,46)
colSums(temp1[1,2,drop=FALSE])
temp1[12,2:3]<-colSums(temp1[,-1])
temp1$Variable_Type[12]<-"Total"
datatable(temp1, options = list(pageLength = 13,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}")))
library(DT)

########################Linear regression


nonlintra <- function(y)
{
  hardcoded_values <- c(-1.6, 0.7, 0.3, 3.15, 4.53, 6.5, 6.77, 9.0)
  return(hardcoded_values[y])
}

y <- nonlintra(train_clean$Response)
qbmic <- 0.8
qbmic2 <- 0.9

model <- return_model(tra, test, 'lm')

#**The dimension of data of post PCA is reduced from `r length(train_clean[,1])` * `r length(train_clean)`   to    `r length(train_apca[,1])` * `r length(train_apca)`**
  

### **4.4	Boruta Classification Algorithm**

#Boruta is an all relevant feature selection wrapper algorithm. 
#The method performs a top-down search for relevant features by comparing original attributes' 
#importance with importance achievable at random.  It computes this by using permuted copies of 
#features, and progressively eliminating irrelevant features. All the atributes post PCA are deemed
#important by the algorithm.



## **5. Methodology**

### **5.1 Multinominal Logistict Model**


#We start by dividing the data inot two portions and using one portion to predict the model and the other portion for model validation and insample prediction.
#View(train_apca)

train_apca6$Response<-train$Response
train_apca6$random <- runif(nrow(train_apca6))
write.csv(train_apca6,'file1.csv')
train_apca_70 <- train_apca6[train_apca6$random <= 0.7,]
train_apca_30 <- train_apca6[train_apca6$random > 0.7,]
train_apca_90 <- train_apca6[train_apca6$random <= 0.9,]
train_apca_10<-train_apca6[train_apca6$random > 0.9,]


#View(train_apca_70)
#View(train_apca_30)

#We look at distribution of response on both the portions of train data.

temp1<-data.frame(round(table(train_apca_70$Response)/nrow(train_apca_70),2))
round(table(train_apca_70$Response)/nrow(train_apca_70),2)
View(temp1)


temp1$Freq_test<-as.data.frame(round(table(train_apca_30$Response)/nrow(train_apca_30),2))[,2]

round(table(train_apca_30$Response)/nrow(train_apca_30),2)


#### Comparing distribution of test and train data

plot_ly(data = temp1, x=~Var1,y=~Freq, type = "bar", name="Train")%>%
add_trace(y=~Freq_test, name="Test")%>%
layout(barmode = 'group')

colnames(train_apca_70)


#The distribution looks same for both the portions therefore we assume that both the portions follow similar distribution.

nodel.mlm <- multinom(Response ~ ., data = train_apca_70)
#str(train_apca_70)
#   summary(nodel.mlm)
train_apca_30$Prediction<-predict(nodel.mlm, newdata=train_apca_30, type="class")
kappa_multi<-ScoreQuadraticWeightedKappa(as.numeric(train_apca_30$Prediction),as.numeric(train_apca_30$Response))
auc_multi<-multiclass.roc(train_apca_30$Response,as.numeric(train_apca_30$Prediction),levels = levels(factor(train_apca_30$Response)))
accuracy_multi<-(classificationMetrics(train_apca_30$Response,as.numeric(train_apca_30$Prediction)))[1]
accuracy_multi
str(auc_multi)
#### Performance Metrics

#The Kappa score for this multinominal model is `
a<-ScoreQuadraticWeightedKappa(as.numeric(train_apca_30$Prediction),as.numeric(train_apca_30$Response))
a
library(pROC)
### **5.2 Random Forest**

#Here as well we start by dividing the data inot two portions and using one portion to predict the model and the other portion for model validation and insample prediction. We will be using the entire data set for prediction.

train_clean$random <- runif(nrow(train_clean))
train_clean_70 <- train_clean[train_clean$random <= 0.7,] 
train_clean_30 <- train_clean[train_clean$random > 0.7,] 
train_clean_90 <- train_clean[train_clean$random <= 0.9,] 
train_clean_10 <- train_clean[train_clean$random <= 0.1,] 
test_clean$random <- runif(nrow(test_clean))
dim(test_clean)
#We look at distribution of response on both the portions of train data.


temp1<-data.frame(round(table(train_clean_70$Response)/nrow(train_clean_70),2))
round(table(train_clean_70$Response)/nrow(train_clean_70),2)

temp1$Freq_test<-as.data.frame(round(table(train_clean_30$Response)/nrow(train_clean_30),2))[,2]
round(table(train_clean_30$Response)/nrow(train_clean_30),2)


#### Comparing distribution of test and train data 

plot_ly(data = temp1, x=~Var1,y=~Freq, type = "bar", name="Train")%>%
add_trace(y=~Freq_test, name="Test")%>%
layout(barmode = 'group')



#The distribution looks same for both the portions therefore we assume that 
#both the portions follow similar distribution.

#model <- randomForest(Response~., data = train_clean_70[,-c(1,120)], importance = TRUE)
model<-rpart(Response~., data = train_clean_70[,-c(1,120)])
train_clean_30$Prediction <- as.integer(round(predict(model, train_clean_30)))
kappa_rf<-ScoreQuadraticWeightedKappa(train_clean_30$Prediction,as.numeric(train_clean_30$Response))
auc_rf<-multiclass.roc(train_clean_30$Prediction,
                       as.numeric(train_clean_30$Response),
                       levels = levels(factor(train_clean_30$Response)))
library(pROC)
accuracy_rf<-classificationMetrics(train_clean_30$Prediction,
                                   as.numeric(train_clean_30$Response))[1]

install.packages("ranger")
library(ranger)
score.cv<-as.numeric()
vec.n.trees <- as.numeric()
vec.n.mtry <-as.numeric()
for (n.trees in seq(500, 5500, by = 1000)){
  for (n.mtry in seq(12, 120, by = 24)){
    set.seed(888)
    md.rf <- ranger(Response ~.
                    , data = train_clean_90
                    , num.trees = n.trees
                    , mtry = n.mtry
                    , importance = "impurity"
                    , write.forest = T
                    , min.node.size = 20
                    , num.threads = 8
                    , verbose = T
    )
    
    pred.train <- predict(md.rf, train_clean_90)
    pred.train <- predictions(pred.train)
    # pred.train <- as.integer(pred.train)
    
    #pred.valid <- predict(md.rf, dt.valid)
    #pred.valid <- predictions(pred.valid)
    # pred.valid <- as.integer(pred.valid)
    
    pred.test <- predict(md.rf, train_clean_10)
    pred.test <- predictions(pred.test)
    # pred.test <- as.integer(pred.test)
    
    cat("optimising the cuts on pred.train ...\n")
    SQWKfun <- function(x = seq(1.5, 7.5, by = 1)){
      cuts <- c(min(pred.train), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(pred.train))
      pred <- as.integer(cut2(pred.train, cuts))
      err <- ScoreQuadraticWeightedKappa(pred, train_clean_90$Response, 1, 8)
      return(-err)
    }
    optCuts <- optim(seq(1.5, 7.5, by = 1), SQWKfun)
    optCuts
    
    cat("applying optCuts on valid ...\n")
    cuts.test <- c(min(pred.test), optCuts$par, max(pred.test))
    pred.test.op <- as.integer(cut2(pred.test, cuts.test))
    score <- ScoreQuadraticWeightedKappa(train_clean_10$Response, pred.test.op)
    # [1] 0.6265719 (originally .550676; num.trees = 500, regression tree on raw features)
    # [1] 0.5286518 (originally .5286518; num.trees = 500, classification tree on raw features)
    # [1] 0.6444031 (originally .5558768; num.trees = 5000, regression tree on raw features)
    
    print(paste("-------- n.trees:", n.trees, "; mtry:", n.mtry, "; score:", score))
    
    score.cv <- c(score.cv, score)
    vec.n.trees <- c(vec.n.trees, n.trees)
    vec.n.mtry <- c(vec.n.mtry, n.mtry)
  }
}

#########################

md.rf <- ranger(Response ~.
                , data = train_clean
                , num.trees = 3500
                , mtry = 36
                , importance = "impurity"
                , write.forest = T
                , min.node.size = 20
                , num.threads = 8
                , verbose = T
)

pred.train <- predict(md.rf, train_clean)
pred.train <- predictions(pred.train)
# pred.train <- as.integer(pred.train)

#pred.valid <- predict(md.rf, dt.valid)
#pred.valid <- predictions(pred.valid)
# pred.valid <- as.integer(pred.valid)
test_clean

pred.test <- predict(md.rf, test_clean)
View(pred.test)
write.csv(cbind(test_clean$Id,pred.test),'File4.csv')
?write.csv
pred.test <- predictions(pred.test)
pred.test <- round(as.numeric(pred.test))
# pred.test <- as.integer(pred.test)

cat("optimising the cuts on pred.train ...\n")
SQWKfun <- function(x = seq(1.5, 7.5, by = 1)){
  cuts <- c(min(pred.train), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(pred.train))
  pred <- as.integer(cut2(pred.train, cuts))
  err <- ScoreQuadraticWeightedKappa(pred, train_clean$Response, 1, 8)
  return(-err)
}
optCuts <- optim(seq(1.5, 7.5, by = 1), SQWKfun)
optCuts

cat("applying optCuts on valid ...\n")
cuts.test <- c(min(pred.test), optCuts$par, max(pred.test))
pred.test.op <- as.integer(cut2(pred.test, cuts.test))
score <- ScoreQuadraticWeightedKappa(train_clean_10$Response, pred.test.op)
# [1] 0.6265719 (originally .550676; num.trees = 500, regression tree on raw features)
# [1] 0.5286518 (originally .5286518; num.trees = 500, classification tree on raw features)
# [1] 0.6444031 (originally .5558768; num.trees = 5000, regression tree on raw features)

print(paste("-------- n.trees:", n.trees, "; mtry:", n.mtry, "; score:", score))

score.cv <- c(score.cv, score)
vec.n.trees <- c(vec.n.trees, n.trees)
vec.n.mtry <- c(vec.n.mtry, n.mtry)
########################

md.rf <- ranger(Response ~.
                , data = train_clean_90
                , num.trees = 3500
                , mtry = 36
                , importance = "impurity"
                , write.forest = T
                , min.node.size = 20
                , num.threads = 8
                , verbose = T
)
pred.test <- predict(md.rf, test)
pred.test<-predictions(pred.test)

pred.train <- predict(md.rf, train_apca_90)
pred.train<-predictions(pred.train)
trainForOpt <- sample(length(pred.train), length(pred.train) * .8)
pred.train.forOpt <- pred.train[trainForOpt]
SQWKfun <- function(x = seq(1.5, 7.5, by = 1)){
  cuts <- c(min(pred.train.forOpt), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(pred.train.forOpt))
  pred <- as.integer(cut2(pred.train.forOpt, cuts))
  err <- ScoreQuadraticWeightedKappa(pred, y.train[trainForOpt], 1, 8)
  return(-err)
}


optCuts <- optim(seq(1.5, 7.5, by = 1), SQWKfun)
optCuts
require(Hmisc)
library(Hmisc)
cat("applying optCuts on valid ...\n")
cuts.test <- c(min(pred.test), optCuts$par, max(pred.test))
pred.test.op <- as.integer(cut2(pred.test, cuts.test))
View(pred.test)

score <- ScoreQuadraticWeightedKappa(as.numeric(round(pred.test)), as.numeric(y.test))
score
# 0.6359023
b<-ScoreQuadraticWeightedKappa(as.numeric(round(train_clean_30$Prediction)),as.numeric(train_clean_30$Response)) 


cat("applying optCuts on test ...\n")
cuts.test <- c(min(pred.test), optCuts$par, max(pred.test))
pred.test.op <- as.integer(cut2(pred.test, cuts.test))








#train_clean_30$Prediction <- as.integer(round(predict(md.rf, train_clean_30)))
kappa_rf<-ScoreQuadraticWeightedKappa(train_clean_30$Prediction,as.numeric(train_clean_30$Response))
auc_rf<-multiclass.roc(train_clean_30$Prediction,
                       as.numeric(train_clean_30$Response),
                       levels = levels(factor(train_clean_30$Response)))
library(pROC)
accuracy_rf<-classificationMetrics(train_clean_30$Prediction,
                                   as.numeric(train_clean_30$Response))[1]



#The Kappa score for random forest model is 
b<-ScoreQuadraticWeightedKappa(as.numeric(train_clean_30$Prediction),as.numeric(train_clean_30$Response)) 
b
#submit <- data.frame(train_clean)
#write.csv(submit,file="'train1.csv",row.names=F)


#### The Rattel plot (or tree) used for predicting the model is as follows: 



fancyRpartPlot(model1)


### 5.3 XGBoost

#XGBoost is short for "Extreme Gradient Boosting", where the term "Gradient Boosting" is proposed in the paper Greedy Function Approximation: A Gradient Boosting Machine, by Friedman. XGBoost is based on this original model. XGBoost is used for supervised learning problems, where we use the training data (with multiple features) $x_i$ to predict a target variable $(y_i)$. 



####  Outpout from XGBoost

#Since we are using the same data set for XGBoost as well we will divide it again.
#{r echo=TRUE, warning=FALSE,message=FALSE,error=FALSE,fig.keep='all', include=FALSE}
## dividing data
train$random <- runif(nrow(train))

train_70 <- train[train$random <= 0.7,] 
train_30 <- train[train$random > 0.7,] 

#we look at distribution of response on train_70 and train_30

round(table(train_70$Response)/nrow(train_70),2)
round(table(train_30$Response)/nrow(train_30),2)
### looks same we can proceed
# built a model with train_70
library(xgboost)
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
#xgboost()
#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
library(Matrix)
col_nam<-colnames(train)[2:(length(train_70)-2)]
model.xg<- xgboost(data = data.matrix(train_70[,col_nam]),
                   label = train_70$Response,
                   nrounds = 100,
                   objective   = "reg:linear",
                   eval_metric = "rmse")
#xgboost::xgboost()
#?xgboost
train_30$Prediction<-as.integer(round(predict(model.xg, data.matrix(train_30[,col_nam]))))
train_30[which(train_30$Prediction<1),2] <-1
train_30[which(train_30$Prediction>8),2] <-8
kappa_xg<-ScoreQuadraticWeightedKappa(train_30$Prediction,as.numeric(train_30$Response))
auc_xg<-multiclass.roc(train_30$Response,train_30$Prediction,levels = levels(factor(train_30$Response)))
accuracy_xg<-classificationMetrics(train_30$Response,train_30$Prediction)[1]
library(Metrics)
#3
.?multiclass.roc()
#Metrics::ScoreQuadraticWeightedKappa()
#The Kappa score for this model is `r 
c<-ScoreQuadraticWeightedKappa(as.numeric(train_30$Prediction),as.numeric(train_30$Response))
c
#&nbsp;

#We plot the importance matrix for the top 30 variable.
library(xgb.plot.tree)
library(Ckmeans.1d.dp)
#require(Ckmeans.1d.dp)
#install.packages("Ckmeans.1d.dp")
imp_matrix<-xgb.importance(col_nam,model.xg)
imp_matrix1<-imp_matrix[c(1:30),]
xgb.ggplot.importance(imp_matrix1)



#&nbsp;

#The XGBoost tree 
install.packages("DiagrammeR")
library('DiagrammeR')
xgb.plot.tree(feature_names = col_nam,model.xg, n_first_tree = 0)




## **6. Output and Results**

### **6.1 Performance Metrics**

#### **Accuracy**

#Accuracy is the probablity of correct prediction.

#$$
#Accuracy= \frac{N(Correct Prediction)}{N(Total Prediction)}
#$$

#### **Kappa Score**

#Quadratic weighted kappa measures the agreement between two ratings. This metric typically varies from 0 (random agreement) to 1 (complete agreement). In the event that there is less agreement between the raters than expected by chance, this metric may go below 0.

#In the current dataset the response variable has 8 possible ratings.  Each application is characterized by a tuple (ea,eb), which corresponds to its scores by Rater A (actual risk) and Rater B (predicted risk).  The quadratic weighted kappa is calculated as follows.

#First, an N x N histogram matrix O is constructed, such that Oi,j corresponds to the number of applications that received a rating i by A and a rating j by B. An N-by-N matrix of weights, w, is calculated based on the difference between raters' scores:
  
#  $$
 # w_{i,j} = \frac{\left(i-j\right)^2}{\left(N-1\right)^2}
#$$
 # An N-by-N histogram matrix of expected ratings, E, is calculated, assuming that there is no correlation between rating scores.  This is calculated as the outer product between each rater's histogram vector of ratings, normalized such that E and O have the same sum.

#From these three metrics, the quadratic weighted kappa is calculated as: 

#$$
#\kappa=1-\frac{\sum_{i,j}w_{i,j}O_{i,j}}{\sum_{i,j}w_{i,j}E_{i,j}}.


performance_mat<-data.frame(Algorithm=c("Multinominal", "Random Forest", "XGBoost"))
performance_mat$Accuracy<-c(round(accuracy_multi,3),round(accuracy_rf,3),round(accuracy_xg,3))
performance_mat$AUC<-c(round(auc_multi$auc,3),round(auc_rf$auc,3),round(auc_xg$auc,3))
performance_mat$Kappa<-c(round(kappa_multi,3),round(kappa_rf,3),round(kappa_xg,3))
datatable(performance_mat, options = list(initComplete = JS(
"function(settings, json) {",
"$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
"}")))


plot_ly(data = performance_mat, x=~Algorithm, y=~Accuracy, type = 'scatter', mode = 'lines', name="Accuracy")%>%
  add_trace(y=~AUC, name="AUC")%>%
  add_trace(y=~Kappa, name="Kappa Score")%>%
  layout(title= "Comparision of Preformance Metrics", xaxis=list(title= "Model Type", showgrid=T), yaxis=list(title="Value"))


#Comparing all the three models we find that training the data using Gradent Boosting Algorithim
#results better resu
