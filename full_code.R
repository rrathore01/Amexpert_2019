library(data.table)
library(ggplot2)
library(lightgbm)
library(dplyr)
library(magrittr)
library(ROCR)
library(rBayesianOptimization)

#import files

train<-fread("train.csv")
test<-fread("test_QyjYwdj.csv")
item<-fread("item_data.csv")
coupon<-fread("coupon_item_mapping.csv")
campaign<-fread("campaign_data.csv")
custom_dem<-fread("customer_demographics.csv")
custom_trans<-fread("customer_transaction_data.csv")


#merge train and test with campaign_id 

train<-train%>%left_join(campaign,by="campaign_id")%>%left_join(custom_dem,by="customer_id")
test<-test%>%left_join(campaign,by="campaign_id")%>%left_join(custom_dem,by="customer_id")

setDT(train)
setDT(test)

#convert date variables to date format
train$start_date<-as.Date(train$start_date,"%d/%m/%y")
train$end_date<-as.Date(train$end_date,"%d/%m/%y")
test$start_date<-as.Date(test$start_date,"%d/%m/%y")
test$end_date<-as.Date(test$end_date,"%d/%m/%y")

#merge customer_transaction file with item file

custom_trans<-custom_trans%>%left_join(item,by="item_id")

#convert date variables to date format
custom_trans$date<-as.Date(custom_trans$date,"%Y-%m-%d")

#merge unique item associated with each coupon_id with train and test
#the new variable will be as column of lists
unique_coupon<-coupon[,.(list(unique(item_id))),by=.(coupon_id)]

train<-train%>%left_join(unique_coupon,by="coupon_id")
test<-test%>%left_join(unique_coupon,by="coupon_id")

names(train)[15]<-"item_list"
names(test)[14]<-"item_list"
setDT(train)
setDT(test)

#create a file containing list of unique item transacted by each customer
setDT(custom_trans)
unique_customer_item=custom_trans[,.(list(unique(item_id))),by=.(customer_id)]

#merge above unique_consumer_item with train and test
#resulting variable will be column of lists of unique transacted item by each consumer

train<-train%>%left_join(unique_customer_item,by="customer_id")
test<-test%>%left_join(unique_customer_item,by="customer_id")
names(train)[16]<-"item_transacted"
names(test)[15]<-"item_transacted"
setDT(train)
setDT(test)

#create variable that is intersetion of itemList and item_transacted

train[,common_item:=.(list(intersect(unlist(item_list),unlist(item_transacted)))),by=id]
test[,common_item:=.(list(intersect(unlist(item_list),unlist(item_transacted)))),by=id]


#average coupon discount and average price columns for each id

price_df<-custom_trans[,mean(selling_price),by=item_id]
disc_df<-custom_trans[,mean(coupon_discount),by=item_id]
inter_df<-price_df%>%left_join(disc_df,by="item_id")

train1<-train[,c(1,3)]
test1<-test[,c(1,3)]
train1<-train1%>%inner_join(coupon,by="coupon_id")
train1<-train1%>%left_join(inter_df,by="item_id")
setDT(train1)
temp1<-train1[,mean(V1.x),by=id]
temp2<-train1[,mean(V1.y),by=id]
train<-train%>%left_join(temp1,by="id")
train<-train%>%left_join(temp2,by="id")
rm(train1,temp1,temp2)
test1<-test1%>%inner_join(coupon,by="coupon_id")
test1<-test1%>%left_join(inter_df,by="item_id")
setDT(test1)
temp1<-test1[,mean(V1.x),by=id]
temp2<-test1[,mean(V1.y),by=id]
test<-test%>%left_join(temp1,by="id")
test<-test%>%left_join(temp2,by="id")
rm(test1,temp1,temp2)

setDT(train)
setDT(test)
names(train)[18:19]<-c("avg_price","avg_disc")
names(test)[17:18]<-c("avg_price","avg_disc")


#this function melts item list variable and returns a dataframe with item and corresponding id as columns

melt_item_id<-function(df,var){
  setDF(df)
  df<-df[,c("id","coupon_id","customer_id",var)]
  setDT(df)
  df[,length:=length(unlist(get(var))),by=id]
  id<-NULL
  df1<-df[,.(list(rep(id,length))),by=id]
  id<-apply(df1[,2],2,unlist)
  item<-apply(df[,4],2,unlist)
  new_df<-data.frame(item_id=item,id=id)
  names(new_df)<-c("item_id","id")
  new_df
}

#average price and average discoun for common_item variable
x<-melt_item_id(df=copy(train),var="common_item")
x<-x%>%left_join(inter_df,by="item_id")
setDT(x)
temp1<-x[,mean(V1.x),by=id]
temp2<-x[,mean(V1.y),by=id]
train<-train%>%left_join(temp1,by="id")
train<-train%>%left_join(temp2,by="id")
rm(x,temp1,temp2)

x<-melt_item_id(df=copy(test),var="common_item")
x<-x%>%left_join(inter_df,by="item_id")
setDT(x)
temp1<-x[,mean(V1.x),by=id]
temp2<-x[,mean(V1.y),by=id]
test<-test%>%left_join(temp1,by="id")
test<-test%>%left_join(temp2,by="id")
rm(x,temp1,temp2)

names(train)[20:21]<-c("avg_price_common","avg_disc_common")
names(test)[19:20]<-c("avg_price_common","avg_disc_common")
setDT(train)
setDT(test)

#create other features 

#count of unique item for each id

train$count_item<-sapply(train$item_list,function(x){length(unlist(x))})
test$count_item<-sapply(test$item_list,function(x){length(unlist(x))})

#create other features
unique_custom_price<-custom_trans[,mean(selling_price),by=.(customer_id)]
unique_custom_discount<-custom_trans[,mean(coupon_discount),by=.(customer_id)]
unique_custom_quantity<-custom_trans[,mean(quantity),by=.(customer_id)]
unique_custom_odiscount<-custom_trans[,mean(other_discount),by=.(customer_id)]

unique_custom_brand<-custom_trans[,length(unique(brand)),by=.(customer_id)]
unique_custom_disc_per_bond<-custom_trans[,mean(coupon_discount),by=.(customer_id,brand)]
unique_custom_disc_per_bond<-unique_custom_disc_per_bond[,mean(V1),by=.(customer_id)]

target<-train$redemption_status

train[,redemption_status:=NULL]

data<-rbind(train,test)

data<-data%>%left_join(unique_custom_price,by="customer_id")
data<-data%>%left_join(unique_custom_discount,by="customer_id")
data<-data%>%left_join(unique_custom_quantity,by=c("customer_id"))
data<-data%>%left_join(unique_custom_odiscount,by=c("customer_id"))
data<-data%>%left_join(unique_custom_brand,by=c("customer_id"))
data<-data%>%left_join(unique_custom_disc_per_bond,by=c("customer_id"))

setDT(data)
data[,id:=NULL]

names(data)[22:27]<-c("past_price","past_disc","past_quant","past_odisc",
                      "past_brand","past_discperbrand")

#feature processing and some new features

fact_var<-c("campaign_id","coupon_id","customer_id","campaign_type",
            "age_range","family_size","no_of_children",
             "marital_status","rented")

date_var<-c("start_date","end_date")

#convert categorical features into integers and frequency encoding

data[,(fact_var):=lapply(.SD,function(x){as.integer(as.factor(x))}),.SDcols=fact_var]

for(i in fact_var){
  data[,(i):=.N,by=i]
}

temp1<-temp[,c(3,4)]
setDT(temp1)


#build some more features
temp1<-data[,c(3,4)]
setDT(temp1)

temp1<-temp1%>%inner_join(coupon,by="coupon_id")
unique_category<-custom_trans[,unique(category),by=.(item_id)]
temp1<-temp1%>%left_join(unique_category,by="item_id")
avgprice_category<-custom_trans[,mean(selling_price),by=.(category)]
temp1<-temp1%>%left_join(avgprice_category,by=c("V1"="category"))
avgdisc_category<-custom_trans[,mean(coupon_discount),by=.(category)]
temp1<-temp1%>%left_join(avgdisc_category,by=c("V1"="category"))

unique_brand<-custom_trans[,unique(brand),by=.(item_id)]
temp1<-temp1%>%left_join(unique_brand,by="item_id")
avgprice_brand<-custom_trans[,mean(selling_price),by=.(brand)]
temp1<-temp1%>%left_join(avgprice_brand,by=c("V1.y.y.y"="brand"))
avgdisc_brand<-custom_trans[,mean(coupon_discount),by=.(brand)]
temp1<-temp1%>%left_join(avgdisc_brand,by=c("V1.y.y.y"="brand"))

names(temp1)[4:9]<-c("category","cat_price","cat_disc","brand","brand_price","brand_disc")
setDT(temp1)

inter_df<-temp1[,mean(cat_price),by=.(customer_id)]
data<-data%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(cat_disc),by=.(customer_id)]
data<-data%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(brand_price),by=.(customer_id)]
data<-data%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(brand_disc),by=.(customer_id)]
data<-data%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(cat_price),by=.(coupon_id)]
data<-data%>%left_join(inter_df,by="coupon_id")
inter_df<-temp1[,mean(cat_disc),by=.(coupon_id)]
data<-data%>%left_join(inter_df,by="coupon_id")
inter_df<-temp1[,mean(brand_price),by=.(coupon_id)]
data<-data%>%left_join(inter_df,by="coupon_id")
inter_df<-temp1[,mean(brand_disc),by=.(coupon_id)]
data<-data%>%left_join(inter_df,by="coupon_id")

rm(temp1,inter_df)

names(data)[28:35]<-c("cat_price_custom","cat_disc_custom",
                      "brand_price_custom","brand_disc_custom",
                      "cat_price_coupon","cat_disc_coupon",
                      "brand_price_coupon","brand_disc_coupon")
setDT(data)

#build count of local or established brand_type features

item_type<-custom_trans[,unique(brand_type),by=item_id]
temp<-melt_item_id(df=copy(data),var="item_list")
temp<-temp%>%left_join(item_type,by="item_id")

setDT(temp)
item_type_local<-temp[,sum(V1=="Local"),by=id]
item_type_estab<-temp[,sum(V1=="Established"),by=id]
data<-data%>%left_join(item_type_local,by="id")
data<-data%>%left_join(item_type_estab,by="id")

rm(temp,item_type_local,item_type_estab)
setDT(data)

names(data)[36:37]<-c("local_type_count","estab_type_count")


#create sum of discount count per consumer 
custom_trans[,any_disc:=ifelse(coupon_discount<0,1,0)]
temp<-custom_trans[,sum(any_disc),by=customer_id]
data<-data%>%left_join(temp,by="customer_id")
setDT(data)

#create sum of discount per consumer per month
custom_trans[,month:=month(date)]
month_custom_disc_count<-custom_trans[,sum(any_disc),by=.(customer_id,month)]
data[,month:=month(end_date)]
data<-data%>%left_join(month_custom_disc_count,by=c("customer_id","month"))

names(data)[c(38,40)]<-c("disc_count_custom","disc_count_month")
setDT(data)


#remove irrelevant and list features

data[,(date_var):=NULL]
data[,":="(id=NULL,month=NULL,item_list=NULL,item_transacted=NULL,common_item=NULL)]

#build model

trainx<-as.matrix(data[1:nrow(train),])
testx<-as.matrix(data[(1+nrow(train)):nrow(data),])

#generate indices for stratified cross validation
y<-KFold(target,nfolds = 7,stratified = T)
y<-melt(y)

lgb.grid = list(objective = "binary", 
                boost="gbdt",
                metric="auc",
                is_unbalance=T,
                boost_from_average="false",
                num_threads=7,
                learning_rate = 0.01,
                num_leaves = 83,
                max_depth=15,
                tree_learner = "serial",
                feature_fraction = 0.471428571428572,
                bagging_freq = 5,
                bagging_fraction = 0.789285714285714,
                lambda_l1=1,
                lambda_l2=1,
                
                min_data_in_leaf =23,
                min_sum_hessian_in_leaf = 10.0,
                verbosity = -1
)

pred<-rep(0,nrow(test))

for(i in 1:7){
  
  ltrain2<-lgb.Dataset(data=trainx[y$value[y$L1!=i],],label=target[y$value[y$L1!=i]])
  lval2<-lgb.Dataset(data=trainx[y$value[y$L1==i],],label=target[y$value[y$L1==i]])
  lgb.fit2<-lgb.train(params = lgb.grid,data=ltrain2,
                      valids = list(train=ltrain2,test=lval2),
                      nrounds=2500,
                      early_stopping_rounds = 300,
                      eval_freq=100,
                      seed=999)
  
  pred<-predict(lgb.fit2,testx)/7+pred
  print(lgb.fit2$best_score)
  print(lgb.fit2$best_iter)
  
  gc()
}






