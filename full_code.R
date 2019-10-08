library(data.table)
library(ggplot2)
library(lightgbm)
library(dplyr)
library(magrittr)
library(ROCR)



train<-fread("train.csv")
test<-fread("test_QyjYwdj.csv")
item<-fread("item_data.csv")
coupon<-fread("coupon_item_mapping.csv")
campaign<-fread("campaign_data.csv")
custom_dem<-fread("customer_demographics.csv")
custom_trans<-fread("customer_transaction_data.csv")


train<-train%>%left_join(campaign,by="campaign_id")%>%left_join(custom_dem,by="customer_id")
test<-test%>%left_join(campaign,by="campaign_id")%>%left_join(custom_dem,by="customer_id")

setDT(train)
setDT(test)

train$start_date<-as.Date(train$start_date,"%d/%m/%y")
train$end_date<-as.Date(train$end_date,"%d/%m/%y")
test$start_date<-as.Date(test$start_date,"%d/%m/%y")
test$end_date<-as.Date(test$end_date,"%d/%m/%y")

custom_trans<-custom_trans%>%left_join(item,by="item_id")
custom_trans$date<-as.Date(custom_trans$date,"%Y-%m-%d")


unique_coupon<-coupon[,.(list(unique(item_id))),by=.(coupon_id)]
train<-train%>%left_join(unique_coupon,by="coupon_id")
test<-test%>%left_join(unique_coupon,by="coupon_id")

setDT(train)
setDT(test)

train$avg_price<-sapply(train$V1.x,function(x){mean(custom_trans$selling_price[custom_trans$item_id%in%unlist(x)])})
train$avg_discount<-sapply(train$V1.x,function(x){mean(custom_trans$coupon_discount[custom_trans$item_id%in%unlist(x)])})
test$avg_price<-sapply(test$V1.x,function(x){mean(custom_trans$selling_price[custom_trans$item_id%in%unlist(x)])})
test$avg_discount<-sapply(test$V1.x,function(x){mean(custom_trans$coupon_discount[custom_trans$item_id%in%unlist(x)])})

#create unique item transacted by each customer
setDT(custom_trans)
unique_customer_item=custom_trans[,.(list(unique(item_id))),by=.(customer_id)]
#merge above df with train and test
train<-train%>%left_join(unique_customer_item,by="customer_id")
test<-test%>%left_join(unique_customer_item,by="customer_id")
setDT(train)
setDT(test)
#create variable that is intersetion of v1.x and v1.y
train[,common_item:=.(list(intersect(unlist(V1.x),unlist(V1.y)))),by=id]
test[,common_item:=.(list(intersect(unlist(V1.x),unlist(V1.y)))),by=id]

unique_custom_price<-custom_trans[,mean(selling_price),by=.(customer_id)]
unique_custom_discount<-custom_trans[,mean(coupon_discount),by=.(customer_id)]
unique_custom_quantity<-custom_trans[,mean(quantity),by=.(customer_id)]
unique_custom_odiscount<-custom_trans[,mean(other_discount),by=.(customer_id)]

unique_custom_brand<-custom_trans[,length(unique(brand)),by=.(customer_id)]
unique_custom_disc_per_bond<-custom_trans[,mean(coupon_discount),by=.(customer_id,brand)]
unique_custom_disc_per_bond<-unique_custom_disc_per_bond[,mean(V1),by=.(customer_id)]

train$un_item<-sapply(train$V1.x,function(x){length(unlist(x))})
test$un_item<-sapply(test$V1.x,function(x){length(unlist(x))})
train[,":="(V1.x=NULL,common_item=NULL)]
test[,":="(V1.x=NULL,common_item=NULL)]

target<-train$redemption_status
train[,redemption_status:=NULL]

data<-rbind(train,test)

data<-data%>%left_join(unique_custom_price,by="customer_id")
data<-data%>%left_join(unique_custom_discount,by="customer_id")
data<-data%>%left_join(unique_custom_quantity,by=c("x"="customer_id"))
data<-data%>%left_join(unique_custom_odiscount,by=c("x"="customer_id"))
data<-data%>%left_join(unique_custom_brand,by=c("x"="customer_id"))
data<-data%>%left_join(unique_custom_disc_per_bond,by=c("x"="customer_id"))

setDT(data)
data[,id:=NULL]

#feature processing and some new features

data$nday_campaign<-as.integer(data$end_date-data$start_date)

data$marital_status<-ifelse(data$marital_status=="",NA,data$marital_status)

data[,marital_status:=ifelse(is.na(marital_status)==F,ifelse(marital_status=="Married",1,0),NA)]

fact_var<-c("campaign_id","coupon_id","customer_id","campaign_type",
            "age_range","family_size","no_of_children")

bin_var<-c("marital_status","rented")

num_var<-c("income_bracket","avg_price","avg_discount","un_item")

date_var<-c("start_date","end_date")

setDT(data)
data[,(fact_var):=lapply(.SD,function(x){as.integer(as.factor(x))}),.SDcols=fact_var]

for(i in fact_var){
  data[,(i):=.N,by=i]
}

data[,":="(start_date=NULL,id=NULL,end_date=NULL)]

trainx<-as.matrix(data[1:nrow(train),])
testx<-as.matrix(data[(1+nrow(train)):nrow(data),])


#from file 5
temp<-temp%>%left_join(unique_custom_price,by="customer_id")
temp<-temp%>%left_join(unique_custom_discount,by="customer_id")
temp<-temp%>%left_join(unique_custom_quantity,by=c("customer_id"))
temp<-temp%>%left_join(unique_custom_odiscount,by=c("customer_id"))
temp<-temp%>%left_join(unique_custom_brand,by=c("customer_id"))
temp<-temp%>%left_join(unique_custom_disc_per_bond,by=c("customer_id"))

names(temp)[17:22]<-c("past_price","past_disc","past_quant","past_odisc",
                      "past_brand","past_discperbrand")
#some experiment

temp1<-temp[,c(3,4)]
setDT(temp1)


#build some new feats
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
temp<-temp%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(cat_disc),by=.(customer_id)]
temp<-temp%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(brand_price),by=.(customer_id)]
temp<-temp%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(brand_disc),by=.(customer_id)]
temp<-temp%>%left_join(inter_df,by="customer_id")
inter_df<-temp1[,mean(cat_price),by=.(coupon_id)]
temp<-temp%>%left_join(inter_df,by="coupon_id")
inter_df<-temp1[,mean(cat_disc),by=.(coupon_id)]
temp<-temp%>%left_join(inter_df,by="coupon_id")
inter_df<-temp1[,mean(brand_price),by=.(coupon_id)]
temp<-temp%>%left_join(inter_df,by="coupon_id")
inter_df<-temp1[,mean(brand_disc),by=.(coupon_id)]
temp<-temp%>%left_join(inter_df,by="coupon_id")

names(temp)[23:30]<-c("cat_price_custom","cat_disc_custom",
                      "brand_price_custom","brand_disc_custom",
                      "cat_price_coupon","cat_disc_coupon",
                      "brand_price_coupon","brand_disc_coupon")
setDT(temp)
temp[,id:=NULL]

temp[,(date_var):=NULL]

factor_var<-c(fact_var,bin_var)
setDT(temp)
temp[,(factor_var):=lapply(.SD,function(x){as.integer(as.factor(x))}),.SDcols=factor_var]



item_price<-custom_trans[,mean(selling_price),by=item_id]
item_disc<-custom_trans[,mean(coupon_discount),by=item_id]

merge_item<-function(df,var){
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
x<-merge_item(df=copy(train),var="common_item")
y<-x%>%left_join(item_price,by="item_id")
z<-x%>%left_join(item_disc,by="item_id")
setDT(y)
setDT(z)
y<-y[,mean(V1),by=id]
z<-z[,mean(V1),by=id]
setDT(train)
setDT(test)

train<-train%>%left_join(y,by="id")
train<-train%>%left_join(z,by="id")

x<-merge_item(df=copy(test),var="common_item")
y<-x%>%left_join(item_price,by="item_id")
z<-x%>%left_join(item_disc,by="item_id")
setDT(y)
setDT(z)
y<-y[,mean(V1),by=id]
z<-z[,mean(V1),by=id]
setDT(train)
setDT(test)
test<-test%>%left_join(y,by="id")
test<-test%>%left_join(z,by="id")

trainx<-as.matrix(temp[1:nrow(train),])
testx<-as.matrix(temp[(1+nrow(train)):nrow(temp),])


#build local or established vars

item_type<-custom_trans[,unique(brand_type),by=item_id]
x<-merge_item(df=copy(train),var="V1.x")

x<-x%>%left_join(item_type,by="item_id")
setDT(x)
item_type_local<-x[,sum(V1=="Local"),by=id]
item_type_estab<-x[,sum(V1=="Established"),by=id]
train<-train%>%left_join(item_type_local,by="id")
train<-train%>%left_join(item_type_estab,by="id")

setDT(train)
x<-merge_item(df=copy(test),var="V1.x")
x<-x%>%left_join(item_type,var="item_id")

setDT(x)
item_type_local<-x[,sum(V1=="Local"),by=id]
item_type_estab<-x[,sum(V1=="Established"),by=id]
test<-test%>%left_join(item_type_local,by="id")
test<-test%>%left_join(item_type_estab,by="id")

setDT(train)
setDT(test)

#discount count per month variable
month_custom_disc_count<-custom_trans[,sum(any_disc),by=.(customer_id,month)]
train[,month_1:=month(start_date)]
train[,month_2:=month(end_date)]
test[,month_1:=month(start_date)]
test[,month_2:=month(end_date)]
train<-train%>%left_join(month_custom_disc_count,by=c("customer_id","month_1"="month"))
train<-train%>%left_join(month_custom_disc_count,by=c("customer_id","month_2"="month"))
test<-test%>%left_join(month_custom_disc_count,by=c("customer_id","month_1"="month"))
test<-test%>%left_join(month_custom_disc_count,by=c("customer_id","month_2"="month"))



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





