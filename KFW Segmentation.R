## ----setup, include=FALSE------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----include=FALSE-------------------------------
library(haven)
library(expss)
library(tidyverse)
library(scales)
library(kableExtra)


## ----eval=FALSE, include=FALSE-------------------
## df <- haven::read_stata("../C - Documentation/KFW_segmentation_data.dta")
## df <- expss::add_labelled_class(df)
## df <- values2labels(df)
## expss::write_labelled_xlsx(df,"kfw_data.xlsx")
## rm(df)


## ----include=FALSE-------------------------------
kfw_data <- read.csv("../C - Documentation/KFW_segmentation_data.csv", na.strings = c(""," "))


## ----include=FALSE-------------------------------
theme_set(theme_light())


## ------------------------------------------------
dim(kfw_data)


## ----echo=FALSE----------------------------------
missing_recs <- as.data.frame(colSums(is.na(kfw_data))) %>% 
  rename("Missing_Recs" = "colSums(is.na(kfw_data))") %>%
  rownames_to_column("Variable") %>% 
  mutate(Total_Recs = nrow(kfw_data)) %>% 
  mutate(Proportion = percent(Missing_Recs/Total_Recs,accuracy = 0.01))

missing_recs <- cbind(missing_recs,as.data.frame(sapply(kfw_data,class))) %>% 
  rename("Var_Type"="sapply(kfw_data, class)") %>% 
  arrange(desc(Missing_Recs))

#xlsx::write.xlsx(missing_recs,"vars_missing_data.xlsx")
missing_recs %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("hover","striped"))


## ----echo=FALSE----------------------------------

kfw_data_imputed <- kfw_data[,colSums(is.na(kfw_data))<=4]

kfw_data_imputed <- sapply(kfw_data_imputed,function(x) {if(is.integer(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x})

kfw_data_imputed <- as.data.frame(kfw_data_imputed)

as.data.frame(colSums(is.na(kfw_data_imputed))) %>% 
  rename("Missing" = "colSums(is.na(kfw_data_imputed))") %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("hover","striped"))


## ----echo=FALSE----------------------------------
kfw_data_merged <- cbind(kfw_data[,!names(kfw_data) %in% names(kfw_data_imputed)],kfw_data_imputed)

dim(kfw_data_merged)


## ----echo=FALSE----------------------------------
kfw_data_complete <- kfw_data_merged[,colSums(is.na(kfw_data_merged))==0]

dim(kfw_data_complete)


## ----echo=FALSE----------------------------------
nzv <- caret::nearZeroVar(kfw_data_complete)

kfw_nzv <- kfw_data_complete[,-nzv]

dim(kfw_nzv)


## ----echo=FALSE----------------------------------
names(kfw_nzv)

id_vars <- c("subjectid","first_name")

kfw_final <- kfw_nzv[,!names(kfw_nzv) %in% id_vars]

dim(kfw_final)


## ----echo=FALSE----------------------------------
library(cluster)

diss_mat <- daisy(x = kfw_final, metric = "gower")

seg_tree <- hclust(d = diss_mat, method = "ward.D2")

plot(seg_tree)
abline(h=0.95,col="red")


## ----echo=FALSE, message=FALSE, warning=FALSE----
library(factoextra)

seg.hc <- eclust(x = diss_mat, FUNcluster = "hclust", k = 3, hc_method = "ward.D2", seed = 1, hc_metric = "euclidean")

fviz_cluster(seg.hc, main = "Visualisation of the KFW Segments")


## ----echo=FALSE, message=FALSE-------------------
segments <- cutree(seg_tree,3)

kfw_final_segmented <- cbind(kfw_final,segments)

kfw_final_segmented %>% 
  count(segments) %>% 
  rename("Participants"="n") %>% 
  mutate(Proportion = percent(Participants/sum(Participants),accuracy = 0.01)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("hover","striped"))

#xlsx::write.xlsx(kfw_final_segmented,"kfw_final_segmented.xlsx")
#xlsx::write.xlsx(cbind(kfw_data,segments),"kfw_data_segmented.xlsx")


## ----eval=FALSE, include=FALSE-------------------
## 
## first_set_vars <- c("previous_access_finance","monthly_income","employment_status","female","whatisyourage")
## 
## second_set_vars <- c("access_finance_perception","access_finance_knowledge","risk_willingness","technology_time","technology_career","supply_chain","value_chain","agri_opportunities","agri_interest","preparedness")
## 
## vars_int <- append(first_set_vars,second_set_vars)


## ----eval=FALSE, include=FALSE-------------------
## # kfw_data_retained <- (kfw_data[,(names(kfw_data) %in% vars_int)])
## #
## # vars_int[which(!vars_int %in% names(kfw_data_retained))]

