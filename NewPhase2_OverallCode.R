Hospital_Train<-read.csv('~/Desktop/2019SpringTerm/758T Data Mining and Predictive Analytics/Project Files/Hospitals_train.csv' ,nrows =38221,row.names = 'INDEX',na.strings = c('','NA'))

#part 1 cleaning
#Use KNN to fill NA in 'RACE' and 'ETHNICITY' 
library(VIM)
library(DMwR)
knnOutput <- knnImputation(Hospital_Train[,c(2:6)])
Hospital_Train$RACE<-knnOutput$RACE
Hospital_Train$ETHNICITY<-knnOutput$ETHNICITY
#part 2 cleaning
#Delete 'NA' in Return in training dataset
Hospital_Train<-Hospital_Train[Hospital_Train$RETURN!='#N/A',]
#transforming
#combine RACE(Declined to Answer/Two or More Races/Hispanic)
levels(Hospital_Train$RACE)<-c("American Indian or Alaskan Native", "Asian","Black or African American" ,"Declined to Answer or Two or More Races or Hispanic" ,"Declined to Answer or Two or More Races or Hispanic" ,"Native Hawaiian or Other Pacific Islander","Other" , "Declined to Answer or Two or More Races or Hispanic", "Unknown" ,"White" )
Hospital_Train$SAME_DAY <- as.factor(Hospital_Train$SAME_DAY)
#transform consultation
Hospital_Train$CONSULT_IN_ED[is.na(Hospital_Train$CONSULT_IN_ED)]<-0
Hospital_Train$CONSULT_ORDER<-as.factor(Hospital_Train$CONSULT_ORDER)
Hospital_Train$CONSULT_CHARGE<-as.factor(Hospital_Train$CONSULT_CHARGE)
Hospital_Train$CONSULT_IN_ED<-as.factor(Hospital_Train$CONSULT_IN_ED)
#transform charges class
Hospital_Train$CHARGES<-as.numeric(Hospital_Train$CHARGES)
#transform Return
Hospital_Train$RETURN=ifelse(Hospital_Train$RETURN=='Yes',1,0)
Hospital_Train$RETURN=as.factor(Hospital_Train$RETURN)
#clean admit_result NA
Hospital_Train$ADMIT_RESULT<-as.character(Hospital_Train$ADMIT_RESULT)
Hospital_Train$RISK<-as.character(Hospital_Train$RISK)
Hospital_Train$SEVERITY<-as.character(Hospital_Train$SEVERITY)
Hospital_Train$ADMIT_RESULT<-ifelse(is.na(Hospital_Train$ADMIT_RESULT),'Discharge Or Others',Hospital_Train$ADMIT_RESULT)
Hospital_Train$ADMIT_RESULT<-as.factor(Hospital_Train$ADMIT_RESULT)
Hospital_Train$RISK<-ifelse(Hospital_Train$ADMIT_RESULT=='Discharge Or Others','Discharge Or Others',Hospital_Train$RISK)
Hospital_Train$SEVERITY<-ifelse(Hospital_Train$ADMIT_RESULT=='Discharge Or Others','Discharge Or Others',Hospital_Train$SEVERITY)
#clean rest of RISK/SEVERITY
Hospital_Train$RISK<-ifelse(is.na(Hospital_Train$RISK),'Not Recorded',Hospital_Train$RISK)
Hospital_Train$SEVERITY<-ifelse(is.na(Hospital_Train$SEVERITY),'Not Recorded',Hospital_Train$SEVERITY)
Hospital_Train$RISK<-as.factor(Hospital_Train$RISK)
Hospital_Train$SEVERITY<-as.factor(Hospital_Train$SEVERITY)
#clean rest Acuity
Hospital_Train$ACUITY_ARR<-as.character(Hospital_Train$ACUITY_ARR)
Hospital_Train$ACUITY_ARR<-ifelse(is.na(Hospital_Train$ACUITY_ARR),'LWBS or others',Hospital_Train$ACUITY_ARR)
Hospital_Train$ACUITY_ARR<-as.factor(Hospital_Train$ACUITY_ARR)
#clean ED_RESULT
Hospital_Train$ED_RESULT<-as.character(Hospital_Train$ED_RESULT)
Hospital_Train$ED_RESULT<-ifelse(is.na(Hospital_Train$ED_RESULT),'ED Result not recorded',Hospital_Train$ED_RESULT)
Hospital_Train$ED_RESULT<-as.factor(Hospital_Train$ED_RESULT)


#combine DC_RESULT(# less than 10 as Other)
levels(Hospital_Train$DC_RESULT)<-c("Other", "Admitted as an Inpatient - OP Only", "AWOL - IP Only", "Other", "Discharge to acute care (medical/surgical) hospital", "Discharge to Juvenile/Adult Detention or Police Custody", "Discharge to on-site distinct psychiatric unit from acute care - IP Only", "Other", "Other", "Other", "Discharge to Shelters", "Other", "Other", "Other", "Discharged to Other Facility", "Other", "Expired", "Home Health Care Svc", "Home or Self Care", "Hospice/Home", "Hospice/Medical Facility", "Other", "Left Against Medical Advice", "Other", "LEFT W/O BEING SEEN AFTER TRIAGE", "LEFT W/O BEING SEEN BEFORE TRIAGE", "LEFT W/O COMPLETED TREATMENT", "Other", "Not specified Other or Unknown", "Other", "Psychiatric Unit or Psychiatric Hospital", "Rehab Facility", "Skilled Nursing Facility", "Still a Patient", "Other", "To this institution for OP services as specified by the disch plan of care - OP Only")

#newcode clean ED_RESULT
#levels(Hospital_Train$ED_RESULT)<-c("Admit","Admit to External Psychiatric Facility","Admit to UMMS Psychiatry ","AMA","Arrived in Error","Deceased","Discharge","ED Result not recorded",'Left', 'Left','Left', 'Left',"LWBS before Triage","Observation","Send to L&D after Rooming","Observation","Transfer")
#newcode clean DC_RESULT
#levels(Hospital_Train$DC_RESULT)<-c("Discharged to 0.5" , "Admitted as an Inpatient - OP Only" ,"AWOL - IP Only" ,                                                                   
#"Discharge small count 0" , "Discharge to acute care (medical/surgical) hospital" , "Discharge to Juvenile/Adult Detention or Police Custody" ,                            
#"Discharge to on-site distinct psychiatric unit from acute care - IP Only", "Discharge small count 0" , "Discharged to 0.5"  ,                                            
#"Discharged to 0.5" , "Discharge to Shelters" , "Discharge small count 0" , "Discharge small count 0" , "Discharge small count 0"  ,                                    
#"Discharged to Other Facility" ,"Discharge small count 0","Discharge small count 0" , "Home Health Care Svc", "Home or Self Care"  ,                                                                 
#"Hospice/Home","Discharge small count 0"  ,"Discharge small count 0"  ,"Left Against Medical Advice"   ,"Discharged to 0.5",                                                               
#"LEFT W/O BEING SEEN AFTER TRIAGE", "LEFT W/O BEING SEEN BEFORE TRIAGE" , "LEFT W/O COMPLETED TREATMENT", "Discharge small count 0" ,                                                                     
#"Not specified Other or Unknown","Discharge small count 0","Psychiatric Unit or Psychiatric Hospital" , "Discharged to Other Facility"  ,                                                                    
#"Discharged to Other Facility", "Still a Patient" , "Discharged to Other Facility" ,"Discharged to Other Facility")








#copy Hospital Training
Hospital_Train2<-data.frame(Hospital_Train)
#Partition data into: training/validation/testing
set.seed(34567)
num_obs<-nrow(Hospital_Train)
test_obs<-sample(num_obs,0.25*num_obs)
Hospital_Test<-Hospital_Train[test_obs,]
Hospital_Train<-Hospital_Train[-test_obs,]
num_obs2<-nrow(Hospital_Train)
validation_obs<-sample(num_obs2,0.25*num_obs2)
Hospital_Valid<-Hospital_Train[validation_obs,]
Hospital_Train<-Hospital_Train[-validation_obs,]

#Model part
#Logistic Regression
#(1)
fit1<-glm(Hospital_Train$RETURN~HOSPITAL+GENDER+AGE+RACE+ETHNICITY+FINANCIAL_CLASS+WEEKDAY_ARR+HOUR_ARR+MONTH_ARR+WEEKDAY_DEP+HOUR_DEP+MONTH_DEP+SAME_DAY+ED_RESULT+ACUITY_ARR+DC_RESULT+ADMIT_RESULT+CONSULT_ORDER+CONSULT_CHARGE+CONSULT_IN_ED+DIAGNOSIS+DIAG_DETAILS+RISK+SEVERITY+CHARGES,data = Hospital_Train,family = 'binomial')
#fit1<-glm(Hospital_Train$RETURN~HOSPITAL+GENDER+AGE+RACE+ETHNICITY+FINANCIAL_CLASS+WEEKDAY_ARR+HOUR_ARR+MONTH_ARR+WEEKDAY_DEP+HOUR_DEP+MONTH_DEP+SAME_DAY+ED_RESULT+ACUITY_ARR+DC_RESULT+ADMIT_RESULT+DIAGNOSIS+DIAG_DETAILS+RISK+SEVERITY+CHARGES,data = Hospital_Train,family = 'binomial')
fit1$xlevels[["FINANCIAL_CLASS"]]<-union(fit1$xlevels[["FINANCIAL_CLASS"]], levels(Hospital_Test$FINANCIAL_CLASS))
fit1$xlevels[["ED_RESULT"]]<-union(fit1$xlevels[["ED_RESULT"]], levels(Hospital_Test$ED_RESULT))
fit1$xlevels[["ACUITY_ARR"]]<-union(fit1$xlevels[["ACUITY_ARR"]], levels(Hospital_Test$ACUITY_ARR))
fit1$xlevels[["DC_RESULT"]]<-union(fit1$xlevels[["DC_RESULT"]], levels(Hospital_Test$DC_RESULT))
fit1$xlevels[["RACE"]]<-union(fit1$xlevels[["RACE"]], levels(Hospital_Test$RACE))
pred=predict(fit1,newdata =Hospital_Test,type = 'response')
pred<-ifelse(pred>0.5,1,0)
table=table(pred,Hospital_Test[,26])
print(table)
lgr_acc=(table[1,1]+table[2,2])/sum(table)
print(lgr_acc)
lgr_tpr=(table[2,2])/sum((table[2,2]+table[1,2]))
print(lgr_tpr)
#retrain with Train2
lgr<-glm(RETURN~HOSPITAL+GENDER+AGE+RACE+ETHNICITY+FINANCIAL_CLASS+WEEKDAY_ARR+HOUR_ARR+MONTH_ARR+WEEKDAY_DEP+HOUR_DEP+MONTH_DEP+SAME_DAY+ED_RESULT+ACUITY_ARR+DC_RESULT+ADMIT_RESULT+CONSULT_ORDER+CONSULT_CHARGE+CONSULT_IN_ED+DIAGNOSIS+DIAG_DETAILS+RISK+SEVERITY+CHARGES,data = Hospital_Train2,family = 'binomial')






#tree
library(tree)
hospital.tree=tree(RETURN~HOSPITAL+GENDER+AGE+RACE+ETHNICITY+FINANCIAL_CLASS+SAME_DAY+ED_RESULT+ACUITY_ARR+DC_RESULT+ADMIT_RESULT+CONSULT_ORDER+CONSULT_CHARGE+CONSULT_IN_ED+DIAGNOSIS+DIAG_DETAILS+RISK+SEVERITY+CHARGES,data=Hospital_Train)
plot(hospital.tree)
text(hospital.tree)
tree_preds = predict(hospital.tree,newdata=Hospital_Test
)
tree_probs=tree_preds[,2]
tree_class=ifelse(tree_probs>0.5,1,0)
tree_acc=sum(ifelse(tree_class==Hospital_Test$RETURN,1,0))/nrow(Hospital_Test)
tree_acc
tpr=0
#try differenct cutoff
list=c()
a=1
for (i in c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)){
  hospital.tree=tree(RETURN~HOSPITAL+GENDER+AGE+RACE+ETHNICITY+FINANCIAL_CLASS+SAME_DAY+ED_RESULT+ACUITY_ARR+DC_RESULT+ADMIT_RESULT+CONSULT_ORDER+CONSULT_CHARGE+CONSULT_IN_ED+DIAGNOSIS+DIAG_DETAILS+RISK+SEVERITY+CHARGES,data=Hospital_Train)
  tree_preds = predict(hospital.tree,newdata=Hospital_Valid)
  tree_probs=tree_preds[,2]
  tree_class=ifelse(tree_probs>i,1,0)
  acc_tree1=sum(ifelse(tree_class==Hospital_Valid$RETURN,1,0))/nrow(Hospital_Valid)
  list[a]=acc_tree1
  a=a+1
}
print(list)





#Random Forest
library(randomForest)
#try differenct tree numbers at validiation
#list=c()
#a=1
#for (i in seq(5,12)){
#rf.trees=randomForest(RETURN~.,data=Hospital_Train,mtry=i,ntree=1000,importance=TRUE)
#rf_pred<-predict(rf.trees,newdata = Hospital_Valid)
#table=table(rf_pred,Hospital_Valid[,26])
#acc=(table[1,1]+table[2,2])/sum(table)
#list[a]=acc
#a=a+1
#}
#pick highest accuracy mtry numbers
#mtry=seq(7,12)[list==max(list)]
#acc=list[list==max(list)]
#print(mtry)
#print(acc)
#model we used, we pick 1000 as ntrees in parameter, as it has the highest performance in validation
rf.trees=randomForest(RETURN~.,data=Hospital_Train,mtry=6,ntree=1000,importance=TRUE)
rf_pred<-predict(rf.trees,newdata = Hospital_Test)
table=table(rf_pred,Hospital_Test[,26])
print(table)
rf_acc=(table[1,1]+table[2,2])/sum(table)
print(rf_acc)
rf_tpr=(table[2,2])/sum((table[2,2]+table[1,2]))
print(rf_tpr)




#bagging
bagging=randomForest(RETURN~.,data=Hospital_Train,mtry=25,ntree=1000,importance=TRUE)
bagging_pred<-predict(rf.trees,newdata = Hospital_Test)
table=table(bagging_pred,Hospital_Test[,26])
print(table)
bagging_acc=(table[1,1]+table[2,2])/sum(table)
print(bagging_acc)
bg_tpr=(table[2,2])/sum((table[2,2]+table[1,2]))
print(bg_tpr)

#boosting
library(gbm)
boosting=gbm(RETURN~.,data=Hospital_Train,distribution="gaussian",n.trees=1000)
boosting_pred<-predict(boosting,newdata = Hospital_Test)
table=table(boosting_pred,Hospital_Test[,26])
print(table)
boosting_acc=(table[1,1]+table[2,2])/sum(table)
print(boosting_acc)
bst_tpr=(table[2,2])/sum((table[2,2]+table[1,2]))
print(bst_tpr)


Hospital_Train <- model.matrix(~. ,data=Hospital_Train)
Hospital_Test <- model.matrix(~. , data=Hospital_Test)



#xgboosting
levels(Hospital_Train$ACUITY_ARR)<-c("1-Immediate", "2-Emergent", "3-Urgent", "4-Less Urgent", "5-Non-Urgent" , "5-Non-Urgent", "LWBS or others" )
levels(Hospital_Train$ED_RESULT)<-c("Admit", "Admit","Admit to UMMS Psychiatry ","AMA", "Arrived in Error","Deceased","Discharge","ED Result not recorded","Elopement","Left prior to completing treatment","Left without signing discharge instructions", "LWBS after Triage", "LWBS before Triage", "Observation","Send to L&D after Rooming", "Send to L&D Before Rooming (Mom)", "Transfer")
dilibrary(xgboost)
require(Matrix)
label=as.numeric(as.character(Hospital_Train$RETURN))
sparse_matrain<-model.matrix(~.+0,data =Hospital_Train[,-1] ,with=F)
#sparse_matrain <- sparse.model.matrix(RETURN ~ ., data = Hospital_Train)[,-1]
xgbst <- xgboost(data = sparse_matrain, label = label, max.depth = 2, eta = 1, nround = 50, 
                 objective = "binary:logistic")
table(Hospital_Test$GENDER)
Hospital_Test$GENDER<-ifelse(is.na(Hospital_Test$GENDER),2,Hospital_Test$GENDER)
Hospital_Test$GENDER<-ifelse(Hospital_Test$GENDER==1,'Female','Male')
Hospital_Test$GENDER<-as.factor(Hospital_Test$GENDER)
#sparse_matest <- sparse.model.matrix(RETURN ~ ., data = Hospital_Test)[,-1]
sparse_matest<-model.matrix(~.+0,data =Hospital_Test[,-1] ,with=F)
xgbst_pred<-predict(xgbst,newdata = sparse_matest)
xgbst_pred<-ifelse(xgbst_pred>0.5,1,0)
table=table(xgbst_pred,Hospital_Test[,26])
print(table)
xgbst_acc=(table[1,1]+table[2,2])/sum(table)
print(xgbst_acc)


label=as.numeric(as.character(Hospital_Train$RETURN))
levels(Hospital_Train$ACUITY_ARR)<-c("1-Immediate", "2-Emergent", "3-Urgent", "4-Less Urgent", "5-Non-Urgent" , "5-Non-Urgent", "LWBS or others" )
levels(Hospital_Train$ED_RESULT)<-c("Admit", "Admit","Admit to UMMS Psychiatry ","AMA", "Arrived in Error","Deceased","Discharge","ED Result not recorded","Elopement","Left prior to completing treatment","Left without signing discharge instructions", "LWBS after Triage", "LWBS before Triage", "Observation","Send to L&D after Rooming", "Send to L&D Before Rooming (Mom)", "Transfer")
Hospital_Train <- model.matrix(~. ,data=Hospital_Train)
Hospital_Test <- model.matrix(~. , data=Hospital_Test)
xgbst2 <- xgboost(data = Hospital_Train , label = label, max.depth = 2, eta = 1, nround = 50, 
                 objective = "binary:logistic")





#retrain with Train2
bagging=randomForest(RETURN~.,data=Hospital_Train2,mtry=25,ntree=1000,importance=TRUE,na.action=na.roughfix)
rf.trees=randomForest(RETURN~.,data=Hospital_Train2,mtry=6,ntree=1000,importance=TRUE,na.action=na.roughfix)
library(gbm)
boosting=gbm(RETURN~.,data=Hospital_Train2,distribution="gaussian",n.trees=1000)





