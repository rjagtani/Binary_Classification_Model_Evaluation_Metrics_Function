library(caret)
#library(caTools)
library(data.table)
library(mltools)
library(xlsx)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\rjagtani\\OneDrive - Tata industries\\Confidential\\2017-18\\Projects\\Functions\\Model Evaluation Binary Classification')
df1=read.csv('train_titanic.csv')
train_test_split=sample.split(df1$Survived)
df1_train=df1[train_test_split,]
df1_test=df1[!train_test_split,]
a=glm(data=df1_train,formula=Survived~Sex+Fare,family = 'binomial')
pred=a$fitted.values
actual=df1_train$Survived
pred_test=predict(a,newdata=df1_test,type='response')
actual_test=df1_test$Survived

model_eval=function(pred_prob,actual_values,prob_cutoff=0.5,filename='model_eval',ntile=10,pred_prob_test=NULL,actual_values_test=NULL)
{
  
  #pred_prob=pred;actual_values=actual;prob_cutoff=0.5;pred_class=NULL
  me <- createWorkbook(type='xlsx')
  metrics_sheet=createSheet(wb=me,sheetName ='Train Evaluation Metrics')
  
  ##### AUC , ROC Curve
  eval_metrics=function(prob,act)
  {
  auc=auc_roc(prob,act)
  gini = (2 * auc) - 1 
  auc_roc=as.data.frame(auc_roc(prob,act,returnDT=TRUE))
  png("roc.png", height=800, width=1200, res=250, pointsize=8)
  print(ggplot(data=auc_roc,aes(x=auc_roc$CumulativeFPR,y=auc_roc$CumulativeTPR)) + geom_line(color='white') + geom_polygon(x=auc_roc$CumulativeFPR,y=auc_roc$CumulativeTPR,fill='dodgerblue4')  + geom_abline(intercept=0,slope=1,color='white')  + scale_x_continuous(expand = c(0.00, 0.00))  + scale_y_continuous(expand = c(0.00, 0.00)) + geom_text(aes(x=0.75,y=0.4,label=paste0('AUC : ',round(auc,2))),color='dodgerblue4') + geom_text(aes(x=0.75,y=0.29,label=paste0('GINI : ',round(2*auc-1,2))),color='dodgerblue4') + theme_minimal() + theme(axis.title = element_text(colour = "dodgerblue4"),plot.title = element_text(size= 15 ,hjust = 0.4,color='dodgerblue4',family='serif')) + ggtitle('ROC Curve') + labs(aes(x='\nFalse Positive Rate',y='True Positive Rate\n'))) 
  dev.off()
  
  ###### Lift curve
  
  lift = function(depvar, predcol, groups=ntile) 
  {
    if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
    if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
    helper = data.frame(cbind(depvar, predcol))
    helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(depvar), funs(total = n(),
                                      totalresp=sum(., na.rm = TRUE),totalnonresp=total-totalresp)) %>%
      mutate(Cumresp = cumsum(totalresp),Cumresp_percent=Cumresp/sum(totalresp),Cumnonresp=cumsum(totalnonresp),Cumnonresp_percent=Cumnonresp/sum(totalnonresp),
             Cum_resp_minus_cum_non_resp=Cumresp_percent-Cumnonresp_percent,Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups)))
    gaintable=as.data.frame(gaintable)
    ks=c(max(gaintable$Cum_resp_minus_cum_non_resp),which.max(gaintable$Cum_resp_minus_cum_non_resp))
    names(ks)=c('KS_value','KS_decile')
    return(list(gaintable,ks))
  }  
  
  
  cumulative_table=lift(depvar = act,predcol = prob)
  lift_table=as.data.frame(cumulative_table[[1]])
  ks_stats=cumulative_table[[2]]
  #lift_curve_sheet=createSheet(wb=me,sheetName = 'Lift Curve')
  png("cumulative_lift_curve.png", height=800, width=1200, res=250, pointsize=8)
  print(ggplot(data=lift_table,aes(x=bucket,y=Cumlift))  + geom_point(color='black') + geom_line(color='dodgerblue4')   + labs(aes(x='\nDecile',y='Cumulative Lift\n',colour='dodgerblue4')) + scale_x_continuous(breaks = 1:10) + theme_minimal() + theme(axis.title = element_text(colour = "dodgerblue4"),plot.title = element_text(size=15,family='serif',hjust = 0.4,color='dodgerblue4')) + ggtitle('Cumulative Lift Chart') + geom_text(aes(x=lift_table$bucket,y=lift_table$Cumlift,label=round(lift_table$Cumlift,1)),position = position_nudge(x=0.25,y = 0.1),color='dodgerblue4'))
  dev.off()

  ######## Cumulative 1s vs 0s
  
  lift_table_1=as.data.frame(t(rep(0,11)))
  colnames(lift_table_1)=colnames(lift_table)
  lift_table2=rbind(lift_table_1,lift_table)
  png("cumulative_ones_curve.png", height=800, width=1300, res=250, pointsize=8)
  print(ggplot(data=lift_table2,aes(bucket)) + geom_line(aes(y=Cumresp_percent,col='Cum % 1s')) + geom_point(aes(y=Cumresp_percent),col='blue') + geom_line(aes(y=Cumnonresp_percent,col='Cum % 0s')) + geom_point(aes(y=Cumnonresp_percent),col='red') + labs(aes(x='\nDecile',y='Cumulative Percentage\n')) + scale_x_continuous(breaks = 1:10) + theme_minimal()  + theme(axis.title = element_text(colour = "dodgerblue4"),plot.title = element_text(size= 15 ,hjust = 0.4,color='dodgerblue4',family='serif')) + ggtitle('Cumulative Percent Events vs Non Events') + labs(colour='') + geom_text(aes(x=9,y=0.16,label=paste0('KS : ',round(ks_stats[1]*100,2),' %')),color='dodgerblue4'))
  dev.off()
  colnames(lift_table)=c('NTile','Total Records','Total 1s','Total 0s','Cumulative 1s','Cumulative 1s percent','Cumulative 0s','Cumulative 0s percent','Cumulative percent difference','Gain','Cumulative lift')
  #saveWorkbook(me,'model_eval.xlsx')
  #res=file.remove("cumulative_ones_curve.png")
  
  
  ############  Confusion Matrix
  pred_class=ifelse(prob>=prob_cutoff,1,0)
  pred_class=as.factor(pred_class)
  act=as.factor(act)
  cm=confusionMatrix(pred_class,act)
  metrics_1=cm[[3]][c(1,2,5)]
  metrics_2=cm[[4]][c(1:4,7:8)]
  Total_Records=length(act)
  Value=c(metrics_1,metrics_2,ks_stats,auc,gini,Total_Records)
  names(Value)[c(3,12,13,14)]=c('No Information Rate','AUC','Gini','Total Records')
  Value=Value[c(1,12,13,10,2:9,11,14)]
  Value=as.data.frame(Value)
  Value$Model_Evaluation_Metrics=row.names(Value)
  Value=Value[,c(2,1)]
  con_matrix=as.data.frame(cm[[2]])
  cm1=spread(con_matrix,key=c(2),value=3,sep='_')
  cm1$Prediction=NULL
  row.names(cm1)=c('Prediction_0','Prediction_1')
  ####Adding dataframes to excel sheet
  
  addDataFrame(Value,sheet=metrics_sheet,col.names = T,row.names = F,startColumn = 1,startRow = 1)
  addDataFrame(cm1,sheet=metrics_sheet,col.names = T,row.names = T,startColumn = 1,startRow = 17)
  addDataFrame(lift_table,sheet=metrics_sheet,col.names = T,startColumn = 1,startRow = 21,row.names = F)
  #### Adding plots to excel sheet
  
  addPicture("cumulative_ones_curve.png",metrics_sheet, scale = 1, startRow = 2 ,startColumn = 5)
  res=file.remove("cumulative_ones_curve.png")
  addPicture("cumulative_lift_curve.png",metrics_sheet, scale = 1, startRow = 2,startColumn = 15)
  res=file.remove("cumulative_lift_curve.png")
  addPicture("roc.png",metrics_sheet, scale = 1, startRow = 20,startColumn = 13)
  res=file.remove("roc.png")
  }
  eval_metrics(act = actual_values,prob=pred_prob)
  if(!is.null(actual_values_test) & !is.null(pred_prob_test))
  {
    metrics_sheet=createSheet(wb=me,sheetName ='Test Evaluation Metrics')
    eval_metrics(act = actual_values_test,prob=pred_prob_test)
  }
  saveWorkbook(me,paste0(filename,'.xlsx'))
}


model_eval(actual_values = actual,pred_prob = pred,filename='mv1',pred_prob_test = pred_test,actual_values_test = actual_test)