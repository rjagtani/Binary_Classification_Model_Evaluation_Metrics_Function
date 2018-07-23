# Binary_Classification_Model_Evaluation_Metrics_Function
A function which takes model probability and actual values as argument and returns an xlsx with model evaluation metrics (Gini,KS,AUC,Lift Table,Confusion Matrix,Precision,Recall etc) for training and test datsets.
Update 1 : Includes publication quality charts such as Cumulative Lift Chart, ROC Curve & Cumulative Events vs Non Events
Update 2 : Incudes option to optimize threshold by maximizing model performance metrics such as Accuracy,F1-Score,Kappa Value & Youden's J Statistic. The confusion matrix is then obtained using this optimized threshold.

List of arguments -
1) pred_prob(numeric) : estimated probability output of the model
2) actual_values(numeric/integer/factor) : vector of actual_values of the dependent_variable. Takes only two values - 0 & 1 where 0 is non event and 1 is event.
3)threshold_optimize_metric(char) : Metric maximized to find optimal threshold probability for classifying 0s and 1s. Used to calculate Confusion Matrix and other related metrics. Metrics available : 'accuracy','kappa','f1_score','youdens_statistic'; Default : 'accuracy'
4) ntile(int) : number of buckets required for Lift table ( Default is 10 - deciles )
5) filename(char) : name of excel file passed as a character vector (.xlsx need not be included in filename);Default : 'demo_file'
6) pred_prob_test(numeric) : estimated probability output of the model on test dataset;optional
7) actual_values_test(numeric/integer) : integer vector of actual_values of the_dependent_variable for test dataset;optional

The output of the function contains the following :
1. Metrics - AUC,Gini,KS_value,Confusion Matrix using optimized threshold,Accuracy,Kappa,No Information Rate,Sensitivity,Specificity,Pos Pred Value,Neg Pred Value,F1,Prevalence,KS_decile,Optimized Threshold
2. Table - Lift table with Gains and Cumulative Lift
3. Charts - ROC Curve , Cumulative Events vs Non Events , Cumulative Lift Chart vs Ntile


Function call : 
model_eval(actual_values = actual,pred_prob = pred,filename='demo_file',ntile=10,pred_prob_test = pred_test,actual_values_test = actual_test,threshold_optimize_metric='accuracy')

#### Output of this function call (demo_file.xlsx) and modelling dataset (train_titanic.csv) provided in Repository
