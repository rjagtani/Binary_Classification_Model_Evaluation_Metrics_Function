# Binary_Classification_Model_Evaluation_Metrics_Function
A function which takes model probability and actual values as argument and returns an xlsx with model evaluation metrics (Gini,KS,AUC,Lift Table &amp; Charts,Confusion Matrix,Precision,Recall etc) for training and test datsets.

List of arguments -
1) pred_prob : estimated probability output of the model
2) actual_values : integer vector of actual_values of the dependent_variable
3) prob_cutoff : Threshold probability for classifying 0s and 1s. Used to calculate Confusion Matrix and other related metrics.Default is taken as 0.5
4) ntile : number of buckets required for Lift table ( Default is 10 - deciles )
5) filename : name of excel file passed as a character vector (.xlsx need not be included in filename)
6) pred_prob_test : estimated probability output of the model for test dataset;optional
7) actual_values_test : integer vector of actual_values of the_dependent_variable for test dataset;optional

The output of the function contains the following :
1. Metrics - AUC,Gini,KS_value,Confusion Matrix,Accuracy,Kappa,No Information Rate,Sensitivity,Specificity,Pos Pred Value,Neg Pred Value,F1,Prevalence,KS_decile 
2. Table - Lift table with Gains and Cumulative Lift
3. Charts - ROC Curve , Cumulative Events vs Non Events , Cumulative Lift Chart vs Ntile


