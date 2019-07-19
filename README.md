# Loan-Prediction
Identify the customers segments, those are eligible for loan amount so that the finance company can specifically target these customers

Dream Housing Finance company deals in all home loans. They have presence across all urban, semi urban and rural areas. Customer first apply for home loan after that company validates the customer eligibility for loan.

Problem
Company wants to automate the loan eligibility process (real time) based on customer detail provided while filling online application form. These details are Gender, Marital Status, Education, Number of Dependents, Income, Loan Amount, Credit History and others. To automate this process, they have given a problem to identify the customers segments, those are eligible for loan amount so that they can specifically target these customers. Here they have provided a partial data set.

Link : https://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii/

Solution:

There are missing values in variables Gender, Married,Dependents, Self-Employed, LoanAmount,Loan_Amount_Term and Credit_History.
The values are imputed by the median or most frequently occuring values.

There are no outliers observed in the dataset.

Feature Engineering :
I] There are derived variables deduced such as :
-> Term in yrs 
-> TotalIncome 
-> LoanTermRatio 
-> IncomeLoanRatio

II] The variables Education,Gender,Married,SelfEmployed are converted from qualitative to quantitative

III] One-hot encoding is performed on variables Dependents,PropertyArea and binned TermYrs

IV] Scaling of variables LoanAmount, TotalIncome, LoanTermRatio, IncomeLoanRatio using min-max normalization method.

V] On performing logistic regression, an accuracy of value 0.7077656 was obtained.

VI] Further to this, various other models were implemented with their accuracy values mentioned:
-> KNN : 0.71
-> SVM : 0.77
-> Average Neural Network : 0.78
-> XGB : 0.78

