# Mental-Health-Analysis
Problem Statement : Finding the most influential mental health predictors

Brief : The project is to find the most influential mental health predictors using feature selection techniques like random forest, boruta algorithm, chi-square test and mutual information. Classification models like Naive Bayes, Random forest, logistic regression and SVM is built to predict if a person is prone to mental illness based on the workplace environment.

The dataset "survey.csv" was obtained from kaggle.

Details of Code:

The project is implemented using "R".

1. The file "feature_sel_and_classification.R" contains the code to implement feature selection using techniques like random forest, boruta algorithm, chi-square test and mutual information. It also contains code for the implementation of classification models like Naive Bayes, Random forest, logistic regression and SVM.
The following packages are needed to be installed to run this file:
	a) caret
	b) Boruta
	c) FSelector
	d) randomForest
	e) e1071
	f) pROC

2. The file "grouped_barchart.R" contains the code to plot grouped bar chart for finding proportion of the respones for treatment and mental health interview over the months.
The following packages are needed to be installed to run this file:
	a) plotly

3. The file "piechart.R" contains the code to plot pie charts for all finding proportion of the respnses for each attribute in dataset.

4. The file "wordcloud.R" contains the code to obatin the wordcloud for the comments in the dataset.
The following packages are needed to be installed to run this file:
	a) tm
	b) SnowballC
	c) RColorBrewer
	d) wordcloud
	e) plyr
