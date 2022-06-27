# Towards explainable AI in Credit Risk Management

This repository contains the code to the explainable AI project. Machine Learning (ML) allows creditors to reduce lending
risk by evaluating an abundance of customer data. However, these models lack the transparency required by regulators. We propose a visual analytics tool for understanding the inner-workings of ML models as they apply to credit scoring.

Specifically, we propose building a visual analytics (VA) tool, specifically tailored to the context of credit risk evaluation,
useable for both model developers (i.e. Swiss financial intermediaries operating in the commercial or consumer credit space) as well as model evaluators (Swiss regulatory bodies that have to validate the models). More importantly, such a visual tool will enable model evaluators, a non-technical audience, to gain some insight into how AI models applied to credit scoring work and identify the reasons behind the decisions taken.

Link to collaborative folder on Google Drive:
https://drive.google.com/drive/folders/1cVyRpcwS_23Gp1JmUBrIXaH5KqmmIIlK


## Folder structure

* data: raw contains all the raw data, clean all the cleaned data ready for further use.
* models: contains all the calculated models
* notebooks: All RMarkdown notebooks
* references: Reference materials like data dictionaries etc.
* reports: visualizations, figures, etc.
* scripts: code snippets used for processing and calculations


## Ressources

- [Interpretable Machine Learning](https://christophm.github.io/interpretable-ml-book/)
- [Website of DARPA](https://www.darpa.mil/program/explainable-artificial-intelligenceo)
- [Predicting Loan Defaults on LendingClub.com](https://github.com/jgcorliss/lending-club)
- [Stanford Course P2P Loan Selection](http://stanford.edu/class/msande448/2016/final/group4.pdf)


## Pre-processing and feature selection 

In supervised predictive modelling, algorithms learn by mapping input factors to a target variable. We cannot fit and evaluate machine learning models on raw data but rather datasets have to be processed and transformed so to fit the requirements of individual machine learning models. Even more importantly, we need to identify a representation of the data that best exposes the underlining patters between the input and output features, given a specific machine learning model. As a result, one of the main and most challenging tasks of any machine learning project is the data pre-processing step. 

Specifically, there is an interplay between the data and the choice of algorithms:
*	Some algorithms cannot deal with missing observations
*	Some algorithms assume each variable (in some cases including the target feature), to have a specific probability distribution
*	Some algorithms are negatively impacted if two or more input variables are highly correlated.
*	Some algorithms are known to perform worse if there are input variables that are irrelevant or redundant to the target variable
*	Some algorithms have very few requirements concerning the input data, but in turn, may require many examples in order to learn how to make good predictions

In this project, we developed an automated data pre-processing step, containing the following actions:
*	Factorize: Convert character columns in a data.frame to factors
*	Binarize: Function to binarize a multiclass dataset
*	Target class balance: Undersampling of a data frame with a binary target
*	Missing features: add additional columns to a data frame for all numerical columns with at least one missing value.
*	Transform: Power transform all numerical positive variables using boxcox transformation 
*	Group levels of features: Function to group attributes containing free text based on defined key words. Those keywords are given as a vector where earlier keywords have a higher priority than later keywords. For example; if the `keywords` vector is: c("Manager", "operations") and the text in the observation is "Operations Manager", the new level will be "manager".

Following, the initial data pre-processing step, we continue the feature selection process. Specifically:
1.	Dealing with missing features:
      - In the initial data pre-processing step, we created additional columns to the dataframe for all numerical features that have at least one missing value. The objective of this step is to consequently investigate whether the existence of missing values is associated with our target feature (loan_status). For this purpose, we run a chi square test. The null hypothesis of the Chi-Square test is that there is no relationship whereas the alternative hypothesis assumes that there is an association between the two variables. Results: For all 112 variables the p-value was > 0.05 hence we can conclude that the existence of missing observations is not associated with our target. 
      - Furthermore, we carry-out a 2-step process to deal with the missing features
         - Step 1: cancel columns with over 50% missing observations
         - Step 2: row-wise complete cases
2.	Dealing with highly correlated features: we remove features that are highly correlated (ex. fico_range_low and fico_range_high).
3.	Factor screening: we carry-out an in-depth analysis of the factor variables included in the dataset. All features for which we observe no variability, are removed from further analysis (ex. hardship_type has only one level: interest only-3 months deferral. 
4.	Further feature selection -- Boruta algorithm. As discussed previously, for all data driven models, feature selection can significantly affect model performance. Well-designed features increase models' flexibility and robustness. The literature distingishes between several different techniques for feature selection: 
      - Embedded methods -- where feature selection is an integral part of the ML algorithm
      - Filter methods -- where each feature is assigned a score based on a specific statistical procedure
      - Wrapper methods -- where we compare the predictive utility of ML models that are trained on different coalition of features
      - Hybrid methods -- where we combine at least two of the above mentioned techniques 

   In the context of this project, we applied the Boruta algorithm which arises from the spirit of random forest and further adds randomness to the system. The main idea behind the Boruta algorithm is quite streightforward (Miron Kursa et al. 2010): we make a randomised copy of the system, merge the copy with the original and build the classifier for this extended system. To asses importance of the variable in the original system we compare it with that of the randomised variables. Only variables for whose importance is higher than that of the randomised variables are considered important. The applied procedure is as follows (Miron Kursa et al. 2010):
* We build an extended system, with replicated variables which are then randomply permuted. As a result, all correlations between the replicated and original variables are random by design;
* We perform several RF runs
* For each run we compute the importance of all attributes.
* The attribute is deemed important for a single run if its importance is higher than maximal importance of all randomised attributes.
* We perform a statistical test for all attributes. The null hypothesis is that importance of the variable is equal to the maximal importance of the random attributes (MIRA). The test is a two-sided equality test – the hypothesis may be rejected either when importance of the attribute is significantly higher or significantly lower than MIRA. For each attribute we count how many times the importance of the attribute was higher than MIRA (a hit is recorded for the variable).
* Variables which are deemed unimportant are removed from the information system, usually with their randomised mirror pair. 
* The procedure is performed for predefined number of iterations, or until all attributes are either rejected or conclusively deemed important, whichever comes first

The outputs from our Boruta algo is visualized below.

![Boruta](https://github.zhaw.ch/storage/user/2647/files/6ef55535-b858-4de5-bd63-3dc29dd590b2)

## Training Models

In this project, we rely on the AutoML interface which is designed is a comprahensive framework for training and testing ML models in both R and Python. The algoithms we train and test are as follows:
* DRF (This includes both the Distributed Random Forest (DRF) and Extremely Randomized Trees (XRT) models. Refer to the Extremely Randomized Trees section in the DRF chapter and the histogram_type parameter description for more information.)
* GLM (Generalized Linear Model with regularization)
* XGBoost (XGBoost GBM)
* GBM (H2O GBM)
* DeepLearning (Fully-connected multi-layer artificial neural network)
* StackedEnsemble (Stacked Ensembles, includes an ensemble of all the base models and ensembles using subsets of the base models)

The results from the models trained via the AutoML function are presented below.
![Models_ranked](https://github.zhaw.ch/storage/user/2647/files/7a8fde53-5c41-430e-9e77-1b2424a7a169)

These results are obtained through 5-fold cross validation on the training set (metrics computed for combined holdout prediction). In terms of how this works in detail, in general, for all algos that support the nfolds parameter, H2O’s cross-validation works as follows (Cross-Validation - H20 3.36.0.3 Documentation):
* If we invoke a nfolds=5, 6 models are built. The first 5 models (cross-validation models) are built on 80% of the training data, and a different 20% is held out for each of the 5 models. Then the main model is built on 100% of the training data. This main model is the model you get back from H2O in R, Python and Flow (though the CV models are also stored and available to access later).
* This main model contains training metrics and cross-validation metrics (and optionally, validation metrics if a validation frame was provided). The main model also contains pointers to the 5 cross-validation models for further inspection.
* All 5 cross-validation models contain training metrics (from the 80% training data) and validation metrics (from their 20% holdout/validation data). To compute their individual validation metrics, each of the 5 cross-validation models had to make predictions on their 20% of rows of the original training frame, and score against the true labels of the 20% holdout.
* For the main model, this is how the cross-validation metrics are computed: The 5 holdout predictions are combined into one prediction for the full training dataset (i.e., predictions for every row of the training data, but the model making the prediction for a particular row has not seen that row during training). This “holdout prediction” is then scored against the true labels, and the overall cross-validation metrics are computed.
* This approach has some implications. Scoring the holdout predictions freshly can result in different metrics than taking the average of the 5 validation metrics of the cross-validation models. For example, if the sizes of the holdout folds differ a lot (e.g., when a user-given fold_column is used), then the average should probably be replaced with a weighted average. Also, if the cross-validation models map to slightly different probability spaces, which can happen for small DL models that converge to different local minima, then the confused rank ordering of the combined predictions would lead to a significantly different AUC than the average.

### Performance and prediction
Given a trained H2O model, the h2o.performance() function computes a model’s performance on a given dataset. H2O-3 provides a variety of metrics that can be used for evaluating supervised and unsupervised models. H2O-3 calculates regression metrics for classification problems. The following additional evaluation metrics are available for classification models:
* Gini Coefficient
* Absolute MCC (Matthews Correlation Coefficient)
* F1
* F0.5
* F2
* Accuracy
* Logloss
* AUC (Area Under the ROC Curve)
* AUCPR (Area Under the Precision-Recall Curve)
* Kolmogorov-Smirnov (KS) Metric

We provide details on each of the different evaluation criteria (Performance and Prediction, h2o documentation). 

1. Gini Coefficient
The Gini index is a well-established method to quantify the inequality among values of a frequency distribution, and can be used to measure the quality of a binary classifier. A Gini index of zero expresses perfect equality (or a totally useless classifier), while a Gini index of one expresses maximal inequality (or a perfect classifier). The Gini index is based on the Lorenz curve. The Lorenz curve plots the true positive rate (y-axis) as a function of percentiles of the population (x-axis). The Lorenz curve represents a collective of models represented by the classifier. The location on the curve is given by the probability threshold of a particular model. (i.e., Lower probability thresholds for classification typically lead to more true positives, but also to more false positives.) The Gini index itself is independent of the model and only depends on the Lorenz curve determined by the distribution of the scores (or probabilities) obtained from the classifier.

2. Absolute MCC (Matthews Correlation Coefficient)
Setting the absolute_mcc parameter sets the threshold for the model’s confusion matrix to a value that generates the highest Matthews Correlation Coefficient. The MCC score provides a measure of how well a binary classifier detects true and false positives, and true and false negatives. The MCC is called a correlation coefficient because it indicates how correlated the actual and predicted values are; 1 indicates a perfect classifier, -1 indicates a classifier that predicts the opposite class from the actual value, and 0 means the classifier does no better than random guessing.

3. F1
The F1 score provides a measure for how well a binary classifier can classify positive cases (given a threshold value). The F1 score is calculated from the harmonic mean of the precision and recall. An F1 score of 1 means both precision and recall are perfect and the model correctly identified all the positive cases and didn’t mark a negative case as a positive case. If either precision or recall are very low it will be reflected with a F1 score closer to 0.

4. F0.5
The F0.5 score is the weighted harmonic mean of the precision and recall (given a threshold value). Unlike the F1 score, which gives equal weight to precision and recall, the F0.5 score gives more weight to precision than to recall. More weight should be given to precision for cases where False Positives are considered worse than False Negatives. For example, if your use case is to predict which products you will run out of, you may consider False Positives worse than False Negatives. In this case, you want your predictions to be very precise and only capture the products that will definitely run out. If you predict a product will need to be restocked when it actually doesn’t, you incur cost by having purchased more inventory than you actually need.

5. F5
The F2 score is the weighted harmonic mean of the precision and recall (given a threshold value). Unlike the F1 score, which gives equal weight to precision and recall, the F2 score gives more weight to recall (penalizing the model more for false negatives then false positives). An F2 score ranges from 0 to 1, with 1 being a perfect model.

6. Accuracy 
In binary classification, Accuracy is the number of correct predictions made as a ratio of all predictions made. In multiclass classification, the set of labels predicted for a sample must exactly match the corresponding set of labels in y_true.

7. Logloss
The logarithmic loss metric can be used to evaluate the performance of a binomial or multinomial classifier. Unlike AUC which looks at how well a model can classify a binary target, logloss evaluates how close a model’s predicted values (uncalibrated probability estimates) are to the actual target value. For example, does a model tend to assign a high predicted value like .80 for the positive class, or does it show a poor ability to recognize the positive class and assign a lower predicted value like .50? Logloss can be any value greater than or equal to 0, with 0 meaning that the model correctly assigns a probability of 0% or 100%.

8. AUC
This model metric is used to evaluate how well a binary classification model is able to distinguish between true positives and false positives. An AUC of 1 indicates a perfect classifier, while an AUC of .5 indicates a poor classifier, whose performance is no better than random guessing. H2O uses the trapezoidal rule to approximate the area under the ROC curve. (Tip: AUC is usually not the best metric for an imbalanced binary target because a high number of True Negatives can cause the AUC to look inflated. For an imbalanced binary target, we recommend AUCPR or MCC.)

9. AUCPR (Area Under the Precision-Recall Curve)
This model metric is used to evaluate how well a binary classification model is able to distinguish between precision recall pairs or points. These values are obtained using different thresholds on a probabilistic or other continuous-output classifier. AUCPR is an average of the precision-recall weighted by the probability of a given threshold. The main difference between AUC and AUCPR is that AUC calculates the area under the ROC curve and AUCPR calculates the area under the Precision Recall curve. The Precision Recall curve does not care about True Negatives. For imbalanced data, a large quantity of True Negatives usually overshadows the effects of changes in other metrics like False Positives. The AUCPR will be much more sensitive to True Positives, False Positives, and False Negatives than AUC. As such, AUCPR is recommended over AUC for highly imbalanced data.

In the context of the mcc, f1, f0.5, f2 and the overall accuracy, we report the decision thresholds which lead to the highest values. 

![Performance_models](https://github.zhaw.ch/storage/user/2647/files/1b6b4676-b6b4-4560-a9f2-68c5dbfcb1ea)

We further report, the logloss, the auc and the aucpr values for each of the best performing models in a category.
![Performance_models 2](https://github.zhaw.ch/storage/user/2647/files/f02643d0-7c03-429a-a7c0-72bd108acd84)

### ROC and Precision Recall Curve

Below we plot the ROC and the Precision Recall Curves for each of the best performing models in a category.

* SE 

![SE_ROC_PR](https://github.zhaw.ch/storage/user/2647/files/1eee6cc2-ba7b-421c-b48a-4cfb54156942)

* GLM

![GLM_ROC_PR](https://github.zhaw.ch/storage/user/2647/files/41d7306a-5bdf-4280-b1d1-6e9c3588ad99)

* GBM

![GBM_ROC_PR](https://github.zhaw.ch/storage/user/2647/files/35a77293-37ed-4364-8297-aba328e7f962)

* DRF

![DRF_ROC_PR](https://github.zhaw.ch/storage/user/2647/files/62f3532b-9824-411e-898d-bd510fb80ea8)


* DL

![DL_ROC_PR](https://github.zhaw.ch/storage/user/2647/files/767f7df8-d80d-4d98-a147-ea29958fa526)
