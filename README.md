# Towards eXplainable AI in Financial Applications 

This repository contains the code to the explainable AI project funded by Innosuisse 41084.1 IP-SBM project. In this project, we aim to build a VA framework which will enable users to get some insights into the innerworkings of ML models as applied to financial problem sets. Specifically, we consider two different use cases:
*   credit riks modelling 
*   financial time series forecasting 

## Credit Risk Use Case 
Machine Learning (ML) allows creditors to reduce lending risk by evaluating an abundance of customer data. However, these models lack the transparency required by regulators. We propose a visual analytics tool for understanding the inner-workings of ML models as they apply to credit scoring.

Specifically, we propose building a visual analytics (VA) tool, specifically tailored to the context of credit risk evaluation, useable for both model developers (i.e. Swiss financial intermediaries operating in the commercial or consumer credit space) as well as model evaluators (Swiss regulatory bodies that have to validate the models). More importantly, such a visual tool will enable model evaluators, a non-technical audience, to gain some insight into how AI models applied to credit scoring work and identify the reasons behind the decisions taken.

In this context the repository contains all the scripts used for:
* data preprocessing
* feature selection (2 scripts)
* data exploration and visualization 
* models' training and evaluation
* running various explainability approaches on the trained models
* sensitivity analysis 


### Folder structure for the credit risk modelling use case 

* data: raw (source data), beta_data (after inital feature selection step), alpha_data (after secondary (boruta) feature selection), final dataset (used for the analysis conducted)
* models: contains all the developed models
* notebooks: All RMarkdown notebooks created in the project
* functions developed


## Finacial Time Series Forecasting 

A key objective of the projet is to review the utility of classical XAI methods for financial problem sets and if necessary propose new methods for assigning meaning to the output of a complex ML model as applied to financial data. In this context, the research conducted, suggestes that classical XAI approaches and their current implementation are not tailored for financial time series (subject to trends, vola-clusters,...).

Specifically, perturbation-based methods are fully dependent on the ability to perturb samples in a meaningful way. In the context of financial data:

* if features are correlated, the artificial coalitions created will lie outside of the multivariate joint distribution of the data,
* if the data are independent, coalitions/generated data points can still be meaningless; 
* generating artificial data points through random replacement disregards the time sequence hence producing unrealistic values for the feature of interest.

In this context, we propose a XAI approach for neural nets based on a family of X-functions which preserve and exploit the natural time ordering and the possible non-stationary dependence structure of the data.


### Folder structure for the financial time series forecasting use case 
* data: containing the time series data use in the project
* functions: script containing all generated functions 
* scripts: full scripts containing the step-by-step process of training various DL models and runnding the X-function proposed within this project

## Explainable AI Application

This app is part of an InnoSuisse project with the aim to apply explainable AI to financial use-cases.

### How to run the app
1. Clone the GitHub Repository to your local machine. 
2. Create a folder called "environment" in the data folder and extract the environment ZIP there.
3. Open the ui.R or server.R in you R-Studio.
4. Click run in the top right corner of the script to execute the app.
## Ressources

- [Interpretable Machine Learning](https://christophm.github.io/interpretable-ml-book/)
- [Website of DARPA](https://www.darpa.mil/program/explainable-artificial-intelligenceo)
- [Predicting Loan Defaults on LendingClub.com](https://github.com/jgcorliss/lending-club)
- [Stanford Course P2P Loan Selection](http://stanford.edu/class/msande448/2016/final/group4.pdf)

