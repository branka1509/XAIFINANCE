# Towards explainable AI in Credit Risk Management

This repository contains the code to the explainable AI project funded by Innosuisse xxx project. In this project, we aim to build a VA framework which will enable users to get some insights into the innerworkings of ML models as applied to financial problem sets. Specifically, we consider two different use cases:
*   credit riks modelling 
*   financial time series forecasting 

## Credit Risk Use Case 
Machine Learning (ML) allows creditors to reduce lending risk by evaluating an abundance of customer data. However, these models lack the transparency required by regulators. We propose a visual analytics tool for understanding the inner-workings of ML models as they apply to credit scoring.

Specifically, we propose building a visual analytics (VA) tool, specifically tailored to the context of credit risk evaluation, useable for both model developers (i.e. Swiss financial intermediaries operating in the commercial or consumer credit space) as well as model evaluators (Swiss regulatory bodies that have to validate the models). More importantly, such a visual tool will enable model evaluators, a non-technical audience, to gain some insight into how AI models applied to credit scoring work and identify the reasons behind the decisions taken.


## Folder structure for the credit risk modelling use case 

* data: 
** raw 
** beta_data, after inital feature selection step 
** alpha_data, after secondary (boruta) feature selection
** final dataset, used for the analysis conducted 
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

