
The qeML package:  Categorized Function List

# Predictive Methods

## Tree-based

* qeAdaBoost: AdaBoost
* qeDT: one tree; has plot function
* qeGBoost: gbm package
* qeLightGBoost: LightGBoost; fast
* qeRF: random forests; has plot function
* qeRFgrf: random forests; bias correction
* qeRFranger: random forests
* qeskRF: sklearn, needs Python
* qeXGBoost: XGBoost

## Nearest neighbors

* qeKNN: k-NN; bias correction; can give different weights
* qeLinKNN: linear model with kNN correction
* qePolyLinKNN: polynomial linear model with kNN correction

## Neural networks

* qeDeepnet: neural networks, basic, Python-free
* qeNeural: neural networks, using Keras, CNN included, Python needed

## Linear/generalized/regularized linear

* qeLASSO: LASSO; has plot function
* qeLin: wrapper for lm(), with additions
* qeLogit: wrapper for glm(), logit case
* qeNCVregCV: wrapper for ncvreg::cv.ncvreg(); has plot function
* qePolyLASSO: polynomial terms added, then LASSO
* qePolyLin: polynomial terms added, then linear model
* qePolyLog: polynomial terms added, then logit

## SVM

* qeSVM: e1071
* qeliquidSVM: liquid SVM, fast
* qeskSVM: sklearn, needs Python

## Specialized application areas 

* qeText: text classification
* qeTS: time series classification

# Model Fitting, parameter tuning, dimension reduction

* plotClassesUMAP: 2-D plot of UMAP applied to any dataset
* plotPairedResids: for any qe-series output object, finds the
   residuals in the holdout set, and plots them against pairs
   of predictor variables
* qeCompare: compare several qe-series functions on a dataset
* qeFOCI:  advanced nonparametric feature selection
* qeFOCIrand: multiple random runs of qeFOCI
* qeFT: grid search, and qe-series function, including standard errors,
   has plot function
* qeLASSO: feature selection, including "importance" in the form
   of whenEntered component in output
* qeNCVregCV: feature selection similar to LASSO
* qeParallel: parallel run of qe-series function, multiple holdout sets
* qePCA: PCA, followed by qe-series function on result
* qeUMAP: UMAP, followed by qe-series function on result
* qeRFranger: variable.importance component of output
* qeROC: ROC computed on output of qe-series function

# missing value methods

* qeLinMV, qeLogitMV, qeKNNMV: nonimputational methods for predicting
   new cases; both the training data AND the new cases may have NAs

# Utilities

* levelCounts, factorToTopLevels, dataToTopLevels: deletion of rare
   factor levels

