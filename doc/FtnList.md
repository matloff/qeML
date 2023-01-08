
# The qeML package:  Categorized function list

# Predictive Methods

## Tree-based

* qeAdaBoost: AdaBoost
* qeDT: one tree; has plot function
* qeGBoost: gbm package
* qeLightGBoost: LightGBoost; fast
* qeRF: random forests; has plot function
* qeRFgrf: random forests; bias correction
* qeRFranger: random forests
* qeSVM: SVM
* qeliquidSVM: liquidSVM, fast
* qeskSVM: sklearn SVM, more output info

## Nearest neighbors

* qeKNN: k-NN; bias correction; can give different weights
* qeKNNna: k-NN, dealing with NAs
* qeLinKNN: linear model with kNN correction

## Neural networks

* qeNeural: neural networks, using Keras

## Linear/generalized linear

* qeLASSO: LASSO; has plot function
* qeLin: wrapper for lm(), with additions
* qeLogit: wrapper for glm(), logit case
* qePolyLASSO: polynomial terms added, then LASSO
* qePolyLin: polynomial terms added, then linear model
* qePolyLog: polynomial terms added, then logit

## Special data typs

* qeText: text data; user specifies which qe* function to call
* qeTS: time series data; user specifies which qe* function to call

# Model Fitting, hyperparameter tuning, dimension reduction

* qeCompare: compare several qe* functions on a given dataset
* qeFOCI: sophisticated method of feature selection
* qeFOCIranda: variant of qeFOCI
* qeFT: hyperparameter tuning, including standard errors
* qeParallel: parallel evaluation of cross-validation
* qePCA: run PCA, then user-specified qe*() function on result
* qeROC: plot, print ROC for any qe* function output
* qeUMAP: run UMAP, then user-specified qe*() function on result

# Other

* doubleD: explore "double descent" phenonemon

