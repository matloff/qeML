# The qeML package: "Quick and easy" wrappers for machine learning packages in R.

## "Easy for learners, powerful for experts"

## Norm Matloff, UC Davis, 

I am a professor of computer science, and a former professor of
statistics, highly active in the areas of machine learning and
statistical computing, [bio](heather.cs.ucdavis.edu/matloff.html).

# What this package is about


* "Quick and Easy" ML

    * "Works right out of the box!"

    * much simpler interface than **caret**, **mlr3** and **SuperML**

    * easy for learners, convenient for experts

* Special Feature for ML Learners

    * includes a tutorial on major ML methods

* Special Features for ML Experts

    * large variety of ML algorithms

    * advanced plotting utilities, e.g. Double Descent

## Overview

(Also see extensive Function List section below.)

The letters 'qe' in the package title stand for "quick and easy,"
alluding to the convenience goal of the package.  We bring together a
variety of machine learning (ML) tools from standard R packages,
providing wrappers with a simple, uniform interface.  Hence the term
"quick and easy."

For instance, consider the **mlb** data included in the package,
consisting of data on professional baseball players.  Say we wish to
predict weight of a player.  For SVM, we would make the simple call

``` r
qeSVM(mlb,'Weight')
```

For gradient boosting, the call would be similar,

``` r
qeGBoost(mlb,'Weight')
```

and so on.  It couldn't be easier!

Default values are used on the above calls, but nondefaults can be
specified, e.g.

``` r
qeSVM(mlb,'Weight',gamma=0.8)
```

(Only selected arguments are available.  Suggestions for additions are
very welcome.)

## Prediction

Each qe-series function is paired with a **predict** method, e.g.

``` r
> z <- qeRF(mlb,'Position')
> predict(z,mlb[8,-1])
$predClasses
[1] "Catcher"
...
...
```

## Holdout sets

By default, the qe functions reserve a holdout set on which to assess
accuracy.  

``` r
> z <- qeRF(mlb,'Weight')
holdout set has  101 rows
Loading required package: randomForest
randomForest 4.6-14
Type rfNews() to see new features/changes/bug fixes.
> z$testAcc
[1] 13.32896
> z$baseAcc
[1] 16.68574
```

The mean absolute prediction error was about 13.3; if one simply
predicted every player using the overall mean weight, the MAPE would be
about 16.7.

One can skip this by setting the **holdout** argument to NULL.

## Dimension reduction

One can preprocess the data, both when fitting the training data and
later when predicting new cases, using either PCA or UMAP.  For
instance, consider the **pef** dataset included with the package.  It
consists of Census data on programmers and engineers in 2000.  We'll
specify that we want as many principal components as will comprise 60%
of the total variance.

``` r
> w <- qePCA('pef','wageinc','qeKNN',pcaProp=0.6,holdout=NULL)
> head(pef)
       age     educ occ sex wageinc wkswrkd
1 50.30082 zzzOther 102   2   75000      52
2 41.10139 zzzOther 101   1   12300      20
3 24.67374 zzzOther 102   2   15400      52
4 50.19951 zzzOther 100   1       0      52
5 51.18112 zzzOther 100   2     160       1
6 57.70413 zzzOther 100   1       0       0
> predict(w,pef[8,-5])
      [,1]
[1,] 31316
```

## Function list

* ML algorithms

    * **qeAdaBoost()**: Ada Boosting, wraps **Jousboost** pkg

    * **qeDT()**: decision trees, wraps **party** pkg

    * **qeGBoost()**: gradient boosting, wraps **gbm** pkg

    * **qeISO()**: isotonice regression

    * **qeKNN()**: k-Nearest Neighbors, wraps **regtools** pkg; includes
      predictor importance settings; allows linear interpolation within
      a bin

    * **qeKNNna()**: k-Nearest Neighbors for NA-ridden data, special
      algorithm

    * **qeLASSO()**: LASSO and ridge regression, wraps **glmment** pkg

    * **qelightGBoost()**: gradient boosting, wraps **lightgbm** pkg

    * **qeLin()**: wraps R's **lm()**; can be used for multiclass
      classification, for speed

    * **qeLogit()**: wraps R's **glm()**

    * **qeNeural()**: wraps **keras** package, including CNN

    * **qePolyLASSO()**: LASSO/ridge applied to polynomial regression;
      wraps **glmnet**, **polyreg** pkgs

    * **qePolyLin()**: polynomial regression on linear models;
      uses Moore-Penrose inverse if overfitting; wraps **polyreg** pkg

    * **qePolyLog()**: polynomial regression on logistic models;
      wraps **polyreg** pkg

    * **qeRF()**: random forests, wraps **randomforest** pkg

    * **qeRFgrf**: random forests, wraps **grf** pkg; allows linear
      interpolation within a bin

    * **qeRFranger()**: random forests, wraps **ranger** pkg

    * **qeskRF()**: random forests, wraps Python **Scilearn** pkg

    * **qeskSVM()**: SVM, wraps Python **Scilearn** pkg

    * **qeSVM()**: SVM, wraps **e1071** pkg

    * **qeSVMliquid()**: SVM, wraps **liquid SVM** pkg

    * k-NN, dec. trees, random forests, gradient boosting,
      SVM, linear/gen. linear models, ridge, LASSO, NNs, CNNs

* model-fitting

    * **qeFOCI()**: fully nonparametric method for feature selection

    * **qeFT()**:  automted grid hyperparameter search, with
    Bonferroni-Dunn corrected standard errors

    * **qeDoubleD()**: computation and plotting for exploring Double Descent
```

* application-specific functions (elementary)

    * **qeROC()**:  ROC computation and plotting, wraps **pROC** pkg
    
    * **qeText()**  text classification

    * **qeTS():**  time series
    
    * *Image classification*:  Our **imageClassR** package uses qe
      functions for this.  (Under construction.)

* utilities

    * **qeCompare()**:  compare the accuracy various ML methods on a 
      given dataset

    * **qeParallel()**:  apply "Software Alchemy" to parallize
      qe functions

    * **qePCA()**:  apply PCA before running specified qe ML function

    * **qeUMAP()**:  apply UMAP before running specified qe ML function


