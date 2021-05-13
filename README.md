# The qeML package

"Quick and easy" wrappers for machine learning packages in R.

Norm Matloff, UC Davis, *matloff@cs.ucdavis.edu*

## Overview

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

## ML methods available 

In addition to the SVM and gradient boosting examples above,
the package includes k-NN, random forests, neural networks,
linear/logit, LASSO/ridge and others.

There are wrappers for two different implementations of SVM, and three
different implementations of random forests.

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

## Application-specific functions (elementary)

* **qeTS():**  Time series.

* **qeText()**  Text classification.

* **Image classification**  Our **imageClassR** package uses qe
  functions for this.  (Under construction.)

## Other functions

* **qeFT():**  Grid search for hyperparameter tuning.

* **qeCompare()**  Compare the accuracy various ML methods on the given dataset.

