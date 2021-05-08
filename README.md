# qeML

## Overview

The letters 'qe' in the package title stand for "quick and easy,"
alluding to the convenience goal of the package.  We bring together a
variety of machine learning (ML) tools from standard R packages,
providing wrappers with a simple, uniform interface.

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

and so on.  It couldn't be easier@!

Default values are used on the above calls, but nondefaults can be
specified, e.g.

``` r
qeSVM(mlb,'Weight',gamma=0.8)
```

