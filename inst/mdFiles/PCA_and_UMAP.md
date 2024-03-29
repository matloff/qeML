
#  Clearing the Confusion: PCA and UMAP--How Are They Used in Prediction, and What about Claims That They Require Scaling?

Principal Components Analysis (PCA) is a well-established method of
dimension reduction.  It is often used as a means of gaining insight
into the "hidden meanings" in a dataset.  But in prediction
contexts--ML--it is mainly a technique for avoiding overfitting and excess
computation.

This tutorial has two goals:

* We will give a more concrete, more real-world-oriented, overview of
  PCA than those given in most treatments.

* We will take a critical look at the commonly-held view that one must
  scale one's data prior to performing PCA.`

A number of "nonlinear versons" of PCA have been developed, including 
Uniform Manifold Approximation and Projection (UMAP), which we will also
discuss briefly here.

# PCA

**All PCA does is form new columns from the original columns 
of our data.**  Those new columns form our new feature set.
It's that simple.

Each new column is some linear combination of our original columns.
Moreover, the new columns are uncorrelated with each other, the
importance of which we will also discuss.

Let's see concretely what all that really means.

## Dataset--mlb

Consider **mlb**, a dataset included with **qeML**.  We will look at
heights, weights and ages of American professional baseball players.
(We will delete the first column, which records position played.)

``` r
> data(mlb)
> mlb <- mlb[,-1]
> head(mlb)
  Height Weight   Age
1     74    180 22.99
2     74    215 34.69
3     72    210 30.78
4     72    210 35.43
5     73    188 35.71
6     69    176 29.39
> mlb <- as.matrix(mlb)
> dim(mlb)
[1] 1015    3   # 1015 players, 3 measurements each
```

## Apply PCA

The standard PCA function in R is **prcomp**:

``` r
> z <- prcomp(mlb)  # defaults center=TRUE, scale.=FALSE)
```

We will look at the contents of **z** shortly.  But first, it is key to
note that all that is happening is that *we started with 3 variables*,
Height, Weight and Age, and now *have created 3 new variables*, **PC**,
**PC2** and **PC3**.  Those new variables are stored in **z$x**.  Again,
we will see the details below, but the salient point is: 3 new features.

We originally had 3 measurements on each of 1015 people, and now we have 3 new
measurements on each of those people:

``` r
> dim(z$x)
[1] 1015    3
```

Well then, what *is* in there in **z**?

``` r
> str(z)
List of 5
 $ sdev    : num [1:3] 20.87 4.29 1.91
 $ rotation: num [1:3, 1:3] -0.0593 -0.9978 -0.0308 -0.1101 -0.0241 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:3] "Height" "Weight" "Age"
  .. ..$ : chr [1:3] "PC1" "PC2" "PC3"
 $ center  : Named num [1:3] 73.7 201.3 28.7
  ..- attr(*, "names")= chr [1:3] "Height" "Weight" "Age"
 $ scale   : logi FALSE
 $ x       : num [1:1015, 1:3] 21.46 -13.82 -8.6 -8.74 13.14 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:1015] "1" "2" "3" "4" ...
  .. ..$ : chr [1:3] "PC1" "PC2" "PC3"
 - attr(*, "class")= chr "prcomp"

```

Let's look at **rotation** first.

As noted, each principal component (PC) is a linear combination
of the input features.  These coefficients are stored in **rotation**:

``` r
> z$rotation
               PC1         PC2         PC3
Height -0.05934555 -0.11013610 -0.99214321
Weight -0.99776279 -0.02410352  0.06235738
Age    -0.03078194  0.99362420 -0.10845927

```

For instance,

PC2 = -0.11 Height - 0.02 Weight + 0.99 Age

As noted, those 3 new variables are stored in the **x** component of
**z**.  For instance, consider the first person in the dataset:

``` r
> mlb[1,]
Height Weight    Age 
 74.00 180.00  22.99 
```

In the new data, his numbers are:

``` r
> z$x[1,]
      PC1       PC2       PC3 
21.458611 -5.201495 -1.018951 

```

Let's check!  Since the PCi are linear combinations of the original
columns, we can compute them via matrix multiplication:

``` r
> mlbc <- center(mlb)  # remember, prcomp did centering
> mlbc[1,] %*% z$rotation[,2]
          [,1]
[1,] -5.201495
```

Ah yes, it checks.

## Key properties

The key properties of PCA are that the PCs

a. are arranged in order of decreasing variances, and

b. they are uncorrelated.

The variances (actually standard deviations) are reported in the return
object from **prcomp**:

``` r
> z$sdev
[1] 20.869206  4.288663  1.911677
```

Yes, (a) holds.  Let's double check, say for PC2:

``` r
> sd(z$x[,2])
[1] 4.288663
```

Yes.

What about (b)?

``` r
> cor(z$x)
              PC1           PC2          PC3
PC1  1.000000e+00 -1.295182e-16 2.318554e-15
PC2 -1.295182e-16  1.000000e+00 2.341867e-16
PC3  2.318554e-15  2.341867e-16 1.000000e+00
```

Yes indeed, those new columns are uncorrelated.

## Practical importance of (a) and (b)

The reader of this document has probably seen properties (a) and (b)
before.  *But why are they so important?*

Many data analysts, e.g. social scientists, use PCA to search for
patterns in the data.  In the ML context, though, our main interest is
prediction. 

Our focus:

> If we have a large number of predictor variables, we would like to
> reduce that number, in order to avoid avoid overfitting, reduce
> computation and so on.  PCA can help us do that.  Properties (a) and
> (b) will play a central role in this.

Toward that end, we will first introduce another dataset, and then
discuss *dimension reduction*--reducing the number of predictor
variables--in the context of that data.  That will lead us to the
importance of properties (a) and (b).

### Dataset--fiftyksongs

Here we will use another built-in dataset in **qeML**, a song database
named **fiftyksongs**.  It is a 50,000-row random subset of the famous Million
Song Dataset.

The first column of the data set is the year of release of the song,
while the other 90 are various audio measurements.  The goal is to
predict the year.  

``` r
> dim(fiftyksongs)
[1] 50000    91

> w <- prcomp(fiftyksongs[,-1])  # first column is "Y", to be predicted
> w$sdev
 [1] 2127.234604 1168.717654  939.840843  698.575319  546.683262  464.683454
 [7]  409.785038  395.928095  380.594444  349.489142  333.322277  302.017413
[13]  282.819445  260.362550  255.472674  248.401464  235.939740  231.404983
[19]  220.682026  194.828458  193.645669  189.074051  187.455170  180.727969
[25]  173.956554  166.733909  156.612298  151.194556  144.547790  138.820897
[31]  133.966493  124.514162  122.785528  115.486330  112.819657  110.379903
[37]  109.347994  106.551231  104.787668   99.726851   99.510556   97.599960
[43]   93.161508   88.559160   87.453436   86.870468   82.452985   80.058511
[49]   79.177031   75.105451   72.542646   67.696172   64.079955   63.601079
[55]   61.105579   60.104226   56.656737   53.166604   52.150838   50.515730
[61]   47.954210   47.406341   44.272814   39.914361   39.536682   38.653450
[67]   37.228741   36.007748   34.192456   29.523751   29.085855   28.387604
[73]   26.325406   24.763188   22.192984   20.203667   19.739706   18.453111
[79]   14.238237   13.935897   10.813426    9.659868    8.938295    7.725284
[85]    6.935969    6.306459    4.931680    3.433850    3.041469    1.892496
```

### Dimension reduction

One hopes to substantially reduce the number of predictors from 90.  But
how many should we retain?  And which ones?

There are 2<sup>90</sup> possible sets of predictors to use.  It of
course would be out of the question to check them all.  (And given the
randomness, the odds are high that the "best"-predicting set is just an
accident.)

So, we might consider the following predictor sets, following the order
of the features (which are named **V2**, **V3**, **V4**,...)

**V2** alone;
**V2** and **V3**;
**V2** and **V3** and **V4**;
**V2** and **V3** and **V4** and **V5**;
etc.

Now we have only 90 predictor sets to check--a lot, but far better than
2<sup>90</sup>.  Yet there are two problems that would arise:

* Presumably the **Vi** are not arranged in order of importance as
  predictors.  What if, say, **V12**, **V28** and **V88** make for an
  especially powerful predictor set?  The scheme considered here would
  never pick that up.

* Possibility of substantial duplication:  What if, say, **V2** and
  **V3** are very highly correlated?  Then once **V2** is in our
  predictor set, we probably would not want to include **V3**; we are
  trying to find a parsimonious predictor set, and inclusion of
  (near-)duplicates would defeat the purpose.  Our second candidate set
  above would be **V2** and **V4**, the third would be **V2** and **V4**
  and **V5**; and so on.  We may wish to skip some other **Vi** as well.
  Checking for such correlation at every step would be cumbersome and
  time-consuming.

Both problems are addressed by using the PCs **Pi** instead of the
original variables **Vi**.  We will consider these predictor sets,

**P1** alone;
**P1** and **P2**;
**P1** and **P2** and **P3**;
**P1** and **P2** and **P3** and **P4**;
etc.

What does this buy us?

* Recall that Var(P<sub>i</sub>) is decreasing (technically
  nonincreasing).  For large i, Var(P<sub>i</sub>) is typically tiny.
  And a random variable with small variance is essentially constant,
  thus of no value as a predictor.  That does not necessarily mean that
  the actual predictive power of the **Pi** is decreasing in i, but 
  at least we now have a reasonable ordering of our predictor sets.

* By virtue of their uncorrelated nature, the **Pi** basically do not 
  duplicate each other.  While it is true that uncorrelatedness does not 
  necessarily imply independence, again we have a reasonable solution to the 
  duplication problem raised earlier.

In summary, then, we will try predictor sets of size m, of the form
**P1**,...,**Pm**, assessing each via cross-validations.  For
convenience, well use a linear model

``` r
> f50s <- fiftyksongs
> w <- prcomp(f50s[,-1])  # omit year
> f50s[,-1] <- w$x  # replace orig. features by PCs
> dim(f50s)  
[1] 50000    91
```

We predicted **f50s[,1]** from the first m PCs, for m = 1,...,9.  For
instance, for m = 3, we predicted column 1 of **f50s** from columns 2
through 16.  We took average mean absolute prediction error as our
goodness criterion, and did 50 runs at each value of m.

The results were

``` r
 [1] 7.910858 7.865185 7.761906 7.692510 7.661526 7.620048 7.646959 7.633888
 [9] 7.539551 7.567980 7.631762 7.652315
```

This is the familiar U-shaped bias-variance tradeoff curve, albeit
somewhat flat.  

It seems m = 45 did best.  of course, some other ML method than a linear
model may do substantially better.  The above is just a start.  But the
value of PCA in dimension reduction and in avoiding overfitting is clear.

## The Scaling Issue

One often hears that prior to performing PCA on a dataset, one should
center and scale the data, so that each column has mean 0 and variance.
This is generally reasonable, but it can be counterproductive in some
settings.  Let's take a look.

### Overview

The recommendation to scale is common.  Here are some examples: 

* R **prcomp()** man page

    They say "scaling is advisable":

> scale.: a logical value indicating whether the variables should be
>           scaled to have unit variance before the analysis takes place.
>           The default is ‘FALSE’ for consistency with S, but in general
>           scaling is advisable.

* [Scikit-Learn](https://scikit-learn.org/stable/auto_examples/preprocessing/plot_scaling_importance.html):

    Actually, the mention of normal distributions is misleading and in
    any case not relevant, but again there is a rather imperative
    statement to scale:

> Feature scaling through standardization (or Z-score normalization) can
> be an important preprocessing step for many machine learning algorithms.
> Standardization involves rescaling the features such that they have the
> properties of a standard normal distribution with a mean of zero and a
> standard deviation of one.

* [DataCamp](https://www.datacamp.com/community/tutorials/pca-analysis-r)

Again, their phrasing is rather imperative:

> Note that the units used [in the **mtcars** dataset] vary and occupy
> different scales...You will also set two arguments, center and scale, to
> be TRUE. 

* [caret](https://cran.r-project.org/package=caret), **preProcess** man
  page

    Scaling done unless you say no:

> If PCA is requested but centering and scaling are not, the values will
> still be centered and scaled. 

* [Visually Enforced](https://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/)

The word "must" is used here:

> Since most of the times the variables are measured in different scales,
> the PCA must be performed with standardized data (mean = 0, variance =
> 1).

## The perceived problem

As the DataCamp statement notes, some features may be "large" while other
features are "small."  There is a concern that, without scaling, the large
ones will artificially dominate.  This is especially an issue in light
of the variation in measurement systems -- should a variable measured in
kilometers be given more weight than one measured in miles?

## Motivating counterexample

Consider a setting with two independent variables, A and B, with means
100, and with Var(A) = 500 and Var(B) = 2.  Let A' and B' denote these
variables after centering and scaling.

PCA is all about removing variables with small variance, as they are
essentially constant.  If we work with A and B, we would of course use
only A.  **But if we work with A' and B', we would use both of them, as
they both have variance 1.0** (and they are independent).  Scaling has
seriously misled us here.

## Alternatives

The real goal should be to make the variables *commensurate*.
Standardizing to mean 0, variance 1 is not the only way one can do this.
Consider the following alternatives.

* Do nothing.  In many data sets, the variables of interest are already
  commensurate.  Consider survey data, say, with each survey question
asking for a response on a scale of 1 to 5.  No need to transform the
data here, and worse, standardizing would have the distortionary effect
of exaggerating rare values in items with small variance.

* Map each variable to the interval [0,1], i.e. t -> (t-m)/(M-m), where
  m and M are the minimum and maximum values of the given variable.
  This is typically better than standardizing, but it does have some
  problems.  First, it is sensitive to outliers.  This might be
  ameliorated with a modified form of the transformation (and ordinary
  PCA has the same problem), but a second problem is that new data --
  new data in prediction applications, say -- may stray from this [0,1]
  world.

* Instead of changing the *standard deviation* of a variable to 1.0,
  change its *mean* to 1.0.  This addresses the miles-vs.-kilometers
concern more directly, without inducing the distortions I described
above.  And if one is worried about outliers, then divide the variable
by the median or other trimmed mean.

# And What about UMAP?

Again, PCA forms new variables that are *linear* functions of the
original ones.  That can be quite useful, but possibly constraining.
In recent years, other dimension reduction method have become popular,
notably t-SNE and UMAP.  Let's take a very brief look at the latter.

``` r
library(umap)
custom.settings <- umap.defaults
custom.settings$n_components <- 6
umOut <- umap(fiftyksongs[,-1],config=custom.settings)

```


The new variables will then be returned in **umOut$layout**, analogous
to our **z$x** above.  We will now have a 50000 x 6 matrix, replacing
our original 50000 x 90 data.

So, what does UMAP actually do?  The math is quite arcane; even the
basic assumption, "uniform distribution on a manifold," is beyond the
scope of this tutorial.  

But roughly speaking, the goal is to transform the original data,
dimension 90 here, to a lower-dimensional data set (6 here) in such a
way that "local" structure is retained.  The latter condition means that
rows in the data that were neighbors of each other in the original data
are likely still neighbors in the new data, subject to the
hyperparameter **n_neighbors**: a data point **v** counts as a neighbor
of **u** only if **v** is among the **n_neighbors** points to **u**.

In terms of the Bias-Variance Tradeoff, smaller values of
**n_neighbors** reduce bias while increasing variance, in a similar
manner to the value of **k** in the k-Nearest Neighbors predictive
method.
