
# Clearing the Confusion: How (and Why) Should We Select Just a Few of Our (Possible Many) Features?

In many applications we may have a larger number of features---dozens,
hundreds or even more.  In machine learning contexts, where we are
interested primarily in prediction, we typically don't want to use all
the features, for a couple of reasons:

* Avoiding overfitting.

    As we add more and more features to a model, bias is reduced but
    variance increases.  At some point, variance overwhelms bias, and our
    predictive ability declines.

* Avoiding large computation time and/or memory usage.

    Depending on the solution method used, computation time for a linear
    model increases with the cube, or at least the square, of the number
    of features.  Modern ML models can be even worse in terms of
    computational burden.  In addition, large models may result in lack
    of convergence or instability of the output.

# Which Method to Use?

Many, many methods for feature selection have been developed over the
years, and you will often hear someone claim that their favorite is *the*
one to use, The Best.  As you may guess, the fact that there are
conflicting claims as to "best" reflects the fact that no such Best 
method exists.  Indeed, much of the feature selection process is *ad hoc*.

We will cover a few feature selection methods here, to explain their
rationales and how to use them in qeML.  You may typically use two or
more of them in any given application.

# How Many Is Too Many?

We will use standard notation here, with n and p denoting the number of
points in our dataset (i.e. number of rows) and the number of features
(i.e. number of columns), respectively.  Important points to remember:

## General principles

* The larger n is, then the larger a value of p that can be used before
  variance increase overwhelms bias reduction.

* However, that bias-variance tradeoff transition point for p depends on the
  application. There is no magic formula for best p as a function of n.

* Nevertheless, a commonly-used rule of thumb is to limit p to at most
  n<sup>0.5</sup>.  Some theoretical analyses even suggest the more
  conservative rule p < log(n).  Again, these are oversimplifications,
  but you may find them useful as guides to intuition.

* The value of p depends on "expanded" versions of any categorical features.

## Impact of categorical variables on p

In many large-p datasets, the culprit is primarily categorical
variables.  Let's see why. 

Consider the dataset **nycdata** included in qeML, which is
derived from taxi trip data made publicly available by the New York City
government.  Many public analyses have been made on various versions of
the NYC taxi data, frequently with the goal of predicting trip time.
Let's take a look:

``` r
> data(nyctaxi)
> dim(nyctaxi)
[1] 10000     5
> head(nyctaxi)
        passenger_count trip_distance PULocationID DOLocationID PUweekday
2969561               1          1.37          236           43         1
7301968               2          0.71          238          238         4
3556729               1          2.80          100          263         3
7309631               2          2.62          161          249         4
3893911               1          1.20          236          163         5
4108506               5          2.40          161          164         5
        DOweekday tripTime
2969561         1      598
7301968         4      224
3556729         3      761
7309631         4      888
3893911         5      648
4108506         5      977
```


(Time-of-day, month etc. data is also available but not in this
particular dataset.)

So n = 10000, p = 5.  At first glance, one might guess that we could use
all 5 features.  Let's try a linear model:

``` r
> z <- qeLin(nyctaxi,'tripTime')
holdout set has  1000 rows
Warning messages:
1: In eval(tmp, parent.frame()) :
  7 rows removed from test set, due to new factor levels
2: In predict.lm(object, newx) :
  prediction from a rank-deficient fit may be misleading
3: In predict.lm(object, newx) :
  prediction from a rank-deficient fit may be misleading
```

To understand those warning messages, let's look at the estimated beta
coefficients:

``` r
> summary(z)

Call:
lm(formula = cbind(tripTime) ~ ., data = xy)

Residuals:
    Min      1Q  Median      3Q     Max
-3807.5  -186.0   -49.8   131.8  3179.2

Coefficients: (4 not defined because of singularities)
                 Estimate Std. Error t value Pr(>|t|)
(Intercept)     -2125.474    351.347  -6.049 1.51e-09 ***
trip_distance     163.056      1.670  97.628  < 2e-16 ***
PULocationID4    1424.775    345.770   4.121 3.81e-05 ***
PULocationID7    1212.331    349.196   3.472 0.000520 ***
PULocationID10    655.251    371.870   1.762 0.078097 .
PULocationID12   1829.276    408.229   4.481 7.52e-06 ***
PULocationID13   1271.201    338.220   3.758 0.000172 ***
PULocationID17   1167.109    477.898   2.442 0.014619 *
PULocationID24   1356.014    342.706   3.957 7.66e-05 ***
PULocationID25   1381.439    354.736   3.894 9.92e-05 ***
PULocationID26   1282.229    506.842   2.530 0.011429 *
PULocationID33   1353.187    363.058   3.727 0.000195 ***
PULocationID35    726.147    474.147   1.531 0.125687
PULocationID36    972.828    391.276   2.486 0.012927 *
...
PULocationID260   972.537    375.500   2.590 0.009614 **
PULocationID261  1334.116    340.437   3.919 8.97e-05 ***
PULocationID262  1362.842    337.713   4.036 5.50e-05 ***
PULocationID263  1343.784    337.113   3.986 6.77e-05 ***
PULocationID264  1378.455    341.150   4.041 5.38e-05 ***
PULocationID265  2163.796    370.248   5.844 5.27e-09 ***
DOLocationID3     869.664    339.996   2.558 0.010549 *
DOLocationID4     913.146    108.088   8.448  < 2e-16 ***
DOLocationID7    1065.241    105.990  10.050  < 2e-16 ***
DOLocationID10   1703.133    211.081   8.069 8.06e-16 ***
...
DOLocationID119  1077.036    274.406   3.925 8.74e-05 ***
DOLocationID121  1515.904    340.850   4.447 8.80e-06 ***
DOLocationID123        NA         NA      NA       NA
DOLocationID124  2707.981    339.513   7.976 1.70e-15 ***
DOLocationID125  1123.702    105.203  10.681  < 2e-16 ***
...
DOLocationID262   933.146     96.473   9.673  < 2e-16 ***
DOLocationID263   885.520     94.578   9.363  < 2e-16 ***
DOLocationID264   960.527    108.120   8.884  < 2e-16 ***
DOLocationID265   169.068    123.391   1.370 0.170666
DayOfWeek2         44.691     15.004   2.979 0.002903 **
DayOfWeek3        100.480     14.188   7.082 1.53e-12 ***
DayOfWeek4        105.223     13.990   7.522 5.95e-14 ***
DayOfWeek5        133.312     13.775   9.678  < 2e-16 ***
DayOfWeek6         97.030     14.461   6.710 2.07e-11 ***
DayOfWeek7         57.133     14.579   3.919 8.97e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 327.2 on 8663 degrees of freedom
Multiple R-squared:  0.7128,	Adjusted R-squared:  0.7017
F-statistic:    64 on 336 and 8663 DF,  p-value: < 2.2e-16

```

There were 266 pickup/dropoff locations.  That means 265 dummy-variable
features each for pickup and dropoff.  Similarly, there 6 more dummies
for day of the week.  R's **lm** function, which **qeLin** wraps,
automatically converts factors to dummies.

So, p is not 5 after all; it's 5 + 530 + 6 = 541!

We are assuming no interactions here.  But if some pickup/dropoff
location combinations behave quite differently from others, we ought to
consider interaction terms.  These will have their own dummy variables,
hugely increasing p if we include them all.

Now, in those warning messages, the term "rank-deficient" is referring
to multicollinearity in the data, i.e. strong relations among the
features, producing a possibly unstable result.  Note that the model was
so unstable that the coefficient for DOLocationID123 turned out to be
NA.

A more subtle problem is in the warning, "7 rows removed from test set,
due to new factor levels."  By default, all **qeML** predictive
functions split the data into training and holdout sets.  Say some
pickup location had only two rows in our data, both of which happen to
be in the holdout set.  Then the **lm** function would say, "Wait, I've
never heard of these factor levels before," so **qeLin** removes them.
If many rows are removed, our predictive ability suffers.

# Desiderata

What should we look for in a "good" feature selection method?  We take
the following as goals:

* **Desired Property A:** The method should have predictive ability as a central goal.

* **Desired Property B:** The method should be reasonably easy to explain.

* **Desired Property C:** The method should produce an ordered sequence of
candidate models. 

    All the methods discussed here will have this property. 
    This is so important that we will give it its own subsection:

## Feature selection methods should produce an ordered sequence of candidate models

A nethod should produce some kind of ordered list, for instance, best
single predictor; best pair of predictors; best triplet etc.  We can
evaluate each of the p feature sets via cross-validation, then choose
the one that peforms best.  (A method may simply rank features according
to importance, but we still would assess the performance of the first
feature, the first two features together, the first three features
together and so on.)

In the *all possible subsets* (APS) approach, by contrast, one looks at all
the 2<sup>p</sup> possible feature sets, a prohibitively large number
computationally.  

Moreover, remember, we are working with randomness.  We choose holdout
sets randomly, and usually the dataset itself is considered to be a
random sample from some (real or conceptual) population.  Due to this
randomness, it's possible that some combination of features will appear
to perform well, just by accident.  The more feature sets we look at,
the greater the chance of this occurring.  This consideration really
makes APS infeasible, and even with a linear sequence of candidate
feature sets, this can be problematic with large p.

Note that some of the methods discussed below do their own internal
cross-validation, and ultimately give us the "best" feature set
according to that criterion.  Nevertheless, they still provide an
ordered feature set.

This last point is important because we may wish to use the selected
feature set sequence as input to a different ML method.  There is no
theoretical reason to believe the best feature set for one ML method is
also best for another, but as noted, feature selection is an *ad hoc*
business.

# Feature Selection Methodology Overview

## Methods based on p-values

A classic approach for linear and generalized linear models is to first
fit a full model, then retain only those features that are
"statistically significant".  In the **nyctaxi** taxi data shown above,
we would retain, for instance **PULocationID33** but not **PULocationID35**.

This would violate our Desired Property A above.  Use of p-values in
geneeral is problematic (see the "NoPVals" vignette), and this is no
exception.  Just because &beta;<sub>i</sub> is nonzero does not mean
that Feature i has strong predictive power; what if  &beta;<sub>i</sub>
is nonzero but tiny?

One could generate an ordered sequence of feature sets in the above
manner by first setting the threshold for a p-value very low, then
progressively higher  This is similar to  *stepwise regression*:  In the
*forward* version, one starts with no features, but adds them one at a
time.  At each step, one tests for the &beta; coefficient being 0.  When
no new feature is found to be "significant," the process stops, and we
use whatever features managed to make it in to the model so far.  The
*backward* version starts with a full model, and removes features one at
a time.  Though this one may seem to be an improvement, in that it does
take into account that a feature may be useful individually but not if
similar features are already in the equation, it still suffers from the
fundamental problems of p-values.

## The LASSO

Here we fit a linear model, subject to the constraint that no estimated
&beta;<sub>i</sub> is larger than &lambda;, a hyperparameter.  Starting
at 0, we increase &lambda; to ever-larger values, having the effect that
one estimated &beta;<sub>i</sub> becomes nonzero at a time.  This gives
us an ordered sequence of candidate features, as desired.

This is similar to a forward stepwise method, but NOT based on p-values;
instead, the decision of which feature to use next is based on
cross-validated mean squared prediction error.  

Here is the LASSO approach on the **nyctaxi** data:

``` r
> lassout <- qeLASSO(nyctaxi,'tripTime') 
> lassout$lambda.whichmin
[1] 51
> lassout$whenEntered
  trip_distance PULocationID.132 PULocationID.186      DayOfWeek.1
               2               29               31               31
DOLocationID.132 DOLocationID.124      DayOfWeek.5  DOLocationID.33
              33               34               34               35
DOLocationID.189 DOLocationID.225 PULocationID.213  PULocationID.76
              35               35               37               38
PULocationID.263   DOLocationID.1 DOLocationID.177      DayOfWeek.2
              38               38               38               38
PULocationID.124  DOLocationID.35  DOLocationID.36  DOLocationID.89
              39               39               39               39
DOLocationID.231 DOLocationID.263 PULocationID.239 DOLocationID.155
              39               39               40               40
 PULocationID.40  PULocationID.43 PULocationID.146 PULocationID.161
              41               41               41               41
PULocationID.230  DOLocationID.22  DOLocationID.37 DOLocationID.161
              41               41               41               41
DOLocationID.162 DOLocationID.251 PULocationID.163 PULocationID.211
              41               41               42               42
PULocationID.257  DOLocationID.49  DOLocationID.81 DOLocationID.179
              42               42               42               42
...
```

For each value of &lambda; run by the code, one or more features joined the
model.  The cross-valued mean squared prediction error reached its
minimum at the 51st &lambda;.  Along the way, **trip_distance** was the
first feature added, at the 2nd &lambda;, followed 
by PULocationID132 (29th), PULocationID186 (31st), 
DayOfWeek.1 (31st) and so o.

## Methods based on measures of feature importance

Consider the Random Forests (RF) ML method.  It has various
hyperparameters, such as maximum number of levels in a tree, and we wish
to find a "good" combination of those settings.  As a byproduct, though,
the fact that at each level RF is entering a new feature, many RF
implemntations compute some kind of *variable importance* ranking--thus
giving us our desired ordered sequence.  

We can then run RF on each feature set in the sequence, or as mentioned,
try the sequence on some other ML method.  We would then use whichever
feature set yielded the best cross-validated performance.

Here is an example with the **nyctaxi** data:

``` r
> z <- qeRFranger(nyctaxi,'tripTime')
holdout set has  1000 rows
Loading required namespace: ranger
Warning message:
In eval(tmp, parent.frame()) :
  9 rows removed from test set, due to new factor levels
> z$variable.importance
trip_distance  PULocationID  DOLocationID     DayOfWeek
   2447411690     212569502     201650058      85733194
```

So, our feature set sequence might be:

<pre>
trip_distance
trip_distance and PULocationID
trip_distance and PULocationID and DOLocationID
trip_distance and PULocationID and DOLocationID
trip_distance and PULocationID and DOLocationID and DayOfWeek
</pre>

Note that the algorithm chose the categorical variables as a whole here,
not breaking down into their dummy variable components.

Another example is  Prnciple Components Analysis (PCA).  Here our p
features are replaced by p linear combinations (the "principle
components," PCs) of the original features.  The PCs are uncorrelated.

The PCs form our new features, and are ordered by variance, largest to
smallest.  Since a variable with small variance is essentially constant
and thus has little predictive value, we take only the PCs with larger
variance.  

For the first m PCs, say, we compare the sum of their variances 
(which is also the variance of their sum) to the total variance of the
response variable Y (i.e. the variable we are trying to predict).  By
varying the proportion, we produce an ordered sequence of feature sets,
as desired.

The **qeML** function **qePCA** first finds the PCs, then applies
whatever ML method is specified by the user.  The function **qeUMAP**
does the same for UMAP, a kind of nonlinear analog of PCA.

One more measure of variable importance is *Shapley values*.  This
concept describes an attempt to use game theory to apportion credit for
a win among several players in a team; the "players" here are the
features.  While it is a clever idea, it has been the subject of much
criticism in terms of practical value.

## Feature Ordering by Conditional Independence (FOCI)

This is my personal "go to" method for feature selection.  Its
theoretical foundations are complex, but it boils down to measuring the
reduction in variance in predicting Y from X, versus predicting Y simply
from its mean.  

FOCI is implemented in a CRAN package of the same name, and **qeML**'s
**qeFOCI** function provides a convenient interface.  Example:

``` r
> w <- qeFOCI(nyctaxi,'tripTime')
> w$selectedVar
   index            names
1:     1    trip_distance
2:   202  DOLocationID.75
3:   348      DayOfWeek.1
4:    78 PULocationID.151

```

Trip distance, dropoff location 72, Mondays and pickup location 151 were
chosen, in that order, after which the algorithm decided that adding any
further features was counterproductive.  Again, we could simply take
that 4-feature set for whatever ML method we wish, or we could use the
above to set up an ordered sequence of feature sets,

<pre>
trip_distance
trip_distance and DOLocationID.75
trip_distance and DOLocationID.75 and DayOfWeek.1
trip_distance and DOLocationID.75 and DayOfWeek.1 and PULocationID.151
</pre>

running our desired ML method on each feature set, then choosing the one
with best cross-validation performance.

## Direct dimension reduction for categorical data

Rather than doing feature selection *per se*, we might transform the
data to summary variables.  We could group the pickup/dropoff locations
by ZIP Code, for instance.  

Or, we could consider only the more frequently used locations.  The qeML
function **factorToTopLevels** would help here.  We will use it to
reduce pickup location to just a few places that appear a lot in our
data; all others will be lumped together as 'other'.

We invoke the function as follows:

``` r
> factorToTopLevels(nyctaxi$PULocationID,lowCountThresh=0)
```


![alt text](LowCountThresh.png)

Ah, yes, lots of locations have small counts, less than 50 and probably
many in the single-digits range.  So, **PULocationID** is a prime
candidate for simplication.  Let's go for a cutoff of 75.

``` r
> newPUlocs <- factorToTopLevels(nyctaxi$PULocationID,lowCountThresh=75)
```

So, now we have a new, simpler version of the pickup location data.
Let's take a look at old versus new.

``` r
> head(newPUlocs,50)
 [1] 236   238   100   161   236   161   238   107   170   other 170   137  
[13] 239   143   164   164   151   239   161   100   142   107   238   43   
[25] 237   170   238   238   236   161   236   other 230   138   other 79   
[37] 186   132   other 100   234   142   151   263   236   142   142   144  
[49] 236   230  
43 Levels: 100 107 113 114 13 132 137 138 140 141 142 143 144 148 151 ... other
> head(nyctaxi$PULocationID,50)
 [1] 236 238 100 161 236 161 238 107 170 211 170 137 239 143 164 164 151 239 161
[20] 100 142 107 238 43  237 170 238 238 236 161 236 41  230 138 261 79  186 132
[39] 152 100 234 142 151 263 236 142 142 144 236 230
224 Levels: 1 3 4 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... 265
```

So for example pickup location in row 10 of our data was location 211,
but it was one of the "rare" locations, so it was recoded to "other".

How much dimension reduction did we accomplish?

``` r
> length(levels(newPUlocs))
[1] 43
```

That's much less than the original 265.  Of course, we could reduce even
further by using a larger threshold.

So, let's try out the new data:

``` r
> newDOlocs <- factorToTopLevels(nyctaxi$DOLocationID,lowCountThresh=75)
> nyctaxi$PULocationID <- newPUlocs
> nyctaxi$DOLocationID <- newDOlocs
> z <- qeLin(nyctaxi,'tripTime')
```

Dimension reduction at least got us a stable solution.  Does it predict
well?

``` r
> z$testAcc
[1] 240.5642
> z$baseAcc
[1] 430.4696
```

Mean Absolute Prediction Error is much less than what one would have by
simply guessing all cases to be the overall mean trip time.

By varying **lowCountThresh**, we could generate an ordered sequence of
feature sets, as before.

