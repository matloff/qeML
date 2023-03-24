
#  Clearing the Confusion: on the Generality of R<sup>2</sup> 

From time to time, I hear questi/concerns regarding the suitability of 
R<sup>2</sup> as a measure of predictive ability of a model.  I think
much of this stems from confusion as to what R<sup>2</sup> really
measures, especially in "nonstandard" settings.  This document aims to
clear the confusion.

# Summary

* R<sup>2</sup> is a quite valuable measure, as it has a very simple,
  intuitive definition, involving Mean Squared Prediction Error (MSPE):

  **Proportional Reduction (PropRed)**: In predicting Y from a set of
  variables X, R<sup>2</sup> is the proportional reduction in MSPE
  arising from using X to predict Y, rather than using the overall mean
  Y value as our prediction of Y.

  This is easily understood, even by those with limited stat background,
  making it an especially attractive measure.

* What happens regarding R<sup>2</sup> if we fit a linear 
  model to data in which the linearity assumption is 
  justified, but the normality and homoscedasticity 
  assumptions are not?  Answer: The PropRed property for 
  the outputted value of R<sup>2</sup> *is still valid*.

* What happens regarding R<sup>2</sup> if we fit a linear 
  model to data in which the linearity assumption is 
  not justified?  Answer: The PropRed property for the outputted
  R<sup>2</sup> value *is still valid*. 

* Is there an analog of R<sup>2</sup> for nonlinear models, say random
  forests? Answer:  Yes, in fact with the same formula.

# Linear Models, Sample vs. Population

## Setting

The linear model assumes that on the population level,

E(Y | X=t) = &beta;'t

for some constant vector &beta;.  (I am incorporating a 1 in X and t in
order to accommodate the intercept term.)  For now, we'll assume the
model holds (and of course we are in the setting of numeric Y).

It can be shown that h = &beta; minimizes

E[(Y - h'X)<sup>2</sup>]

That motivates estimating &beta; from sample data by the value of b that
minimizes

&Sum;<sub>i</sub> [(Y<sub>i</sub> - b'X<sub>i</sub>)<sup>2</sup>]

## R<sup>2</sup> as computed by, e.g. R's lm() function

The outputted value of R<sup>2</sup> is then 

R<sup>2</sup> = (SSE1 - SSE2) / SSE1

where

SSE1 = &Sum; <sub>i</sub> (Y<sub>bar</sub> - Y<sub>i</sub>)<sup>2</sup>

and 

SSE2 = &Sum; <sub>i</sub> (b'X<sub>i</sub> - Y<sub>i</sub>)<sup>2</sup>

Note that in both SSE1 and SSE2, we are summing squared prediction
errors, (Y<sub>pred</sub> - Y<sub>i</sub>)<sup>2</sup>.

The definition of PropRed might be clarified, to PropRedLin:

  **Proportional Reduction (PropRedLin)**: In predicting Y from a set of
variables X, R<sup>2</sup> is the proportional reduction in MSPE arising
from using *a linear model* with X to predict Y, rather than using the
overall mean Y value as our prediction of Y.

So we see that:

* The outputted R<sup>2</sup> value is exactly PropRedLin.

* Neither the normality nor homoscedasticity assumptions play any role.

## What if the linearity assumption is not justified?

Though the relation between X and mean Y is never exactly linear, in some
situations it is good enough.  But what if that is not the case?

If we still compute b to minimize the usual sum of squares, 

&Sum;<sub>i</sub> [(Y<sub>i</sub> - b'X<sub>i</sub>)<sup>2</sup>]

then what is b estimating?  The answer is that it is estimating whatever
value of h minimizes

E[(Y - h'X)<sup>2</sup>]

just as before.  But now &beta;'X becomes *the best-fitting linear
predictor of Y based on X*.

The key point then is that the outputted value of R<sup>2</sup>, 
i.e. (SSE1-SSE2) / SSE1 *is still as in PropRedlin*.  Nothing 
has changed.

## R<sup>2</sup> can be used unchanged in nonlinear models

Suppose we fit, say, a random forests model to our data.  We can define
R<sup>2</sup> exactly as before, needing only to update the form of our
predictor.

SSE2 = &Sum; <sub>i</sub> (rf(X<sub>i</sub>) - Y<sub>i</sub>)<sup>2</sup>

where rf(X<sub>i</sub>) is the predicted value of Y<sub>i</sub> based on
our fitted random forests model.

The interpretation of R<sup>2</sup> is then just as valid as before,
just as easy to compute, and just as easy to explain to those with
limited (if any) stat background.


