
# Don't use p-values, for any statistical procedure. In particular,
# don't use goodness-of-fit tests; the model is always wrong to some
# degree, and H_0 will be rejected for large enough n, even if the model
# is a good approximation to the population reality.

# Let's use a linear regression model as our example.

# Though assessing a model via a goodness-of-fit test can be very
# misleading, one cannot ignore the fact that imperfections in our model
# may create problems. If our goal is prediction, we may do well even if
# those imperfections are quite substantial, but if the goal is effect
# estimation, say through estimating linear regression coefficients, the
# model's accuracy may be quite important.

# Note that if the model is wrong -- which it always is, at least to
# some degree -- we are still estimating SOMETHING. If we fit a linear
# regression model, we are estimating the closest population linear
# model to the real E(Y|X), in the sense that our estimated coefficients
# will go to those of the closest population linear model as our sample
# size goes to infinity.  Again, if our model is not that far from
# the population reality, this may be very useful. 

# To address these issues, the code here has the following purposes.

# 1. Compute realistic standard errors of estimators, NOT assuming 
#    the model is correct.
# 
# 2. Optionally, estimate the distance between the population model
#    (the limit of our sample estimates) and the population reality.
# 
# 3. Or, optionally estimate the distance between our population model
#    and another population model. We may for instance wish to compare
#    a linear model without interaction terms to with these terms.
# 
# 4. In cases (2) and (3), we also can optionally find standard
#    errors for the distances.
# 
# All standard errors are computed using "bootstrap style" resampling.



