---
title: "Correlated Independent Variables"
author: "Phil Shea"
date: "2023-03-14"
output: 
  html_document: 
    keep_md: yes
---



# Introduction

If the independent variables are themselves correlated, it can be difficult to tease out which should be included in the regression model.   This notebook compares the Gram-Schmidt process with Principle Component Analysis (PCA).  

Briefly, Gram-Schmidt allows one to orthogonalize the data one step at a time.  It allows the researcher to select the variable considered most important and step through the variable, orthogonalizing each one at a time. PCA will simply find the best orthogonal representation of the data, but, as we will show, it can be difficult to relate the resulting regression coefficients to the original data, and, even when this difficulty is overcome, it ultimately will have little meaning. 

# `matlib` Vignette

The following was taken from the `matlib` vignette "Gram-Schmidt Orthogonalization and Regression" by Michael Friendly [link here](https://cran.r-project.org/web/packages/matlib/vignettes/gramreg.html).

___

This vignette illustrates the process of transforming a set of variables to a new set of uncorrelated (orthogonal) variables. It carries out the Gram-Schmidt process **directly** by successively projecting each successive variable on the previous ones and subtracting (taking residuals).  This is equivalent by replacing each successive variable by its residuals from a least squares regression on the previous variables.

When this method is used on the predictors in a regression problem, the resulting orthogonal variables have exactly the same `anova()` summary (based on "Type I", sequential sums of squares) as do original variables.

## Setup

We use the `class` data set, but convert the character factor `sex` to a dummy (0/1) variable `male`.


```r
library(matlib)
data(class)
class$male <- as.numeric(class$sex=="M")
```

For later use in regression, we create a variable `IQ` as a response variable

```r
class <- transform(class, 
         IQ = round(20 + height + 3*age - 0.1*weight -3*male +
                       10*rnorm( nrow( class))))
head(class)
```

```
##         sex age height weight male  IQ
## Alfred    M  14   69.0  112.5    1 113
## Alice     F  13   56.5   84.0    0 110
## Barbara   F  13   65.3   98.0    0 107
## Carol     F  14   62.8  102.5    0 119
## Henry     M  14   63.5  102.5    1 111
## James     M  12   57.3   83.0    1  98
```

Reorder the predictors we want, forming a numeric matrix, `X`.

```r
X <- as.matrix(class[,c(3,4,2,5)])
head(X)
```

```
##         height weight age male
## Alfred    69.0  112.5  14    1
## Alice     56.5   84.0  13    0
## Barbara   65.3   98.0  13    0
## Carol     62.8  102.5  14    0
## Henry     63.5  102.5  14    1
## James     57.3   83.0  12    1
```

## Orthogonalization by projections

The Gram-Schmidt process treats the variables in a given order, according to the columns in `X`. We start with a new matrix `Z` consisting of `X[,1]`. Then, find a new variable `Z[,2]` orthogonal to `Z[,1]` by subtracting the projection of `X[,2]` on `Z[,1]`.


```r
Z <- cbind(X[,1], 0, 0, 0)
Z[,2] <- X[,2] - Proj(X[,2], Z[,1])
crossprod(Z[,1], Z[,2])     # verify orthogonality
```

```
##           [,1]
## [1,] 7.276e-12
```
Continue in the same way, subtracting the projections of `X[,3]` on the previous columns, and so forth

```r
Z[,3] <- X[,3] - Proj(X[,3], Z[,1]) - Proj(X[,3], Z[,2]) 
Z[,4] <- X[,4] - Proj(X[,4], Z[,1]) - Proj(X[,4], Z[,2]) - Proj(X[,4], Z[,3])
```
Note that if any column of `X` is a linear combination of the previous columns, the corresponding column of `Z` will be all zeros.

These computations are similar to the following set of linear regressions:

```r
z2 <- residuals(lm(X[,2] ~ X[,1]), type="response")
z3 <- residuals(lm(X[,3] ~ X[,1:2]), type="response")
z4 <- residuals(lm(X[,4] ~ X[,1:3]), type="response")
```

The columns of `Z` are now orthogonal, but not of unit length,

```r
zapsmall(crossprod(Z))     # check orthogonality
```

```
##       [,1] [,2] [,3] [,4]
## [1,] 57888    0    0    0
## [2,]     0 3249    0    0
## [3,]     0    0    7    0
## [4,]     0    0    0    2
```

We make standardize column to unit length, giving `Z` as an **orthonormal** matrix, such that $Z' Z = I$.

```r
Z <- Z %*% diag(1 / len(Z))    # make each column unit length
zapsmall( crossprod(Z))         # check orthonormal
```

```
##      [,1] [,2] [,3] [,4]
## [1,]    1    0    0    0
## [2,]    0    1    0    0
## [3,]    0    0    1    0
## [4,]    0    0    0    1
```

```r
colnames(Z) <- colnames(X)
```

### Relationship to QR factorization
The QR method uses essentially the same process, factoring the matrix $\mathbf{X}$ as $\mathbf{X = Q R}$, where $\mathbf{Q}$ is the orthonormal matrix corresponding to `Z` and $\mathbf{R}$ is an upper triangular matrix. However, the signs of the columns of $\mathbf{Q}$ are arbitrary, and `QR()` returns `QR(X)$Q` with signs reversed, compared to `Z`.


```r
# same result as QR(X)$Q, but with signs reversed
head(Z, 5)
```

```
##         height   weight     age     male
## Alfred  0.2868  0.07545 -0.3687  0.12456
## Alice   0.2348 -0.08067  0.3569 -0.02177
## Barbara 0.2714 -0.07715 -0.3862 -0.45170
## Carol   0.2610  0.07058  0.1559 -0.20548
## Henry   0.2639  0.05132  0.1047  0.40538
```

```r
head(-QR(X)$Q, 5)
```

```
##        [,1]     [,2]    [,3]     [,4]
## [1,] 0.2868  0.07545 -0.3687  0.12456
## [2,] 0.2348 -0.08067  0.3569 -0.02177
## [3,] 0.2714 -0.07715 -0.3862 -0.45170
## [4,] 0.2610  0.07058  0.1559 -0.20548
## [5,] 0.2639  0.05132  0.1047  0.40538
```

```r
all.equal( unname(Z), -QR(X)$Q )
```

```
## [1] TRUE
```

## Regression with X and Z

We carry out two regressions of `IQ` on the variables in `X` and in `Z`. These are equivalent, in the sense that

- The $R^2$ and MSE are the same in both models
- Residuals are the same
- The Type I tests given by `anova()` are the same.


```r
class2 <- data.frame(Z, IQ=class$IQ)
```

Regression of IQ on the original variables in `X`

```r
mod1 <- lm(IQ ~ height + weight + age + male, data=class)
anova(mod1)
```

```
## Analysis of Variance Table
## 
## Response: IQ
##           Df Sum Sq Mean Sq F value Pr(>F)  
## height     1    171     171    2.33  0.158  
## weight     1      8       8    0.11  0.751  
## age        1    437     437    5.95  0.035 *
## male       1      2       2    0.03  0.872  
## Residuals 10    734      73                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
Regression of IQ on the orthogonalized variables in `Z`

```r
mod2 <- lm(IQ ~ height + weight + age + male, data=class2)
anova(mod2)
```

```
## Analysis of Variance Table
## 
## Response: IQ
##           Df Sum Sq Mean Sq F value Pr(>F)  
## height     1    171     171    2.33  0.158  
## weight     1      8       8    0.11  0.751  
## age        1    437     437    5.95  0.035 *
## male       1      2       2    0.03  0.872  
## Residuals 10    734      73                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

This illustrates that `anova()` tests for linear models are *sequential* tests.  They test hypotheses about the extra contribution of each variable over and above all previous ones, in a given order.  These usually do not make substantive sense, except in testing ordered ("hierarchical") models.

___

*This ends the original Vignette as extracted on 2/13/23.*

## Intepretation

As a reminder, IQ was created by the following:

$$
IQ = {\tt round}(20 + h + 3 a - 0.1 w -3 m + 10 \epsilon)
$$

where $\epsilon$ is a normal (0, 1) random variable and $h$, $a$, $w$, and $m$ are `height`, `age`, `weight`, and `male`, respectively.  The fitted values aren't even close to the creating equations.


```r
summary(mod1)
```

```
## 
## Call:
## lm(formula = IQ ~ height + weight + age + male, data = class)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.497  -3.392   0.281   3.571  10.660 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   20.296     43.197    0.47    0.649  
## height         0.127      0.995    0.13    0.901  
## weight        -0.417      0.285   -1.46    0.174  
## age            9.153      4.128    2.22    0.051 .
## male           0.908      5.495    0.17    0.872  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.57 on 10 degrees of freedom
## Multiple R-squared:  0.457,	Adjusted R-squared:  0.24 
## F-statistic:  2.1 on 4 and 10 DF,  p-value: 0.155
```
This is primarily due to the noise in the data combined with the co-linearity of the predictors as can be seen below. 


```r
plot(class[,c(2,3,4,6)])
```

![Data Plot](Regression_files/figure-html/dataplot-1.png)

```r
cor(X)
```

```
##        height weight    age   male
## height 1.0000 0.8874 0.8498 0.3118
## weight 0.8874 1.0000 0.8782 0.3465
## age    0.8498 0.8782 1.0000 0.1008
## male   0.3118 0.3465 0.1008 1.0000
```
`height` is highly correlated with both `weight` and `age`, as might be expected.  Both `age` and `weight` are highly correlated too.  This causes the random variations (which are rather large) to drive the correlated coefficients to unusual values.  Since they are correlated, one coefficient may get a large value and the other a small value and yet we still get a good correlation coefficient.  

Both the `anova` and the coefficients indicate that neither `weight` nor `male` are good predictors, so let's eliminate those.


```r
mod1.2 <- lm(IQ ~ height + age, data=class)
anova(mod1.2)
```

```
## Analysis of Variance Table
## 
## Response: IQ
##           Df Sum Sq Mean Sq F value Pr(>F)  
## height     1    171   170.9    2.26  0.159  
## age        1    274   273.7    3.62  0.081 .
## Residuals 12    907    75.6                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod1.2)
```

```
## 
## Call:
## lm(formula = IQ ~ height + age, data = class)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -16.891  -3.008   0.837   4.241  12.644 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   72.424     26.972    2.69    0.020 *
## height        -0.679      0.823   -0.82    0.426  
## age            6.004      3.155    1.90    0.081 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.69 on 12 degrees of freedom
## Multiple R-squared:  0.329,	Adjusted R-squared:  0.217 
## F-statistic: 2.94 on 2 and 12 DF,  p-value: 0.0914
```

While `anova` indicates that both variables are needed and somewhat significant, the estimated `height` coefficient is not significant.  We know, of course, that it should be positive unity.  Note that while the r-squared is lower, the second fit is actually better.


```r
AIC(mod1, mod1.2)
```

```
##        df   AIC
## mod1    6 112.9
## mod1.2  4 112.1
```

# Principle Component Analysis (PCA)

PCA can further quantify this idea of finding which components are significant by rotating them optimally.


```r
(pr <- prcomp(class[,c(2,3,4,5)], scale=TRUE))
```

```
## Standard deviations (1, .., p=4):
## [1] 1.6878 0.9678 0.3589 0.2928
## 
## Rotation (n x k) = (4 x 4):
##            PC1      PC2     PC3     PC4
## age    -0.5451  0.31250 -0.4860  0.6075
## height -0.5652  0.05861  0.8148  0.1146
## weight -0.5738  0.02953 -0.2927 -0.7643
## male   -0.2326 -0.94765 -0.1190  0.1836
```

```r
summary( pr)
```

```
## Importance of components:
##                          PC1   PC2    PC3    PC4
## Standard deviation     1.688 0.968 0.3589 0.2928
## Proportion of Variance 0.712 0.234 0.0322 0.0214
## Cumulative Proportion  0.712 0.946 0.9786 1.0000
```

```r
plot(pr)
```

![](Regression_files/figure-html/prcomp-1.png)<!-- -->

The cumulative proportion line shows that the first two principle components (PC) contains most (95%) of the variance of the data.  The other two PCs seem redundant. We'll fit the whole lot though.


```r
class.PCA <- cbind( class, pr$x) # Make new dataframe including PCs.
mod3 <- lm( IQ ~ PC1 + PC2 + PC3 + PC4, data=class.PCA)
anova( mod3)
```

```
## Analysis of Variance Table
## 
## Response: IQ
##           Df Sum Sq Mean Sq F value Pr(>F)  
## PC1        1    186   186.3    2.54  0.142  
## PC2        1    145   145.2    1.98  0.190  
## PC3        1     16    16.4    0.22  0.647  
## PC4        1    270   269.9    3.68  0.084 .
## Residuals 10    734    73.4                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary( mod3)
```

```
## 
## Call:
## lm(formula = IQ ~ PC1 + PC2 + PC3 + PC4, data = class.PCA)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.497  -3.392   0.281   3.571  10.660 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   110.47       2.21   49.94  2.5e-13 ***
## PC1            -2.16       1.36   -1.59    0.142    
## PC2             3.33       2.37    1.41    0.190    
## PC3            -3.01       6.38   -0.47    0.647    
## PC4            14.99       7.82    1.92    0.084 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.57 on 10 degrees of freedom
## Multiple R-squared:  0.457,	Adjusted R-squared:  0.24 
## F-statistic:  2.1 on 4 and 10 DF,  p-value: 0.155
```

Now that we have all of the useful info in one component, we can see that there is very little correlation. 


```r
plot(IQ~PC1, data=class.PCA)
abline(mod3)
```

![](Regression_files/figure-html/PC1Plot-1.png)<!-- -->


```r
plot( class.PCA[,c(6:10)])
```

![](Regression_files/figure-html/plot.PCA-1.png)<!-- -->

## Transforming New Variable to Principle Components

The Gram-Schmidt process is nice because it builds on individual observation vectors.  If you pick a good starting vector, you can interpret the results directly.  The PCs do not provide a direct way to interpret their meaning. 

The following is a simplified version of that found in [Principal Component Analysis in R: prcomp vs princomp](http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp): first `scale` and then right-multiply the rotation matrix. The `sum` shows that the conversion of the original data to the PCs are identical to those returned by `prcomp`.


```r
# The R scale function works with the prcomp results directly.
# Followed by a simple dot product with the rotation element.

new.coord <- scale(class[,2:5], center = pr$center, scale = pr$scale) %*%
   pr$rotation

sum( new.coord - pr$x)
```

```
## [1] 0
```

### Converting PC Coefficients to Original Data

While one can always convert the coordinates for a new sample to PCs as shown above, the coefficients of the PCs are difficult to interpret in any meaningful way.  We will ignore, for the moment, that one should never do this.  There will be a lot of detailed data wrangling required.

The `scale` function does the following: $(\vec x - \vec c) \mathbf S^{-1}$, where $\vec x$ is the original measurement vector, $\vec c$ is the center element vector, $\mathbf S$ is a diagonal matrix of the scale elements (we need to divide each element of $\vec y$ by the scale factor for that column).

The PCs are created by 

$$
\vec p = (\vec x - \vec c) \mathbf S^{-1}  \mathbf R
$$
where $\vec p$ is the PC vector, and $\mathbf R$ is the rotation matrix returned by `prcomp`. In order to operate on the rows, we use `sweep` with a `margin` of `2` (`1` operates on rows, `2` columns).  The reciprocal of the diagonal elements forms the inverse of the matrix $\mathbf S$. Casting the selected elements of the data frame as a matrix, the rows form a series of $\vec x$ as row vectors, and we can execute the following transformation and compare it to the PCs.


```r
x <- as.matrix(class[,2:5])
new.coord2 <- sweep( x, 2, pr$center) %*% 
   diag(1/pr$scale) %*% pr$rotation
#%*% 
sum( (new.coord2 - pr$x)^2)
```

```
## [1] 3.374e-31
```

We can also use the `scale` function again to execute $(\vec x - \vec c) \mathbf S^{-1}$ (and it appears to be more accurate):


```r
new.coord3 <- scale( x, center=pr$center, scale=pr$scale) %*% pr$rotation
sum( (new.coord3 - pr$x)^2)
```

```
## [1] 0
```

If we have a PC vector $\vec p$, we can solve for $\vec x$ via

$$
\vec x = \vec c +   \vec p \mathbf R^{-1}\mathbf S 
$$

This is demonstrated below (and we cannot use `scale` because of the order of operations).


```r
old.coord <- sweep( pr$x %*% solve( pr$rotation) %*% diag( pr$scale), 
                    2, -pr$center)

sum(x - old.coord)
```

```
## [1] -6.939e-15
```

A regression returns coefficients which may be used to predict a mean response to a new observation.  The coefficients returned from a PCA must be applied to the PCs as follows:

$$
y = \vec \alpha_p \cdot \vec p'
$$

where $\alpha_p$ is a vector of the coefficients for the PCs, and $\vec p'$ is the PC vector augmented with a leading one (to catch the constant).  We have to construct the vector in the same order as the coefficients.  The below code snippet will show four methods of getting the same answer.


```r
(alph1 <- coef( mod1))
```

```
## (Intercept)      height      weight         age        male 
##     20.2961      0.1271     -0.4170      9.1535      0.9079
```

```r
# first element
x <- as.numeric( c(1, class[1,c("height", "weight", "age", "male")])) 

# dot product with original data.
x %*% coef( mod1)
```

```
##       [,1]
## [1,] 111.2
```

```r
# R predict function with original data.
predict( mod1, newdata=class[1, 2:5])
```

```
## Alfred 
##  111.2
```

```r
# dot product with PCs.
coef( mod3) %*% c( 1, pr$x[1,])
```

```
##       [,1]
## [1,] 111.2
```

```r
# R predict function with PCs.
predict( mod3, newdata=as.data.frame( pr$x)[1,])
```

```
## Alfred 
##  111.2
```

We now wish to transform the $m=n+1$ length $\vec \alpha_p$ coefficients into ones we can relate to the original data $y=\vec \alpha_x \cdot \vec x'$.  Before we do this, we must deal with the transformation into the $n+1$ length *primed* form of the vectors.  We need to do this to represent algebraically what we are doing.  

### Matrix Algebra

The transform is given by the following:

$$
\eqalign {
\vec v' &=  \vec v \mathbf P + \vec o \\
   &=  \vec v\begin{bmatrix}0 & 1 & 0  & 0 & \cdots & 0 & 0 \\
0 & 0 & 1 & 0 & \cdots & 0 & 0 \\
0 & 0 & 0 & 1 & \cdots & 0 & 0 \\
 \vdots &  \vdots &  \vdots  &  \vdots & \ddots & \vdots & \vdots \\
 0 & 0 & 0 & 0 &\cdots & 1 & 0 \\
 0 & 0 & 0 & 0 & \cdots & 0 & 1 \end{bmatrix} 
 +
  \begin{bmatrix} 1 \\ 0 \\ 0 \\\vdots \\ 0 \\ 0\end{bmatrix}^\intercal
}
$$

where $\mathbf\ P$ is a simple $n \times n + 1$ permutation matrix and $\vec o$ is an $n +1$ length row vector with one in the new first column and zeros elswhere.  Finally, if we did not want to keep all the PCs, we can simply truncate the bottom of $\mathbf P$ and $\vec c$ to have fewer rows.  For example, if only two PCs were desired, keep only the first *three* rows of $\mathbf P$ (one for the constant, and a coefficient for each PC).

Here is the algebra to get coefficients of $\vec x$:

$$
\eqalign {
\vec \alpha_x \cdot \vec x' &=  \vec \alpha_p \cdot \vec p' \\
   &= \vec \alpha_p \cdot (\vec p \mathbf P + \vec o) \\
   &= \vec \alpha_p \cdot 
         \left[ \left( \vec x - \vec c \mathbf S^{-1} \mathbf R\right) \mathbf P 
         + \vec o\right] \\
   &= \vec \alpha_p \cdot 
   \left( \vec x \mathbf S^{-1} \mathbf R \mathbf P \right) - 
      \vec \alpha_p \cdot 
      \left( \vec c \mathbf S^{-1} \mathbf R \mathbf P \right) +
      \vec \alpha_p \cdot \vec o\ \\
   &= \vec \alpha_p \cdot \left( \vec x \mathbf H \right) - 
      \vec \alpha_p \cdot 
      \left( \vec c \mathbf H \right) + \vec \alpha_p \cdot \vec o 
}
$$

where $\mathbf H = \mathbf S^{-1} \mathbf R \mathbf P$. These products for vector sums as follows (assuming the dot product between a row and column vector is simply between two vectors, an assumption which applies in this case):

$$
\eqalign {
 \vec \alpha_p \cdot (\vec x \mathbf H) &= 
   \begin{bmatrix} \alpha_1 \\ \alpha_2 \\ \vdots \\ \alpha_m \end{bmatrix}
      \cdot \begin{bmatrix} x_1 \\ x_2 \\ \vdots \\ x_n \end{bmatrix}^\intercal 
      \begin{bmatrix} 0 & h_{12} & \cdots & h_{1m} \\
      0 & h_{22} & \cdots & h_{2m} \\
      \vdots &  \vdots & \ddots & \vdots \\
      0 & h_{n2} & \cdots & h_{nm} \end{bmatrix} \\
 &= \begin{bmatrix} \alpha_1 \\ \alpha_2 \\ \vdots \\ \alpha_m \end{bmatrix} \cdot 
   \begin{bmatrix} 0 \\ \sum_{i=1}^n x_i h_{i2} \\
   \vdots \\ \sum_{i=1}^n x_i h_{im} \end{bmatrix} \\
 &= \sum_{j=2}^m \alpha_j \left( \sum_{i=1}^n x_i h_{ij} \right) \\
 &= \sum_{i=1}^n x_i \left( \sum_{j=2}^m \alpha_j h_{ij} \right) \\
 &= \vec x \cdot \vec \beta_x
}
$$

The coefficients sought are given by $\beta_i = \sum_{j=2}^m \alpha_j h_{ij}$, which is the coefficient as a row vector $\vec \alpha_p$ multiplying the matrix $\mathbf H$

### Resulting Transform

The desired coefficients of each original $x_i$ is given by the $\beta_i$ above.  With the PC coefficients $\alpha_i$, the rotation matrix $\mathbf R$, and the scale matrix $\mathbf S$, we compute $\mathbf H' = \mathbf S^{-1} \mathbf R$ and form the vector $\beta_x$ whose elements are given by $\beta_j = \sum_{i=1}^m \alpha_i h_{ij}$.


```r
(H <- t( diag( 1 / pr$scale) %*% pr$rotation))
```

```
##        [,1]     [,2]      [,3]    [,4]
## PC1 -0.3901 -0.10550 -0.025898 -0.4587
## PC2  0.2237  0.01094  0.001333 -1.8688
## PC3 -0.3478  0.15209 -0.013211 -0.2347
## PC4  0.4347  0.02138 -0.034494  0.3620
```

```r
beta <- coef(mod3)[2:5] %*% H
colnames( beta) <- rownames( pr$rotation) 
beta
```

```
##        age height weight   male
## [1,] 9.153 0.1271 -0.417 0.9079
```

```r
coef(mod1)
```

```
## (Intercept)      height      weight         age        male 
##     20.2961      0.1271     -0.4170      9.1535      0.9079
```

It can be seen that these are the same as those estimated directly from the data.  This is because we used all of the PCs, which is usually not justified (and why we are using principle components in the first place).  The folly of this can be seen if we use only a few of the PCs.


```r
mod4 <- lm( IQ ~ PC1 + PC2, data=class.PCA)
anova( mod4)
```

```
## Analysis of Variance Table
## 
## Response: IQ
##           Df Sum Sq Mean Sq F value Pr(>F)
## PC1        1    186     186    2.19   0.16
## PC2        1    145     145    1.71   0.22
## Residuals 12   1020      85
```

```r
summary( mod4)
```

```
## 
## Call:
## lm(formula = IQ ~ PC1 + PC2, data = class.PCA)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -22.18  -3.34   1.35   4.27  12.71 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   110.47       2.38   46.40  6.6e-15 ***
## PC1            -2.16       1.46   -1.48     0.16    
## PC2             3.33       2.55    1.31     0.22    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.22 on 12 degrees of freedom
## Multiple R-squared:  0.245,	Adjusted R-squared:  0.119 
## F-statistic: 1.95 on 2 and 12 DF,  p-value: 0.185
```

```r
AIC( mod3, mod4)
```

```
##      df   AIC
## mod3  6 112.9
## mod4  4 113.9
```

The higher AIC for `mod4` indicates it is a worse fit (indeed, nothing will fit this data well; it is too noisy), but we can create coefficients with the original data regardless.  To accomplish this, we truncate the bottom rows of `H`.


```r
beta2 <- coef(mod4)[2:3] %*% H[1:2,]
colnames( beta2) <- rownames( pr$rotation) 
beta2
```

```
##        age height weight   male
## [1,] 1.587 0.2644 0.0604 -5.227
```

```r
coef(mod1)
```

```
## (Intercept)      height      weight         age        male 
##     20.2961      0.1271     -0.4170      9.1535      0.9079
```

These answers are, of course, pretty meaningless.  Only two slopes were estimated, but we get a *reading* for what they might have been.

A final observation: a seed was used so that definitive statements could be made about the calculations.  Experience has shown that this is a particularly bad set of observed IQs.  The noise term has a variance of 100, and the age term (with a factor of three in the IQ equation) has a variance of 258.  With only 15 observation, this is a very noisy data set.
