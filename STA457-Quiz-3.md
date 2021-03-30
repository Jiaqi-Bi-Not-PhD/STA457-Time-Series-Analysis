**Quiz 8**
================
Jiaqi Bi
2021/3/26

``` r
library(astsa)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
data(globtemp)
```

## Question a

``` r
plot(globtemp, main = "Original Graph for globtemp", ylab="Mean", xlab="Year")
```

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
acf2(globtemp, main="ACF and PACF for Original Graph")
```

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

    ##      [,1] [,2] [,3] [,4]  [,5] [,6] [,7] [,8]  [,9] [,10] [,11] [,12] [,13]
    ## ACF  0.91 0.86 0.84 0.82  0.79 0.76 0.74 0.72  0.68  0.66  0.62  0.60  0.57
    ## PACF 0.91 0.17 0.19 0.13 -0.07 0.02 0.03 0.02 -0.11  0.07 -0.15  0.04 -0.03
    ##      [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22]
    ## ACF   0.55  0.52  0.50  0.49  0.47  0.44  0.42  0.39  0.37
    ## PACF  0.05 -0.03  0.04  0.08 -0.07 -0.06  0.00 -0.07  0.02

Since we find a significant increasing trend in the original graph, we
will need to do the differencing using specific code `diff()`. The
difference order is 1, which means \(d=1\), but we will use the
differencing model instead for future `sarima()` command. That is, we do
not need to set \(d=1\) in the `sarima()` command. The ACF model has
linear trend as well.

## Question b

``` r
globtemp_diff <- diff(globtemp)
plot(globtemp_diff, main="Graph with Differencing", ylab = "Mean", xlab="Year")
```

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
acf2(globtemp_diff, 20, main= "ACF and PACF for Diff")
```

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

    ##       [,1]  [,2]  [,3] [,4]  [,5]  [,6]  [,7] [,8]  [,9] [,10] [,11] [,12]
    ## ACF  -0.24 -0.19 -0.08 0.20 -0.15 -0.03  0.03 0.14 -0.16  0.11 -0.05  0.00
    ## PACF -0.24 -0.26 -0.23 0.06 -0.16 -0.09 -0.05 0.07 -0.09  0.11 -0.03 -0.02
    ##      [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
    ## ACF  -0.13  0.14 -0.01 -0.08     0  0.19 -0.07  0.02
    ## PACF -0.10  0.02  0.00 -0.09     0  0.11  0.04  0.13

By looking at the differencing model’s ACF and PACF’s cut offs and tail
offs, I decide to use `ARIMA(0,0,3)`, `ARIMA(3,0,0)` and `ARIMA(3,0,3)`,
while we have defined the differencing model.

``` r
sarima(globtemp_diff, 0,0,3)
```

    ## initial  value -2.220513 
    ## iter   2 value -2.301394
    ## iter   3 value -2.309608
    ## iter   4 value -2.309941
    ## iter   5 value -2.311492
    ## iter   6 value -2.312474
    ## iter   7 value -2.312669
    ## iter   8 value -2.312672
    ## iter   9 value -2.312672
    ## iter   9 value -2.312672
    ## iter   9 value -2.312672
    ## final  value -2.312672 
    ## converged
    ## initial  value -2.311272 
    ## iter   2 value -2.311279
    ## iter   3 value -2.311281
    ## iter   4 value -2.311283
    ## iter   5 value -2.311283
    ## iter   5 value -2.311283
    ## iter   5 value -2.311283
    ## final  value -2.311283 
    ## converged

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## stats::arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, 
    ##     Q), period = S), xreg = xmean, include.mean = FALSE, transform.pars = trans, 
    ##     fixed = fixed, optim.control = list(trace = trc, REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##           ma1      ma2      ma3   xmean
    ##       -0.3760  -0.2115  -0.0464  0.0072
    ## s.e.   0.0929   0.0779   0.0865  0.0032
    ## 
    ## sigma^2 estimated as 0.009798:  log likelihood = 120.47,  aic = -230.93
    ## 
    ## $degrees_of_freedom
    ## [1] 131
    ## 
    ## $ttable
    ##       Estimate     SE t.value p.value
    ## ma1    -0.3760 0.0929 -4.0458  0.0001
    ## ma2    -0.2115 0.0779 -2.7157  0.0075
    ## ma3    -0.0464 0.0865 -0.5370  0.5922
    ## xmean   0.0072 0.0032  2.2574  0.0256
    ## 
    ## $AIC
    ## [1] -1.710615
    ## 
    ## $AICc
    ## [1] -1.708336
    ## 
    ## $BIC
    ## [1] -1.603012

For `ARIMA(0,0,3)`, the standardized residuals look like the white
noise, normal QQ plot generally lies on the blue line, two tails are in
the Confidence Interval, the ACF of Residuals generally lies within two
bounded dash lines, and p-values for Ljung Box Statistic are all above
the line. So we say this model is in the significance level. However, by
looking at p-value for `ma3`, we found it’s p-value is above 0.05, which
is not significant.

``` r
sarima(globtemp_diff,3,0,0)
```

    ## initial  value -2.215090 
    ## iter   2 value -2.289035
    ## iter   3 value -2.306884
    ## iter   4 value -2.308838
    ## iter   5 value -2.309367
    ## iter   6 value -2.309746
    ## iter   7 value -2.309749
    ## iter   7 value -2.309749
    ## iter   7 value -2.309749
    ## final  value -2.309749 
    ## converged
    ## initial  value -2.314672 
    ## iter   2 value -2.314677
    ## iter   3 value -2.314679
    ## iter   4 value -2.314682
    ## iter   4 value -2.314682
    ## iter   4 value -2.314682
    ## final  value -2.314682 
    ## converged

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## stats::arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, 
    ##     Q), period = S), xreg = xmean, include.mean = FALSE, transform.pars = trans, 
    ##     fixed = fixed, optim.control = list(trace = trc, REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3   xmean
    ##       -0.3669  -0.3421  -0.2363  0.0071
    ## s.e.   0.0842   0.0849   0.0838  0.0044
    ## 
    ## sigma^2 estimated as 0.009733:  log likelihood = 120.93,  aic = -231.85
    ## 
    ## $degrees_of_freedom
    ## [1] 131
    ## 
    ## $ttable
    ##       Estimate     SE t.value p.value
    ## ar1    -0.3669 0.0842 -4.3592  0.0000
    ## ar2    -0.3421 0.0849 -4.0319  0.0001
    ## ar3    -0.2363 0.0838 -2.8186  0.0056
    ## xmean   0.0071 0.0044  1.6168  0.1083
    ## 
    ## $AIC
    ## [1] -1.717413
    ## 
    ## $AICc
    ## [1] -1.715133
    ## 
    ## $BIC
    ## [1] -1.60981

For `ARIMA(3,0,0)`, the standardized residuals look like the white
noise, normal QQ plot generally lies on the blue line, two tails are in
the Confidence Interval, the ACF of Residuals generally lies within two
bounded dash lines, and p-values for Ljung Box Statistic are all above
the line. So we say this model is in the significance level.

``` r
sarima(globtemp_diff, 3,0,3)
```

    ## initial  value -2.215090 
    ## iter   2 value -2.305114
    ## iter   3 value -2.307864
    ## iter   4 value -2.310262
    ## iter   5 value -2.312606
    ## iter   6 value -2.313924
    ## iter   7 value -2.314038
    ## iter   8 value -2.314104
    ## iter   9 value -2.314177
    ## iter  10 value -2.314831
    ## iter  11 value -2.315131
    ## iter  12 value -2.316197
    ## iter  13 value -2.316617
    ## iter  14 value -2.317086
    ## iter  15 value -2.317312
    ## iter  16 value -2.318336
    ## iter  17 value -2.318444
    ## iter  18 value -2.318603
    ## iter  19 value -2.318981
    ## iter  20 value -2.319496
    ## iter  21 value -2.320178
    ## iter  22 value -2.320361
    ## iter  23 value -2.320587
    ## iter  24 value -2.320614
    ## iter  25 value -2.320620
    ## iter  26 value -2.320621
    ## iter  27 value -2.320622
    ## iter  28 value -2.320625
    ## iter  29 value -2.320628
    ## iter  30 value -2.320630
    ## iter  31 value -2.320630
    ## iter  32 value -2.320630
    ## iter  33 value -2.320630
    ## iter  34 value -2.320630
    ## iter  34 value -2.320630
    ## iter  34 value -2.320630
    ## final  value -2.320630 
    ## converged
    ## initial  value -2.328786 
    ## iter   2 value -2.329056
    ## iter   3 value -2.329181
    ## iter   4 value -2.329470
    ## iter   5 value -2.330024
    ## iter   6 value -2.330280
    ## iter   7 value -2.331636
    ## iter   8 value -2.332115
    ## iter   9 value -2.332351
    ## iter  10 value -2.332415
    ## iter  11 value -2.332431
    ## iter  12 value -2.332441
    ## iter  13 value -2.332461
    ## iter  14 value -2.332470
    ## iter  15 value -2.332473
    ## iter  16 value -2.332474
    ## iter  17 value -2.332475
    ## iter  18 value -2.332476
    ## iter  19 value -2.332478
    ## iter  20 value -2.332478
    ## iter  21 value -2.332478
    ## iter  22 value -2.332478
    ## iter  23 value -2.332478
    ## iter  24 value -2.332478
    ## iter  25 value -2.332479
    ## iter  25 value -2.332479
    ## iter  25 value -2.332479
    ## final  value -2.332479 
    ## converged

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## stats::arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, 
    ##     Q), period = S), xreg = xmean, include.mean = FALSE, transform.pars = trans, 
    ##     fixed = fixed, optim.control = list(trace = trc, REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3     ma1      ma2      ma3   xmean
    ##       -0.8913  -0.1106  -0.1516  0.5428  -0.4722  -0.1773  0.0071
    ## s.e.   0.3091   0.2590   0.2108  0.3073   0.1942   0.2545  0.0035
    ## 
    ## sigma^2 estimated as 0.009376:  log likelihood = 123.33,  aic = -230.66
    ## 
    ## $degrees_of_freedom
    ## [1] 128
    ## 
    ## $ttable
    ##       Estimate     SE t.value p.value
    ## ar1    -0.8913 0.3091 -2.8838  0.0046
    ## ar2    -0.1106 0.2590 -0.4270  0.6701
    ## ar3    -0.1516 0.2108 -0.7192  0.4733
    ## ma1     0.5428 0.3073  1.7667  0.0797
    ## ma2    -0.4722 0.1942 -2.4310  0.0164
    ## ma3    -0.1773 0.2545 -0.6965  0.4874
    ## xmean   0.0071 0.0035  2.0276  0.0447
    ## 
    ## $AIC
    ## [1] -1.708561
    ## 
    ## $AICc
    ## [1] -1.702029
    ## 
    ## $BIC
    ## [1] -1.536397

For `ARIMA(3,0,3)`, the standardized residuals look like the white
noise, normal QQ plot generally lies on the blue line, two tails are in
the Confidence Interval, the ACF of Residuals generally lies within two
bounded dash lines, and p-values for Ljung Box Statistic are all above
the line. So we say this model is in the significance level.

## Question c

``` r
AIC_3_0_3 <- sarima(globtemp_diff, 3,0,3, details = FALSE)$AIC
AIC_3_0_0 <- sarima(globtemp_diff, 3,0,0, details = FALSE)$AIC
AIC_0_0_3 <- sarima(globtemp_diff, 0,0,0, details = FALSE)$AIC

BIC_3_0_3 <- sarima(globtemp_diff, 3,0,3, details = FALSE)$BIC
BIC_3_0_0 <- sarima(globtemp_diff, 3,0,0, details = FALSE)$BIC
BIC_0_0_3 <- sarima(globtemp_diff, 0,0,3, details = FALSE)$BIC


df <- data.frame(
  MODEL = c("ARIMA(3,0,3)", "ARIMA(3,0,0)", "ARIMA(0,0,3)"),
  AIC_VALUE = c(AIC_3_0_3, AIC_3_0_0, AIC_0_0_3),
  BIC_VALUE = c(BIC_3_0_3, BIC_3_0_0, BIC_0_0_3)
)

formattable(df,
            align =c("c", "c", "c"),
            list(MODEL=formatter(
              "span", style = ~style(color = "black", font.weight = "bold")
            )))
```

<table class="table table-condensed">

<thead>

<tr>

<th style="text-align:center;">

MODEL

</th>

<th style="text-align:center;">

AIC\_VALUE

</th>

<th style="text-align:center;">

BIC\_VALUE

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:center;">

<span style="color: black; font-weight: bold">ARIMA(3,0,3)</span>

</td>

<td style="text-align:center;">

\-1.708561

</td>

<td style="text-align:center;">

\-1.536397

</td>

</tr>

<tr>

<td style="text-align:center;">

<span style="color: black; font-weight: bold">ARIMA(3,0,0)</span>

</td>

<td style="text-align:center;">

\-1.717413

</td>

<td style="text-align:center;">

\-1.609810

</td>

</tr>

<tr>

<td style="text-align:center;">

<span style="color: black; font-weight: bold">ARIMA(0,0,3)</span>

</td>

<td style="text-align:center;">

\-1.573519

</td>

<td style="text-align:center;">

\-1.603012

</td>

</tr>

</tbody>

</table>

Comparing the AIC and BIC values for these 3 models, `ARIMA(3,0,0)` has
lowest AIC value, and it also has the lowest BIC value. Therefore,
looking back to the graph and sarima, this model does not violate the
assumption. Hence, we say we can choose this model for our forecasting.

## Question d

``` r
sarima.for(globtemp, 3,1,0, n.ahead = 10)
```

![](STA457-Quiz-3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## $pred
    ## Time Series:
    ## Start = 2016 
    ## End = 2025 
    ## Frequency = 1 
    ##  [1] 0.8019181 0.7783942 0.7957839 0.8273691 0.8292172 0.8274488 0.8338266
    ##  [8] 0.8454812 0.8532674 0.8587421
    ## 
    ## $se
    ## Time Series:
    ## Start = 2016 
    ## End = 2025 
    ## Frequency = 1 
    ##  [1] 0.09865757 0.11676853 0.12408773 0.12994270 0.14125209 0.15166269
    ##  [7] 0.15979743 0.16691098 0.17444079 0.18193157

Using the original plot to forecast, with \(d=1\) that we used to
eliminate the trend.

## Question e

When we are choosing AR and MA, the ACF and PACF does not give us a
distinct value, but we can subjectively choose the one we think best
fit, that I chose `AR(3)` and `MA(3)`. Moreover, by looking at the
forecasting graph, the forecasts cannot properly show the unsteady part
of the forecasting while datas we generated definitely has unsteady
parts. The prediction becomes a steady trend at the last part of the
forecast.
