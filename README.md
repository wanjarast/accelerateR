
<!-- README.md is generated from README.Rmd. Please edit that file -->
accelerateR
===========

accelerateR offers a framework for acceleration data analysis. It was developed for data recoreded in high sample frequency bursts. The starting point would be accelerationdata that was stored in the [movebank](https://www.movebank.org). The downloaded data can be transformed with this package to become usable in R which also creates the correct data structure for all functions of accelerateR. The most important function sum.data will calculate summary statistics to prepare the acceleration data to be used for any maschine learning approach.

Installation
------------

You can install accelerateR from github with:

``` r
# install.packages("devtools")
devtools::install_github("wanjarast/accelerateR")
```

Functions
---------

Transform the moveank data into a structure, that is suitable for R

``` r
move.trans(data =  ... , time = "..." , burst = ... , naxes = 3)
```

Check for data integrety. All burst are supposed to have the same amount of measurments. This function will report all bursts were this asumtion is violated. The output can be used to filter aou these bursts.

``` r
count_test(data = ... , burstcount = ... , time = "timestamp")
```

Calculate summary statistics for every burst separatly. This works for data with one, two or three axes. A number of summary statistic parameters can be choosen to be calculated which can be specified with the stats argument. It is possible to shrink the measurment intevall to only analyse smaller parts of every bursts through specification of the windowstart and burstcount argument.

Extra information can be added to the output data frame.

``` r
sum.data(data = ... , time = "..." , windowstart = 1 , burstcount = NULL, 
         x = "..." , y = NULL , z = NULL , stats , 
        IntDur = NA , ID = NA , Tag.ID = NA , frequency = NA , sex = NA)
```

Creates a confusion matrix for a model evaluation. Also provides the precission and recall (after Bidder et al. 2014).

``` r
pre_metrics(predicted = ... , expected = ... , uncertain = NULL)
```

Plots the absence or present of one or more target behaviours in an actogram style.

``` r
acto(data = ... , time = "..." , date = "..." , behaviour = "..." , target.bev = ... , daily = FALSE)
```

Plots the Overall dynamic body acceleration (ODBA) in an actogram style.

``` r
acto.odba(data = ... , time = "..." , ODBA = "ODBA" , cutoff = 1 , night.shift = FALSE)
```
