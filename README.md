
<!-- README.md is generated from README.Rmd. Please edit that file -->
The MonteCarlo Package
======================

The MonteCarlo package allows to create simulation studies and to summarize their results in LaTeX tables quickly and easily. In the following, an example for the use of the MonteCarlo package is presented. In addition to that, there is a brief discussion of the more advanced features of the package. Further information is included in the package vignette.

There are only two main functions in the package:

1.  *MonteCarlo()* runs a simulation study for a user defined parameter grid. It handles the generation of loops over these parameter grid and parallelizes the computation on a user specified number of CPU units.

2.  *MakeTable()* creates LaTeX tables from the output of *MonteCarlo()*. It stacks high dimensional output arrays into tables with a user specified ordering of rows and columns.

To run a simulation study, the user has to nest both - the generation of a sample and the calculation of the desired statistics from this sample - in a single function. This function is passed to *MonteCarlo()*. No additional programming is required.

The idea behind this approach is to allow the user full control and flexibility with regard to the design of the Monte Carlo experiment. It also makes *MonteCarlo()* very versatile. Finally, it is very intuitive. The user formulates his experiment as if he/she was only interested in a single draw.

A First Example
---------------

The best way to get working with the MonteCarlo package is to look at an example. Suppose we want to evaluate the performance of a standard t-test for the hypothesis that the mean is equal to zero. We are interested to see how the size and power of the test change with the sample size (*n*), the distance from the null hypothesis (*loc* for location) and the standard deviation of the distribution (*scale*). The sample is generated from a normal distribution.

To conduct this analysis, we proceed as follows. First, we load the MonteCarlo package.

``` r
library(MonteCarlo)
```

Then define the following function.

``` r
#########################################
##      Example: t-test

# Define function that generates data and applies the method of interest

ttest<-function(n,loc,scale){
  
  # generate sample:
    sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
    stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision:
    decision<-abs(stat)>1.96
  
  # return result:
    return(list("decision"=decision))
}
```

As discussed above, *ttest()* is formulated in a way as if we only want to generate a single test decision. The arguments of the function are the parameters we are interested in. *ttest()* carries out 4 steps:

1.  Draw a sample of *n* observations from a normal distribution with mean *loc* and standard deviation *scale*.
2.  Calculate the t-statistic.
3.  Determine the test decision.
4.  Return the desired result in form of a list.

We then define the combinations of parameters that we are interested in and collect them in a list. The elements of the lists must have the same names as the parameters for which we want to supply grids.

``` r
# define parameter grid:

  n_grid<-c(50,100,250,500)
  loc_grid<-seq(0,1,0.2)
  scale_grid<-c(1,2)

# collect parameter grids in list:
  param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
```

To run the simulation, the function *ttest()* and the parameter grid (*param\_list*) are passed to *MonteCarlo()*, together with the desired number of Monte Carlo repetitions (*nrep=1000*).

``` r
# run simulation:

  MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)
```

There is no further coding required. All the mechanics of the Monte Carlo experiment are handled by the *MonteCarlo()* function.

Calling summary produces a short information on the simulation.

``` r
  summary(MC_result)
```

    ## Simulation of function: 
    ## 
    ## function(n,loc,scale){
    ##   
    ##   # generate sample:
    ##     sample<-rnorm(n, loc, scale)
    ##   
    ##   # calculate test statistic:
    ##     stat<-sqrt(n)*mean(sample)/sd(sample)
    ##   
    ##   # get test decision:
    ##     decision<-abs(stat)>1.96
    ##   
    ##   # return result:
    ##     return(list("decision"=decision))
    ## }
    ## 
    ## Required time: 12.33 secs for nrep = 1000  repetitions on 1 CPUs 
    ## 
    ## Parameter grid: 
    ## 
    ##      n : 50 100 250 500 
    ##    loc : 0 0.2 0.4 0.6 0.8 1 
    ##  scale : 1 2 
    ## 
    ##  
    ## 1 output arrays of dimensions: 4 6 2 1000

As one can see from the summary, the simulation results are stored in an array of dimension `c(4,6,2,1000)`, where the Monte Carlo repetitions are collected in the last dimension of the array.

To summarize the results in a reasonable way and to include them as a table in a paper or report, we have to represent them in a matrix. This is handled by the *MakeTable()* function that stacks the submatrices collected in the array in the rows and columns of a matrix and prints the result in the form of code to generate a LaTeX table.

To determine in which order the results are stacked in rows and columns, we supply the function arguments *rows* and *cols*. These are vectors of the names of the parameters in the order in which we want them to appear in the table (sorted from the inside to the outside).

``` r
# generate table:

  MakeTable(output=MC_result, rows="n", cols=c("loc","scale"), digits=2, include_meta=FALSE)
```

    ## \begin{table}[h]
    ## \centering
    ## \resizebox{ 1 \textwidth}{!}{%
    ## \begin{tabular}{ rrrrrrrrrrrrrrr }
    ## \hline\hline\\\\
    ##  scale && \multicolumn{ 6 }{c}{ 1 } &  & \multicolumn{ 6 }{c}{ 2 } \\ 
    ## n/loc &  & 0 & 0.2 & 0.4 & 0.6 & 0.8 & 1 &  & 0 & 0.2 & 0.4 & 0.6 & 0.8 & 1 \\ 
    ##  &  &  &  &  &  &  &  &  &  &  &  &  &  &  \\ 
    ## 50 &  & 0.06 & 0.30 & 0.83 & 0.98 & 1.00 & 1.00 &  & 0.05 & 0.11 & 0.28 & 0.55 & 0.79 & 0.95 \\ 
    ## 100 &  & 0.05 & 0.51 & 0.98 & 1.00 & 1.00 & 1.00 &  & 0.05 & 0.16 & 0.54 & 0.84 & 0.97 & 1.00 \\ 
    ## 250 &  & 0.06 & 0.89 & 1.00 & 1.00 & 1.00 & 1.00 &  & 0.04 & 0.34 & 0.91 & 1.00 & 1.00 & 1.00 \\ 
    ## 500 &  & 0.06 & 1.00 & 1.00 & 1.00 & 1.00 & 1.00 &  & 0.04 & 0.58 & 0.99 & 1.00 & 1.00 & 1.00 \\ 
    ## \\
    ## \\
    ## \hline\hline
    ## \end{tabular}%
    ## }
    ## \caption{ decision  }
    ## \end{table}

To change the ordering, just change the vectors *rows* and *cols*.

``` r
# generate table:

  MakeTable(output=MC_result, rows=c("n","scale"), cols="loc", digits=2, include_meta=FALSE)
```

    ## \begin{table}[h]
    ## \centering
    ## \resizebox{ 1 \textwidth}{!}{%
    ## \begin{tabular}{ rrrrrrrrr }
    ## \hline\hline\\\\
    ## scale & n/loc &  & 0 & 0.2 & 0.4 & 0.6 & 0.8 & 1 \\ 
    ##  &  &  &  &  &  &  &  &  \\ 
    ## \multirow{ 4 }{*}{ 1 } & 50 &  & 0.06 & 0.30 & 0.83 & 0.98 & 1.00 & 1.00 \\ 
    ##  & 100 &  & 0.05 & 0.51 & 0.98 & 1.00 & 1.00 & 1.00 \\ 
    ##  & 250 &  & 0.06 & 0.89 & 1.00 & 1.00 & 1.00 & 1.00 \\ 
    ##  & 500 &  & 0.06 & 1.00 & 1.00 & 1.00 & 1.00 & 1.00 \\ 
    ##  &  &  &  &  &  &  &  &  \\ 
    ## \multirow{ 4 }{*}{ 2 } & 50 &  & 0.05 & 0.11 & 0.28 & 0.55 & 0.79 & 0.95 \\ 
    ##  & 100 &  & 0.05 & 0.16 & 0.54 & 0.84 & 0.97 & 1.00 \\ 
    ##  & 250 &  & 0.04 & 0.34 & 0.91 & 1.00 & 1.00 & 1.00 \\ 
    ##  & 500 &  & 0.04 & 0.58 & 0.99 & 1.00 & 1.00 & 1.00 \\ 
    ## \\
    ## \\
    ## \hline\hline
    ## \end{tabular}%
    ## }
    ## \caption{ decision  }
    ## \end{table}

Now we can simply copy the code and add it to our paper, report or presentation. That is all. The user can focus on the design of the experiment he is interested in and the interpretation of the results.

Further Functionality of the Package
====================================

The MonteCarlo package provides several options to modify the behavior of *MonteCarlo()* and the appearance of tables generated by *MakeTable()*. To give a short overview, the tables below contain information from the help files about the function parameters.

The MonteCarlo() Function
-------------------------

As can be seen from the table below, the *MonteCarlo()* function has a number of optional arguments. The most important ones among them are *export\_also* that is needed to export datasets if these are required in a parallezied simulation <!--,*debug* that turns on the debug mode--> and *time\_n\_test* that produces an estimate of the time required for the desired simulation.

<table style="width:83%;">
<colgroup>
<col width="18%" />
<col width="65%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Argument</th>
<th align="left">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">func</td>
<td align="left">The function to be evaluated.</td>
</tr>
<tr class="even">
<td align="center">nrep</td>
<td align="left">An integer that specifies the desired number of Monte Carlo repetitions.</td>
</tr>
<tr class="odd">
<td align="center">param_list</td>
<td align="left">A list whose components are named after the parameters of func and each component is a vector containing the desired grid values for that parameter</td>
</tr>
<tr class="even">
<td align="center">ncpus</td>
<td align="left">An integer specifying the number of cpus to be used. Default is ncpus=1. For ncpus&gt;1 the simulation is parallized automatically using ncpus CPU units.</td>
</tr>
<tr class="odd">
<td align="center">max_grid</td>
<td align="left">Integer that specifies for which grid size to throw an error, if grid becomes to large. Default is max_grid=1000.</td>
</tr>
<tr class="even">
<td align="center">time_n_test</td>
<td align="left">Boolean that specifies whether the required simulation time should be estimated (useful for large simulations or slow functions). See details. Default is time_n_test=FALSE.</td>
</tr>
<tr class="odd">
<td align="center">save_res</td>
<td align="left">Boolean that specifies whether the results of time_n_test should be saved to the current directory. Default is save_res=FALSE.</td>
</tr>
</tbody>
</table>

<!--| debug | Boolean that activates/deactivates the debug mode. If debug=TRUE all relevant variables are assigned to the global environment and the core loop is printed. This allows to run it manually and to see how MonteCarlo works internally. Default is debug=FALSE. |-->
raw | Boolean that specifies whether the output should be averaged over the nrep repetitions. Default is raw=TRUE. |
export\_also | A list that specifies additional objects that are supposed to be exported to the cluster. This allows to export data or to bypass the automatic export of functions. Default is export\_also=NULL.|

The MakeTable() Function
------------------------

The *MakeTable()* function also comes with some optional arguments. Most importantly *collapse* and *transform* allow to influence how the results of a simulation are aggregated and presented. This makes the package much more versatile. Additional examples that demonstrate the use of these features can be found in the examples of the help file and in the vignette.

<table style="width:83%;">
<colgroup>
<col width="18%" />
<col width="65%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Argument</th>
<th align="left">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">output</td>
<td align="left">List of class MonteCarlo generated by MonteCarlo()</td>
</tr>
<tr class="even">
<td align="center">rows</td>
<td align="left">Vector of parameter names to be stacked in the rows of the table. Ordered from the inside to the outside.</td>
</tr>
<tr class="odd">
<td align="center">cols</td>
<td align="left">Vector of parameter names to be stacked in the columns of the table. Ordered from the inside to the outside.</td>
</tr>
<tr class="even">
<td align="center">digits</td>
<td align="left">Number of digits displayed in table. Default is digits=4.</td>
</tr>
<tr class="odd">
<td align="center">collapse</td>
<td align="left">Optional list of the same length as output giving the names of functions to be applied to the repective components of output when collapsing the results to a table. By default means are taken. Another example could be sd().</td>
</tr>
<tr class="even">
<td align="center">transform</td>
<td align="left">Optional argument to transform the output table (for example from MSE to RMSE). If a function is supplied, it is applied to all tables. Alternatively, a list of functions can be supplied that has the same length as output. For tables that are supposed to stay unchanged set list element to NULL.</td>
</tr>
<tr class="odd">
<td align="center">include_meta</td>
<td align="left">Booloean that determines whether the meta data provided by summary() is included in comments below the table. Default is include_meta==TRUE.</td>
</tr>
<tr class="even">
<td align="center">width_mult</td>
<td align="left">Scaling factor for width of the output table. Default is width_mult=1.</td>
</tr>
<tr class="odd">
<td align="center">partial_grid</td>
<td align="left">An optional list with the elements named after of the parameters for which only a part of the grid values is supposed to be included in the table. Each component of the list is a vector that specifies the grid values of interest.</td>
</tr>
</tbody>
</table>
