## For October 26

1. Make time continuous, not discrete so that the ticks show up on the rug part of the plot.

2. Comparing months on the overlap for one particular species.

3. Keep in mind that we can switch to ggplot with their density data frame output:

```
ggplot(data = test_plot, aes(x = x, y = densityA)) +
  geom_line() +
  theme_minimal()
```

## For October 19

1. Look into date functions: <https://highamm.github.io/ds234_quarto/11-lubridate.html#functions-for-date-variables>

2. Goal: Create a plot for a single species with density on y-axis, time of day on x-axis, and tick marks for species occurrence using either `overlap` or `camtrapr` packages. 

    1. If having enough time, compare distributions of different species and/or compare a single species abundance distribution across different forests or different forest types.
    
Extra source: <https://jniedballa.github.io/camtrapR/articles/camtrapr4.html>

## For October 5

1. Section 3.2 of vignette (MH: look at delta 4 estimators).

2. Review DATA/STAT 234 (<https://highamm.github.io/ds234_quarto/03-dplyr.html>)

3. Data Exploration

    1. variable should have correct types
    
    2. exploratory plots of the species (over time)
    
    3. exploratory summary stats of the species.

## For September 28

1. Section 3.2 of vignette (MH: look at delta 4 estimators).

2. Data Exploration

    1. variable should have correct types
    
    2. exploratory plots of the species (over time)
    
    3. exploratory summary stats of the species.

## For September 21

1. Follow along with code from <https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf>

    1. Change some function arguments with kerinci data in the package.

2. Chapter 12 <https://bookdown.org/c_w_beirne/wildCo-Data-Analysis/activity.html> (will not be able to follow along).

## For September 14

1. https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf

(follow along with code to see how to create density estimates).

2. https://bookdown.org/c_w_beirne/wildCo-Data-Analysis/activity.html

Follow along with ideas for density estimates

3. https://www.jstor.org/stable/pdf/20696577.pdf

(focus on intro and discussion, skim the rest).