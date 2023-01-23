
# Pollen seasons

Data for a selection of pollen species and monitoring stations is
available as

``` r
library(tidyverse)
data <- read_csv("data/pollen_counts.csv")
glimpse(data)
```

    ## Rows: 71,179
    ## Columns: 6
    ## $ date     <date> 2008-02-08, 2008-02-08, 2008-02-09, 2008-02-12, 2008-02-12, …
    ## $ station  <chr> "Stockholm", "Stockholm", "Stockholm", "Stockholm", "Stockhol…
    ## $ swe_name <chr> "Al", "Hassel", "Hassel", "Al", "Hassel", "Al", "Hassel", "Al…
    ## $ lat_name <chr> "Alnus", "Corylus", "Corylus", "Alnus", "Corylus", "Alnus", "…
    ## $ count    <dbl> 5, 1, 2, 1, 9, 2, 7, 2, 2, 1, 1, 1, 2, 1, 2, 6, 4, 3, 3, 11, …
    ## $ factor   <dbl> 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1…

Note that zero-counts are usually not recorded. The `factor` column is
related to the microscope used for counting (see
<https://www.youtube.com/watch?v=KgiFLbYWqDA&ab_channel=Naturhistoriskariksmuseet>
for a description of how the pollen are counted) and `count * factor` is
supposedly more comparable across years/stations than just `count`.
