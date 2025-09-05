## Minimal example

``` r
library(gdp)
data(Stroke_Rate)
population<-get_census_population()
strokes<-generate_event_locations(rate=Stroke_Rate,
                                  rate_per=1000,
                                  family = "negative_binomial",
                                  population = population,
                                  state_fip_code = "01",
                                  control = list(size=10))
```
