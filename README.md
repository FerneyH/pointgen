## Minimal example

``` r
library(Geospatial_MAP-STROKE)
load(Stroke_Rate)
population<-get_census_population()
strokes<-generate_event_locations(rate=Stroke_Rate,
                               family = "negative_binomial",
                               population = population,
                               state_fip_code = "01",
                               control = list(size=10))
```
