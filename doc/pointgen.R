## ----setup, include=FALSE-----------------------------------------------------
library(dplyr)
library(ggplot2)
library(pointgen)

## ----message=FALSE, warning=FALSE, progress=FALSE-----------------------------
data("stroke_hospitalization")
data("stroke_mortality")

## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
population2023<-get_census_population(geography="state",state = "California",vintage = 2023)

mortality<-generate_event_locations(geography="state",
                                   rate=stroke_mortality,
                                   population = population2023,
                                   family = "negative_binomial",
                                   control = list(size=10),
                                   rate_per = 100000,
                                   state = "California")


## -----------------------------------------------------------------------------
head(mortality)

## ----fig.width=8, fig.height=6------------------------------------------------
plot(mortality)

## ----message=FALSE, warning=FALSE, progress=FALSE, results='hide'-------------
population2023<-get_census_population(geography="state",vintage = 2023)

mortality<-generate_event_locations(geography="state",
                                   rate=stroke_mortality,
                                   population = population2023,
                                   family = "negative_binomial",
                                   control = list(size=10),
                                   rate_per = 100000)

## ----fig.width=8, fig.height=6------------------------------------------------
plot(mortality)

## -----------------------------------------------------------------------------
summary(mortality)

## ----message=FALSE, warning=FALSE, progress=FALSE, results='hide'-------------
population<-get_census_population(geography="county",age_group="65plus", state="IOWA", county = "103", vintage = 2021)

mortality<-generate_event_locations(geography="county",
                                   rate=stroke_hospitalization,
                                   population = population,
                                   family = "negative_binomial",
                                   control = list(size=10),
                                   rate_per = 1000,
                                   state = "Iowa",
                                   county = "103",
                                   labels=c("LVO","Non-LVO"),
                                   probs=c(0.6,0.4))


## ----fig.width=8, fig.height=6------------------------------------------------
plot(mortality,size=1)

## ----message=FALSE, warning=FALSE, progress=FALSE, results='hide'-------------
population<-get_census_population(geography="county",age_group="65plus", state="ILLINOIS", vintage = 2021)

hospitalization<-generate_event_locations(geography="county",
                                   rate=stroke_hospitalization,
                                   population = population,
                                   family = "negative_binomial",
                                   control = list(size=10),
                                   rate_per = 1000,
                                   labels=c("LVO","Non-LVO"),
                                   probs=c(0.6,0.4),
                                   state = "Illinois")


## ----fig.width=8, fig.height=6------------------------------------------------
plot(hospitalization)

