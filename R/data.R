#' Stroke Hospitalization Rates 
#' 
#' Data from the U.S. Centers for Disease Control and Prevention (CDC) Interactive 
#' Atlas of Heart Disease and Stroke with annual stroke hospitalization rates for 
#' medicare beneficiaries aged 65 and older at the county level from 2014 to 2020.
#' @format
#' A data frame with 3226 rows and 3 columns:
#' \describe{
#'   \item{display_name}{county name.}
#'   \item{event_rate}{Stroke rate.}
#'   \item{GEOID}{Unique geographic identifier.}
#'   }
#'
"stroke_hospitalization"


#' Stroke Mortality Rates
#' 
#' Data from the U.S. Centers for Disease Control and Prevention (CDC) with mortality rate per 100,000 total population at the state level for 2023.
#' @format
#' A data frame with 51 rows and 4 columns:
#' \describe{
#'   \item{event_rate}{Stroke rate.}
#'   \item{GEOID}{Unique geographic identifier.}
#'   \item{NAME}{State name.}
#'   }
#'
#'
"stroke_mortality"