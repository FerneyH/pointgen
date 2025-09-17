#' Prepare Event Rates with Boundaries
#'
#' @description
#' Combines event rates, population, and state or county boundary into a single object for 
#' simulation. 
#'
#' @param rate List with event rate \code{event_rate} and \code{GEOID}.
#' @param population List with \code{GEOID} and estimated population \code{count}
#' for the target rate.
#' @param rate_per Rate of events per unit population (e.g., per 1,000; 1000,000). 
#'   Defaults to \code{1000}.
#' @param state The state for which the data is requested. State names, postal codes, 
#'   and FIPS codes are accepted. Defaults to \code{NULL}.
#' @param county The county for which the data is requested. FIPS 
#'   codes are accepted. Defaults to \code{NULL}.
#' @param time Number of time periods to generate. Defaults to \code{1}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{data} A data frame of counties with event rates, population, 
#'   and estimated counts.
#'   \item \code{boundary} An \code{sf} object of the requested state or county 
#'   boundary.
#' }
#'
#' @examples
#' data(Stroke_Rate)
#' pop<-get_census_population(geography="county",age_group = "65plus")
#' get_rates(rate = Stroke_Rate,population=pop)
#' get_rates(rate = Stroke_Rate,population=pop,state = "AL")
#' get_rates(rate = Stroke_Rate,population=pop,state = "01",county = "001")
#'
#' @import dplyr
#' @import tigris
#' @export
#' 
get_rates <- function(rate,
                      population, 
                      rate_per = 1000,
                      state = NULL, 
                      county = NULL,
                      time = 1) {
  
  # Sanity checks
  if (!inherits(rate, c("list", "data.frame"))) {
    stop("'rate' must be a list or a data frame")
  }
  if (!inherits(population, c("list", "data.frame"))) {
    stop("'population' must be a list or a data frame")
  }
  
  
  # standardize names
  population<-dplyr::bind_rows(population)
  names(rate) <- tolower(names(rate))
  names(population) <- tolower(names(population))
  
  # standardize state
   if (state %in% tigris::fips_codes$state) {
      state<-sprintf("%02s", tigris::fips_codes$state_code[tigris::fips_codes$state == state])
    } else if (state %in% tigris::fips_codes$state_name) {
      state<-sprintf("%02s", tigris::fips_codes$state_code[tigris::fips_codes$state_name == state])
    } else if (state %in% tigris::fips_codes$state_code) {
      state<-sprintf("%02s", state)
    } else {
      stop("Unrecognized state identifier")
    }

  
  # Replace missing values (NA) with zero to ensure valid counts/rates
  rate$event_rate[is.na(rate$event_rate)] <- 0
  
  if (!all(c("geoid", "event_rate") %in% names(rate)))
    stop("'rate' must contain 'geoid' and 'event_rate'")
  if (!all(c("geoid", "count") %in% names(population)))
    stop("'population' must contain 'geoid' and 'count'")
  
  # filter by state/county if given
  if (!is.null(state) && is.null(county)) {
    rate <- dplyr::filter(rate, substr(geoid, 1, 2) == state)
    population <- dplyr::filter(population, substr(geoid, 1, 2) == state)
  } else if (!is.null(state) && !is.null(county)) {
    rate <- dplyr::filter(rate, substr(geoid, 1, 2) == state & substr(geoid, 3, 5) == county)
    population <- dplyr::filter(population, substr(geoid, 1, 2) == state & substr(geoid, 3, 5) == county)
  }
  
  # merge rate and population, calculate estimated counts
  df <- dplyr::full_join(population, rate, by = "geoid") %>%
    dplyr::mutate(estimated = time * event_rate * count / rate_per)
  
return(df)
}
