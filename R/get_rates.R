#' Prepare Event Rates with Boundaries
#'
#' @description
#' Combines event rates, population, and geographic regions into a single object for 
#' simulation. 
#' 
#' @param rate List with event rate \code{event_rate} and unique geographic identifier \code{GEOID}.
#' @param population List with \code{GEOID} and estimated population \code{count}
#' for the target rate.
#' @param rate_per Multiplier constant (e.g., events per 1,000 individuals). Default to \code{1000}.  
#' @param state The state for which the data is requested. State names, postal codes, and FIPS codes are accepted. Default to \code{NULL}.
#' @param county The county for which the data is requested. FIPS codes are accepted. Default to \code{NULL}.
#' @param time Number of time periods to generate. Default to \code{1}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{data:} A data frame of geographic regions with event rates, population, 
#'   and estimated counts.
#'   \item \code{boundary:} An \code{sf} object of the requested state or county 
#'   boundary.
#' }
#'
#' @examples
#' library(pointgen)
#' data(Stroke_Rate)
#' pop<-get_census_population(geography="county",age_group = "65plus")
#' get_rates(rate = Stroke_Rate,population=pop)
#' get_rates(rate = Stroke_Rate,population=pop,state = "AL")
#' get_rates(rate = Stroke_Rate,population=pop,state = "01",county = "001")
#'
#' @import dplyr
#' @import tigris
#' 
#' @references 
#' \insertRef{Rpack:bibtex}{Rdpack}
#' 
#' \insertRef{R-tigris}{pointgen}
#' 
#' \insertRef{R-dplyr}{pointgen}
#' 
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
  if(!is.null(state)){
    fips <- tigris::fips_codes |> dplyr::distinct(state, state_name, state_code)
   if (state %in% fips$state) {
      state<-sprintf("%02s", fips$state_code[fips$state == state])
    } else if (state %in% fips$state_name) {
      state<-sprintf("%02s", fips$state_code[fips$state_name == state])
    } else if (state %in% fips$state_code) {
      state<-sprintf("%02s", state)
    } else {
      stop("Unrecognized state identifier")
    }
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
