#' A Simulated Set of Events for a Specific State or County.
#' 
#' @param rate Data frame with event rate \code{event_rate} and county FIP \code{county_fip}.
#' @param rate_per Numeric. Rate of events per unit population (e.g., per 1.000).
#' @param family A distribution family used to generate event counts. Options include \code{poisson} or \code{negative_binomial}.
#' @param control A list of parameters controlling the chosen distribution (e.g., dispersion for \code{negative_binomial}).
#' @param population List with \code{GEOID} and estimated population \code{count}
#' for the target rate.
#' @param state The state for which the data is requested. State names, postal codes, 
#'   and FIPS codes are accepted. Defaults to \code{NULL}.
#' @param county The county for which data is requested. FIPS 
#'   codes are accepted. Defaults to \code{NULL}.
#' @param labels Label assigned to each generated event.
#' @param probs Numeric vector of probabilities corresponding to \code{labels}. If omitted, labels are sampled uniformly.
#' @param time Number of time periods to generate. By default 1.
#' @param warn Logical. If TRUE, display warnings during simulation.
#' @param ... Additional arguments passed to \code{tigris::counties()} and \code{geodata::population()}.
#' @return A data frame of generated locations with:
#' \itemize{
#'   \item \code{longitude} Longitude.
#'   \item \code{latitude} Latitude.
#'   \item \code{county_fip} Unique county-level geographic identifier.
#'   \item \code{label} Label assigned to each generated event.
#' }
#' 
#' 
#' @details
#'
#' We generate realistic in-silico cohorts of events using rates at the county level.
#' Counts for each county are computed as: \code{time * event_rate *  count / rate_per}
#' 
#' Counts can be generated using different statistical distributions via the \code{family} argument, 
#' including \code{poisson} or \code{negative binomial}, with control over 
#' distribution-specific parameters (e.g., dispersion for negative binomial distribution \code{size}).
#' 
#' The events are distributed within counties in proportion to estimated population counts.
#' Latitude and longitude of each event are generated randomly, weighted by the GPWv4 population density map,
#' so denser areas are more likely to receive events, while sparsely populated areas receive fewer. The algorithm 
#' also accounts for pixel area to avoid bias due to differing cell sizes.
#' 
#' A recursive accept-reject algorithm based on county or state boundaries ensures that all
#' generated points fall inside the target polygons while preserving the target counts and proportions.
#' 
#' 
#' @importFrom Rdpack reprompt
#' @importFrom stats rnbinom rpois
#' @importFrom exactextractr exact_extract
#' @importFrom purrr map_dfr
#' @importFrom geodata population
#' @import dplyr
#' @import tigris
#' 
#' @references 
#' \insertRef{Rpack:bibtex}{Rdpack}
#' 
#' \insertRef{R-exactextractr}{gdp}
#' 
#' \insertRef{R-purrr}{gdp}
#' 
#' \insertRef{R-tigris}{gdp}
#' 
#' \insertRef{R-geodata}{gdp}
#' 
#' \insertRef{R-dplyr}{gdp}
#' 
#' 
#' @examples
#' library(gdp)
#' data(Stroke_Rate)
#' population<-get_census_population(geography="county")
#' strokes<-generate_event_locations(rate=Stroke_Rate,
#'                                   rate_per=1000,
#'                                   population = population,
#'                                   family = "negative_binomial",
#'                                   control = list(size=10),
#'                                   state = "01")
#' 
#' 
#' @export


generate_event_locations <- function(rate,
                                     rate_per,
                                     population,
                                     family = c("poisson", "negative_binomial"),
                                     control = list(),
                                     state = NULL,
                                     county = NULL,
                                     labels = NULL,
                                     probs = NULL,
                                     time = 1,
                                     warn = TRUE,
                                     ...) {
  
  # Sanity checks
  if (is.null(labels) && is.null(probs)) {
    labels <- "event"
    probs  <- 1
  } else if (!is.null(labels) && is.null(probs)) {
    probs <- rep(1 / length(labels), length(labels))
  } else if (is.null(labels) && !is.null(probs)) {
    stop("If 'probs' is provided, 'labels' must also be provided.")
  } else {
    if (length(labels) != length(probs)) {
      stop("'labels' and 'probs' must have the same length")
    }
    if (abs(sum(probs) - 1) > 1e-6) {
      stop("Probabilities do not sum to 1.")
    }
  }
  
  if(!is.logical(warn) || length(warn) != 1) stop("warn must be TRUE or FALSE")
  
  
  datatemp<-get_rates(rate = rate,population=population, rate_per=rate_per,state = state,county = county)

  
  # Family distribution
  family <- match.arg(family)

  # Generate counts based on distribution family
  datatemp$mu <- switch(family,
                        poisson = rpois(n = rep(1,nrow(datatemp)), lambda = datatemp$estimated),
                        negative_binomial = rnbinom(n = rep(1,nrow(datatemp)), mu = datatemp$estimated, size = control$size))
  
  
  USA_pop_density<-get_density(state = state, county = county)
  
  # Pixel resolution
  res=unique(USA_pop_density[[1]]$res_pixel)
  
  # The coverage area is square meters, we need square kilometers
  USA_pop_density_counties<-USA_pop_density%>%dplyr::bind_rows(.id="geoid")%>%dplyr::mutate(coverage_area=coverage_area/1e6)
  
  # weights for population density
  USA_pop_density_counties_weights<-USA_pop_density_counties%>%dplyr::group_by(geoid)%>%
    dplyr::mutate(weight=(value*coverage_area)/sum(value*coverage_area,na.rm = TRUE))%>%
    dplyr::mutate(weight=ifelse(is.na(weight),0,weight))
  
  # Combining density population and datatemp
  USA_pop_density_counties_weights_mu<-USA_pop_density_counties_weights%>%dplyr::full_join(datatemp,by="geoid")
  
  
  
  
  # Random events, generating labels
  random_events_per_cell<-USA_pop_density_counties_weights_mu%>%dplyr::ungroup()%>%
    dplyr::group_split(geoid)%>%
    purrr::map_dfr(., ~ slice_sample(.x,n=unique(.x$mu), weight_by =.x$weight,replace = TRUE))%>%
    dplyr::mutate(labels=sample(labels,length(.$geoid),replace = TRUE,prob = probs))%>%
    dplyr::select(c("x", "y", "labels", "geoid", "display_name"))
  
  ## Accept-reject sampling
  # Creating the boundary for intersections
  if(!is.null(state)){
    boundary <- tigris::states(progress = FALSE)%>%dplyr::filter(STATEFP == state)%>%
                                              dplyr::select(geometry)%>%
                                              st_transform("EPSG:4326")}
  
  if(!is.null(county)){
    boundary <- tigris::states(progress = FALSE)%>%dplyr::filter(STATEFP == county)%>%
      dplyr::select(geometry)%>%
      st_transform("EPSG:4326")}
  
  sampled_points<-accept_reject_sampling(random_events_per_cell,boundary,res)
  
  return(sampled_points)
}
