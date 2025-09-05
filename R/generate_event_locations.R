#' A Simulated Set of Patients/Events for a Specific State or County.
#' 
#' @param rate Data frame with event rate \code{event_rate} and county FIP \code{county_fip}.
#' @param rate_per Numeric. Rate of events per unit population (e.g., per 1.000).
#' @param family A distribution family used to generate event counts. Options include \code{poisson}, \code{negative_binomial}, or \code{normal}.
#' @param control A list of parameters controlling the chosen distribution (e.g., dispersion for \code{negative_binomial}).
#' @param population Data frame with \code{county_fip} and  estimated counts \code{count} of the target population.
#' @param state_fip_code State FIPS code. Default FALSE.
#' @param county_fip_code County FIPS code. Default FALSE.
#' @param labels Label assigned to each generated event.
#' @param probs Numeric vector of probabilities corresponding to \code{labels}. If omitted, labels are sampled uniformly.
#' @param time Number of time periods to generate. By default 1.
#' @param year_density Integer. Specifies the year to use for the density. By default 2020.
#' @param res_density Numeric. Resolution for density. By default 0.5.
#' @param year_counties Integer. Specifies which year’s county boundaries are used.
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
#' We generate realistic in-silico cohorts of patients/events using rates at the county level.
#' Counts for each county are computed as: \code{time * event_rate *  count / rate_per}
#' 
#' Counts can be generated using different statistical distributions via the \code{family} argument, 
#' including \code{poisson}, \code{negative binomial}, or \code{normal}, with control over 
#' distribution-specific parameters (e.g., dispersion for negative binomial distribution \code{size}, standard deviation for the normal distribution \code{sd}).
#' 
#' The  patients/events are distributed within counties in proportion to estimated population counts.
#' Latitude and longitude of each event are generated randomly, weighted by the GPWv4 population density map,
#' so denser areas are more likely to receive events, while sparsely populated areas receive fewer. The algorithm 
#' also accounts for pixel area to avoid bias due to differing cell sizes.
#' 
#' A recursive accept-reject algorithm based on county or state boundaries ensures that all
#' generated points fall inside the target polygons while preserving the target counts and proportions.
#' 
#' 
#' @importFrom Rdpack reprompt
#' @import stats
#' @import dplyr 
#' @importFrom exactextractr exact_extract
#' @importFrom purrr map_dfr
#' @import tigris
#' @importFrom geodata population
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
#' population<-get_census_population()
#' strokes<-generate_event_locations(rate=Stroke_Rate,
#'                                   rate_per=1000,
#'                                   family = "negative_binomial",
#'                                   population = population,
#'                                   state_fip_code = "01",
#'                                   control = list(size=10))
#' 
#' 
#' @export


generate_event_locations <- function(rate,
                                     rate_per,
                                     family = c("poisson", "negative_binomial", "normal"),
                                     control = list(),
                                     population,
                                     state_fip_code = FALSE,
                                     county_fip_code = FALSE,
                                     labels = NULL,
                                     probs = NULL,
                                     time = 1,
                                     year_density = 2020,
                                     res_density = 0.5,
                                     year_counties=NULL,
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
  
  if(!("data.frame" %in% class(rate))) stop("rate must be a data frame")
  
  if(!("data.frame" %in% class(population))) stop("population must be a data frame")
  
  if((county_fip_code==FALSE & state_fip_code==FALSE))stop("You must provide 'county_fip_code' or 'state_fip_code'")
  
  
  # Standardize FIPS codes to 5-character strings
  rate$county_fip <- sprintf("%05d", as.numeric(rate$county_fip))
  
  if(!all(nchar(rate$county_fip)==5)) stop("Invalid 'county_fip' in rate data, this is an example of a valid 'county_fip': '01001'")
  
  
  if(state_fip_code==FALSE){
  if(!all(nchar(county_fip_code)==5)) stop("Invalid 'county_fip_code', this is an example of a valid 'county_fip_code': '01001'")
  }
  
  if(county_fip_code==FALSE){
  if(!all(nchar(state_fip_code)==2)) stop("Invalid 'state_fip_code', this is an example of a valid 'state_fip_code': '03'")
  }
  
  names(rate) <- tolower(names(rate))
  if(!all(c("county_fip","event_rate") %in% names(rate))) stop("rate must contain 'county_fip' and the 'event_rate' columns")
  
  
  names(population) <- tolower(names(population))
  if(!all(c("county_fip", "count")%in%names(population))) stop("The population data frame must have 'county_fip' and 'count'")
  
  
  # Getting counties geometry
  if(state_fip_code==FALSE){
    state=substr(county_fip_code, 1, 2)
    counties<-tigris::counties(state = state, cb = FALSE, resolution = "500k", year = year_counties,...)
  }
  else{
    counties<-tigris::counties(state = state_fip_code, cb = FALSE, resolution = "500k", year = year_counties,...)
  }
  
  # Getting population density
  density <- geodata::population(year = year_density, res = res_density, path = tempdir(),...)
  
  # Resolution for the density
  res<-res(density)[1]
  
  # State or county
  if(county_fip_code==FALSE){
    rate<-rate%>%dplyr::filter(substr(county_fip, 1, 2) ==state_fip_code)
    population<-population%>%dplyr::filter(substr(county_fip, 1, 2) == state_fip_code)
  }  
  else{
    counties<-counties%>%dplyr::filter(GEOID == county_fip_code)
    rate<-rate%>%dplyr::filter(county_fip == county_fip_code)
    population<-population%>%dplyr::filter(county_fip == county_fip_code)
  }
  
  # Project counties to match raster CRS
  counties <- st_transform(counties, crs = st_crs(density))
  

  # Replace missing values (NA) with zero to ensure valid counts/rates
  rate[is.na(rate$event_rate)] <- 0
  
  # Full join
  datatemp <- population %>%
    dplyr::full_join(rate, by = "county_fip") %>%
    dplyr::mutate(estimated = time * event_rate * count / rate_per)
  
  
  # Family distribution
  family <- match.arg(family)

  # Generate counts based on distribution family
  datatemp$mu <- switch(family,
                        poisson = rpois(n = rep(1,nrow(datatemp)), lambda = datatemp$estimated),
                        negative_binomial = rnbinom(n = rep(1,nrow(datatemp)), mu = datatemp$estimated, size = control$size),
                        normal = round(rnorm(n = rep(1,nrow(datatemp)), mean = datatemp$estimated, sd = control$sd)))
  
  
  # Extracting coordinates of population density using the geometry of the county or state selected (state_fip_code or county_fip_code)
  USA_pop_density<-exactextractr::exact_extract(density,counties,include_xy = TRUE,include_cell=TRUE,coverage_area=TRUE,progress = FALSE) 
  # Including identifier by counties
  names(USA_pop_density)<-counties$GEOID
  
  # The coverage area is square meters, we need to change the value to square kilometers
  USA_pop_density_counties<-USA_pop_density%>%dplyr::bind_rows(.id="county_fip")%>%dplyr::mutate(coverage_area=coverage_area/1e6)
  
  # weights for population density
  USA_pop_density_counties_weights<-USA_pop_density_counties%>%dplyr::group_by(county_fip)%>%
    dplyr::mutate(weight=(value*coverage_area)/sum(value*coverage_area,na.rm = TRUE))%>%
    dplyr::mutate(weight=ifelse(is.na(weight),0,weight))
  
  # Combining density population and datatemp
  USA_pop_density_counties_weights_mu<-USA_pop_density_counties_weights%>%dplyr::full_join(datatemp,by="county_fip")
  
  # Random patients/events, generating labels
  random_events_per_cell<-USA_pop_density_counties_weights_mu%>%dplyr::ungroup()%>%
    dplyr::group_split(county_fip)%>%
    purrr::map_dfr(., ~ slice_sample(.x,n=unique(.x$mu), weight_by =.x$weight,replace = TRUE))%>%
    dplyr::mutate(labels=sample(labels,length(.$county_fip),replace = TRUE,prob = probs))
  
  # Geometry for the state 
  state <- tigris::states(progress = FALSE,year = year_counties)%>%dplyr::filter(STATEFP == state_fip_code)%>%dplyr::select(geometry)%>%st_transform("EPSG:4326")
  
  # Changing name 
  counties<-counties%>%rename(county_fip=GEOID)
  
  # Accept-reject sampling
  sampled_points<-accept_reject_sampling(random_events_per_cell,state,counties,county_fip_code,res)
  
  return(sampled_points)
}