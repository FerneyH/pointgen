#' A Simulated Set of Events for a Specific State or County.
#' 
#' @param geography Character string specifying the boundary type from the tigris package. 
#' 
#' Available geographies include: 'state', 'county', 'cbsa', 'combined statistical area'
#' 
#' "cbsa" is used as alias for "metropolitan statistical area/  statistical area".
#' @param rate Data frame with event rate \code{event_rate} and unique geographic identifier \code{GEOID}.
#' @param rate_per Numeric. Rate of events per unit population (e.g., per 1.000).
#' @param population List with \code{GEOID} and estimated population \code{count}
#' for the target rate.
#' @param family A distribution family used to generate event counts. Options include \code{poisson} or \code{negative_binomial}.
#' @param control A list of parameters controlling the chosen distribution (e.g., dispersion for \code{negative_binomial}).
#' @param state The state for which the data is requested. State names, postal codes, 
#'   and FIPS codes are accepted. Default to \code{NULL}.
#' @param county The county for which data is requested. FIPS 
#'   codes are accepted. Default to \code{NULL}.
#' @param labels Label assigned to each generated event.
#' @param probs Numeric vector of probabilities corresponding to \code{labels}. If omitted, labels are sampled uniformly.
#' @param time Number of time periods to generate. By default 1.
#' @param parallel A logical (TRUE/FALSE) argument. If TRUE, the function runs in parallel using multiple cores. Default to \code{TRUE}
#' @param progress By default FALSE.
#' @param ... Additional arguments passed to \code{tigris::counties()} and \code{geodata::population()}.
#' @return A data frame of generated locations with:
#' \itemize{
#'   \item \code{longitude}.
#'   \item \code{latitude}.
#'   \item \code{geoid:} Unique geographic identifier.
#'   \item \code{label:} Label assigned to each generated event.
#' }
#' 
#' @references 
#' \insertRef{Rpack:bibtex}{Rdpack}
#' 
#' \insertRef{R-exactextractr}{pointgen}
#' 
#' \insertRef{R-purrr}{pointgen}
#' 
#' \insertRef{R-tigris}{pointgen}
#' 
#' \insertRef{R-geodata}{pointgen}
#' 
#' \insertRef{R-dplyr}{pointgen}
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
#' 
#' @examples
#' library(pointgen)
#' data(stroke_hospitalization)
#' population<-get_census_population(geography="county")
#' strokes<-generate_event_locations(geography="county",
#'                                   rate=stroke_hospitalization,
#'                                   rate_per=1000,
#'                                   population = population,
#'                                   family = "negative_binomial",
#'                                   control = list(size=10),
#'                                   labels=c("LVO","Non-LVO"),
#'                                   probs=c(0.6,0.4),
#'                                   state = "19")
#' 
#' @export


generate_event_locations <- function(geography=c("state", "county", "cbsa","combined statistical area"),
                                     rate,
                                     rate_per,
                                     population,
                                     family = c("poisson", "negative_binomial"),
                                     control = list(),
                                     state = NULL,
                                     county = NULL,
                                     labels = NULL,
                                     probs = NULL,
                                     time = 1,
                                     parallel=TRUE,
                                     n_cores=4,
                                     progress=FALSE,
                                     ...) {
  
  geography <- match.arg(geography)
  
  
  # standardize names
  population<-dplyr::bind_rows(population)
  names(rate) <- tolower(names(rate))
  names(population) <- tolower(names(population))
  
  if (!all(c("event_rate", "geoid") %in% colnames(rate))) {
    stop("The 'rate' dataset must contain columns 'event_rate' and 'geoid'.")
  }
  if (!all(c("count", "geoid") %in% colnames(population))) {
    stop("'count' and 'geoid' must be provided in the population data")
  }
  
  
  if (is.null(labels) && is.null(probs)) {
    labels <- "Event"
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
  
  # progress bar
  options(purrr.show_progress = progress)
  

  datatemp<-get_rates(rate = rate,population=population, rate_per=rate_per,state = state,county = county,time=time)

  
  # Family distribution
  family <- match.arg(family)

  # Generate counts based on distribution family
  datatemp$mu <- switch(family,
                        poisson = rpois(n = rep(1,nrow(datatemp)), lambda = datatemp$estimated),
                        negative_binomial = rnbinom(n = rep(1,nrow(datatemp)), mu = datatemp$estimated, size = control$size))
  
  
  USA_pop_density<-get_density(geography=geography,state = state, county = county,progress=progress)
  
  # Pixel resolution
  res=unique(USA_pop_density[[1]]$res_pixel)
  
  # The coverage area is square meters, we need square kilometers
  USA_pop_density_geoid<-USA_pop_density%>%dplyr::bind_rows(.id="geoid")%>%dplyr::mutate(coverage_area=coverage_area/1e6)
  
  # weights for population density
  USA_pop_density_geoid_weights<-USA_pop_density_geoid%>%dplyr::group_by(geoid)%>%
    dplyr::mutate(weight=(value*coverage_area)/sum(value*coverage_area,na.rm = TRUE))%>%
    dplyr::mutate(weight=ifelse(is.na(weight),0,weight))
  
  # Combining density population and datatemp
  USA_pop_density_geoid_weights_mu<-USA_pop_density_geoid_weights%>%dplyr::full_join(datatemp,by="geoid")%>%
                                                                    dplyr::mutate(mu = ifelse(is.na(mu), 0, mu))
 
  
  
  # Random events, generating labels, excluding Alaska, Puerto Rico, etc
  random_events_per_cell<-USA_pop_density_geoid_weights_mu%>%dplyr::ungroup()%>%
    dplyr::group_split(geoid)%>%
    purrr::map_dfr(., ~ slice_sample(.x,n=unique(.x$mu), weight_by =.x$weight,replace = TRUE))%>%
    dplyr::mutate(labels=sample(labels,length(.$geoid),replace = TRUE,prob = probs))%>%
    dplyr::select(c("x", "y", "labels", "geoid"))%>%dplyr::filter(!(geoid %in% c("66","69","60","02","72","15","78")))
  
  ## Accept-reject sampling
  # Creating the boundary for intersections, it only checks specific county or state

  boundary <- get_boundary(geography =geography, state =state, county = county)
  
  
  boundary <- boundary%>%dplyr::select(GEOID,geometry)%>%st_transform("EPSG:4326")%>%dplyr::filter(!(GEOID %in% c("66","69","60","02","72","15","78")))
  names(boundary) <- tolower(names(boundary))
  
  rows<-nrow(random_events_per_cell)
  
  ## Using parallel when it is viable
  if (parallel &&  rows > 5000) {
  
   # Split data by ncores
   split_indices <- split(1:rows, cut(1:rows, breaks = n_cores, labels = FALSE))
  
   data_chunks <- lapply(split_indices, function(idx) random_events_per_cell[idx, ])
  
   # Create cluster
   cl <- parallel::makeCluster(n_cores)
  
   # Export necessary objects and functions to cluster
   parallel::clusterExport(cl, varlist = c("accept_reject_sampling"))
   parallel::clusterEvalQ(cl,{library(dplyr)})
  
  
   # Run accept_reject_sampling in parallel
   sampled_points_list <- parallel::parLapply(cl, data_chunks, function(df_chunk){accept_reject_sampling(df_chunk, boundary, res)})
   # Stop the cluster
   parallel::stopCluster(cl)
  
   # Combine results
   sampled_points <- dplyr::bind_rows(sampled_points_list)}
  else{
    sampled_points<-accept_reject_sampling(random_events_per_cell,boundary,res)
  }
  
  
  # Put boundary into an attribute
  attr(sampled_points, "geometry") <- boundary
  
  # Class
  class(sampled_points) <- c("pointgen", class(sampled_points))
  
  return(sampled_points)
}

