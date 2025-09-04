
#' Accept-Reject Sampling for Spatial Points
#' 
#' Generates random points within a polygon using a vectorized accept-reject algorithm.
#' Each point is sampled within a population density cell ensuring
#' it falls inside the target polygon (e.g., state or county boundary). 
#' The algorithm uses a recursive approach, allowing large numbers of points to 
#' be generated efficiently. 
#'
#' @param dataset Data frame containing at least the following columns:
#'   \itemize{
#'     \item \code{x}: Longitude of the centroid of the population density cell.
#'     \item \code{y}: Latitude of the centroid of the population density cell.
#'     \item \code{labels} Label assigned to each generated event.
#'     \item \code{county_fip}: Unique county-level geographic identifier.
#'     \item \code{display_name}: Name of the county.
#'   }
#' @param state An \code{sf} object representing the state polygon.
#' @param counties An \code{sf} object representing county polygons, must include the column \code{county_fip}.
#' @param county_fip Either \code{FALSE} (to sample across the entire state) or a single county FIPS code.
#' @param res Numeric value specifying the resolution of sampling around each centroid (in degrees). 
#' Determines the uniform sampling range around \code{x} and \code{y}.
#'
#' @return A data frame containing the accepted points with columns:
#' \code{longitude}, \code{latitude}, \code{labels}, \code{county_fip}, \code{display_name}.
#'
#' @details
#' The input coordinates \code{x} and \code{y} are the centroids of the population density raster pixels.
#' The function generates candidate points by sampling uniformly around these centroids within a square
#' of size \code{res}. Points outside the target polygon are rejected, keeping only valid locations.
#' 
#' @references 
#' \insertRef{R-dplyr}{accept_reject_sampling}
#' \insertRef{sf2023}{accept_reject_sampling}
#' 
#' @import dplyr 
#' @import sf
#' @export

accept_reject_sampling <- function(dataset, state, counties, county_fip = FALSE, res,output=data.frame()) {
  
  if (!all(c("x", "y", "labels", "county_fip", "display_name") %in% names(dataset))) {
    stop("dataset must contain columns: x, y, labels, county_fip, display_name")
    }
  if (!inherits(state, "sf")) stop("state must be an sf object")
  if (!inherits(counties, "sf")) stop("counties must be an sf object")
  if (!is.numeric(res) || length(res) != 1 || res <= 0) {
    stop("res must be a single positive numeric value")
    }
  if (!(is.logical(county_fip) || (is.character(county_fip) && nchar(county_fip) == 5))) {
    stop("`county_fip` must be logical (TRUE/FALSE) or a valid 5-digit county FIPS code")
    }
  
  # Generate random coordinates
  dataset <- dataset %>% dplyr::mutate(
                                  longitude = runif(nrow(dataset), x - res/2, x + res/2),
                                  latitude  = runif(nrow(dataset), y - res/2, y + res/2)) %>%
                         dplyr::select(c("x", "y", "longitude", "latitude", "labels", "county_fip", "display_name")) %>%
                         dplyr::left_join(counties, by = "county_fip")
  
  # Convert to sf geometry
  geometry <- sf::st_as_sf(dataset, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)$geometry
  
  # Intersect with polygons
  if (county_fip==FALSE) {
      dataset$intercept <- sf::st_intersects(geometry, state, sparse = FALSE) %>% as.vector()
   } else {
      dataset$intercept <- sf::st_intersects(geometry, counties, sparse = FALSE) %>% as.vector()
    }
  
  # Keep valid points 
  valid_points <- dataset %>%
  dplyr::filter(intercept == TRUE) %>%
  dplyr::select(c("longitude", "latitude", "labels", "county_fip", "display_name"))
  
  output <- rbind(output, valid_points)
  
  # Remaining points 
  temp <- dataset %>% dplyr::filter(intercept == FALSE)
  
  if (nrow(temp) == 0) {
    return(output)
  }
  
  # Recursive call
  accept_reject_sampling(temp, state, counties, county_fip, res,output)
}
