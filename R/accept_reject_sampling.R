
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
#'     \item \code{geoid}: Unique geographic identifier.
#'     \item \code{display_name}: Name.
#'   }
#' @param boundary Geographic boundary boundary. See get_boundary function.
#' @param res_pixel Numeric value specifying the resolution of sampling around each centroid (in degrees). 
#' Determines the uniform sampling range around \code{x} and \code{y}.
#' @param output Optional data frame for storing generated results. By default data.frame()
#' @param max_iter The maximum number of iterations the algorithm will attempt. By default 100.
#' @param iter An internal counter that tracks the current iteration number
#' @return A data frame containing the accepted points with columns:
#' \code{longitude}, \code{latitude}, \code{labels}, \code{geoid}, \code{display_name}.
#'
#' @details
#' The input coordinates \code{x} and \code{y} are the centroids of the population density raster pixels.
#' The function generates candidate points by sampling uniformly around these centroids within a square
#' of size \code{res_pixel}. Points outside the target polygon are rejected, keeping only valid locations.
#' 
#' @importFrom Rdpack reprompt
#' @importFrom stats runif
#' @import dplyr 
#' @import sf
#' 
#' @export


accept_reject_sampling <- function(dataset, boundary, res_pixel,output=data.frame(), max_iter = 100, iter = 1) {
  
  if (!all(c("x", "y", "geoid") %in% names(dataset))) {
    stop("'dataset' must contain the columns: x, y, labels, geoid")
    }
  if (!is.numeric(res_pixel) || length(res_pixel) != 1 || res_pixel <= 0) {
    stop("'res_pixel' must be a single positive numeric value")
  }
  if (iter > max_iter) {
    warning("Maximum iterations reached; some points may remain outside boundaries")
    return(output)
  }
 
  # Generate random coordinates
  datatemp <- dataset %>% dplyr::mutate(
                                  longitude = runif(nrow(dataset), x - res_pixel/2, x + res_pixel/2),
                                  latitude  = runif(nrow(dataset), y - res_pixel/2, y + res_pixel/2))
  
  # Convert to sf geometry
  geometry <- sf::st_as_sf(datatemp, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)$geometry
  
  boundary <- sf::st_transform(boundary, crs = sf::st_crs(geometry))
  
  boundary<-dplyr::left_join(datatemp,boundary,by="geoid")
  
  dataset$intercept <-  as.logical(mapply(sf::st_within, geometry, boundary$geometry))
   
  
  # Keep valid points 
  valid_points <- dataset %>%
  dplyr::filter(intercept == TRUE)%>%dplyr::select(-c("intercept"))
  
  output <- rbind(output, valid_points)
  
  # Remaining points 
  temp <- dataset %>% dplyr::filter(intercept != TRUE)
  
  if (nrow(temp) == 0) {
    return(output)
  }
  
  # Recursive call
  accept_reject_sampling(temp, boundary, res_pixel,output,max_iter = max_iter, iter=iter + 1)
}
