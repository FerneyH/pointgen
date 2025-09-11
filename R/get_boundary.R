#' Get State or County Boundaries
#'
#' @param state The state for which data is requested. State names, postal codes, 
#'   or FIPS codes are accepted. Defaults to \code{NULL}.
#' @param county The county for which data is requested. County names or FIPS codes 
#'   are accepted. Must be combined with a value supplied to \code{state}. Defaults to \code{NULL}.
#' @param year Integer. Year of the boundary data. Default is \code{2024}.
#' @param cb Logical. If \code{TRUE}, return cartographic boundary files 
#'   (generalized for plotting). Default is \code{FALSE}.
#' @param resolution 
#' @param ... Additional arguments passed to \code{tigris::counties()}.
#'
#' @return An \code{sf} object with geographic boundaries.
#'
#' @details 
#' This function retrieves county or state boundaries using the 
#' \pkg{tigris} package. If both \code{state} and \code{county} are \code{NULL}, 
#' all U.S. counties are returned.
#'
#' @examples
#' # Get all counties in Alabama
#' get_boundary(state = "AL")
#'
#' # Get a single county
#' get_boundary(state = "AL", county = "01001")
#'
#' @importFrom tigris counties
#' 
#' @export

get_boundary <- function(state = NULL, county = NULL, year = 2024, cb = TRUE, ...) {
  
  boundary <- tigris::counties(state = state, year = year, cb = cb, ...)
  
  if (!is.null(county)) {
    boundary <- boundary[boundary$GEOID == county, ]
  }

  return(boundary)
}


