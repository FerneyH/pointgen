#' Get Population Density Values
#'
#' @param year Year of the population density data. One of 2000, 2005, 2010, 2015, 2020. Default is \code{2020}.
#' @param res Resolution of the raster, validvalues are 10, 5, 2.5, and 0.5 (minutes of a degree). Default is \code{0.5}.
#' @param path Character. Directory path to save the downloaded raster. 
#'   If \code{NULL}, a temporary directory is used.
#' @param ... Additional arguments passed to \code{geodata::population()}.
#'
#' @return Extracted values of cells from a raster (e.g., population density) within the 
#' boundary of a specified state or county.
#'
#' @details 
#' This function retrieves global population density rasters from 
#' the \pkg{geodata} package. Data is derived from the GPWv4 dataset.
#'
#' @examples
#' # Get 2020 population density at 0.5 resolution
#' get_density()
#' get_density(state="01")
#'
#' @importFrom geodata population
#' @importFrom @importFrom exactextractr exact_extract
#' 
#' @export
#' 
get_density <- function(state=NULL, county=NULL, year = 2020, res = 0.5, path = tempdir(), ...) {
  
  # Getting density
  density <- geodata::population(year = year, res = res, path = path, ...)
  
  # Getting boundary
  counties<-get_boundary(state = state, county = county)
  counties <- st_transform(counties, crs = st_crs(density))
  
  # Extracting coordinates of population density using the geometry of the county or state selected (state or county)
  USA_pop_density<-exactextractr::exact_extract(density,counties,include_xy = TRUE,include_cell=TRUE,coverage_area=TRUE,progress = FALSE) 
  # Including identifier by counties
  names(USA_pop_density)<-counties$GEOID
  
  return(USA_pop_density)
}
