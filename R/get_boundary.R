#' Get Geographic Boundaries
#' 
#' Geographic boundaries for a variety of administrative and statistical 
#' units, including states, counties, ZIP Code Tabulation Areas (ZCTAs), and 
#' metropolitan areas.
#'
#' @param geography Character string specifying the boundary type from the tigris package. 
#' 
#' Available geographies include: 'state', 'county', 'cbsa', 'zcta', 'combined statistical area'
#' 
#' "cbsa" is used as alias for "metropolitan statistical area/micropolitan statistical area".
#' 
#' @param state A state name, postal code, or FIPS code. Required for 
#'   county and place boundaries.
#' @param county A 3-digit county FIPS code (character). 
#' @param ... Additional arguments passed to the underlying \pkg{tigris} 
#'   functions.
#'
#' @return An \code{sf} object containing the requested geographic boundaries.
#'
#' @examples
#' # Retrieve all state boundaries
#' get_boundary("state")
#'
#' # Counties in Texas
#' get_boundary("county", state = "TX")
#' 
#' # Texas
#' get_boundary("state", state = "TX")
#'
#' # Single county by FIPS
#' get_boundary("county", state = "TX", county = "201")
#'
#' # ZIP Code Tabulation Areas (ZCTAs)
#' get_boundary("zcta",starts_with = c("37", "38", "72"))
#'
#' # Core-Based Statistical Areas
#' get_boundary("cbsa")
#'
#'
#' @export

get_boundary <- function(geography = c("state", "county", "zcta", "cbsa","combined statistical area"),
                         state = NULL,
                         county = NULL,
                         ...) {
  
  geography <- match.arg(geography)
  
  # Validate county codes if counties requested
  if (geography == "county" && !is.null(county)) {
    if (!all(nchar(county) == 3)) {
      stop("Invalid 'county'. Example of a valid county FIPS code: '001'")
    }
  }
  
  # Select correct tigris function
  boundary_fun <- switch(
    geography,
    state = tigris::states,
    county = tigris::counties,
    zcta   = tigris::zctas,
    cbsa   = tigris::core_based_statistical_areas
  )
  
  fips<-tigris::fips_codes
  # standardize state
  if(!is.null(state)){
    if (state %in% fips$state) {
    state<-unique(sprintf("%02s", fips$state_code[fips$state == state]))
    } else if (state %in% fips$state_name) {
    state<-unique(sprintf("%02s", fips$state_code[fips$state_name == state]))
    } else if (state %in% fips$state_code) {
    state<-unique(sprintf("%02s", state))
    } else {
    stop("Unrecognized state identifier")
    }
  }  
  
  boundary <- boundary_fun(...)
  
  # Retrieve boundary
  if(!is.null(state) && is.null(county)){
  boundary<-boundary[boundary$STATEFP %in% state, ]
  }
  
  # Apply county filter if needed
  if (geography == "county" && !is.null(county)) {
  boundary<-boundary[boundary$STATEFP %in% state, ]
  boundary <- boundary[boundary$COUNTYFP %in% county, ]
  }
  
  # Standardizing geoid name
  if(geography=="zcta"){
  boundary<-boundary%>%rename(geoid=GEOID20)
  }
  
  return(boundary)
}



