#' Population Estimates by Age Group from Census Bureau
#'
#' A helper function that uses \code{tidycensus::get_estimates()} to obtain
#' U.S. Census Bureau Population Estimates. The function allows filtering by
#' age group, race, sex, and Hispanic origin.
#' 
#' @param geography The geography of your data. 
#' 
#' Available geographies include: 'state', 'county', 'cbsa', 'combined statistical area'
#' 
#' "cbsa" is used as alias for "metropolitan statistical area/micropolitan statistical area". 
#'
#' @param breakdown The population breakdown(s). Acceptable values are \code{AGEGROUP}, \code{"RACE"}, 
#'   \code{"SEX"}, and \code{"HISP"}.
#' @param age_group Character keywords (e.g., \code{"65plus"}, 
#'   \code{"15to69"}) or numeric code as defined by the U.S. Census Bureau. See arguments for
#'   \code{get_age_group} function.
#' @param state The state for which the data is requested. State names, postal codes, and FIPS
#' codes are accepted. Default to NULL.
#' @param county The county for which the data is requested. County FIP codes
#' are accepted.
#' @param geometry if FALSE (the default), return a regular tibble. if TRUE, uses 
#' the tigris package to return an sf tibble with simple feature geometry in the
#' ‘geometry‘ column.
#' @param vintage Vintage year; see tidycensus for details.
#' @param ... Additional arguments passed to \code{tidycensus::get_estimates()}.
#'
#' @return 
#' A list of data frames, one per strata. Each data frame contains:  
#' \describe{
#'   \item{GEOID}{Unique geographic identifier.}
#'   \item{Name}{County or state name.}
#'   \item{population}{Estimated population for the given strata.}
#'   \item{age_group}{Age group.}
#' }
#' 
#' @importFrom Rdpack reprompt
#' @importFrom tidycensus get_estimates
#' @import dplyr 
#' 
#' @references  
#' \insertRef{Rpack:bibtex}{Rdpack}
#' 
#' \insertRef{R-dplyr}{gdp}
#' 
#' \insertRef{R-tidycensus}{gdp}
#' 
#' 
#' @examples
#' get_census_population(geography="county")
#' get_census_population(geography="county",breakdown=c("AGEGROUP","SEX"))
#' get_census_population(geography="state",breakdown=c("AGEGROUP","SEX"),state="Alabama")
#' get_census_population(geography="county",breakdown=c("AGEGROUP","SEX"),state="01", county ="001")
#' get_census_population(geography="cbsa",breakdown=c("AGEGROUP","SEX"))
#' get_census_population(geography="combined statistical area")
#' @export

get_census_population <- function(geography=c("state", "county", "cbsa",
                                              "metropolitan statistical area/micropolitan statistical area", 
                                              "combined statistical area"),
                                  breakdown="AGEGROUP",
                                  age_group = "Total",
                                  state = NULL,
                                  county = NULL,
                                  geometry = TRUE,
                                  vintage = 2024,
                                  ...){
  
  geography <- match.arg(geography)
  # Age groups
  codes<-unlist(get_age_group(age_group))
  
  # Age group equal to total is not consistent through states and counties
  if(age_group == "Total" && is.null(county)){codes<-c(1:18)}
  

  # Estimated population data
  pop_est <- tidycensus::get_estimates(
    geography = geography,
    product   = "characteristics",
    breakdown = breakdown,
    state=state, 
    county=county, 
    geometry=geometry,
    vintage= vintage,
    ...)


  # Adding population groups according to age_group and creating datasets
  # with the corresponding strata
  group_vars <- breakdown[breakdown != "AGEGROUP"]
  if(length(group_vars)==0){
    pop_group <- pop_est %>%
     dplyr::filter(AGEGROUP %in% codes) %>%
     dplyr::group_by(GEOID, NAME) %>%
      dplyr::summarise(count = sum(value), .groups = "drop")%>%
      dplyr::mutate(age_group=age_group)
  } else {
    pop_group <- pop_est %>%
      dplyr::filter(AGEGROUP %in% codes) %>%
      dplyr::group_by(GEOID, NAME,dplyr::across(dplyr::all_of(group_vars)))%>%
      dplyr::summarise(count = sum(value), .groups = "drop")%>%
      dplyr::mutate(age_group=age_group)
  }
  
  # Group and split
  pop_list <- pop_group %>%
    dplyr::group_by(across(all_of(group_vars))) %>%
    dplyr::group_split()

return(pop_list)
}

