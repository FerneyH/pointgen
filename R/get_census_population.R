#' Population Estimates by Age Group from Census Bureau
#'
#' A helper function that uses \code{tidycensus::get_estimates()} to obtain
#' U.S. Census Bureau Population Estimates data.The functions allows filtering by
#' age group, race, sex, and Hispanic origin.
#' 
#' @param geography The geography of your data. Available geographies for the most 
#' recent data vintage are listed \href{https://api.census.gov/data/2019/pep/population/geography.html}{here}. "cbsa" may be used an alias for "metropolitan 
#' statistical area/micropolitan statistical area".

#' @param breakdown The population breakdown(s). 
#'   Acceptable values are \code{AGEGROUP}, \code{"RACE"}, 
#'   \code{"SEX"}, and \code{"HISP"}.
#' @param age_group Character keywords (e.g., \code{"65plus"}, 
#'   \code{"15to69"}) or numeric code as defined by the U.S. Census Bureau. See arguments for
#'   \code{get_age_group} function.
#' @param state The state for which you are requesting data. State names, postal codes, and FIPS
#' codes are accepted. Defaults to NULL.
#' @param county The county for which you are requesting data. County names and FIPS codes
#' are accepted. Must be combined with a value supplied to ‘state‘. Defaults to NULL.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data. if TRUE, uses 
#' the tigris package to return an sf tibble with simple feature geometry in the
#' ‘geometry‘ column.
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
#' @import stats
#' @import dplyr
#' @importFrom tidycensus get_estimates
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
#' get_census_population(geography="county",breakdown=c("AGEGROUP"))
#' get_census_population(geography="county",breakdown=c("AGEGROUP","SEX"))
#' get_census_population(geography="county",breakdown=c("AGEGROUP","SEX"),state="01", county ="001")
#' @export

get_census_population <- function(geography,
                                  breakdown="AGEGROUP",
                                  age_group = "65to84",
                                  state = NULL,
                                  county = NULL,
                                  geometry = TRUE,
                                  ...){
  # Age groups
  codes<-unlist(get_age_group(age_group))

  # Estimated population data
  pop_est <- tidycensus::get_estimates(
    geography = geography,
    product   = "characteristics",
    breakdown = breakdown,
    state=state, 
    county=county, 
    geometry=geometry,
    vintage   = 2024,
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

