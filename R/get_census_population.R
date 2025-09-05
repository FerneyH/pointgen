#' Population Estimates by Age Group from Census Bureau
#'
#' A helper function that uses \code{tidycensus::get_estimates()} to obtain
#' U.S. Census Bureau Population Estimates data.The functions allows filtering by
#' age group, race, sex, and Hispanic origin.
#' 
#' @param vintage Vintage year (e.g., \code{2022}).
#' @param year Year within the vintage.
#' @param breakdown The population breakdown(s). 
#'   Acceptable values are \code{"AGEGROUP"}, \code{"RACE"}, 
#'   \code{"SEX"}, and \code{"HISP"}. Multiple values may be supplied.
#' @param age_group Character keywords (e.g., \code{"65plus"}, 
#'   \code{"15to69"}) or numeric code as defined by the U.S. Census Bureau.
#' @param ... Additional arguments passed to \code{tidycensus::get_estimates()}.
#'
#' @return 
#' A list of data frames, one per strata. Each data frame contains:  
#' \describe{
#'   \item{county_fip}{Unique county-level geographic identifier.}
#'   \item{Name}{County name.}
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
#' @export

get_census_population <- function(vintage   = 2021,
                                  year      = 2021,
                                  breakdown="AGEGROUP",
                                  age_group = "65plus",
                                  ...){
  
  # Sanity checks
  if (!is.numeric(vintage) || !is.numeric(year)) {
    stop("Both `vintage` and `year` must be numeric.")
  }

  if (year > vintage){
    stop("`year` cannot exceed `vintage`.")
  } 

  if (vintage >2022){
   stop("The most recent `vintage` released is Vintage 2022.")
  }

  # AGEGROUP reference codes
  # Source: U.S. Census Bureau, Population Division
  # Release date: June 2024
  age_dict <- list(
    total    = 0,
    `0to4`   = 1,
    `5to9`   = 2,
    `10to14` = 3,
    `15to19` = 4,
    `20to24` = 5,
    `25to29` = 6,
    `30to34` = 7,
    `35to39` = 8,
    `40to44` = 9,
    `45to49` = 10,
    `50to54` = 11,
    `55to59` = 12,
    `60to64` = 13,
    `65to69` = 14,
    `70to74` = 15,
    `75to79` = 16,
    `80to84` = 17,
    `85plus` = 18
  )

  # Change input age_group to numeric codes
  codes <- c()
  for (ag in age_group) {
    if (is.character(ag)) {
      if (ag %in% names(age_dict)) {
        codes <- c(codes, age_dict[[ag]])
      } else if (grepl("^[0-9]+plus$", ag)) {
        start_age <- as.numeric(sub("plus", "", ag))
        # select codes >= start_age
        # lower bound for each AGEGROUP
        age_low <- c(NA,0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
        codes <- c(codes, which(age_low >= start_age))
        } else {
         stop("Invalid age group: ", ag)
        }
    } else if (grepl("^[0-9]+to[0-9]+$", ag)) {
      parts <- as.numeric(unlist(strsplit(ag, "to")))
      start_age <- parts[1]
      end_age   <- parts[2]
      age_low <- c(NA,0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
      age_upper = c(NA, 4, 9, 14, 19, 24, 29, 34, 39, 44,49, 54, 59, 64, 69, 74, 79, 84)
      if (end_age <= start_age) {
        stop("Invalid range: ", ag)
      }
      if (!(start_age %in% age_low)) {
        stop("Invalid range:", ag)
      }
      if (!(end_age %in% age_upper)) {
        stop("Invalid range:", ag)
      }
      codes <- c(codes, which(age_low >= start_age & age_low <= end_age))
      
    }else if (is.numeric(ag)) {
       codes <- c(codes, ag)
      } else {
        stop("age_group must be numeric or valid keywords")
        }
  }
  codes <- unique(codes)
  codes<-as.numeric(age_dict[codes])

  # Estimated population data
  pop_est <- tidycensus::get_estimates(
    geography = "county",
    product   = "characteristics",
    breakdown = breakdown,
    vintage   = vintage,
    year      = year,
    ...)


  # Summarize population
  group_vars <- breakdown[breakdown != "AGEGROUP"]
  if(length(group_vars)==0){
    pop_group <- pop_est %>%
     dplyr::filter(AGEGROUP %in% codes) %>%
     dplyr::group_by(GEOID, NAME) %>%
      dplyr::summarise(population = sum(value), .groups = "drop")%>%
       rename(county_fip=GEOID,count=population)%>%
        mutate(age_group=age_group)
  } else {
    pop_group <- pop_est %>%
      dplyr::filter(AGEGROUP %in% codes) %>%
      dplyr::group_by(GEOID, NAME,dplyr::across(dplyr::all_of(group_vars)))%>%
      dplyr::summarise(population = sum(value), .groups = "drop")%>%
       rename(county_fip=GEOID,count=population)%>%
        mutate(age_group=age_group)
  }
  
  # Group and split
  pop_list <- pop_group %>%
   group_by(across(all_of(group_vars))) %>%
    group_split()

return(pop_group)
}

