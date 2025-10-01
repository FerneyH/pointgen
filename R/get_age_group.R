#' Validate and Convert Age Group to Census Codes
#'
#' A helper function that checks whether an age group keyword or numeric
#' code is valid according to Census definitions, and converts it to the
#' corresponding numeric AGEGROUP code(s).
#'
#' Dictionary of valid age groups (Source: U.S. Census Bureau, Population Division, 
#' Release date: June 2024):
#' 
#' \itemize{
#'   \item \code{"total"} = 0
#'   \item \code{"0to4"} = 1
#'   \item \code{"5to9"} = 2
#'   \item \code{"10to14"} = 3
#'   \item \code{"15to19"} = 4
#'   \item \code{"20to24"} = 5
#'   \item \code{"25to29"} = 6
#'   \item \code{"30to34"} = 7
#'   \item \code{"35to39"} = 8
#'   \item \code{"40to44"} = 9
#'   \item \code{"45to49"} = 10
#'   \item \code{"50to54"} = 11
#'   \item \code{"55to59"} = 12
#'   \item \code{"60to64"} = 13
#'   \item \code{"65to69"} = 14
#'   \item \code{"70to74"} = 15
#'   \item \code{"75to79"} = 16
#'   \item \code{"80to84"} = 17
#'   \item \code{"85plus"} = 18
#' }
#'
#' @param age_group Character or numeric. Acceptable values include:
#'   \itemize{
#'     \item Keywords from the dictionary above.
#'     \item Open-ended notation like \code{"65plus"}.
#'     \item Ranges like \code{"15to69"}.
#'     \item Numeric codes 0–18 (see dictionary).
#'   }
#'
#' @return 
#' A numeric vector of valid census AGEGROUP codes.
#' 
#' @examples 
#' library(pointgen)
#' get_age_group("65plus")
#' get_age_group("65to79")
#' get_age_group(1)
#'
#'@export

get_age_group <- function(age_group="total") {
  
  # sanity check
  if(is.character(age_group)){
  age_group<-tolower(age_group)}
  
  # Dictionary
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
  
  age_low   <- c(NA,0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
  age_upper <- c(NA,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84)
  
  if (is.character(age_group)) {
      if (age_group %in% names(age_dict)) {
        # Returning the code for that age group
        return(age_dict[[age_group]])
      } 
    
      else if (grepl("^[0-9]+plus$", age_group)) {
        start_age <- as.numeric(sub("plus", "", age_group))
        if(!(start_age %in% age_low)) stop("Invalid age group: ", age_group)
        idx <- which(age_low >= start_age)
        if (length(idx) == 0) stop("Invalid age group: ", age_group)
        # Returning the codes for that age group
        return(age_dict[idx])
        
      } else if (grepl("^[0-9]+to[0-9]+$", age_group)) {
        parts <- as.numeric(unlist(strsplit(age_group, "to")))
        start_age <- parts[1]
        end_age   <- parts[2]
        if (end_age <= start_age) stop("Invalid range: ", age_group)
        if (!(start_age %in% age_low)) stop("Invalid start age: ", age_group)
        if (!(end_age %in% age_upper)) stop("Invalid end age: ", age_group)
        idx <- which(age_low >= start_age & age_low <= end_age)
        # Returning the codes for that age group
        return(age_dict[idx])
        
      } else {
        stop("Invalid age group keyword: ", age_group)
      }
    
    } else if (is.numeric(age_group)) {
      if (!(age_group %in% unlist(age_dict))) stop("Invalid numeric age group code: ", age_group)
      # Returning the codes for that age group
      return(age_dict[age_dict==age_group])
      
    } else {
      stop("age_group must be character or numeric.")
    }
}

