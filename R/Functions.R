#' Get population estimates by age group from Census PEP
#'
#' @param vintage PEP vintage year (e.g., 2022)
#' @param year Year within the vintage
#' @param breakdown The population breakdown used when product = "characteristics". 
#' Acceptable values are "AGEGROUP", "RACE", "SEX", and "HISP", for
#' Hispanic/Not Hispanic. These values can be combined in a vector, 
#' returning population estimates in the value column for all combinations 
#' of these breakdowns. For years 2020 and later, "AGE" is also available for 
#' single-year age when using geography = "state".
#' @param age_group Character keywords (e.g., "65plus", "18to64") or numeric AGEGROUP codes
#' @param percent Logical, include percent of total population
#' @param ... Additional arguments passed to tidycensus::get_estimates function
#'
#' @return Tibble with GEOID, NAME, population, and optional percent
#' @import tidycensus
#' @export
 
get_population_age <- function(vintage   = 2022,
                               year      = 2022,
                               breakdown,
                               age_group = "65plus",
                               ...) 

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
  
 # Resolve input age_group to numeric codes
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
   } else if (is.numeric(ag)) {
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
    ...
  )

  
  # Summarize population
  group_vars <- breakdown[breakdown != "AGEGROUP"]
  if(length(group_vars)==0){
    pop_group <- pop_est %>%
      dplyr::filter(AGEGROUP %in% codes) %>%
      dplyr::group_by(GEOID, NAME) %>%
      dplyr::summarise(population = sum(value), .groups = "drop")
  } else {
    pop_group <- pop_est %>%
      dplyr::filter(AGEGROUP %in% codes) %>%
      dplyr::group_by(GEOID, NAME,dplyr::across(dplyr::all_of(group_vars)))%>%
      dplyr::summarise(population = sum(value), .groups = "drop")
  }
    
  # Group and split
  pop_list <- pop_group %>%
    group_by(across(all_of(group_vars))) %>%
    group_split()
  
  # Names to each list element
  names(pop_list) <- pop_group %>%
    group_by(across(all_of(group_vars))) %>%
    group_keys() %>%
    mutate(name = paste0("SEX", SEX, "_RACE", RACE)) %>%
    pull(name)
  
return(pop_group)
}


#' Accept-Reject Sampling for Spatial Points
#' 
#' Generates random points within a polygon using accept-reject sampling.
#' Each point is sampled within a population density cell and then checked
#' to ensure it falls within the target polygon (state or county).
#'
#' @param dataset Data frame containing at least the following columns:
#'   \itemize{
#'     \item \code{x}: Longitude of the centroid of the population density pixel.
#'     \item \code{y}: Latitude of the centroid of the population density pixel.
#'     \item \code{stroke_type}: Type of stroke event (e.g., "Ischemic", "Hemorrhagic", "Mimic").
#'     \item \code{category}: Category of the event within the stroke type.
#'     \item \code{cnty_fips}: County FIPS code.
#'     \item \code{display_name}: name for the county.
#'   }
#' @param state An \code{sf} object representing the state polygon.
#' @param counties An \code{sf} object representing county polygons, must include column \code{cnty_fips}.
#' @param county_fip Either \code{FALSE} (to sample across the entire state) or a single county FIPS code.
#' @param res_density Numeric value specifying the resolution of sampling around each centroid (in degrees). Determines the uniform sampling range around \code{x} and \code{y}.
#'
#' @return A data frame containing the accepted points with columns:
#' \code{longitude}, \code{latitude}, \code{stroke_type}, \code{category}, \code{cnty_fips}, \code{display_name}.
#'
#' @details
#' The input coordinates \code{x} and \code{y} are the centroids of the population density raster pixels.
#' The function generates candidate points by sampling uniformly around these centroids within a square
#' of size \code{res_density}. Points outside the target polygon are rejected, keeping only valid locations.
#'
#' @export

accept_reject_sampling <- function(dataset, state, counties, county_fip = FALSE, res_density,output=data.frame()) {
  
  if (!all(c("x", "y", "stroke_type", "category", "cnty_fips", "display_name") %in% names(dset))) {
    stop("dset must contain columns: x, y, stroke_type, category, cnty_fips, display_name")
  }
  if (!inherits(state, "sf")) stop("state must be an sf object")
  if (!inherits(counties, "sf")) stop("counties must be an sf object")
  if (!is.numeric(res_density) || length(res_density) != 1 || res_density <= 0) {
    stop("res_density must be a single positive numeric value")
  }
  if (!is.data.frame(output)) stop("output must be a data frame")
  if (!(is.logical(county_fip) || is.character(county_fip) || is.numeric(county_fip))) {
    stop("county_fip must be FALSE or a valid county FIPS code")
  }
  
  # Generate random coordinates
  dset <- dset %>%
    dplyr::mutate(
      longitude = runif(nrow(dset), x - res_density/2, x + res_density/2),
      latitude  = runif(nrow(dset), y - res_density/2, y + res_density/2)
    ) %>%
    dplyr::select(c("x", "y", "longitude", "latitude", "stroke_type", "category", "cnty_fips", "display_name")) %>%
    dplyr::left_join(counties, by = "cnty_fips")
  
  # Convert to sf geometry
  geometry <- sf::st_as_sf(dset, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)$geometry
  
  # Intersect with polygons
  if (identical(county_fip, FALSE)) {
    dset$intercept <- sf::st_intersects(geometry, state, sparse = FALSE) %>% as.vector()
  } else {
    dset$intercept <- sf::st_intersects(
      geometry,
      counties %>% dplyr::filter(cnty_fips == county_fip),
      sparse = FALSE
    ) %>% as.vector()
  }
  
  # Keep valid points 
  valid_points <- dset %>%
    dplyr::filter(intercept == TRUE) %>%
    dplyr::select(c("longitude", "latitude", "stroke_type", "category", "cnty_fips", "display_name"))
  
  output <- rbind(output, valid_points)
  
  # Recursive call for remaining points 
  temp <- dset %>% dplyr::filter(intercept == FALSE)
  
  if (nrow(temp) == 0) {
    return(output)
  }
  
  # Recursive call to Random_strokes (assumes defined elsewhere)
  Random_strokes(temp, state, counties, county_fip, res_density,output)
}


#' A Simulated Set of Patients for a Specific State or County.
#' @param rate  Data frame with Hospitalization Average Rate per 1,000 Medicare Beneficiaries and counties FIPS, ages 65+ (Data from CDC Interactive Atlas of Heart Disease and Stroke).
#' @param population  Data frame with population by county, ages 65+ (Census Data).
#' @param counties Shape file with geometry of counties (Census data).
#' @param state_fip State Code, example: "01".
#' @param county_fip County FIP, example: "003".
#' @param ... Further arguments passed to other functions in the default setup.
#' @import Rdpack
#' @import exactextractr
#' @import purrr
#' @import tigris
#' @import sf
#' @return A data frame including
#' 
#' \code{longitude} Longitude.
#' 
#' \code{latitude} Latitude.
#' 
#' \code{stroke_type} Stroke type: Ischemic and Hemorrhagic.
#' 
#' \code{cnty_fips} County FIP.
#' 
#' \code{display_name} County name.
#' 
#' @author Ferney Henao-Ceballos; Grant Brown
#' 
#' @description
#' We use county-level hospitalization rates from the CDC Interactive
#' Atlas of Heart Disease and Stroke \insertCite{StrokeRates}{RandomStroke}, population 
#' density data from the NASA Socioeconomic Data and Applications Center (SEDAC) 
#' \insertCite{PopulationDensity}{RandomStroke}, and county population data from the U.S. Census Bureau (2023) 
#' \insertCite{census_co_est2023}{RandomStroke} to generate random stroke events for any state or county in the continental United States.
#' 
#'    
#' @details
#' 
#' We generate a realistic in-silico cohorts of patients using the annual hospitalization 
#' rates obtained at a countywide level using the CDC Interactive Atlas of Heart Disease 
#' and Stroke \insertCite{StrokeRates}{RandomStroke}. 
#' The patients were distributed within counties in proportion to U.S. Census 
#' Bureau (2023) population estimates \insertCite{census_co_est2023}{RandomStroke}. Latitude and longitude 
#' of the Patients were generated randomly, weighted by the GPWv4 population density map \insertCite{PopulationDensity}{RandomStroke},
#' which takes into account pixel area while uniformly distributing locations within each pixel. Finally, we employed an recursive accept/reject algorithm
#' based on county or state boundaries to accurately generate the locations within the chosen state or county. 
#' 
#' @references 
#' \insertRef{Rpack:bibtex}{Rdpack}
#' 
#' \insertRef{PopulationDensity}{RandomStroke}
#'
#' \insertRef{StrokeRates}{RandomStroke}
#'
#' \insertRef{census_co_est2023}{RandomStroke}
#'
#' \insertRef{R-exactextractr}{RandomStroke}
#'
#' \insertRef{R-purrr}{RandomStroke}
#'
#' \insertRef{R-sf}{RandomStroke}
#'
#' \insertRef{R-tigris}{RandomStroke}
#'
#' \insertRef{sf2023}{RandomStroke}
#'
#' 
#' @export



GetLocations <- function(rate, population, state_fip=FALSE, county_fip=FALSE, warn = TRUE, years=1, year_density=2020, res_density= 0.5, ...){
  
  # Checking warn
  if(!is.logical(warn) || length(warn) != 1){
    stop("warn must be either TRUE or FALSE")
  }
  
  # Checking for valid values of rate
  if(!("data.frame" %in%class(rate))){
    stop("rate must be a data frame")
  }
  
  names(rate) <- tolower(names(rate))
  if(!all(names(rate) %in% c("cnty_fips", "ischemic_rate","hemorrhagic_rate","display_name"))){
    stop("The rate data frame must have 'cnty_fips', 'hemorrhagic_rate', and 'ischemic_rate'")
  }
  
  if(!all(nchar(rate$cnty_fips)==5)){
    stop("Invalid 'cnty_fips', this is an example of a valid 'cnty_fips': '01003'")
  }
  
  # Checking for valid values of population
  if(!("data.frame" %in% class(population))){
    stop("population must be a data frame")
  }
  
  names(population) <- tolower(names(population))
  if(!all(names(population) %in% c("cnty_fips", "population"))){
    stop("The population data frame must have 'cnty_fips' and 'population' ")
  }
  
  if(!all(nchar(population$cnty_fips)==5)){
    stop("Invalid 'cnty_fips', this is an example of a valid 'cnty_fips': '01003'")
  }
  
  if(!all(nchar(cnty_fips)==5)){
    stop("Invalid 'cnty_fips', this is an example of a valid 'cnty_fips': '01003'")
  }
  
  if(!all(nchar(state_fip)==2)){
    stop("Invalid 'state_fip', this is an example of a valid 'state_fip': '03'")
  }
  
  # Checking for valid format of density
  if(!("RasterLayer" %in% class(density))){
    stop("density must be of class RasterLayer")
  }
  
  # Checking for valid format of counties
  if(!("sf" %in% class(counties))){
    stop("counties must be of class sf")
  }
  
  names(counties) <- tolower(names(counties))
  if(!all(names(counties) %in% c("cnty_fips", "geometry"))){
    stop("counties data must have 'cnty_fips' and 'geometry'")
  }
  
  if(!all(nchar(counties$cnty_fips)==5)){
    stop("Invalid 'cnty_fips', this is an example of a valid 'cnty_fips': '01003'")
  }
  
  if((county_fip==FALSE & state_fip==FALSE)){
    stop("You must provide 'county_fip' or 'state_fip'")
  }
  
  # Getting population density
  density<-geodata::population(year=year_density,res=res_density, path=tempdir(),...)
  
  # Getting counties geometry
  if(state_fip==FALSE){
  state=substr(cnty_fips, 1, 2)
  counties<-tigris::counties(state = state, cb = FALSE, resolution = "500k", year = NULL, ...)
  }
    else{
    counties<-tigris::counties(state = state_fip, cb = FALSE, resolution = "500k", year = NULL, ...)
    }
   
  

  # Random sample for specific state or county
  if(county_fip==FALSE){
    counties<-counties%>%dplyr::filter(substr(cnty_fips, 1, 2) == state_fip)
    rate<-rate%>%dplyr::filter(substr(cnty_fips, 1, 2) == state_fip)
    population<-population%>%dplyr::filter(substr(cnty_fips, 1, 2) == state_fip)
  }  
    else{counties<-counties%>%dplyr::filter(cnty_fips == county_fip)
        rate<-rate%>%dplyr::filter(cnty_fips == county_fip)
        population<-population%>%dplyr::filter(cnty_fips == county_fip)
    }
     
  
  # Generating mu for Ischemic and Stroke events, the rate is per 1000 medicare beneficiaries
  # Assigning 0 to missing rates from CDC 
  rate[is.na(rate)] <- 0
  
  mu<-population|>dplyr::full_join(rate,by="cnty_fips")%>%dplyr::mutate(mu_ischemic=years*ischemic_rate*population/1e3, mu_hemorrhagic=years*hemorrhagic_rate*population/1e3)
  
  mu_hemorrhagic_poisson<-rpois(rep(1,nrow(mu)),mu$mu_hemorrhagic)
  mu_ischemic_poisson<-rpois(rep(1,nrow(mu)),mu$mu_ischemic)
  mu_mimics_poisson<-rpois(rep(1,nrow(mu)),((mu$mu_ischemic+mu$mu_hemorrhagic)*0.057/0.943))
  
  mu_poisson<-cbind(mu,mu_ischemic_poisson,mu_hemorrhagic_poisson,mu_mimics_poisson)
  
  # Extracting coordinates of population density using the geometry of the county or state selected (state_fip or county_fip)
  USA_pop_density_2020<-exactextractr::exact_extract(density,counties,include_xy = TRUE,include_cell=TRUE,coverage_area=TRUE, ...) 
  # Including identifier by counties
  names(USA_pop_density_2020)<-counties$cnty_fips
  # The coverage area is square meters, we need to change the value to square kilometers
  USA_pop_density_2020_counties<-USA_pop_density_2020%>%dplyr::bind_rows(.id="cnty_fips")%>%dplyr::mutate(coverage_area=coverage_area/1e6)
  # weights for population density
  USA_pop_density_2020_counties_weights<-USA_pop_density_2020_counties%>%dplyr::group_by(cnty_fips)%>%
                                          dplyr::mutate(weight=(value*coverage_area)/sum(value*coverage_area,na.rm = TRUE))%>%
                                          dplyr::mutate(weight=ifelse(is.na(weight),0,weight))
  
  # Combining density population and mu_poisson
  USA_pop_density_2020_counties_weights_mu_poisson<-USA_pop_density_2020_counties_weights%>%dplyr::full_join(mu_poisson,by="cnty_fips")
  
  # Random Ischemic Stroke Events, using LVO primary diagnosis
  random_ischemic_stroke_events_per_cell<-USA_pop_density_2020_counties_weights_mu_poisson%>%dplyr::ungroup()%>%
                                            dplyr::group_split(cnty_fips)%>%
                                            purrr::map_dfr(., ~ slice_sample(.x,n=unique(.x$mu_ischemic_poisson), weight_by =.x$weight,replace = TRUE))%>%
                                            dplyr::mutate(stroke_type="Ischemic",category=sample(c("LVO","Non-LVO"),length(.$cnty_fips),replace = TRUE,prob = c(0.4415,0.5585)))
  
  
  # Random Hemorrhagic Stroke Events
  random_hemorrhagic_stroke_events_per_cell<-USA_pop_density_2020_counties_weights_mu_poisson%>%dplyr::ungroup()%>%
                                              dplyr::group_split(cnty_fips)%>%
                                              purrr::map_dfr(., ~ slice_sample(.x,n=unique(.x$mu_hemorrhagic_poisson), weight_by =.x$weight,replace = TRUE))%>%
                                              dplyr::mutate(stroke_type="Hemorrhagic",category="Hemorrhagic")
  
  # Random Stroke Mimics  
  random_mimics_stroke_events_per_cell<-USA_pop_density_2020_counties_weights_mu_poisson%>%dplyr::ungroup()%>%
                                              dplyr::group_split(cnty_fips)%>%
                                              purrr::map_dfr(., ~ slice_sample(.x,n=unique(.x$mu_mimics_poisson), weight_by =.x$weight,replace = TRUE))%>%
                                              dplyr::mutate(stroke_type="Mimic",category="Mimic")
  
  
  state <- tigris::states(progress = FALSE)%>%dplyr::filter(STATEFP == state_fip)%>%dplyr::select(geometry)%>%st_transform("EPSG:4326")
  
  
  
  Data<-rbind(accept_reject_sampling(random_ischemic_stroke_events_per_cell,state,counties,county_fip),
              accept_reject_sampling(random_hemorrhagic_stroke_events_per_cell,state,counties,county_fip),
              accept_reject_sampling(random_mimics_stroke_events_per_cell,state,counties,county_fip))
  return(Data)
 }  
