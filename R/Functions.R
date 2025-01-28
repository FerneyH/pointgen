#' A Simulated Set of Stroke Events for a Specific State or County.
#' @param rate  Data frame with Hospitalization Average Rate per 1,000 Medicare Beneficiaries and counties FIPS, ages 65+ (Data from CDC Interactive Atlas of Heart Disease and Stroke, 2014-2020).
#' @param population  Data frame with population by county, ages 65+ (Census Data).
#' @param density RasterLayer with population density in continental U.S. (excluding AK). 
#' @param counties Shape file with geometry of counties (Census data).
#' @param state_code State Code, example: "01".
#' @param county_code County FIP, example: "003".
#' @param ... Further arguments passed to exact_extract in the default setup.
#' @import dplyr 
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
#' \code{stroke_type} Stroke type: Ischemic, Hemorrhagic, and Mimic.
#' 
#' \code{category} Stroke Category: LVO, Non-LVO, Hemorrhagic, and Mimic.
#' 
#' \code{cnty_fips} County FIP.
#' 
#' \code{display_name} County name.
#' 
#' @author Ferney Henao-Ceballos
#' 
#' @description
#' We use county-level stroke hospitalization rates from the CDC Interactive
#' Atlas of Heart Disease and Stroke \insertCite{StrokeRates}{RandomStroke}, population 
#' density data from the NASA Socioeconomic Data and Applications Center (SEDAC) 
#' \insertCite{PopulationDensity}{RandomStroke}, and county population data from the U.S. Census Bureau (2023) 
#' \insertCite{census_co_est2023}{RandomStroke} to generate random stroke events for any state or county in the continental United States.
#' 
#'    
#' @details
#' Stroke is the fifth leading cause of death in the United States, and its most severe form,
#' large-vessel occlusion, accounts for over 60% of all stroke-related disabilities. 
#' Endovascular therapy can significantly reduce stroke-related disability by 67% for
#' qualifying patients, but this treatment is time-sensitive. Intravenous thrombolytic 
#' therapy is also used for acute ischemic strokes and has been shown to be both effective 
#' and time-sensitive. The location of the patient is crucial, as it directly affects 
#' transport times to stroke centers and is closely correlated with delays in receiving 
#' different treatments. Several research groups have developed promising modeling approaches
#' to better understand the conditions under which treating patients at a local stroke center versus 
#' directly transferring them to an endovascular stroke center yields better outcomes.
#'
#' We generate a realistic in-silico cohorts of stroke patients using the annual stroke hospitalization 
#' rates obtained at a countywide level from 2014 to 2020 using the CDC Interactive Atlas of Heart Disease 
#' and Stroke \insertCite{StrokeRates}{RandomStroke}, which provides ischemic and hemorrhagic metrics. 
#' LVO/non-LVO and mimic event rates were determined using Emergency Department Stroke Visits in the 
#' United States from 2016 to 2020. The strokes were distributed within counties in proportion to U.S. Census 
#' Bureau (2023) population estimates \insertCite{census_co_est2023}{RandomStroke}. Latitude and longitude 
#' of stroke events were generated randomly, weighted by the GPWv4 population density map \insertCite{PopulationDensity}{RandomStroke},
#' which takes into account pixel area while uniformly distributing locations within each pixel. Finally, we employed an accept/reject algorithm
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



GetStrokes <- function(rate, population, density, counties, state_code=FALSE, county_code=FALSE, warn = TRUE, years=1, ...){
  
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
    stop("Invalid 'cnty_fips', this is a example of a valid 'cnty_fips': '01003'")
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
    stop("Invalid 'cnty_fips', this is a example of a valid 'cnty_fips': '01003'")
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
    stop("Invalid 'cnty_fips', this is a example of a valid 'cnty_fips': '01003'")
  }
  
  if((county_code==FALSE & state_code==FALSE)){
    stop("You must provide 'county_code' or 'state_code'")
  }
  
  # Random sample for specific state or county
  if(county_code==FALSE){
    counties<-counties%>%dplyr::filter(substr(cnty_fips, 1, 2) == state_code)
    rate<-rate%>%dplyr::filter(substr(cnty_fips, 1, 2) == state_code)
    population<-population%>%dplyr::filter(substr(cnty_fips, 1, 2) == state_code)
  }  
   else{counties<-counties%>%dplyr::filter(cnty_fips == county_code)
        rate<-rate%>%dplyr::filter(cnty_fips == county_code)
        population<-population%>%dplyr::filter(cnty_fips == county_code)
        }
     
  
  # Generating mu for Ischemic and Stroke events, the rate is per 1000 medicare beneficiaries
  # Assigning 0 to missing rates from CDC 
  rate[is.na(rate)] <- 0
  
  mu<-population|>dplyr::full_join(rate,by="cnty_fips")%>%dplyr::mutate(mu_ischemic=years*ischemic_rate*population/1e3, mu_hemorrhagic=years*hemorrhagic_rate*population/1e3)
  
  mu_hemorrhagic_poisson<-rpois(rep(1,nrow(mu)),mu$mu_hemorrhagic)
  mu_ischemic_poisson<-rpois(rep(1,nrow(mu)),mu$mu_ischemic)
  mu_mimics_poisson<-rpois(rep(1,nrow(mu)),((mu$mu_ischemic+mu$mu_hemorrhagic)*0.057/0.943))
  
  mu_poisson<-cbind(mu,mu_ischemic_poisson,mu_hemorrhagic_poisson,mu_mimics_poisson)
  
  # Extracting coordinates of population density using the geometry of the county or state selected (state_code or county_code)
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
  
  
  state <- tigris::states(progress = FALSE)%>%filter(STATEFP == state_code)%>%dplyr::select(geometry)%>%st_transform("EPSG:4326")
  
  initial<-data.frame()
  Random_strokes <- function(dset, output, state, counties,county_code) {
    # New coordinates
    dset <- dset %>%dplyr::mutate(longitude = runif(nrow(dset), x - 0.00845/2, x + 0.00845/2),
                           latitude = runif(nrow(dset), y - 0.00845/2, y + 0.00845/2)) %>%
                    dplyr::select(c("x", "y", "longitude", "latitude", "stroke_type", "category", "cnty_fips", "display_name")) %>%
                    dplyr::left_join(counties, by = "cnty_fips")
    
    # Convert to sf object and check intersections
    geometry <- sf::st_as_sf(dset, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)$geometry
    
    if(county_code==FALSE){
    dset$intercept <- sf::st_intersects(geometry, state, sparse = FALSE) %>% as.vector()
    }else{dset$intercept <- sf::st_intersects(geometry, counties%>%dplyr::filter(cnty_fips==county_code), sparse = FALSE) %>% as.vector()}
    
    
    # Filter and update output
    valid_points <- dset%>%dplyr::filter(intercept == TRUE) %>%dplyr::select(c("longitude", "latitude", "stroke_type", "category", "cnty_fips", "display_name"))
    output <- rbind(output, valid_points)
    
    # Check remaining points
    temp <- dset%>%dplyr::filter(intercept == FALSE)
    
    if (nrow(temp) == 0) {
      return(output)
    }
    
    Random_strokes(temp, output, state, counties,county_code)
    
  }
  
  Data<-rbind(Random_strokes(random_ischemic_stroke_events_per_cell,initial,state,counties,county_code),
              Random_strokes(random_hemorrhagic_stroke_events_per_cell,initial,state,counties,county_code),
              Random_strokes(random_mimics_stroke_events_per_cell,initial,state,counties,county_code))
  return(Data)
 }  




