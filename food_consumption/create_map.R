create_map <- function(x,co2_map) {
# Create data frame with co2 emissions by country
 
  ## can tidy this up as don't need as many steps
  co2 <- x %>% 
    group_by(country) %>% 
    mutate(co2_per_kg = co2_emmission/consumption) %>% 
    summarise(co2_per_kg = round(mean(co2_per_kg, na.rm = TRUE), 2),# Calculate co2 per kg eaten by country
              total_eaten = sum(consumption)) %>% # sum total kg eaten
    mutate(total_co2 = co2_per_kg * total_eaten) %>% # calculate total co2 by country
    select(country, total_co2)# reduce df to just country and co2 consumption
  
  ## Some names don't match between dataset and map information so need to chnaged to match
  
  correct_names <- co2 %>% 
    mutate(country = replace(country, country == 'Congo', 'Democratic Republic of the Congo'),
           country = replace(country, country == 'Gambia', 'The Gambia'),
           country = replace(country, country == 'Macedonia', 'North Macedonia'),
           country = replace(country, country == 'Russia', 'Russian Federation'),
           country = replace(country, country == 'South Korea', 'Republic of Korea'),
           country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
           country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
           country = replace(country, country == 'Taiwan. ROC', 'Taiwan'),
           country = replace(country, country == 'USA', 'United States'),) %>% 
    rename(NAME_LONG = country)
  
  # set options to correct any geometry issues and hide errors
  tmap_options(check.and.fix = TRUE) 
  sf::sf_use_s2(FALSE)
  # Create st object with projected corodinates
  
  world_projected  <- rnaturalearth::ne_download(returnclass = "sf") %>%
    select(NAME_LONG, ISO_A3_EH, POP_EST, GDP_MD, CONTINENT) %>%
    st_transform("+proj=moll")
  
  # Combine total co2 information with coordinates
  combined_df = left_join(world_projected,
                          correct_names)
  
  combined_df <- combined_df %>%
    mutate(wealth = ntile((log(GDP_MD/POP_EST)), 100),
           total_co2 = ntile(total_co2,100))
  
  tmap_mode("view")
  
  c<- tm_shape(combined_df) +
    tm_polygons(c("total_co2"),
                palette = 'viridis',
                title = c("Percentile of\nCO2 Emissions\nper person (Kg)")) +
    tm_layout(main.title = " Mapping of per capita CO2 Emissions and Wealth",
              frame = FALSE,
    )
  
  
  w<-tm_shape(combined_df) +
    tm_polygons(c("wealth"),
                palette = 'viridis',
                title = c( 'Percentile of\nGDP per\ncapita'),
                n =5) +
    tm_layout(frame = FALSE)
  
  # sf::sf_use_s2(FALSE)
  if(co2_map == TRUE) return(c)
  else return(w)
  
}
