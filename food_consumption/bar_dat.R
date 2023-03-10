bar_dat <- function(x, selected_country, sorting, axis_setting){
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    by_country <- x %>% 
    group_by(country) %>% 
    rename(CO2 = co2_emmission, # rename for clarity
           Consumption = consumption) %>% 
    filter(country == selected_country) %>% # filter dataset to only be selected country
    pivot_longer(cols = c('Consumption', 'CO2'), # setup data in long format to allow grouped bars
                 names_to = ('type'),
                 values_to = 'kg') 
    
  # Create subtitle that is altered by user input
  subt <-paste0('Sorted by ', sorting)
  
  if (sorting == 'Alphabetically') # needed so that when Alpha reselected it resorts data correctly
    by_country <- x %>% 
    group_by(country) %>% 
    rename(CO2 = co2_emmission,
           Consumption = consumption) %>% 
    filter(country == selected_country) %>% 
    pivot_longer(cols = c('Consumption', 'CO2'),
                 names_to = ('type'),
                 values_to = 'kg') 
  
  if (sorting =='Alphabetically')
    subt <-paste0('Sorted ', sorting) # Adjust subtitle based on by user input
  
  if (sorting == 'Consumption') 
    by_country <- x %>% 
    filter(country == selected_country) %>%
    rename(CO2 = co2_emmission,
           Consumption = consumption) %>% 
    mutate(food_category = fct_reorder(food_category, Consumption)) %>%  # set sorting to be by consumption
    pivot_longer(cols = c('Consumption', 'CO2'),
                 names_to = ('type'),
                 values_to = 'kg') 
  
  
  if (sorting == 'CO2') # set sorting to be by CO2
    by_country <- x %>% 
    filter(country == selected_country) %>%  
    rename(CO2 = co2_emmission,
           Consumption = consumption) %>% 
    mutate(food_category = fct_reorder(food_category, CO2)) %>% 
    pivot_longer(cols = c('Consumption', 'CO2'),
                 names_to = ('type'),
                 values_to = 'kg') 

  titl <- paste0('<span style="color:#009E73"> CO2 emmissions </span>and ',
                 '<span style="color:#CC79A7">Consumption </span> by food category for ',
                 selected_country) # Set title to show colours of bars
  
  #create base plot
  
  plot <-  ggplot(by_country, aes(x = food_category, y = kg, fill = type))+
    geom_bar(stat = 'identity', # create dodged bar chart
             position = 'dodge')+
    labs(title = titl, # set titles based on earlier created variables
         subtitle = subt) +
    theme_minimal()+
    coord_flip() + # rotate chart
    scale_fill_manual(values = c("#009E73","#CC79A7"))+ # set colours to match colour scheme
    theme(plot.title = element_markdown(), # need this to allow colours in title
          legend.position = 'none', # remove legend as in title
          panel.grid.major.y = element_blank())+
    xlab(NULL) +
    ylab('KG')
  
  
  if (axis_setting == TRUE)
    subt <- paste0(subt, ' with axis fixed at 0-1750 Kg') # alter subtitle
  
  plot <-  ggplot(by_country, aes(x = food_category, y = kg, fill = type))+
    geom_bar(stat = 'identity',
             position = 'dodge')+
    labs(title = titl,
         subtitle = subt) + # need to rerun this to update subtitle
    theme_minimal()+
    coord_flip() +
    scale_fill_manual(values = c("#009E73","#CC79A7"))+
    theme(plot.title = element_markdown(),
          legend.position = 'none',
          panel.grid.major.y = element_blank())+
    xlab(NULL)+
    ylab('KG')+
    scale_y_continuous(breaks = breaks_extended(6))
  
  if (axis_setting == TRUE) # if user requests fixed axis add setting to plot
    plot +
    scale_y_continuous(breaks = breaks_width(250),
                       limits = c(0,1750))
  
  else 
    plot # otherwise show plot with automatically selected axis
}