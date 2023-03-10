scatter_f <- function(x){
  
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # set title with markdown coloring 
  titl <- "<span style = 'font-weight:bold'>Consumption of Animal Products vs Plant Products and resultant CO2 emissions</span><br>
<span style='color:#009E73'> Total CO2 emissions </span>and <span style='color:#E69F00'>Plant CO2 emissions </span>"
  
    ggplot(x, aes(x = total_con_gp_Animal, y = total_con_gp_Plant ))+
    # Create scatter plot with size as total c02
    geom_point(aes(size = total_co2), colour = cbbPalette[4], alpha = 0.6)+
    # Increase size of points
    scale_size_area(max_size = 15)+
    # add point to show plant co2 inside total co2
    geom_point(aes(size = total_co2_gp_Plant), colour = cbbPalette[2])+ 
    theme_minimal()+
    # add axis titles and title set above
    labs(title = titl,
         x = 'Consumption of Animal Products (KG)',
         y = 'Consumption of Plant Products (KG)',
         size = 'KG of CO2')+
    theme(legend.position=c(0.8,0.85), # inset legend
          legend.direction = 'horizontal',
          plot.title = element_markdown(),# allow colour change of title
          legend.background = element_rect(colour = "black"))+ 
    # adjust position of legend title
    guides(size = guide_legend(title.position = 'top', title.hjust = 0.5, 
                               title.vjust = -1, nrow =2))+
    # set scales and breaks for x and y axis
    scale_x_continuous(breaks = scales::breaks_width(100),
                       limits = c(0,600))+
    scale_y_continuous(breaks = scales::breaks_extended(n = 8),
                       limits = c(0,250))

}                     