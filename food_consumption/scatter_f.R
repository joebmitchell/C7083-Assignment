scatter_f <- function(x){
# set title with markdown coloring 
titl <- "Consumption of Animal Products vs Plant Products and resultant CO2 emmisssions<br>
<span style='color:#E69F00'> Total CO2 emmissions </span>and <span style='color:#56B4E9'>Plant CO2 emmissions </span>"

ggplot(x, aes(x = total_con_gp_Animal, y = total_con_gp_Plant ))+
  # Create scatter plot with size as total c02
  geom_point(aes(size = total_co2), colour ='#E69F00', alpha = 0.6)+
  # Increase size of points
  scale_size_area(max_size = 15)+
 # add point to show plant co2 inside total co2
  geom_point(aes(size = total_co2_gp_Plant), colour = '#56B4E9')+ 
  theme_minimal()+
  # add axis titles and title set above
  labs(title = titl,
       x = 'Consumption of Animal Products (KG)',
       y = 'Consumption of Plant Products (KG)',
       size = 'KG of CO2')+
  theme(legend.position=c(0.8,0.95), # inset legend
        legend.direction = 'horizontal',
        plot.title = element_markdown())+ # allow colour change of title
  # adjust position of legend title
  guides(size = guide_legend(title.position = 'top', title.hjust =0.5, title.vjust = -1, ))+
  # set scales and breaks for x and y axis
  scale_x_continuous(breaks = scales::breaks_width(100),
                     limits = c(0,600))+
  scale_y_continuous(breaks = scales::breaks_extended(n = 8),
                   limits = c(0,250))

}                     