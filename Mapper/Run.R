data <- data.frame( x=2*cos(2*(1:1000)), y=sin(1:1000) )

plot(data$x, data$y)

Mapper_output <- Mapper(data_frame = data, data_description = "data", filter_values =  data$y, lens_description = "lens")

Mapperplot(Mapper_output)

Mappercolor(Mapper_output)
