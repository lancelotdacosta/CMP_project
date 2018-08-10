x <- c(seq(2,10,0.1),2*cos(0.5*(1:200)))
y <- c( seq(0,0,length.out=length(seq(2,10,0.1))),sin(1:200))

data <- data.frame(x =x ,y=y )

plot(data$x, data$y)

Mapper_output <- Mapper(data_frame = data, filter_values =  data$x, num_intervals = 10)

Mapperplot(Mapper_output)

Mappercolor(Mapper_output)
