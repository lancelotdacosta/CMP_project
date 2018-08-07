library(ggplot2)
library(ggthemes)

ggtree <- function(t){
  
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  data <- filter(cc.data, Tree == t)
  
  if(sum(c("CZ", "EZ", "WZ", "MZ") %in% names(data))<4){
    warning("Error in ggtree function")
    stop("Data is missing at least one type of cell (CZ, EZ, WZ, MZ")
  }
  
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
  if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily cell count from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }else{
    caption <- paste(c("Source: daily cell count mean from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }
  
  ggtree <- data %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
    labs(title= title, subtitle = subtitle, caption = caption) +
    geom_point(aes(y = MZ, color = "Mature cells")) +
    geom_point(aes(y = WZ, color = "Wall-thickening cells")) +
    geom_point(aes(y = EZ, color = "Enlarging cells")) +
    geom_point(aes(y = CZ, color = "Cambial cells")) +
    scale_colour_manual("",
                        breaks = c("Cambial cells", "Enlarging cells", "Wall-thickening cells", "Mature cells"),
                        values = c("green", "yellow", "red", "orange")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}

ggfluxes <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  data <-  filter(cc.data, Tree == t)
  
  if(sum(c("WZtoMZ", "EZtoWZ", "CZtoEZ", "toCZ") %in% names(data))<4){
    warning("Error in ggfluxsmooth function")
    stop("Data is missing at least one flux (WZtoMZ, EZtoWZ, CZtoEZ, toCZ")
  }
  
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
  if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }else{
    caption <- paste(c("Source: daily fluxes mean from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }
  
  
  ggtree <- data %>% ggplot(aes(x = DY)) +
    #xlim(1, max(data$DY))+
    ylim(-0.1, 0.5)+     #normal (-0.1,0.5)      
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("royalblue", "orange", "green", "violet")) +
    #scale_fill_brewer(palette="RdYlGn") +
    theme_fivethirtyeight()
  ggtree
}

ggfluxsmooth <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  data <-  filter(cc.data, Tree == t)
  
  if(sum(c("WZtoMZ", "EZtoWZ", "CZtoEZ", "toCZ") %in% names(data))<4){
    warning("Error in ggfluxsmooth function")
    stop("Data is missing at least one flux (WZtoMZ, EZtoWZ, CZtoEZ, toCZ")
  }
  
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
  if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }else{
    caption <- paste(c("Source: daily fluxes mean from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }
  
  
  ggtree <- data %>% ggplot(aes(x = DY)) +
    #xlim(1, max(data$DY))+
    ylim(-0.1, 0.5)+     #normal (-0.1,0.5)      
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    geom_smooth(aes(y = WZtoMZ, color = "WZ to MZ")) +
    geom_smooth(aes(y = EZtoWZ, color = "EZ to WZ")) +
    geom_smooth(aes(y = CZtoEZ, color = "CZ to EZ")) +
    geom_smooth(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("royalblue", "orange", "green", "violet")) +
    #scale_fill_brewer(palette="RdYlGn") +
    theme_fivethirtyeight()
  ggtree
}

ggflux1 <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  
  data <- filter(cc.data, Tree == t)
  
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
  if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }else{
    caption <- paste(c("Source: daily fluxes mean from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }
  
  
  ggtree <- data %>% ggplot(aes(x = DY)) +
    #geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)+
    #geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    #geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ"),
                        values = c("green")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}
ggflux2 <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  
  data <- filter(cc.data, Tree == t)
  
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
  if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }else{
    caption <- paste(c("Source: daily fluxes mean from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }
  
  
  ggtree <- data %>% ggplot(aes(x = DY)) +
    #geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)+
    #geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    #geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("CZ to EZ"),
                        values = c("royalblue")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}
ggflux3 <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  
  data <- filter(cc.data, Tree == t)
  
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
  if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }else{
    caption <- paste(c("Source: daily fluxes mean from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }
  
  ggtree <- data %>% ggplot(aes(x = DY)) +
    #geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)+
    geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    #geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    #geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c( "EZ to WZ"),
                        values = c("orange")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}

ggflux4 <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  
  data <- filter(cc.data, Tree == t)
  
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
  if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }else{
    caption <- paste(c("Source: daily fluxes mean from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
}
  
  ggtree <- data %>% ggplot(aes(x = DY)) +
    geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)+
    #geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    #geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    #geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("WZ to MZ"),
                        values = c("violet")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}

ggfluxsmoothcrop <- function(t){
  options(warn = -1)
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  data <-  filter(cc.data, Tree == t)
  day_first_sample <- filter(data, Sample ==1)$DY[1] -1
  day_last_sample <- filter(data, Sample ==max(data$Sample))$DY[1] +1
  data.in.growth <- filter(data, DY >= day_first_sample & DY <= day_last_sample)
  
  if(sum(c("WZtoMZ", "EZtoWZ", "CZtoEZ", "toCZ") %in% names(data))<4){
    warning("Error in ggfluxsmoothcrop function")
    stop("Data is missing at least one flux (WZtoMZ, EZtoWZ, CZtoEZ, toCZ")
  }
  
  data.in.growth$WZtoMZ[which(data.in.growth$WZtoMZ == 0)] <- NA
  data.in.growth$EZtoWZ[which(data.in.growth$EZtoWZ == 0)] <- NA
  data.in.growth$CZtoEZ[which(data.in.growth$CZtoEZ == 0)] <- NA
  data.in.growth$toCZ[which(data.in.growth$toCZ == 0)] <- NA
 
  title <- paste(c("Tree ", as.character(t)),collapse = "")
  
  subtitle <- paste(c("Species: ", as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]),
                      ", Diameter: ", as.character(data$Diameter[1]),
                      ", Height: ", as.character(data$Height[1]),
                      ", Anat. data: ", as.character(round(filter(data, DY == max(data$DY))$MZ[1], digits = 1))), collapse= "")
  
   if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
   }else{
     caption <- paste(c("Source: daily fluxes mean from ", 
                        as.character(data$Site[1])," ",
                        as.character(data$Year[1])), collapse = "")
  }
  
  
  ggtree <- data.in.growth %>% ggplot(aes(x = DY)) +
    xlim(1, max(data$DY))+
    ylim(-0.1, 0.5)+     #normal (-0.1,0.5)      
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    geom_smooth(aes(y = WZtoMZ, color = "WZ to MZ")) +
    geom_smooth(aes(y = EZtoWZ, color = "EZ to WZ")) +
    geom_smooth(aes(y = CZtoEZ, color = "CZ to EZ")) +
    geom_smooth(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("royalblue", "orange", "green", "violet")) +
    #scale_fill_brewer(palette="RdYlGn") +
    theme_fivethirtyeight()
  options(warn = 0)
  ggtree
}

showdata <- function(cc.data){
  for (i in unique(cc.data$Tree)) {
    grid.arrange(ggtree(i))
    grid.arrange(ggflux1(i), ggflux2(i), ggflux3(i), ggflux4(i), ncol = 2, nrow = 2)
    grid.arrange(ggfluxsmoothcrop(i))
  }
}


