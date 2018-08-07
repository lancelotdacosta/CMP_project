library(ggplot2)
library(ggthemes)

ggtree <- function(t){
  
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  ggtree <- filter(cc.data, Tree == t) %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
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
  ggtree <- filter(cc.data, Tree == t) %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
    geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                      breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                      values = c("yellow", "orange", "green", "red")) +
    #scale_fill_brewer(palette="RdYlGn") +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}

ggfluxsmooth <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  ggtree <- filter(cc.data, Tree == t) %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
    geom_smooth(aes(y = WZtoMZ, color = "WZ to MZ")) +
    geom_smooth(aes(y = EZtoWZ, color = "EZ to WZ")) +
    geom_smooth(aes(y = CZtoEZ, color = "CZ to EZ")) +
    geom_smooth(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("royalblue", "orange", "green", "violet")) +
    #scale_fill_brewer(palette="RdYlGn") +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}

ggflux1 <- function(t){
  
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  ggtree <- filter(cc.data, Tree == t) %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
    #geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    #geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    #geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("green", "orange", "green", "red")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}
ggflux2 <- function(t){
  
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  ggtree <- filter(cc.data, Tree == t) %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
    #geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    #geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    #geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("royalblue", "orange", "green", "red")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}
ggflux3 <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  ggtree <- filter(cc.data, Tree == t) %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
    #geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    #geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    #geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("orange", "orange", "green", "red")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}
ggflux4 <- function(t){
  if(!(t %in% unique(cc.data$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in cc.data"), collapse = ' ')
    stop(error_message)
  }
  ggtree <- filter(cc.data, Tree == t) %>% ggplot(aes(x = DY)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", as.character(cc.data$Species[which(cc.data$Tree == t)[1]])), collapse= "")) +
    geom_point(aes(y = WZtoMZ, color = "WZ to MZ")) +
    #geom_point(aes(y = EZtoWZ, color = "EZ to WZ")) +
    #geom_point(aes(y = CZtoEZ, color = "CZ to EZ")) +
    #geom_point(aes(y = toCZ, color = "to CZ")) +
    scale_colour_manual("",
                        breaks = c("to CZ", "CZ to EZ", "EZ to WZ", "WZ to MZ"),
                        values = c("violet", "orange", "green", "red")) +
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
  
  subtitle <- paste(c("Species: ",
                      as.character(data$Species[1]),
                      ", Age: ", as.character(data$Age[1]), ", Diameter: ",
                      as.character(data$Diameter[1]), ", Height: ",
                      as.character(data$Height[1])), collapse= "")
  
  caption <- paste(c("Source: ", 
                     as.character(data$Site[1])," ",
                     as.character(data$Year[1])),collapse = "")
 
   if(length(unique(cc.data$RF))>1){
    caption <- paste(c("Source: daily fluxes from ", 
                       as.character(data$Site[1])," ",
                       as.character(data$Year[1])), collapse = "")
  }
  
  ########anatomical data
            anat.data <- read.csv(file = "/Users/lancelotdacosta/Desktop/Data/Anatomical data/TDG_ABR2007_Raw.csv", sep = ";")
            if(cc.data$Year[1] != anat.data$Year[1]){
              stop("Error in ggfluxsmoothcrop: Year of flux data and anatomical data do not correspond.")
            }
            anat.data.tree <- filter(anat.data, Tree == t)
            #record mean number of mature cells in anatomical data for that tree
            vector <- seq(0,0,length.out= length(unique(anat.data.tree$PathName)))
            j <- 1 # a counter
            for(p_name in unique(anat.data.tree$PathName)){
              #find the number of mature cells in that PathName and insert it in the vector
              vector[j] <- max(filter(anat.data.tree, PathName == p_name)$CellRank)
              j <- j +1 #increase counter to next position
            }
            mature_cells_tree <- mean(vector, na.rm = TRUE)
            subtitle <- paste(c(subtitle, ", Anat. data: ", as.character(round(mature_cells_tree, digits = 1))), collapse = "" )
  #############
  
  
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


pdf("ABR07fluxesmoothcrop.pdf", onefile = TRUE)

for (i in unique(cc.data$Tree)) {
  grid.arrange(ggfluxsmoothcrop(i))
}

dev.off()
