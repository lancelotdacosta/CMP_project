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




