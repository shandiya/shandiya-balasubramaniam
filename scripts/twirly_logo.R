library(ggplot2)
library(magrittr)
library(here)

twirly_logo <- function(n, x_1, y_1, a, b) {
  
  # create empty vectors
  x <- vector(mode = "double", length = n)
  y <- vector(mode = "double", length = n)
  
  # starting values
  x[1] <- x_1
  y[1] <- y_1
  
  # populate vectors
  for (i in 2:n) {
    
    x[i] <- a*cos(x[i-1]) + 1 - b*sin(y[i-1])
    y[i] <- a*sin(x[i-1]) - 1 + b*cos(y[i-1])
    
  }
  
  # create data frame
  data.frame(x = x, y = y)
  
}

twirly_logo(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 1.4, b = 2.2) %>% 
  ggplot() + 
  geom_point(aes(x, y), shape = 16, alpha = 0.05, size = 0.05) + 
  theme_void() +
  ggsave(here("images", "logo.png"), height = 1, width = 1, units = "in")
