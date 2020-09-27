### CHAOS THEORY VISUALIZED#######################
### x_n+1 = rx_n(1-x_n)### Where "_" denotes a subscript 

###THIS IS THE BASIC MATH SURROUDING CHAOS THEORY
x <- 0.7
r <- 3.4
curve(r*x*(1-x))


#####LOAD THESE TO GET STARTED#############################
library(tidyverse)
library(ggplot2)
library(radiant)
library(tidyr)

growth_rates <- c(seq(from = .5, to = 4.0, by = .001))

                 
### RUN THIS CODE TO WRITE THE CHAOS FUNCTION DO NOT MODIFY
the_chaos_function <- function(r,x,t) {
  R <<- t
  chaos_df  <<- data.frame(growth_rates)
  chaos_output <- (r*x*(1-x))
  for(n in 1:t) {
    chaos_output <- (r*chaos_output*(1-chaos_output))
    print(chaos_output)
    chaos_df[n] <<- chaos_output
  }
}
###########################################################

##this passes our vector 'growth_rate' through the chaos function
the_chaos_function(growth_rates,0.6,1000)

chaos_complete <- t(chaos_df)
chaos_complete <- as.data.frame(chaos_complete)
colnames(chaos_complete) <- as_character(growth_rates)
row.names(chaos_complete) <- c(1:R)

row.names(chaos_df) <- as_character(growth_rates)
colnames(chaos_df) <- c(1:R)

#####This is as far as I got for awhile...#####
ggplot(data = chaos_complete, aes(y = chaos_complete$'1.2', x = c(1:R))) + 
  geom_point() +  labs(title = 'Chaos Theory Visualzed') +
  ggplot(data = chaos_complete, aes(y = chaos_complete$'3', x = c(1:R))) + geom_point() +
  ggplot(data = chaos_complete, aes(y = chaos_complete$'3.8', x = c(1:R))) + geom_point() 

######But then I thought about it some more#####

chaos_complete %>%
  summarize_all(last)

rate_equilibrium <- chaos_complete %>%
                      summarize_all(last)

rate_equilibrium <- data.frame(t(rate_equilibrium))

rate_equilibrium <- mutate(rate_equilibrium, 'Growth_Rate' = growth_rates)

colnames(rate_equilibrium) <- c('Equilibrium_Pop','Growth_Rate')

#############NOW TIME FOR A BETTER PLOT ######################
ggplot(rate_equilibrium, aes(x = Growth_Rate, y = Equilibrium_Pop)) + geom_point()

###################################################################################
rate_equilibrium <- chaos_complete %>%
                      filter(row_number() >= (R-15))

rate_equilibrium <- t(rate_equilibrium)
rate_equilibrium <- data.frame(rate_equilibrium)

rate_equilibrium <- mutate(rate_equilibrium, 'Growth_Rate' = growth_rates)


###Use the function below to go to radiant and speed graph
save(rate_equilibrium, file = "rate_equilibrium.rda")

##########THIS PLOT WAS THE END GOAL AND TOOK HOURS TO GET TO#############
visualize(
  rate_equilibrium, 
  xvar = "Growth_Rate", 
  yvar = c("X1", "X2", "X3", "X4", "X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16"), 
  comby = TRUE, 
  type = "line", 
  nrobs = -1, 
  theme = "theme_dark", 
  base_size = 10, 
  base_family = "sans", 
  labs = list(
    title = "Chaos Theory Visualized", 
    subtitle = "The Full Visual", 
    caption = "", x = "Growth Rate", 
    y = "Equilibrium Point/Pop"
  ), 
  custom = FALSE
)

#### LET US SEE IF WE CAN  VERIFY FEIGENBAUM'S CONSTANT#####
### Keep in mind the plot above and look into the data for the bifurcations
view(rate_equilibrium)

### Notice the first three bifurcations occur at 
r1 = 2.986
r2 = 3.444
r3 = 3.542

# So our first approximation of FEIGENBAUM'S CONSTANT IS:
FEIGENBAUM = (r2 - r1)/(r3-r2)

print(FEIGENBAUM)

###CODE BELOW RECOMMENDED BY PROF. J. LEE#########
rate_eq_long <- rate_equilibrium %>% 
                    gather(key = cases, value = eq, X1:X16)

rate_eq_long %>% 
  ggplot(aes(x=Growth_Rate, y=eq, color=cases)) +
  geom_line() +
  theme_minimal() +
  labs(    title = "Chaos Theory Visualized", 
           subtitle = "The Full Visual", 
           caption = "", x = "Growth Rate", 
           y = "Equilibrium Point/Pop"
  )












