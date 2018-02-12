library(ggplot2) #must load the ggplot package first
data(diamonds) #loads the diamonds data set since it comes with the ggplot package
summary(diamonds)

# carat               cut        color        clarity          depth           table           price             x         
# Min.   :0.2000   Fair     : 1610   D: 6775   SI1    :13065   Min.   :43.00   Min.   :43.00   Min.   :  326   Min.   : 0.000  
# 1st Qu.:0.4000   Good     : 4906   E: 9797   VS2    :12258   1st Qu.:61.00   1st Qu.:56.00   1st Qu.:  950   1st Qu.: 4.710  
# Median :0.7000   Very Good:12082   F: 9542   SI2    : 9194   Median :61.80   Median :57.00   Median : 2401   Median : 5.700  
# Mean   :0.7979   Premium  :13791   G:11292   VS1    : 8171   Mean   :61.75   Mean   :57.46   Mean   : 3933   Mean   : 5.731  
# 3rd Qu.:1.0400   Ideal    :21551   H: 8304   VVS2   : 5066   3rd Qu.:62.50   3rd Qu.:59.00   3rd Qu.: 5324   3rd Qu.: 6.540  
# Max.   :5.0100                     I: 5422   VVS1   : 3655   Max.   :79.00   Max.   :95.00   Max.   :18823   Max.   :10.740  
# J: 2808   (Other): 2531                                                                   
# y                z         
# Min.   : 0.000   Min.   : 0.000  
# 1st Qu.: 4.720   1st Qu.: 2.910  
# Median : 5.710   Median : 3.530  
# Mean   : 5.735   Mean   : 3.539  
# 3rd Qu.: 6.540   3rd Qu.: 4.040  
# Max.   :58.900   Max.   :31.800  

# gives Information about the data set
?diamonds

# to find out how many ordered factors can be
str(diamonds)
# Classes 'tbl_df', 'tbl' and 'data.frame':	53940 obs. of  10 variables:
# $ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
# $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
# $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
# $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
# $ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
# $ table  : num  55 61 65 58 58 57 57 55 61 61 ...
# $ price  : int  326 326 327 334 335 336 336 337 337 338 ...
# $ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
# $ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
# $ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

################################## Price vs. x #########################################
# In this problem set, you'll continue
# to explore the diamonds data set.

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.

# This assignment is not graded and
# will be marked as correct when you submit.

ggplot(data = diamonds, aes(x = x, y = price)) + 
  geom_point(alpha = 1/20) +
  coord_cartesian(xlim = c(3.5, 11)) + 
  scale_x_continuous(breaks = seq(3.5, 11, 1)) + 
  scale_y_continuous(breaks = seq(2000, 20000, 2000))


################################## Correlation #########################################
# What is the correlation between price and x?
with(diamonds, cor.test(price, x))
# o.8844352

# What is the correlation between price and y?
with(diamonds, cor.test(price, y))
# 0.8654209

# What is the correlation between price and z?
with(diamonds, cor.test(price, z))
# 0.8612494


################################ price vs. depth #######################################
# Create a simple scatter plot of price vs depth.

# This assignment is not graded and
# will be marked as correct when you submit.

ggplot(data = diamonds, aes(x = depth, y = price))+
   geom_point(alpha = 1/20) 


######################## Adjustments price vs. depth ###################################
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.

# This assignment is not graded and
# will be marked as correct when you submit.

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) + 
  scale_x_continuous(breaks = seq(min(diamonds$depth), max(diamonds$depth), 2),
                     labels = seq(min(diamonds$depth), max(diamonds$depth), 2))


######################## Correlation - price and depth #################################
#What is the correlation of depth vs. price?
with(diamonds, cor.test(price, depth))
# -0.0106474 


################################ price vs. carat #######################################
# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

# This assignment is not graded and
# will be marked as correct when you submit.

ggplot(data = diamonds, aes(x = carat, y = price))+
  geom_point(alpha=1/20) +
  scale_x_continuous(limits=c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(breaks=seq(0, 18000, 2000), 
                     limits=c(0 , quantile(diamonds$price, 0.99)))



############################### price vs. volume ######################################
# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

# Don't make any adjustments to the plot just yet.

volume <- (diamonds$x * diamonds$y * diamonds$z)

ggplot(data = diamonds, aes(x = volume, y = price))+
  geom_point()


############################ Correlations on Subsets ##################################
# What is the corrleation of price and volume?
# Exclude diamonds that have a volume of 0 or that are greater than or equal to 800.

diamonds2 <- diamonds %>%
  mutate(volume=x*y*z)

with(subset(diamonds2, !(volume == 0 | volume >= 800) ), cor.test(price, volume))


######################### Adjustments - price vs. volume ##############################
# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

# We encourage you to think about this next question and
# to post your thoughts in the discussion section.

# Do you think this would be a useful model to estimate
# the price of diamonds? Why or why not?

# This assignment is not graded and
# will be marked as correct when you submit.

smaller <- diamonds2 %>%
  filter(volume != 0,
         volume <= 800)

ggplot(smaller, aes( x = volume, y = price)) + 
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm",se = TRUE)


############################## Mean Price by Clarity ##################################
# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.

# This assignment WILL BE automatically
# graded!

# DO NOT ALTER THE NEXT THREE LINES OF CODE.
# ======================================================
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
data(diamonds)

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(clarity_mean_price = mean(price),
            clarity_median_price = median(price),
            clarity_min_price = min(price),
            clarity_max_price = max(price),
            n = n()) %>%
  arrange( clarity)

head(diamondsByClarity, 20)



############################## Bar Charts of Mean Price ##################################
# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

# This assignment is not graded and
# will be marked as correct when you submit.

# See the Instructor Notes for more info on bar charts
# and for a hint on this task.

# DO NOT DELETE THE LINES OF CODE BELOW
# ===================================================================
data(diamonds)
library(dplyr)
library(gridExtra)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

bar1 <- ggplot(diamonds_mp_by_clarity, aes(x = clarity, y = mean_price, fill = clarity))+
  geom_bar(stat = "identity", color = "black") 

bar2 <- ggplot(diamonds_mp_by_color, aes(x=color, y = mean_price, fill=color))+
  geom_bar(stat = "identity", color = "black")

grid.arrange(bar1, bar2)



############################## Gapminder Revisited ##################################
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 3 or you can start fresh and choose a different
# data set from Gapminder.

# If you're feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots









