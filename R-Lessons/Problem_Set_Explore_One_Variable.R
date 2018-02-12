################################## Quiz Diamonds #########################################
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


################################## Price Histogram #########################################
# Create a histogram of the price of
# all the diamonds in the diamond data set.

price_histogram <- ggplot(aes(x = price), data = diamonds)+
  geom_histogram(binwidth = 1000, color = 'black', fill = '#00ced1')+
  scale_x_continuous(breaks = seq(0, 20000, 1000))
print(price_histogram)


################################## Diamonds Count ##########################################
# less 500$
sum(diamonds$price < 500)
# 1729

# less 250$
sum(diamonds$price < 250)
# 0

# greater 15000$
sum(diamonds$price >= 15000)
# 1656


################################## Cheaper Diamonds ########################################
# Explore the largest peak in the
# price histogram you created earlier.

# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.

# There won't be a solution video for this
# question so go to the discussions to
# share your thoughts and discover
# what other people find.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Submit your final code when you are ready.

price_histogram <- ggplot(aes(x = price), data = diamonds)+
  geom_histogram(binwidth = 1, color = 'black', fill = '#00ced1')+
  scale_x_continuous(lim = c(0, 5000), breaks = seq(0, 5000, 500))
print(price_histogram)
  
# The most diamonds cost between 500 and 1500$. These are more than 20000
#For diamonds that cost less than $2,000, the most common price of a diamond is around 
#$700 with the mode being $605 (binwidth = 1). 


############################### Price by Cut Histogram #####################################
# Break out the histogram of diamond prices by cut.

# You should have five histograms in separate
# panels on your resulting plot.
price_histogram <- ggplot(aes(x = price), data = diamonds)+
  geom_histogram(binwidth = 1000, color = 'black', fill = '#00ced1') +
  scale_x_continuous(breaks = seq(0, 20000, 1000)) +
  facet_grid(cut ~ .)
print(price_histogram)


################################### Price by Cut  ##########################################
# In the two last exercises, we looked at
# the distribution for diamonds by cut.

# Run the code below in R Studio to generate
# the histogram as a reminder.

# ===============================================================

#qplot(x = price, data = diamonds) + facet_wrap(~cut)

# ===============================================================

# In the last exercise, we looked at the summary statistics
# for diamond price by cut. If we look at the output table, the
# the median and quartiles are reasonably close to each other.

# diamonds$cut: Fair
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     337    2050    3282    4359    5206   18570 
# ------------------------------------------------------------------------ 
# diamonds$cut: Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     327    1145    3050    3929    5028   18790 
# ------------------------------------------------------------------------ 
# diamonds$cut: Very Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     336     912    2648    3982    5373   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Premium
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326    1046    3185    4584    6296   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Ideal
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326     878    1810    3458    4678   18810 

# This means the distributions should be somewhat similar,
# but the histograms we created don't show that.

# The 'Fair' and 'Good' diamonds appear to have 
# different distributions compared to the better
# cut diamonds. They seem somewhat uniform
# on the left with long tails on the right.

# Let's look in to this more.

# Look up the documentation for facet_wrap in R Studio.
# Then, scroll back up and add a parameter to facet_wrap so that
# the y-axis in the histograms is not fixed. You want the y-axis to
# be different for each histogram.

# If you want a hint, check out the Instructor Notes.
price_histogram <- ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 1000, color = 'black', fill = '#00ced1') +
  scale_x_continuous(breaks = seq(0, 20000, 1000)) +
  facet_grid(cut ~ ., scales="free", space="free")
print(price_histogram)


############################# Price per Carat by Cut ####################################
# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

# Adjust the bin width and transform the scale
# of the x-axis using log10.

# Submit your final code when you are ready.
price_carat_histogram <- ggplot(diamonds, aes(x = price/carat)) + 
  geom_histogram(color = "black", fill = '#00ced1', binwidth = .05) + 
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_log10(expression(paste(Log[10], " of Price")),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  facet_grid(cut~., scale = "free") + ylab("Count")
print(price_carat_histogram)


################################# Price Box Plot #######################################
# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

# There won't be a solution video for this
# exercise so go to the discussion thread for either
# BOXPLOTS BY CLARITY, BOXPLOT BY COLOR, or BOXPLOTS BY CUT
# to share you thoughts and to
# see what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
ggplot(diamonds, aes(x = clarity, y = price, color = cut)) + 
  geom_boxplot() + 
  facet_grid(color~., margins = TRUE) 


############################ Interqurtile Range - IQR ###################################
?diamonds
# a. What is the price range for the middle 50% of diamonds with color D?
quantile(subset(diamonds, color == "D")$price)
#0%     25%     50%     75%    100% 
#357.0   911.0  1838.0  4213.5 18693.0

# b. What is the price range for the middle 50% of diamonds with color J?
quantile(subset(diamonds, color == "J")$price)
#0%     25%     50%     75%    100% 
#335.0  1860.5  4234.0  7695.0 18710.0 

# c. What is the IQR for Diamonds with the best color?
IQR(subset(diamonds, color == "D")$price)
#3302.5

# d. What is the IQR for Diamonds with the worst color?
IQR(subset(diamonds, color == "J")$price)
#5834.5


########################## Price per Carat Box Plots by Color ###########################
# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

# Go to the discussions to
# share your thoughts and to discover
# what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
ggplot(diamonds, aes(x = color, y = price/carat, fill = color)) + 
  geom_boxplot() +
  #coord_cartesian(ylim=c(1000, 6000))+
  scale_y_continuous() + 
  xlab("Color") + ylab("Price per Carat")


############################### Carat Frequency Polygon ################################
sizes = c(0.1, 0.3, 0.8, 1.01, 1.6, 2.0, 3.0, 5.0)

summary(diamonds$carat)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.200   0.400   0.700   0.798   1.040   5.010

ggplot(diamonds, aes(x=carat)) + 
  geom_freqpoly(binwidth = 0.1, alpha = 0.75) +
  scale_x_continuous(breaks=sizes, expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  geom_hline(yintercept = 2000, color = 'red') +
  geom_vline(xintercept = c(0.1, 0.8, 1.6, 2.0, 3.0, 5.0), color = "red", linetype="dashed", alpha = 0.75) +
  geom_vline(xintercept = c(0.3, 1.01), color = "forestgreen", linetype = "twodash") +
  xlab("Carat Size") + ylab("Count")


################################### Gapminder Data ####################################
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.

# You might use a simple histogram, a boxplot split over a categorical variable,
# or a frequency polygon. The choice is yours!

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.



########################## Exploring your Friends Birthday ############################
# Your task is to investigate the distribution of your friends'
# birth months and days.

# Here some questions you could answer, and we hope you think of others.

# **********************************************************************

# How many people share your birthday? Do you know them?
# (Reserve time with them or save money to buy them a gift!)

# Which month contains the most number of birthdays?

# How many birthdays are in each month?

# Which day of the year has the most number of birthdays?

# Do you have at least 365 friends that have birthdays on everyday
# of the year?

# **********************************************************************

# You will need to do some data munging and additional research to
# complete this task. This task won't be easy, and you may encounter some
# unexpected challenges along the way. We hope you learn a lot from it though.

# You can expect to spend 30 min or more on this task depending if you
# use the provided data or obtain your personal data. We also encourage you
# to use the lubridate package for working with dates. Read over the documentation
# in RStudio and search for examples online if you need help.

# You'll need to export your Facebooks friends' birthdays to a csv file.
# You may need to create a calendar of your Facebook friends' birthdays
# in a program like Outlook or Gmail and then export the calendar as a
# csv file.

# Once you load the data into R Studio, you can use the strptime() function
# to extract the birth months and birth days. We recommend looking up the
# documentation for the function and finding examples online.

# We've included some links in the Instructor Notes to help get you started.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation below the line. Submit it when you are ready.
# ===============================================================================



























