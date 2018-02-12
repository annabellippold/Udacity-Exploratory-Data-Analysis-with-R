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

################################## Price Histograms with Facet and Color #########################################
# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')
ggplot(aes(x = price, fill = cut), data = diamonds) + 
  geom_histogram() +
  facet_wrap(~ color) +
  scale_fill_brewer(type = 'qual') +
  scale_x_log10(expression(paste(Log[10], " of Price"))) +
  ylab("Count")


###################################### Price vs. Table Colored by Cut ###########################################
# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')
ggplot(aes(x = table, y = price, color = cut), data = diamonds) + 
  geom_point() +
  scale_x_continuous(breaks = seq(50, 80, 2),
                     limits = c(50, 80)) 
  

################################### Price vs. Volume and Diamond Clarity ########################################
# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')
ggplot(subset(diamonds2, volume <= quantile(volume, 0.99) & volume > 0 ), aes(x = volume, y = price, color = clarity)) + 
  geom_jitter(size = 1) + 
  scale_y_log10() +
  scale_color_brewer(type = 'div') + 
  theme_minimal()



################################### Proportion of Friendships Initiated ########################################
# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

pf <- read.csv('H:/Udacity/Nanodegree Data Analyst/Exploratory Data Analysis - R/Skript - R Basic/pseudo_facebook.tsv', sep = '\t')

names(fb)
# Output: 
#[1] "userid"                "age"                   "dob_day"               "dob_year"              "dob_month"            
#[6] "gender"                "tenure"                "friend_count"          "friendships_initiated" "likes"                
#[11] "likes_received"        "mobile_likes"          "mobile_likes_received" "www_likes"             "www_likes_received"   

suppressMessages(library (dplyr))
pf <- pf %>%
  mutate(prop_initiated = friendships_initiated/friend_count)



######################################## prop_initiated vs. tenure ############################################
# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

# The plot should look something like this.
# http://i.imgur.com/vNjPtDh.jpg
# OR this
# http://i.imgur.com/IBN1ufQ.jpg

pf <- pf %>%
  mutate(year_joined = floor(2014 - tenure/365),
         year_joined_bucket = cut(year_joined, breaks=c(2004, 2009, 2011, 2012, 2014))) 

## Adding in 0's for the mean function instead of leaving out NA's gives a better approximation 
## for the proportion of friendships initiated for this plot.

fb2 <- pf %>%
  mutate(prop_initiated = ifelse(friend_count > 0, friendships_initiated/friend_count, 0))

ggplot(subset(fb2, tenure > 0), aes(x=tenure, y=prop_initiated)) +
  geom_line(aes(color=year_joined_bucket), stat='summary', fun.y=mean) + 
  theme_minimal()


#################################### Smoothing prop_initiated vs. tenure ########################################
# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

# There won't be a solution image for this exercise.
# You will answer some questions about your plot in
# the next two exercises.

ggplot(subset(pf, tenure > 0), aes(x=tenure, y=prop_initiated)) +
  geom_smooth(aes(color = year_joined_bucket))



#################################### Largest Group Mean prop_initiated ########################################
## For the group with the largest proportion of 
## friendships initated, what is the group's average 
## (mean) proportion on friendships initiated?

pf %>%
  filter(year_joined_bucket == "(2012,2014]") %>%
  summarise(avg = mean(prop_initiated, na.rm=TRUE))

#        avg
#  0.6653892


################################## Price/Carat Binned, Faceted, & Colored #####################################
# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

# The plot should look something like this.
# http://i.imgur.com/YzbWkHT.jpg.

# Note: In the link, a color palette of type
# 'div' was used to color the histogram using
# scale_color_brewer(type = 'div')

ggplot(diamonds, aes(x = cut, y = price/carat, color = color)) + 
  geom_jitter() + 
  facet_wrap(~clarity) + 
  scale_color_brewer(type = 'div')






