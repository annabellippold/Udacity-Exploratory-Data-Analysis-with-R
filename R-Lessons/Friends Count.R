fb <- read.csv('H:/Udacity/Nanodegree Data Analyst/Exploratory Data Analysis - R/Skript - R Basic/pseudo_facebook.tsv', sep = '\t')

names(fb)
# Output: 
#[1] "userid"                "age"                   "dob_day"               "dob_year"              "dob_month"            
#[6] "gender"                "tenure"                "friend_count"          "friendships_initiated" "likes"                
#[11] "likes_received"        "mobile_likes"          "mobile_likes_received" "www_likes"             "www_likes_received"   

# Create ggplot
library(ggplot2)
qplot(x = friend_count, data = fb)

# limiting axes
# My solution for Adjusting the bin with and break it through the gende and ommitting NA
ggplot(aes(x = friend_count), data = subset(fb, !is.na(gender))) +
  geom_histogram(binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_grid(gender ~ .)

table(fb$gender)
by(fb$friend_count, fb$gender, summary)
#( variable    categor. varible, function)
# Output
#fb$gender: female
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0      37      96     242     244    4923 
#------------------------------------------------------------------------------------------------------- 
#fb$gender: male
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0      27      74     165     182    4917 

#female   male 
#40254  58574
# a lot of more males than females has a profil

###################################################

#Original code:
  qplot(x = friend_count, data = fb, binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))
  
#Equivalent ggplot syntax:
  ggplot(aes(x = friend_count), data = fb) +
  geom_histogram(binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))
#In the alternate solution below, the period or dot in the formula for facet_grid() represents all of the other variables in the data set. Essentially, this notation splits up the data by gender and produces three histograms, each having their own row.

qplot(x = friend_count, data = fb) +
  facet_grid(gender ~ .)

#Equivalent ggplot syntax:
  ggplot(aes(x = friend_count), data = fb) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)
  
  
###################Color#############################
  ggplot(aes(x = tenure), data = fb) +
    geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9')
  
  ggplot(aes(x = tenure/365), data = fb) +
    geom_histogram(binwidth = .25, color = 'black', fill = '#F79420')
  
  # Quiz: 
  ggplot(aes(x = tenure/365), data = fb) +
    geom_histogram(binwidth = 1, color = 'black', fill = '#F79420')
  
  
##################Labeling######################
  ggplot(aes(x = tenure / 365), data = fb) +
    geom_histogram(color = 'black', fill = '#F79420') +
    scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) +
    xlab('Number of years using Facebook') +
    ylab('Number of users in sample')
  
###############################################
  ggplot(aes(x = age), data = fb) +
    geom_histogram(binwidth = 10, fill = '#5760AB') +
    scale_x_continuous(breaks = seq(0, 113, 5)) +
    xlab('Count of Age per Facebook Users') +
    ylab('Age of Users')

###################### Quiz: Transforming Data #####################################
  install.packages('gridExtra')
  library(gridExtra)
  
  # Creating three different plots:
  plot1 <- qplot(x = friend_count, data = fb, binwidth = 30)+
    scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100))
  print(plot1)
  plot2 <- qplot(x = log10(friend_count + 1), data = fb, binwidth = 1)
  print(plot2)
  plot3 <- qplot(x = sqrt(friend_count), data = fb, binwidth = 5)
  print(plot3)
  
  grid.arrange(plot1, plot2, plot3, ncol=1)
  
  # Alternative solution
  plot1 <- ggplot(aes(x = friend_count), data = fb) + geom_histogram()
  plot2 <- plot1 + scale_x_log10()
  plot3 <- plot1 + scale_x_sqrt()
  
  grid.arrange(plot1, plot2, plot3, ncol=1)
  
######################### Frequency Polygons ###########################  
# Ausgangsbasis:
  ggplot(aes(x = friend_count), data = subset(fb, !is.na(gender))) +
    geom_histogram(binwidth = 25) +
    scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
    facet_grid(gender ~ .)
  
# new Frequency Plot
  qplot(x = friend_count, y = ..count../sum(..count..),
        data = subset(fb, !is.na(gender)), 
        xlab = 'Friend Count',
        ylab = 'Proportion of Users with this friend count',
        binwidth = 10, geom = 'freqpoly', color = gender) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50))
  
# Quiz:
  qplot(x = www_likes, y = ..count../sum(..count..),
        data = subset(fb, !is.na(gender)), 
        xlab = 'Likes Count',
        ylab = 'Proportion of Users with this likes count',
        binwidth = 100, geom = 'freqpoly', color = gender) +
    scale_x_continuous(lim = c(0, 700), breaks = seq(0, 700, 50))

############################## Likes on the Web ##########################
  # What is the www_like count for males:
  by(fb$www_likes, fb$gender, sum)  
  
  # Solution:
  #fb$gender: female
  #[1] 3507665
  #------------------------------------------------------------------------------------------------------- 
  # fb$gender: male
  #[1] 1430175
  
  
  # Which gender has more www_likes
  # male or female
  
  # Solution:
  # male
  

  
############################## Box Plots ##########################
  qplot(x = gender, y = friend_count, 
        data = subset(fb, !is.na(gender)),
        geom = 'boxplot')
  
  # Quiz Adjustments
  qplot(x = gender, y = friend_count, 
        data = subset(fb, !is.na(gender)),
        geom = 'boxplot')+
    scale_y_continuous(lim = c(0, 1000))
  

  # Solution 1 Udacity:
  qplot(x = gender, y = friend_count, 
        data = subset(fb, !is.na(gender)),
        geom = 'boxplot', ylim = c(0, 1000))
  
  # Solution 2 Udacity (better):
  qplot(x = gender, y = friend_count, 
        data = subset(fb, !is.na(gender)),
        geom = 'boxplot')+
    scale_y_continuous(lim = c(0, 1000)) 
  
  # Solution 3 Udacity (best):
  qplot(x = gender, y = friend_count, 
        data = subset(fb, !is.na(gender)),
        geom = 'boxplot')+
    coord_cartesian(ylim = c(0, 1000)) 
  
  
############## Box Plots, Quartiles, and Friendships ###############  
by(fb$friend_count, fb$gender, summary)  
  
  #fb$gender: female
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #0      37      96     242     244    4923 
  #------------------------------------------------------------------------------------------------------- 
  #fb$gender: male
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #0      27      74     165     182    4917 
  
  # coord_cartesian should be used to match the summary output!!!
  
  # Quiz:
  by(fb$friend_count, fb$gender, mean)  
  
  #fb$gender: female
  #[1] 241.9699
  #------------------------------------------------------------------------------------------------------- 
  #  fb$gender: male
  #[1] 165.0355
 
   
######################## Getting Logical ##########################  
  
  summary(fb$mobile_likes)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #0.0     0.0     4.0   106.1    46.0 25111.0 
  
  summary(fb$mobile_likes > 0)
  #Mode   FALSE    TRUE 
  #logical   35056   63947 
  
  mobile_check_in <- NA
  fb$mobile_check_in <- ifelse(fb$mobile_likes > 0, 1, 0) 
  fb$mobile_check_in <- factor(fb$mobile_check_in) 
  summary(fb$mobile_check_in)
  #0     1 
  #35056 63947
  
  # Quiz: What percent of check is using mobile
  sum(fb$mobile_check_in == 1)/ length(fb$mobile_check_in)
  #Solution:
    #0.6459097 ~ 65%
  
  

  
  