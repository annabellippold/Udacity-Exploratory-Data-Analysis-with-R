statesdata <- read.csv('H:/Udacity/Nanodegree Data Analyst/Exploratory Data Analysis - R/Skript - R Basic/stateData.csv')
print(statesdata)

subset(statesdata, state.region == 1)
# gives all data with the region 1

statesdata[statesdata$state.region == 1, ]
#dataset[Row, Column]