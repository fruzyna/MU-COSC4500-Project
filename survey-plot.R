survey <- read.csv('rawData/survey-data.csv', row.names = 'X')

# map on world
library(ggplot2)
usa <- map_data("world")

# map each survey
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = survey, aes(x = LocationLongitude, y = LocationLatitude), color = "yellow")

# distributions
hist(survey$Poor, main = 'Distribution of Number of Poor Stations', xlab = 'Number of Poor Stations', ylab = 'Number of Participants')
hist(survey$MapClicks, main = 'Distribution of Number of Map Clicks', xlab = 'Number of Map Clicks', ylab = 'Number of Participants')
hist(survey$DropClicks, main = 'Distribution of Number of Dropdown Clicks', xlab = 'Number of Drop Clicks', ylab = 'Number of Participants')
hist(survey$Time1, main = 'Distribution of Time 1', xlab = 'Time', ylab = 'Number of Participants')
hist(survey$Time2, main = 'Distribution of Time 2', xlab = 'Time', ylab = 'Number of Participants')
hist(survey$Time3, main = 'Distribution of Time 3', xlab = 'Time', ylab = 'Number of Participants')

times <- c(survey$Time1, survey$Time2, survey$Time3)
labs <- c(rep(1, times = length(survey$Time1)), rep(2, times = length(survey$Time2)), rep(3, times = length(survey$Time3)))
alltime <- data.frame(Times = times, Labels = labs)
ggline(alltime, x = 'Labels', y = 'Times', add = c('mean_se', 'jitter'), color = '#00AFBB', main = 'Distributions of Times')

# states
barplot(table(survey$Texas), main = 'Number of Participants Who Knew Where Texas Is.', xlab = 'Knew where Texas was', ylab = 'Number of Participants')
barplot(table(survey$California), main = 'Number of Participants Who Knew Where California Is.', xlab = 'Knew where California was', ylab = 'Number of Participants')
barplot(table(survey$Alaska), main = 'Number of Participants Who Knew Where Alaska Is.', xlab = 'Knew where Alaska was', ylab = 'Number of Participants')

# handedness
hand <- table(survey$Handedness)[c('Left', 'Both', 'Right')]
barplot(hand, main = 'Handedness of Participants', xlab = 'Dominant Hand', ylab = 'Number of Participants')
side <- table(survey$Sidebar)
names(side) <- c('Left', 'Right')
barplot(side, main = 'Distribution of Sidebar', xlab = 'Side', ylab = 'Number of Participants')

# demographics
age <- table(survey$Age)[c('Under 18', '18 - 24', '25 - 34', '35 - 44', '45 - 54', '55 - 64')]
barplot(age, main = 'Age of Participants', xlab = 'Age', ylab = 'Number of Participants')
barplot(table(survey$Latino), main = 'Latino Participants', xlab = 'Latino', ylab = 'Number of Participants')
barplot(table(survey$Race), main = 'Race of Participants', xlab = 'Race', ylab = 'Number of Participants')
edu <- table(survey$Education)[c('Less than high school', 'High school graduate', 'Some college', 'Professional degree', '2 year degree', '4 year degree', 'Doctorate')]
barplot(edu, main = 'Education of Participants', xlab = 'Education Level', ylab = 'Number of Participants')
married <- table(survey$Married)[c('Never married', 'Married', 'Divorced', 'Separated', 'Widowed')]
barplot(married, main = 'Married Participants', xlab = 'Married', ylab = 'Number of Participants')
employment <- table(survey$Employment)[c('Employed full time', 'Employed part time', 'Student', 'Unemployed looking for work', 'Unemployed not looking for work', 'Disabled', 'Retired')]
barplot(employment, main = 'Employment of Participants', xlab = 'Employment Status', ylab = 'Number of Participants')
income <- table(survey$Income)[c('Less than $10,000', '$10,000 - $19,999', '$20,000 - $29,999', '$30,000 - $39,999', '$40,000 - $49,999', '$50,000 - $59,999', '$60,000 - $69,999', '$70,000 - $79,999', '$80,000 - $89,999', '$90,000 - $99,999', '$100,000 - $149,999', 'More than $150,000')]
barplot(income, main = 'Income of Participants', xlab = 'Income Level', ylab = 'Number of Participants')
