survey <- read.csv('rawData/survey-data.csv', row.names = 'X')

library(dplyr)
library(ggpubr)

survey$StartDate <- as.Date(survey$StartDate)
survey$EndDate <- as.Date(survey$EndDate)

#
# Look at map clicks
#
group_by(survey, Age) %>%
  summarize(
    count = n(),
    mean = mean(MapClicks, na.rm = TRUE),
    sd = sd(MapClicks, na.rm = TRUE)
  )
clicks <- survey[survey$MapClicks <= 20,]
ggboxplot(clicks, x = 'Age', y = 'MapClicks', ylab = 'Clicks on Map', xlab = 'Age')
ggboxplot(clicks, x = 'Gender', y = 'MapClicks', ylab = 'Clicks on Map', xlab = 'Age')
ggboxplot(clicks, x = 'Education', y = 'MapClicks', ylab = 'Clicks on Map', xlab = 'Age')
ggboxplot(clicks, x = 'Education', y = 'DropClicks', ylab = 'Clicks on Map', xlab = 'Age')

#
# Look at drop clicks
#
# Drop clicks and age
group_by(survey, Age) %>%
  summarize(
    count = n(),
    mean = mean(DropClicks, na.rm = TRUE),
    sd = sd(DropClicks, na.rm = TRUE)
  )
ggboxplot(survey, x = 'Age', y = 'DropClicks', ylab = 'Clicks on Dropdown', xlab = 'Age')

dropAge <- aov(DropClicks ~ Age, data = clicks)
summary(dropAge)
TukeyHSD(dropAge)
pairwise.t.test(clicks$DropClicks, clicks$Age, p.adjust.method = 'BH')
plot(dropAge, 1)
plot(dropAge, 2)
shapiro.test(x = residuals(object = dropAge))

# Map clicks and age
mapAge <- aov(MapClicks ~ Age, data = clicks)
summary(mapAge)

# Drop clicks and map clicks
ggplot(clicks, aes(MapClicks, DropClicks), na.rm=TRUE) + geom_point()
dropMap <- aov(DropClicks ~ MapClicks, data = clicks)

# Sidebar and drop clicks
ggboxplot(survey, x = 'Sidebar', y = 'DropClicks', ylab = 'Clicks on Dropdown', xlab = 'Sidebar Side')

#
# Look at handedness and sidebar
#
times <- survey
times <- times[!grepl("Both", times$Handedness),]
timesb <- times[times$StartDate <= as.Date('2019-04-14'),]
timesa <- times[times$StartDate > as.Date('2019-04-14'),]
summary(timesb$Time1)
summary(timesa$Time1)
summary(timesb$Time2)
summary(timesa$Time2)
summary(timesb$Time3)
summary(timesa$Time3)
# use times after because it appears a difference was made
times <- timesa

#times <- times[times$StartDate > as.Date('2019-04-14'),]
ggboxplot(times, x = 'Handedness', y = 'Time1', color = 'Sidebar', ylab = 'Time to Click', xlab = 'Dominant Hand')
ggboxplot(times, x = 'Handedness', y = 'Time2', color = 'Sidebar', ylab = 'Time to Click', xlab = 'Dominant Hand')
ggboxplot(times, x = 'Handedness', y = 'Time3', color = 'Sidebar', ylab = 'Time to Click', xlab = 'Dominant Hand')
ggboxplot(times, x = 'Handedness', y = 'Total', color = 'Sidebar', ylab = 'Time to Click', xlab = 'Dominant Hand')

alltimes <- c(survey$Time1, survey$Time2, survey$Time3)
timelabs <- c(rep(1, times = length(survey$Time1)), rep(2, times = length(survey$Time2)), rep(3, times = length(survey$Time3)))
timetab <- data.frame(Times = alltimes, Labels = timelabs)
ggboxplot(timetab, x = 'Labels', y = 'Times', color = '#00AFBB', main = 'Distributions of Times')

group_by(survey, Age) %>%
  summarize(
    count = n(),
    mean = mean(DropClicks, na.rm = TRUE),
    sd = sd(DropClicks, na.rm = TRUE)
  )

sideHand1 <- aov(Time1 ~ Handedness * Sidebar, data = times)
summary(sideHand1)

sideHand2 <- aov(Time2 ~ Handedness * Sidebar, data = times)
summary(sideHand2)

sideHand3 <- aov(Time3 ~ Handedness * Sidebar, data = times)
summary(sideHand3)

sideHandT <- aov(Total ~ Handedness * Sidebar, data = times)
summary(sideHandT)

side3 <- aov(Time3 ~ Sidebar, data = times)
summary(side3)
TukeyHSD(side3)
plot(side3, 1)
plot(side3, 2)
shapiro.test(x = residuals(object = side3))

#
# Look at poor stations
#
ggboxplot(survey, x = 'Age', y = 'Poor', color = 'Gender', ylab = 'Poor Stations', xlab = 'Age')
ggboxplot(survey, x = 'Education', y = 'Poor', ylab = 'Poor Stations', xlab = 'Education')

pooredu <- aov(Poor ~ Education, data = survey)
summary(pooredu)

#
# Screwing with MTurk
#
ai <- table(survey[,as.vector(c('Age', 'Income'))])
table() %>%
  as.data.frame() %>%
  ggplot() +
  aes(x=Age, y=Income, fill=Freq) %>%
  geom_tile()