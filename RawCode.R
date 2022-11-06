#Get Tidyverse for the ggplot Analysis
library(tidyverse)
# Read in the survey data
survey = read.csv("CoffeeStudy.csv")
## Size of data
dim(survey)
## classification of data
str(survey)
## First Plot
ggplot(survey, aes(x = Would.you.consider.yourself.an.active.coffee.drinker...Drink.Coffee.at.least.once.a.Week.)) +
  geom_bar(color="blue", fill="steelblue") +              
  xlab("Do you drink Coffee at least once a week") +
  ylab("Frequency") + 
  ggtitle("Count of active Coffee drinkers vs non-active Coffee Drinkers") + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic" ),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="darkred", size=12, face="bold")
  )

# Rename column where names is "Sepal.Length"
names(survey)[names(survey) == "Are.you.a.University.Student."] <- "isStudent"
names(survey)[names(survey) == "How.many.years.have.you.already.been.at.University."] <- "universityYears"
names(survey)[names(survey) == "How.old.are.you"] <- "age"
names(survey)[names(survey) == "How.many.hours.do.you.sleep.per.night.on.average."] <- "sleepHours"
names(survey)[names(survey) == "How.would.you.consider.your.sleep.quality."] <- "sleepQuality"
names(survey)[names(survey) == "Would.you.consider.yourself.an.active.coffee.drinker...Drink.Coffee.at.least.once.a.Week."] <- "activeCoffeeDrinker"
names(survey)[names(survey) == "What.sort.of.Coffee.do.you.drink.at.least.once.a.Week."] <- "coffeeSort"
names(survey)[names(survey) == "On.Average..How.many.Coffees.do.you.drink.a.week."] <- "coffeeAverage"
names(survey)[names(survey) == "How.many.years.have.you.drank.coffee.at.least.once.a.week."] <- "yearsCoffee"
names(survey)[names(survey) == "Where.do.you.usually.get.your.coffee.from"] <- "coffeeSource"
names(survey)[names(survey) == "On.What.times.of.the.day.do.you.usually.drink.your.coffee."] <- "coffeTimes"

#Drop all empty value rows out of the table and reassign it to the survey table
survey <- na.omit(survey)

## Relation between coffeeAverage and Sleephours
ggplot(survey, aes(x = coffeeAverage, y = sleepHours)) +
  geom_point(shape=23, fill="blue", color="darkred", size=2) +               
  xlab("Average coffee count per Week") +
  ylab("Hours of Sleep per night") + 
  ggtitle("Relationship between sleep hours per night and average coffee Consumption") + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic" ),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="darkred", size=12, face="bold")
  )

### Fitting the model
#Fit a model for Count of Coffee per Week to sleephours
fit <- lm(coffeeAverage~ sleepHours, data = survey)
fit # print
summary(fit)

### Correlation
cor(survey$coffeeAverage,survey$sleepHours)

### Resid vs Fitted
# we use plot() on the `fit` object 
plot(fit, which = 1) 

### Resid vs Fitted - ggplot
ggplot(fit, aes(x = .fitted, y = .resid)) +
  geom_point(shape=23, fill="blue", color="darkred", size=2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  ggtitle("Resid vs Fitted") + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic" ),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="darkred", size=12, face="bold")
  )

### Integrated
ggplot(survey, aes(x = coffeeAverage, y = sleepHours)) +
  geom_point(shape=23, fill="blue", color="darkred", size=2) +                
  geom_smooth(method = "lm") +  # *NEW* add linear regression model
  xlab("Count of Coffee per Week") +
  ylab("Hours of sleep per night") + 
  ggtitle("Relationship between average Coffee consumption and hours of sleep per night") + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic" ),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="darkred", size=12, face="bold")
  )

# Quick look at top 5 rows of data
ggplot(survey, aes(x = coffeeAverage, y = sleepQuality)) +
  geom_point(shape=23, fill="blue", color="darkred", size=2) +                # then, generate a scatter plot layer, PLUS...
  # it would be a good idea to further improve the aesthetics of the plot
  xlab("Count of Coffee per Week") +
  ylab("Quality of sleep from 0 to 10") + 
  ggtitle("Relationship between sleep Quality per night and average coffee Consumption") + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic" ),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="darkred", size=12, face="bold")
  )

#Fit a model for Count of Coffee per Week to sleepQuality
fit <- lm(coffeeAverage~ sleepQuality, data = survey)
fit # print
summary(fit)


cor(survey$coffeeAverage, survey$sleepQuality) #

# we use plot() on the `fit` object 
plot(fit, which = 1) 

## Fitted vs residuals
ggplot(fit, aes(x = .fitted, y = .resid)) +
  geom_point(shape=23, fill="blue", color="darkred", size=2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  ggtitle("Resid vs Fitted") + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic" ),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="darkred", size=12, face="bold")
  )

ggplot(survey, aes(x = coffeeAverage, y = sleepQuality)) +
  geom_point(shape=23, fill="blue", color="darkred", size=2) +                
  geom_smooth(method = "lm") +  # *NEW* add linear regression model
  xlab("Count of Coffee per Week") +
  ylab("Quality of Sleep per night") + 
  ggtitle("Relationship between average Coffee consumption and quality of sleep") + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic" ),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="darkred", size=12, face="bold")
  )

