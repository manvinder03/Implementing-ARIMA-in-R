##### import data into R 
mydata <- read.table(file.choose(), header=TRUE, 
                     sep=",")
head(mydata)

##### preprocessing data

# checking data type
str(mydata)

# rename some column names for convinience 
names(mydata) <- c("country","year","sex", "age", "suicides_no", "population","suicides_per100k",
                   "country-year", "HDI_for_year","gdp_for_year","GDP_per_capita","generation")
head(mydata)

# GDP_for_year is object due to commas, so we remove the commas from GDP_for_year and print out 
# a numeric version of it
library(stringr)
newdata<-str_replace_all(mydata$gdp_for_year, pattern=",", 
                         replace ="") %>% as.numeric
age.group<-str_replace_all(mydata$age, pattern="5-14 years", 
                         replace ="05-14 years")

# replace column GDP_for_year with new version of numeric value
mydata$gdp_for_year<- newdata
mydata$age <- age.group 
head(mydata)
str(mydata)


###### Data visualization

# plot the 10 countries with the highest suicide numbers

library(dplyr)
library(ggplot2)
country_group <- group_by(mydata, country)
mydata_by_country <- summarize(country_group, 
                               sum_suicide = sum(suicides_no))
mydata_by_country <- arrange(mydata_by_country, desc(sum_suicide))

top_10 <- head(mydata_by_country, 10)
top_10

ggplot(data=top_10, aes(x=reorder(country, sum_suicide), y=sum_suicide)) + 
  geom_bar(colour="black", stat = "identity", aes(fill=country)) +
  coord_flip() + guides(fill=FALSE) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Ten countries with the highest suicide numbers (1985-2016)") +
  theme_bw() + theme(plot.title = element_text(hjust=0.5)) +
  xlab('') + ylab("Number of suicides")


# Plot the suicide death number by age 

age_group <- group_by(mydata, age, year)
mydata_by_age <- summarize(age_group, 
                               sum_suicide = sum(suicides_no))

ggplot(aes(x=year, y=sum_suicide/1000, fill=forcats::fct_rev(age)), 
       data = mydata_by_age) + 
  geom_area(colour="black", size=.2, alpha=.8) +
        theme_bw() +
        scale_x_continuous(breaks=seq(1985,2015,5)) +
        scale_y_continuous(breaks=seq(0,300,50)) +
  labs(title = "Suicide deaths number by age, World",
       subtitle = "1985 to 2016",
       x = "Year",
       y = "Number of suicide deaths in Thousands",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
        

# Plot the suicide death rate by age (per 100,000) 

mydata %>% group_by(year, age) %>% summarize(s = sum(suicides_no), 
                                             p = sum(population)) %>%
  ggplot(aes(year, (s/p)*100000, color=forcats::fct_rev(age))) + 
  geom_line(size=1) +
  scale_x_continuous(breaks=seq(1985,2015,5)) +
  scale_y_continuous(breaks=seq(0,40,5)) +
  labs(title = "Suicide death rate by age (per 100,000), World",
       subtitle = "1985 to 2016", 
       x = "Year", y = "Suicide death rate (per 100,000)", color="Age Group") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

# Plot suicide rates (per 100,000) by gender
mydata %>% group_by(year, sex) %>% summarize(s = sum(suicides_no), 
                                             p = sum(population)) %>%
  ggplot(aes(year, (s/p)*100000, color=forcats::fct_rev(sex))) + 
  geom_line(size=1) +
  scale_x_continuous(breaks=seq(1985,2015,5)) +
  scale_y_continuous(breaks=seq(0,40,5)) +
  labs(title = "Suicide death rate by gender (per 100,000), World",
       subtitle = "1985 to 2016", 
       x = "Year", y = "Suicide death rate (per 100,000)", color="Gender") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

                                                     
# Distribution of suicide rates (per 100,000) by gender and age
mydata %>% group_by(sex, age) %>% summarize(rate=mean(suicides_per100k)) %>%
  ggplot(aes(age, rate, group=sex, color=sex, shape=sex))+
  geom_line(size=.8) +
  geom_point(size=3, fill="white") +
  scale_shape_manual(values=c(22,21)) +
  scale_y_continuous(breaks=seq(0,40,5)) +
  labs(title = "Distribution of suicide rates (per 100,000) by gender and age",
       subtitle = "1985 to 2016", 
       x = "Age Group", y = "Suicide death rate (per 100,000)") +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2")

# Plot the orrelation between suicides_no and gdp_for_year
mydata %>%
  ggplot(aes(x = gdp_for_year, y = suicides_no))+
  geom_jitter(alpha = .30)+
  geom_smooth(method = 'lm' ,color = "red")
 
# Plot the number of suicides categorised by different Generation 
mydata %>% 
  ggplot(aes(x = generation , y = suicides_no , fill = generation))+
  geom_boxplot(alpha = .50)+
  coord_cartesian(ylim = c(0,500))


#################################################################################
#Yearly sucide for Russian Federation (highest no.of suicides) 

library(tseries)
library(forecast)
library(timeSeries)
library(zoo)
library(xts)
library(tsbox)
library(lubridate)
library(dplyr)

#Filtering the data required for runing the model
mydata <- mydata %>% filter(mydata$country == "Russian Federation") 
tsdata <- subset(mydata,select=c('year','suicides_no'))
tsdata <- as.ts(tsdata)
class(tsdata)
byyear <- aggregate((suicides_no)~year,
                    data=tsdata,FUN=sum)
head(byyear)

#cor(x, y = NULL, use = "everything", method ="pearson")
cor(byyear, method ="pearson")

plot(byyear)
# Smoothing the data, considering the trend from year 2000 to year 2015
adenoTS = ts(byyear)
arima_fit = auto.arima(adenoTS[,1])

arima_fit = auto.arima(adenoTS[,1], trace = TRUE)
plot.ts(adenoTS[,2])
plot.ts(arima_fit$residuals)

#validate the model
Box.test(adenoTS[,2], lag = 5, type = "Ljung-Box")
Box.test(adenoTS[,2], lag = 10, type = "Ljung-Box")
Box.test(adenoTS[,2], lag = 15, type = "Ljung-Box")

# R Linear Regression 
#X <- subset(mydata,select=c('suicides_no'))
Y <- subset(mydata,select=c('year','suicides_no'))
cor(Y)
plot(Y)

# Fit our regression model
Reg_model <- lm(suicides_no ~ year, # regression formula
                data=mydata) # data set
# Summarize and print the results
summary(Reg_model) # show regression coefficients table

confint(Reg_model)

hist(residuals(Reg_model))

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(Reg_model, which = c(1, 2)) # "which" argument optional
