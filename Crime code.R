library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(lubridate)
library(zoo)
library(cluster)


#deleted rows with year-2017 manually from spreadhsheet crime.csv
#merge all csv files(main.csv and 2017-2022)(Total 7 seven files combined)
df <- list.files(path = "./R project/Data/", full.names = TRUE) %>%
  lapply(read.csv) %>%
  bind_rows
Section 2. Data Pre-processing
#deleted rows with year-2017 manually from spreadhsheet crime.csv
#merge all csv files(main and from 2017-2022)
df <- list.files(path = "./R project/Data/", full.names = TRUE) %>%
  lapply(read.csv) %>%
  bind_rows

#remove latitute and longitude columns
drops <- c("Latitude","Longitude")
df <- df[, !(names(df) %in% drops)]

#count missing values in each column
colSums(is.na(df))

#remove rows with empty values in neighborhood
df <- df[!df$NEIGHBOURHOOD=="",]

#count missing values again
#rows with empty neighborhood values had all NA values in the dataset,
#so no missing values in the dataset now
colSums(is.na(df))
Section 3. Create Visualizations
##Create Visualizations

#plot crime type barplot
df2 <- df %>% group_by(TYPE) %>%   mutate(count_name_occurr = n()) 
g1<-ggplot(data=df2, aes(x=reorder(TYPE,-count_name_occurr), fill=TYPE)) +
  geom_bar(stat="count")
g1 + labs(title = "Offense vs Count", x="Offense Type", y="Number of Occurrences") + theme(axis.text.x = element_blank())


#plot crime rate from 2003 to 2022
g2 <- ggplot(df,aes(x=YEAR)) + geom_line(stat = "count", colour = "darkturquoise",size=1) + geom_point(stat = "count",colour="black",size=5) 
g2 + labs(title = "Crime Rate in Vancouver", y="Number of Occurrences")

#crime rate in vancouver by type over the years
g3 <- ggplot(data = df,aes(x=YEAR, group=NEIGHBOURHOOD, colour=NEIGHBOURHOOD)) + geom_line(stat = "count") + facet_wrap(~NEIGHBOURHOOD, scales="free")
g3 +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            strip.text = element_text(size = 14),
            axis.text = element_text( size = 14 ),
            axis.title = element_text( size = 16, face = "bold"),
            legend.key.size = unit(1, 'cm')) + labs(y="Number of Occurrences")


#top 3 dangerous neighborhoods

#group neighborhoods
neighborhd <- group_by(df, NEIGHBOURHOOD)
#count rows in each group
crime_location <- summarise(neighborhd, n=n())
#order from highest to lowest
crime_location_order <- crime_location[order(crime_location$n,decreasing = TRUE),]
#get top 3
top3_location <- head(crime_location_order,3)

#plot the barplot
ggplot(aes(x=reorder(NEIGHBOURHOOD,-n),y=n), data=top3_location) + geom_bar(stat='identity', width = 0.6) +
  geom_text(aes(label = n), stat = 'identity', data = top3_location, hjust = 0.5, vjust = -0.5, size = 5) +
  xlab('Neighbourhoods') +
  ylab('Number of Occurrences') +
  ggtitle('Neighbourhoods with Most Crimes - Top 3') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"))


#safest neighborhoods
#order from lowest to highest
crime_location_order_i <- crime_location[order(crime_location$n,decreasing = FALSE),]
least3_location <- head(crime_location_order_i,3)

#plot barplot
ggplot(aes(x=reorder(NEIGHBOURHOOD,n),y=n), data=least3_location) + geom_bar(stat='identity', width = 0.6) +
  geom_text(aes(label = n), stat = 'identity', data = least3_location, hjust = 0.5, vjust = -0.5, size = 5) +
  xlab('Neighbourhoods') +
  ylab('Number of Occurrences') +
  ggtitle('Neighbourhoods with Least Crimes - Top 3') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"))


#top crime type across neighbourhoods
type_location_group <- group_by(df,NEIGHBOURHOOD,TYPE)
type_location_group_count <- summarise(type_location_group,n=n())
type_location_group_order <- type_location_group_count[order(type_location_group_count$n,decreasing = TRUE),]
type_by_location_top20 <- head(type_location_group_order, 20)

ggplot(aes(x = NEIGHBOURHOOD, y=n, fill = TYPE), data=type_by_location_top20) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  xlab('Neighbourhood') +
  ylab('Number of Occurrences') +
  ggtitle('Crime Type vs. Neighbourhood') + theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))

#monthly average crime
crime_month_group <- group_by(df,MONTH)
crime_month_count <- summarise(crime_month_group,n=as.integer(n()/20))
crime_month_count$MONTH <- ordered(crime_month_count$MONTH)
levels(crime_month_count$MONTH) <- c('Jan', 'Feb', 'March', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')

#barplot
ggplot(aes(x=MONTH,y=n),data=crime_month_count) +
  geom_bar(stat="identity", width=0.8, fill=viridis(12)) +
  geom_text(aes(label=n), stat="identity", data = crime_month_count, hjust = -0.1, vjust = 0, size = 5)+
  coord_flip() + ylab("Number of Occurrences") + xlab("Month") + ggtitle("Average Crime by Month") +
  theme_bw() + theme(plot.title = element_text(size = 16),
                     axis.title = element_text(size = 12, face = "bold"))


#heatmap month and crime types
crime_count <- df %>% group_by(MONTH,TYPE) %>% summarise(n=n())
crime_count$MONTH <- ordered(crime_count$MONTH)
levels(crime_count$MONTH) <- c('Jan', 'Feb', 'March', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')

#heatmap

ggplot(crime_count, aes(MONTH,TYPE,fill=n)) +
  geom_tile(size=1,color="white") +
  scale_fill_viridis() +geom_text(aes(label=n),color="white") + 
  ggtitle("Crime Type by Month") +
  xlab("Month") +
  ylab("Crime Type") + labs(fill="Total")
theme(plot.title = element_text(size = 16), 
      axis.title = element_text(size = 12, face = "bold"))



#crimes by day of month
crime_count_day <- df %>% group_by(DAY) %>% summarise(n=n())
#plot
ggplot(crime_count_day,aes(x=DAY,y=n)) + geom_segment(aes(x=DAY,xend=DAY,y=0,yend=n),color="snow1", size=1) +
  geom_point(color="turquoise",size=4) + 
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Day") +
  ylab("No. of Ocurrences") + ggtitle("Total Crimes by Day of Month") + theme_dark() +
  theme(plot.title = element_text(size = 16), 
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size=12, face = "bold"))



#heatmap of days and crimetype
crime_month_year <- df %>% group_by(YEAR,MONTH) %>% summarise(n=n())
crime_month_year$MONTH <- ordered(crime_month_year$MONTH)
levels(crime_month_year$MONTH) <- c('Jan', 'Feb', 'March', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')


#plot
ggplot(crime_month_year, aes(YEAR,MONTH,fill=n)) +
  geom_tile(size=1,color="white") +
  scale_fill_viridis() +geom_text(aes(label=n),color="white") + 
  ggtitle("Total Crimes  by Month and Year") +
  ylab("Month") +
  xlab("Year") + labs(fill="Total") +
  theme(plot.title = element_text(size = 16), 
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size=12, face = "bold"))




#Total Crimes by Hour of Day 
ggplot(data=df,aes(x=HOUR)) + geom_line(stat="count", size=1.5, alpha=0.7, color ="mediumseagreen")+ggtitle('Total Crimes by Hour of Day') +
  ylab("Number of Occurences") + xlab("Hour(24-hour clock)") +
  theme_bw() +
  theme(plot.title = element_text(size = 16), 
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size=12, face = "bold"))


#Crime type by hour of day
hour_crime_type <- df %>% group_by(HOUR,TYPE) %>% summarise(n=n())
#plot
ggplot(data = hour_crime_type,aes(x=HOUR,y=n, group=TYPE)) + geom_line(aes(color=TYPE),size =1) +
  geom_point(aes(color=TYPE),size=2)  + ggtitle("Total Crimes by Type by Hour of Day") +
  xlab("Hour") + ylab("No. of Occurrences") + theme_light()

#scatterplot day and crime count
#create date column from three columns
date_crime_count= df %>%
  mutate(date = make_date(YEAR, MONTH, DAY)) %>% group_by(date) %>% summarise(n=n()) %>%
  mutate(tma = rollmean(n, k = 180, fill = NA, align = "right"))


clrs = c("Moving Average"="chartreuse1", "Count per day"="black")
#scatterplot and moving average
ggplot(date_crime_count,aes(x=date))  + 
  geom_point(aes(y=n,color="Day Count"),size=1) +
  geom_line(aes(y=tma,color="Moving Average"),size=1) +
  scale_color_manual(values=clrs) +
  labs(x = "Year",
       y = "Occurrences",
       color = "Legend")
Section 4. Data Clustering
#K-MEANS clustering

#group data by type and then by neighborhood
by_groups <- group_by(df, TYPE, NEIGHBOURHOOD)
#count number of rows in each subgroup
groups <- summarise(by_groups, n=n())
#rearrange columns
groups <- groups[c("NEIGHBOURHOOD","TYPE","n")]
#split crime types into columns
type_variable <- spread(groups, key = TYPE, value = n)


#remove categorical variables
z <- type_variable[,-c(1,1)]

#remove missing(NA) values
z <- z[complete.cases(z),]


#scaling data
m <- apply(z, 2, mean)
s <- apply(z, 2, sd)
z <- scale(z, m, s)

#find ideal number of clusters using withing groups sum of squares
gss <- (nrow(z)-1) * sum(apply(z, 2, var))
for (i in 2:10) gss[i] <- sum(kmeans(z, centers=i)$withiness)
#plot 
plot(1:10, gss, type='b', xlab='Number of Clusters', ylab='Within groups sum of squares')

#fit a kmeans model
kc <- kmeans(z,2)
formattable(kc)

#plot cluster against components results
z1 <- data.frame(z, kc$cluster)
clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis')
Section 5. Time-Series Forecasting with Prophet
Section 5.1 Create date and response dataframe
#forecasting with Prophet
library(prophet)

#create date "ds" and crime count column "y"
dataframe <- df %>%
  mutate(ds = make_date(YEAR, MONTH, DAY)) %>% group_by(ds) %>% summarise(y=n())

#tibble to dataframe
dataframe <- as.data.frame(dataframe)
Section 5.2 Fit Model and Predict
#fit prophet model on entire dataset
m <- prophet(dataframe)

#make future dates
future <- make_future_dataframe(m, periods = 365)
tail(future)

#predict on future
forecast <- predict(m, future)
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])

#plot forecast
plot(m, forecast)

#plot forecast components
prophet_plot_components(m, forecast)
Section 5.3 Cross-Validation
#cross validation
df.cv <- cross_validation(m, initial = 4015, period = 180, horizon = 365, units = "days")
formattable(head(df.cv))

#evaluate
df.p <- performance_metrics(df.cv)

head(df.p)


#plot mape
#plot_cross_validation_metric(df.cv, metric = 'mdape', rolling_window = 0.1)
