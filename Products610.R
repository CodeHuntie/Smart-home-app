library(RMySQL); library(dplyr); library(lubridate); library(chron); 
library(ggplot2); library(ggpubr); library(plotly);library(ggfortify); 
library(forecast); library(tidyr); library(imputeTS); library(constants)


setwd("C:\\Users\\kenne\\OneDrive\\Desktop\\Bx Ubiqum\\C3Task2-Analyze energy data")
## Create a database connection via SQL 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

yr2006<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr2007<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr2008<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr2009<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr2010<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")
#energy79 <- bind_rows(yr2007, yr2008, yr2009)
energy <- bind_rows(yr2006, yr2007, yr2008, yr2009, yr2010)

## Combine Date and Time attribute values in a new attribute column
energy<-cbind(energy,paste(energy$Date,energy$Time), stringsAsFactors=FALSE)
colnames(energy)[7] <-"DateTime"
energy <- energy[,c(ncol(energy), 1:(ncol(energy)-1))]
energy$DateTime <- as.POSIXct(energy$DateTime, " %Y-%m-%d %H:%M:%S", tz = "CET")

#rename variables 
names(energy)[names(energy) == 'Sub_metering_1'] <- 'Kitchen'
names(energy)[names(energy) == 'Sub_metering_2'] <- 'Laundry'
names(energy)[names(energy) == 'Sub_metering_3'] <- 'Temp'

#decide on this  energy <- mutate(energy, TotalSub= Kitchen+ Laundry+ Temp)
## Create "year" attribute with (library) lubridate ## reason N/A is 8 times of 180 (1h missing*3 year)
energy$year <- year(energy$DateTime)
energy$quarter <- quarter(energy$DateTime)
energy$month   <- month(energy$DateTime, label = TRUE)
energy$week    <- week(energy$DateTime)
energy$wday    <- wday(energy$DateTime) # label = TRUE  
energy$day     <- day(energy$DateTime)
energy$hour    <- hour(energy$DateTime)
energy$minute  <- minute(energy$DateTime)

# set Weekday Attributes
energy$wday <- weekdays(energy$DateTime)
energy$wday <- factor(energy$wday, 
                        levels = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

############## Finding and locate the N/A s ########### make sure to use pipeline to glue scripts together 
energy_comp <- energy %>% 
                mutate(Date = as.Date(Date)) %>% 
                complete(year, nesting(month, day, hour, minute))%>%
                mutate(Date=paste(year,month,day))%>%
          filter((ymd(Date) > ymd("2006-12-16")) & (ymd(Date) < ymd("2010-11-26")))
summary(energy_comp) # 26219 N/A is correct 
# string(char) to time == b(month abb) -> m (month ) 
energy_comp$Date <- as.Date(strptime(energy_comp$Date,format="%Y %b %d"), format="%Y-%m-%d")

#Seperate into year > for consumption 
#energy07 <- filter(energy_comp, year ==2007)
# Filling up N/A 
# energy_comp<- na_interpolation(energy_com)
#plot(energy_comp$DateTime ~ energy_comp$TotalSub) # !! Takes long time ###

# Transform to TS data in #####Global power 
#plot NA to look if fits the N/A we know 
# tsminute <- ts(energy_comp$Global_active_power, frequency=525600)
# statsNA(tsminute)
# mycol <- rgb(0, 0, 255, max = 255, alpha = 0) # colors to make sense on plotting
# plotNA.distribution(tsminute, colPoints={mycol})
# plotNA.distributionBar(tsminute, breaks = 20)
# #Let's address the N/A 
# tsmin.Comp<- na_interpolation(tsminute)
# summary(tsmin.Comp)
# plot(tsmin.Comp)
# #Predicting per minutes # data is too big to compute 
# fitmin.Comp <- tslm(tsmin.Comp ~ trend + season) 
# summary(fitmin.Comp)

###################### Real stuff #####
#Group by day 
# daily_sum <- energy %>% 
#   group_by(year,month,day) %>% 
#   summarise(Total = sum(Global_active_power),
#             Kitchen=sum(Kitchen),
#             Laundry=sum(Laundry),
#             Temp=sum(Temp)) %>% 
#   ungroup() # to make the raw data ungrouped , table stays 

#put back Datetime to have continuous x for plotting 
daily_sum_con <- energy_comp %>% 
  group_by(Date,year,month,day,wday) %>%
  summarise(Total = sum(Global_active_power),
            Kitchen=sum(Kitchen),
            Laundry=sum(Laundry),
            Temp=sum(Temp)) %>% 
  ungroup() # to make the raw data ungrouped , table stays 
summary(daily_sum_con)

# try one with the average usage 
daily_sum_week <- energy_comp %>% 
  group_by(year,month,week,wday) %>%
  summarise(Total_w = sum(Global_active_power),
            Kitchen_w =sum(Kitchen),
            Laundry_w =sum(Laundry),
            Temp_w =sum(Temp)
  ) %>% 
  ungroup() 



# filling up N/A 
tsdaily.Comp <- na_interpolation(daily_sum_con)
daily_sum_con <- na_interpolation(daily_sum_con)
# remove (85) N/A that is actually duplicates 
tsdaily.Comp  <- na.omit(tsdaily.Comp) 
daily_sum_con  <- na.omit(daily_sum_con) 

#### Ts data #start is important to define x-axis, 
#frequency to define intervals in 1 year(measure)
#[4:7] to define which columns to include or use the title to describe 
# [rows, columns] 
tsdaily_Total <- ts(tsdaily.Comp[,"Total"], frequency=365, start = c(2007, 1))
tsdaily_Kitc <- ts(tsdaily.Comp[,"Kitchen"], frequency=365, start = c(2007, 1))
tsdaily_Laun <- ts(tsdaily.Comp[,"Laundry"], frequency=365, start = c(2007, 1))
tsdaily_Temp <- ts(tsdaily.Comp[,"Temp"], frequency=365, start = c(2007, 1))

# filling up N/A 
# tsdaily.Comp<- na_interpolation(daily_sum)
# summary(tsdaily.Comp)
# plot(tsdaily.Comp)
# Forecast model - linear 

# 2007 data try out 
# fitdaily07_T <- tslm(tsdaily07_Total ~ trend + season) 
# foredaily07_T <- forecast(fitdaily07_T, h=40, level=c(80,90))
# plot(foredaily07_T, PI = TRUE, ylab= "Watt-Hours", xlab="Time", main = "Forecast Total Consumption- Daily from LM")
# summary(foredaily07_T)

# make list to be able to call them together   ## new skills could try 
#forecastModels <- list(Model1 , model2, model3)

# forecast 2011 based on 2006-2010 

fitdaily_T <- tslm(tsdaily_Total ~ trend + season) 
#foredaily_T <- forecast(fitdaily_T, h=36, level=c(80,90)) # to end 2010
foredaily_T_11 <- forecast(fitdaily_T, h=401, level=c(80,90)) # to end 2011 
#plot(foredaily_T, PI = TRUE, ylab= "Watt-Hours", xlab="Time", main = "Forecast Total Consumption- Daily from LM")

fitdaily_K <- tslm(tsdaily_Kitc ~ trend + season) 
#foredaily_K <- forecast(fitdaily_K, h=36, level=c(80,90))
foredaily_K_11 <- forecast(fitdaily_K, h=401, level=c(80,90))
#plot(foredaily_K, PI = TRUE, ylim= c(0,12000), ylab= "Watt-Hours", xlab="Time", main = "Forecast Kitchen Consumption- Daily from LM")

fitdaily_L <- tslm(tsdaily_Laun ~ trend + season) 
#foredaily_L <- forecast(fitdaily_L, h=36, level=c(80,90))
foredaily_L_11 <- forecast(fitdaily_L, h=401, level=c(80,90))
#plot(foredaily_L, PI = TRUE, ylim= c(0,14000), ylab= "Watt-Hours", xlab="Time", main = "Forecast Laundry Consumption- Daily from LM")

fitdaily_Tp <- tslm(tsdaily_Temp ~ trend + season) 
#foredaily_Tp <- forecast(fitdaily_Tp, h=36, level=c(80,90))
foredaily_Tp_11 <- forecast(fitdaily_Tp, h=401, level=c(80,90))
#plot(foredaily_Tp, PI = TRUE, ylim= c(0,26000), ylab= "Watt-Hours", xlab="Time", main = "Forecast Temperature Con. Consumption- Daily from LM")

#####    Create new data frame to work in dashboard 
# create table of forecasts 
# temp <- data_frame(
#   Date = seq.POSIXt(from = as.POSIXct("2010-11-26"),to = as.POSIXct("2010-12-31"),
#                     by = "day"), 
# ######  foredaily$mean is the forecast nested in the LIST forecast model 
#   Total = as.numeric(foredaily_T$mean), 
#   Kitchen = as.numeric(foredaily_K$mean),
#   Laundry = as.numeric(foredaily_L$mean),
#   Temp = as.numeric(foredaily_Tp$mean)
#     )
# #divide date (POSIXct) to columns 
# temp$year <- year(temp$Date)
# temp$month   <- month(temp$Date)
# temp$day     <- day(temp$Date)
# # list how columns are structured
# temp<- temp[,c(1,6,7,8,2,3,4,5)] 
# daily_total <- rbind(daily_sum_con,temp) #combine rows ## Becareful with 
# #plot.ly 
# plot(daily_total$Date, daily_total$Kitchen, type="n")
# plot_ly(daily_total, x = ~Date, y = ~Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines')

####### Data till end of 2011 
# create table of forecasts 
#seq.POSIXt(from = as.POSIXct("2010-11-26"),to = as.POSIXct("2011-12-31"),   by = "day"), 
######  foredaily$mean is the forecast nested in the LIST forecast model 
temp_11 <- data_frame(
  Date = seq.Date(from = as.Date("2010-11-26"),to = as.Date("2011-12-31"), by = "day"),
  Total = as.numeric(foredaily_T_11$mean), 
  Kitchen = as.numeric(foredaily_K_11$mean),
  Laundry = as.numeric(foredaily_L_11$mean),
  Temp = as.numeric(foredaily_Tp_11$mean))
temp_11$wday = weekdays(as.Date(temp_11$Date)) # add column in the end 

#divide date to columns 
temp_11$year <- year(temp_11$Date)
temp_11$month   <- month(temp_11$Date, label = TRUE)
temp_11$day     <- day(temp_11$Date)
temp_11$Date <- as.character(temp_11$Date)
# list how columns are structured
temp_11<- temp_11[,c(1,7,8,9,6,2,3,4,5)] 
daily_total_11 <- rbind(daily_sum_con,temp_11) #combine rows ## Becareful with 

#single to plot and see 
#plot(daily_total_11$Date, daily_total_11$Total, type="n")
#plot_ly(daily_total_11, x = ~Date, y = ~Kitchen, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') #%>% 

# Weeday behavior per meter 
# 
# 
# plot_ly(daily_total_11, x = ~wday, y= ~Total, name='Total Usage/ Weekdays', type = 'bar')
plot_ly(daily_total_11, x = ~wday, y= ~Kitchen)
# plot_ly(daily_total_11, x = ~wday, y= ~Laundry)
# plot_ly(daily_total_11, x = ~wday, y= ~Temp)
# 
# plot_ly(energy, x = ~hour, y= ~Temp, type='bar')
 plot_ly(daily_total_11, x = ~wday, y= ~mean(Kitchen), type ='bar')


# Decompose to see Trend and Seasonality 
# plot (compose_tsweekly_T <- decompose(tsweekly_Total))
# plot (compose_tsweekly_K <- decompose(tsweekly_Kitc))
# plot (compose_tsweekly_L <- decompose(tsweekly_Laun))
# plot (compose_tsweekly_Tp <- decompose(tsweekly_Temp))
# Forecast model - arima - only in individaul 
# fitweekly_arima <- auto.arima(tsweekly_Total)
# foreweekly_arima_T <- forecast(fitweekly_arima, h=52, level=c(80,90))
# plot(foreweekly_arima_T)
# summary(foreweekly_arima_T)

## choose the right density - to be able to see smooth chart 

#saving to rds file to read in dashboard 
saveRDS(energy_comp, file = "energy_comp.rds") 
