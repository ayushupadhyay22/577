#read the file from local drive
library(readr)
pollution_us_2000_2016 <- read_csv("C:/Users/User/Downloads/pollution_us_2000_2016.csv")
View(pollution_us_2000_2016)
#filter the data set for california
library(dplyr)
pollution_ca_2000_2016 <- filter(pollution_us_2000_2016 , State == "California")
#exploratory analysis 
summary(pollution_ca_2000_2016)
plot(pollution_ca_2000_2016$`NO2 1st Max Value` , pollution_ca_2000_2016$`NO2 Mean`)
pollution_ca_2000_2016 <- na.omit(pollution_ca_2000_2016)
View(pollution_ca_2000_2016)
pollution_us_2000_2016 <- na.omit(pollution_us_2000_2016)
View(pollution_us_2000_2016)
#save the data set into local drive
write.csv(pollution_ca_2000_2016 , "pollution_ca_2000_2016_edited")
write.csv(pollution_us_2000_2016 , "pollution_us_2000_2016_edited")
#histogram of pollutants
hist(pollution_ca_2000_2016$`NO2 Mean`)
hist(pollution_ca_2000_2016$`SO2 Mean`)
hist(pollution_ca_2000_2016$`O3 Mean`)
hist(pollution_ca_2000_2016$`CO Mean`)
str(pollution_us_2000_2016)
#aggregate pollution_us_2000_2016 yearly and state wise
columns <- c(5 , 6 , 7 , 8 , 9 , 11 , 16 , 21 , 26)
pollution_us_2000_2016_year <- pollution_us_2000_2016[,columns  ]
library(dplyr)
library(lubridate)
pollution_us_2000_2016_year <- pollution_us_2000_2016_year %>% 
  group_by(year = floor_date(`Date Local`, "year"),State )%>%
  summarise(`NO2 Mean` = mean(`NO2 Mean`) ,`O3 Mean` = mean(`O3 Mean`) , `SO2 Mean` = mean(`SO2 Mean`) , `CO Mean` = mean(`CO Mean`) )
#plot the No2 pollutant concentration on a heat map
library(ggplot2)
library(maps)
all_states <- map_data("state")
str(all_states)
all_states$region
state_map <- merge(pollution_us_2000_2016_year , all_states)
ggplot(state_map , aes(x = long , y= lat , group = group,  fill = `NO2 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)
#download gganimate package
library(devtools)
library(RCurl)
library(httr)
set_config( config( ssl_verifypeer = 0L ) )
devtools::install_github("dgrtwo/gganimate", force = TRUE)

#animate the concetration of No2 yearwise
library(ggplot2)
library(gganimate)
pNO2 <- ggplot(state_map , aes(x = long , y= lat , group = group, frame = year,  fill = `NO2 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pNO2 , interval = 1.0 , "output.gif")


#plot the o3 pollutant concentration on a heat map
library(ggplot2)
library(maps)
all_states <- map_data("state")
state_map <- merge(pollution_us_2000_2016_year , all_states)
ggplot(state_map , aes(x = long , y= lat , group = group,  fill = `O3 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)
#animate the concetration of o3 yearwise
library(ggplot2)
library(gganimate)
pO3 <- ggplot(state_map , aes(x = long , y= lat , group = group, frame = year,  fill = `O3 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pO3 , interval = 1.0 , "output.gif")


#plot the SO2 pollutant concentration on a heat map
library(ggplot2)
library(maps)
all_states <- map_data("state")
state_map <- merge(pollution_us_2000_2016_year , all_states)
ggplot(state_map , aes(x = long , y= lat , group = group,  fill = `SO2 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)

#animate the concetration of So2 yearwise
library(ggplot2)
library(gganimate)
pSO2 <- ggplot(state_map , aes(x = long , y= lat , group = group, frame = year,  fill = `SO2 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pSO2 , interval = 1.0 , "output.gif")

#plot the CO pollutant concentration on a heat map
library(ggplot2)
library(maps)
all_states <- map_data("state")
state_map <- merge(pollution_us_2000_2016_year , all_states)
ggplot(state_map , aes(x = long , y= lat , group = group,  fill = `CO Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)

#animate the concetration of Co yearwise
library(ggplot2)
library(gganimate)
pCO <- ggplot(state_map , aes(x = long , y= lat , group = group, frame = year,  fill = `CO Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pCO , interval = 1.0 , "output.gif")


#aggregate pollution_ca_2000_2016 yearly and state wise
columns <- c(5 , 6 , 7 , 8 , 9 , 11 , 16 , 21 , 26)
pollution_ca_2000_2016_year <- pollution_ca_2000_2016[,columns  ]
library(dplyr)
library(lubridate)
pollution_ca_2000_2016_year <- pollution_ca_2000_2016_year %>% 
  group_by(year = floor_date(`Date Local`, "year"),County )%>%
  summarise(`NO2 Mean` = mean(`NO2 Mean`) ,`O3 Mean` = mean(`O3 Mean`) , `SO2 Mean` = mean(`SO2 Mean`) , `CO Mean` = mean(`CO Mean`) )
write.csv(pollution_ca_2000_2016_year , "pollution_ca_2000_2016_year")
#plot california NO2 pollutant concentration on heat map
library(ggplot2)
library(maps)
all_county <- map_data("county")
all_county
all_county_california <-  filter(all_county , all_county$region == "california")
all_county_california
county_map_california <- merge(pollution_ca_2000_2016_year , all_county_california)
ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `NO2 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)
#animate the concetration of No2 yearwise
library(ggplot2)
library(gganimate)
pCANo2 <- ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `NO2 Mean` , frame = year))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pCANo2 , interval = 1.0 , "output.gif")


#plot california O3 pollutant concentration on heat map
library(ggplot2)
library(maps)
all_county <- map_data("county")
all_county
all_county_california <-  filter(all_county , all_county$region == "california")
all_county_california
county_map_california <- merge(pollution_ca_2000_2016_year , all_county_california)
ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `O3 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)
#animate the concetration of o3 yearwise
library(ggplot2)
library(gganimate)
pCAo3 <- ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `O3 Mean` , frame = year))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pCAo3 , interval = 1.0 , "output.gif")


#plot california SO2 pollutant concentration on heat map
library(ggplot2)
library(maps)
all_county <- map_data("county")
all_county
all_county_california <-  filter(all_county , all_county$region == "california")
all_county_california
county_map_california <- merge(pollution_ca_2000_2016_year , all_county_california)
ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `SO2 Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)
#animate the concetration of So2 yearwise
library(ggplot2)
library(gganimate)
pCASo2 <- ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `SO2 Mean` , frame = year))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pCASo2 , interval = 1.0 , "output.gif")


#plot california CO pollutant concentration on heat map
library(ggplot2)
library(maps)
all_county <- map_data("county")
all_county
all_county_california <-  filter(all_county , all_county$region == "california")
all_county_california
county_map_california <- merge(pollution_ca_2000_2016_year , all_county_california)
ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `CO Mean`))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()+
  facet_wrap(~ year)

#animate the concetration of Co yearwise
library(ggplot2)
library(gganimate)
pCACo <- ggplot(county_map_california , aes(x = long , y= lat , group = group, fill = `CO Mean` , frame = year))+
  geom_polygon(col = "white") +
  scale_fill_continuous(low="darkblue", high="red", guide="colorbar")+
  coord_map()
gganimate(pCACo , interval = 1.0 , "output.gif")

#forecasting of pollutants citywise
#aggregate california cities by month
library(dplyr)
library(lubridate)
pollution_ca_2000_2016_month <- pollution_us_2000_2016[,columns] %>% 
  filter(State == "California")%>%
  group_by(month = floor_date(`Date Local`, "month"), City)
#filter california to LA city
pollution_la_2000_2016_month <- pollution_ca_2000_2016_month %>%
  filter(City == "Los Angeles")
#creating month column
pollution_la_2000_2016_month$monthly <- format(pollution_la_2000_2016_month$month , "%B-%y")
pollution_la_2000_2016_month <- group_by(pollution_la_2000_2016_month , month)
#autoplot of pollutants concentration in la
library(ggplot2)
library(devtools)
install_github('sinhrks/ggfortify', force = TRUE)
library(ggfortify)
pollution_la_2000_2016_month <- subset(pollution_la_2000_2016_month , select = c(1,2,3,4,6,7,8,9,11))
ts_LA <- ts(pollution_la_2000_2016_month[,5:8], start = c(2000 , 1) , end = c(2015,12) ,frequency = 12)
autoplot(ts_LA, facets = TRUE)
#plot acf for all the pollutnts
library(ggplot2)
library(ggfortify)
library(forecast)
ggAcf(ts_LA[,1])+ 
  ggtitle("ACF No2 mean")

ggAcf(ts_LA[,2])+ 
  ggtitle("ACF O3 mean")

ggAcf(ts_LA[,3])+ 
  ggtitle("ACF SO2 mean")

ggAcf(ts_LA[,4])+ 
  ggtitle("ACF CO mean")

ggAcf(diff(ts_LA[,1]))
#Ljung Box test for pollutants
Box.test(diff(ts_LA[,1]) , lag = 24 , type = "Ljung")
Box.test(ts_LA[,1] , lag = 24 , type = "Ljung")
Box.test(ts_LA[,2] , lag = 24 , type = "Ljung")
Box.test(ts_LA[,3] , lag = 24 , type = "Ljung")
Box.test(ts_LA[,4] , lag = 24 , type = "Ljung")

ggseasonplot(ts_LA[,1], polar = TRUE)

#create training and test data sets
library(ggplot2)
library(ggfortify)
library(dplyr)
library(forecast)
library(timeSeries)
library(devtools)
library(fpp)
devtools::install_github("robjhyndman/forecast")

train_No2_LA <- subset(ts_LA[,1], end = length(ts_LA[,1]) - 24)
train_o3_LA <- subset(ts_LA[,2], end = length(ts_LA[,1]) - 24)
train_So2_LA <- subset(ts_LA[,3], end = length(ts_LA[,1]) - 24)
train_Co_LA <- subset(ts_LA[,4], end = length(ts_LA[,1]) - 24)

#forecasting models
#Simple Exponential Smoothening
#for No2
fses_No2_LA <-  ses(train_No2_LA  , h = 24)
accuracy(fses_No2_LA , ts_LA[,1])
checkresiduals(fses_No2_LA)
autoplot(fses_No2_LA)+  
  ggtitle("Forecast of No2 by Exponential Smoothening")
write.csv(train_No2_LA , "pollution_la_train_No2")
#for o3
fses_o3_LA <-  hw(train_o3_LA  , h = 24)
accuracy(fses_o3_LA , ts_LA[,2])
checkresiduals(fses_o3_LA)
autoplot(fses_o3_LA)+  
  ggtitle("Forecast of o3 by Exponential Smoothening")

#for So2
fses_So2_LA <-  hw(train_So2_LA  , h = 24)
accuracy(fses_So2_LA , ts_LA[,3])
checkresiduals(fses_So2_LA)
autoplot(fses_So2_LA)+  
  ggtitle("Forecast of So2 by Exponential Smoothening")

#for Co
fses_Co_LA <-  hw(train_Co_LA  , h = 24)
accuracy(fses_Co_LA , ts_LA[,4])
checkresiduals(fses_Co_LA)
autoplot(fses_Co_LA)+  
  ggtitle("Forecast of Co by Exponential Smoothening")

#Holt's Trend Method
#for No2
fholt_No2_LA <- holt(train_No2_LA , h = 24)
accuracy(fholt_No2_LA , ts_LA[,1])
checkresiduals(fholt_No2_LA)
autoplot(fholt_No2_LA)+  
  ggtitle("Forecast of No2 by Holt's Trend")

#for o3
fholt_o3_LA <- holt(train_o3_LA , h = 24)
accuracy(fholt_o3_LA , ts_LA[,2])
checkresiduals(fholt_o3_LA)
autoplot(fholt_o3_LA)+  
  ggtitle("Forecast of o3 by Holt's Trend")

#for So2
fholt_So2_LA <- holt(train_So2_LA , h = 24)
accuracy(fholt_So2_LA , ts_LA[,3])
checkresiduals(fholt_So2_LA)
autoplot(fholt_So2_LA)+  
  ggtitle("Forecast of So2 by Holt's Trend")

#for Co
fholt_Co_LA <- holt(train_Co_LA , h = 24)
accuracy(fholt_Co_LA , ts_LA[,4])
checkresiduals(fholt_Co_LA)
autoplot(fholt_Co_LA)+  
  ggtitle("Forecast of Co by Holt's Trend")


#Holt's Winter Additive Method
#For No2
fchw_No2_LA <-  hw(train_No2_LA , seasonal = "additive" , h = 24)
accuracy(fchw_No2_LA , ts_LA[,1])
checkresiduals(fchw_No2_LA)
autoplot(fchw_No2_LA) + 
  ggtitle("Forecast of No2 by Holt's Winter Additive")

#For o3
fchw_o3_LA <-  hw(train_o3_LA , seasonal = "additive" , h = 24)
accuracy(fchw_o3_LA , ts_LA[,2])
checkresiduals(fchw_o3_LA)
autoplot(fchw_o3_LA) + 
  ggtitle("Forecast of o3 by Holt's Winter Additive")

#For So2
fchw_So2_LA <-  hw(train_So2_LA , seasonal = "additive" , h = 24)
accuracy(fchw_No2_LA , ts_LA[,3])
checkresiduals(fchw_So2_LA)
autoplot(fchw_So2_LA) + 
  ggtitle("Forecast of So2 by Holt's Winter Additive")

#For Co
fchw_Co_LA <-  hw(train_Co_LA , seasonal = "additive" , h = 24)
accuracy(fchw_Co_LA , ts_LA[,4])
checkresiduals(fchw_Co_LA)
autoplot(fchw_Co_LA) + 
  ggtitle("Forecast of Co by Holt's Winter Additive")

#Holt's Winter Multiplicative Method
#For No2
fchw_No2_LA_M <-  hw(train_No2_LA , seasonal = "multiplicative" , h = 24)
accuracy(fchw_No2_LA_M , ts_LA[,1])
checkresiduals(fchw_No2_LA_M)
autoplot(fchw_No2_LA_M) + 
  ggtitle("Forecast of No2 by Holt's Winter Multiplicative")

#For o3
fchw_o3_LA_M <-  hw(train_o3_LA , seasonal = "multiplicative" , h = 24)
accuracy(fchw_o3_LA_M , ts_LA[,2])
checkresiduals(fchw_o3_LA_M)
autoplot(fchw_o3_LA_M) + 
  ggtitle("Forecast of o3 by Holt's Winter Multiplicative")

#For So2
fchw_So2_LA_M <-  hw(train_So2_LA , seasonal = "multiplicative" , h = 24)
accuracy(fchw_No2_LA_M , ts_LA[,3])
checkresiduals(fchw_So2_LA_M)
autoplot(fchw_So2_LA_M) + 
  ggtitle("Forecast of So2 by Holt's Winter Multiplicative")

#For Co
fchw_Co_LA_M <-  hw(train_Co_LA , seasonal = "multiplicative" , h = 24)
accuracy(fchw_Co_LA_M , ts_LA[,4])
checkresiduals(fchw_Co_LA_M)
autoplot(fchw_Co_LA_M) + 
  ggtitle("Forecast of Co by Holt's Winter Additive")
#ets 
#for No2
#forecasting model
ets_No2_LA <- ets(train_No2_LA) 
ets_No2_LA
#model output
forecast(ets_No2_LA ,h = 24)
checkresiduals(ets_No2_LA)
accuracy(forecast(ets_No2_LA ,h = 24) , ts_LA[,1])
autoplot(forecast(ets_No2_LA , h = 24))+
  ggtitle("Forecast of No2 by auto selection")
#for o3
#forecasting model
ets_o3_LA <- ets(train_o3_LA)
ets_o3_LA
#model output
forecast(ets_o3_LA ,h = 24)
checkresiduals(ets_o3_LA)
accuracy(forecast(ets_o3_LA ,h = 24) , ts_LA[,2])
autoplot(forecast(ets_o3_LA , h = 24))+
  ggtitle("Forecast of o3 by auto selection")

#for So2
#forecasting model
ets_So2_LA <- ets(train_So2_LA)
ets_So2_LA
#model output
forecast(ets_So2_LA ,h = 24)
checkresiduals(ets_So2_LA)
accuracy(forecast(ets_So2_LA ,h = 24) , ts_LA[,3])
autoplot(forecast(ets_So2_LA , h = 24))+
  ggtitle("Forecast of So2 by auto selection")

#for Co
#forecasting model
ets_Co_LA <- ets(train_Co_LA)
ets_Co_LA
#model output
forecast(ets_Co_LA ,h = 24)
checkresiduals(ets_Co_LA)
accuracy(forecast(ets_Co_LA ,h = 24) , ts_LA[,4])
autoplot(forecast(ets_Co_LA , h = 24))+
  ggtitle("Forecast of So2 by auto selection")
#auto arima
#for No2
#forecasting model
farima_No2_LA <-auto.arima(train_No2_LA )
farima_No2_LA
#model output
forecast(farima_No2_LA , h = 24) 
autoplot(forecast(farima_No2_LA) , h = 24)
accuracy(forecast(farima_No2_LA , h = 24) , ts_LA[,1])
checkresiduals(farima_No2_LA)

#for o3
#forecasting model
farima_o3_LA <-auto.arima(train_o3_LA )
farima_o3_LA
#model output
forecast(farima_o3_LA , h = 24) 
autoplot(forecast(farima_o3_LA) , h = 24)
accuracy(forecast(farima_o3_LA , h = 24) , ts_LA[,2])
checkresiduals(farima_o3_LA)

#for So2
#forecasting model
farima_So2_LA <-auto.arima(train_So2_LA )
farima_So2_LA
#model output
forecast(farima_So2_LA , h = 24) 
autoplot(forecast(farima_So2_LA) , h = 24)
accuracy(forecast(farima_So2_LA , h = 24) , ts_LA[,3])
checkresiduals(farima_So2_LA)

#for Co
#forecasting model
farima_Co_LA <-auto.arima(train_Co_LA )
farima_Co_LA
#model output
forecast(farima_Co_LA , h = 24) 
autoplot(forecast(farima_Co_LA) , h = 24)
accuracy(forecast(farima_Co_LA , h = 24) , ts_LA[,4])
checkresiduals(farima_Co_LA)
