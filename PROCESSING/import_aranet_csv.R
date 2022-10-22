library(stringr);
library(lubridate);
library(dplyr);
library(ggplot2);

file <- "Aranet4 121AF_2022-10-21T18_47_18-0400.csv";
school_name <- "Scotts_Ridge_Elem";
target_date <- "10/21/2022"
time_start <- "8:10 AM";
time_end <- "5:00 PM";

directory <- "PROCESSING/";
timestamp_start <- mdy_hm(paste0(target_date," ",time_start))
timestamp_end <- mdy_hm(paste0(target_date," ",time_end))

d <- read.csv(file=paste0(directory,file))
names(d) <- c("Timestamp","CO2","Temp","Humidity","Pressure");

d$Time <- str_extract(d$Timestamp,"[0-9]*:[0-9]*:[0-9]* [A-Z]*")
str(d)
d$Timestamp2 = dmy_hms(d$Timestamp)

d2 <- d %>%
  filter(Timestamp2 > timestamp_start) %>%
  filter(Timestamp2 < timestamp_end) %>%
  select(Time,CO2,Temp,Humidity)

write.csv(d2,row.names = FALSE,file=paste0(directory,school_name,"_",str_replace_all(target_date,"/",""),".csv"));

#Display
d2$Time2 <- parse_date_time(d2$Time,c('%H:%M:%S %p','%m/%d/%Y %H%M',"'%H:%M:%S'"))
ggplot(d2, aes(x=Time2, y=CO2)) +
  geom_line(size=1) +
  scale_y_continuous(limits=c(400,max(d2$CO2)+200),breaks=seq(400,3200,200)) +
  theme_bw() + xlab("Time") + ylab("CO2 (PPM)") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  geom_hline(yintercept = 800, linetype="dotted", color = "gray", size=.5)
  
