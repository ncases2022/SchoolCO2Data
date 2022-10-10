# If you need to install, run this below
#install.packages("devtools",source="binary");
#install.packages("ggplot2",source="binary");
#install.packages("lubridate",source="binary");
# these packages must load ...
library(devtools);
library(ggplot2);
library(lubridate);
library(tools);
# Open all CSV files ...  
temp = list.files(path="~/Documents/SchoolCO2Data/SchoolCO2Data",pattern="*.csv", recursive=TRUE)
for (x in 1:length(temp))
{
  file = temp[x];
  print(paste0("Opened: ",file))
  d <- read.csv(file=file);
  names(d)
  max <- ceiling((max(d$CO2)+250)/100) * 100;
  d$Time2 <- parse_date_time(d$Time,c('%H:%M:%S %p','%m/%d/%Y %H%M',"'%H:%M:%S'"))
  graph <- ggplot(d, aes(x=Time2, y=CO2)) +
    geom_line(size=1) +
    scale_y_continuous(limits=c(400,max),breaks=seq(400,3200,200)) +
    theme_bw() + xlab("Time") + ylab("CO2 (PPM)") +
    scale_color_manual(values=rev(c("#7fc97f", "#beaed4","#fdc086"))) +
    scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
    geom_hline(yintercept = 800, linetype="dotted", color = "gray", size=.5) + 
    labs(caption=file
    )
  print(graph);
  graph.file <- paste0(file_path_sans_ext(file),".png");
  print(paste0("Saved: ",graph.file))
  ggsave(graph,file=graph.file,width=6*1.3,height=3.5*1.3)
}