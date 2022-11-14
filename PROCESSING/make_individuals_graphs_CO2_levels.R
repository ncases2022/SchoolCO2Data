# If you need to install, run this below
#install.packages("devtools",source="binary");
#install.packages("ggplot2",source="binary");
#install.packages("lubridate",source="binary");
# these packages must load ...
source("PROCESSING/CO2_Functions.R")
library(devtools);
library(ggplot2);
library(lubridate);
library(tools);

overwrite = FALSE;
# Open all CSV files ...  
temp = list.files(path="~/Documents/SchoolCO2Data/SchoolCO2Data",pattern="*\\.csv", recursive=TRUE)

x <- 2
file <- temp[x];
graph.file <- paste0(file_path_sans_ext(file),"_ASHRAE_CODE.png");
d1 <- read.csv(file=file);
g <- makeCO2_Graph_2_CO2_Levels(d=d1,n.students=17
                           ,n.teachers=1
                           ,classroom.size=900
                           ,CFMPerPerson1=10
                           ,CFMPerSQFt1=.12
                           ,CFMPerPerson2=7.5
                           ,CFMPerSQFt2=0
                           ,outsideCO2=420
                           ,Color01="#2ca25f"
                           ,Color02="#2b8cbe"
                           ,Location="TMSA Cary",File=file)
g;
ggsave(g,file=graph.file,width=8,height=9)
