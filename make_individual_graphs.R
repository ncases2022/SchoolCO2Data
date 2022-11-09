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
for (x in 1:length(temp))
{
  file = temp[x];
  print(paste0("Opened: ",file));
  # Write CO2 Graph ...
  graph.file <- paste0(file_path_sans_ext(file),".png");
  file_exists = file.exists(graph.file);
  if(file_exists & !overwrite){
    print(paste0("Graph found, Skipping: ",graph.file))
  }
  if(!file_exists){
    d <- read.csv(file=file);
    graph <- makeCO2_Graph(d);
    print(graph);
    print(paste0("Saved: ",graph.file))
    ggsave(graph,file=graph.file,width=6*1.3,height=3.5*1.3)
  }
  # Write Rebreathed Graph
  graph.file <- paste0(file_path_sans_ext(file),"_rebeathed.png");
  file_exists = file.exists(graph.file);
  if(file_exists & !overwrite){
    print(paste0("Graph found, Skipping: ",graph.file))
  }
  if(!file_exists){
    d <- read.csv(file=file);
    graph <- makeCO2_Rebreathed_Graph(d);
    print(graph);
    print(paste0("Saved: ",graph.file))
    ggsave(graph,file=graph.file,width=6*1.3,height=3.5*1.3)
  }
}
#g <- makeCO2_Rebreathed_Graph(d);
#g;
