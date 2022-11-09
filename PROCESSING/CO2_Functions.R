# See https://github.com/Edderic/breathesafe/blob/refactor/python/notebooks/nc-alliance-for-school-equity-and-safety.ipynb
# For Basis of Calculations ...
library(ggplot2);
makeCO2_Graph <- function(d){
  max <- ceiling((max(d$CO2)+250)/100) * 100;
  d$Time2 <- parse_date_time(d$Time,c('%H:%M:%S %p','%m/%d/%Y %H%M',"'%H:%M:%S'"))
  if (nrow(d) > 1){
    graph <- ggplot(d, aes(x=Time2, y=CO2)) +
      geom_line(size=1) +
      scale_y_continuous(limits=c(400,max),breaks=seq(400,3200,200)) +
      theme_bw() + xlab("Time") + ylab("CO2 (PPM)") +
      scale_color_manual(values=rev(c("#7fc97f", "#beaed4","#fdc086"))) +
      scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
      geom_hline(yintercept = 800, linetype="dotted", color = "gray", size=.5) + 
      labs(caption=file
      )
    return(graph);
  }
  if (nrow(d) == 1){
    graph <- ggplot(d, aes(x=Time, y=CO2)) +
      geom_point(size=5) +
      scale_y_continuous(limits=c(400,max),breaks=seq(400,3200,200)) +
      theme_bw() + xlab("Time") + ylab("CO2 (PPM)") +
      #scale_color_manual(values=rev(c("#7fc97f", "#beaed4","#fdc086"))) +
      #scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
      geom_hline(yintercept = 800, linetype="dotted", color = "gray", size=.5) + 
      labs(caption=file
      )
    return(graph);
  }
  return(NULL)
}
calculate_steady_state_co2 <- function(n.students, n.teachers, classroom.size.sqft, CFMPerPerson
                                       , CFMPerSQFt, outsideCO2=420, classroom.ceiling.ft = 10)
{
  n.total <- n.students + n.teachers;
  G_p_teachers = 0.0166 # ft3 / min / person
  G_p_students = 0.00735 # ft3 / min / person
  CFM_Space <- CFMPerSQFt * classroom.size
  print(paste("CADR for Space: ",CFM_Space))
  CFM_Person <- CFMPerPerson * n.total
  print(paste("CADR for People: ",CFM_Person))
  total_cadr = CFM_Person + CFM_Space;
  print(paste("Total CADR: ",total_cadr));
  CADR_per_person <- total_cadr / n.total
  print(paste("Total CADR/Person: ",round(CADR_per_person,2)));
  classroom.cubic <- classroom.size.sqft * classroom.ceiling.ft;
  ACH <- ((CADR_per_person * n.total) * 60) / classroom.cubic;
  print(paste0("ACH: ",ACH));
  ssCO2 <- outsideCO2 + (((n.students * G_p_students) + (n.teachers * G_p_teachers)) * 1000000) / total_cadr
  print(paste0("Steady State CO2: ", round(ssCO2)))
  return(ssCO2);
}
percent_rebreathed <- function(indoor.CO2, ambient.CO2=420){
  cbreath <- 38000;
  rebreathed.pct <- (indoor.CO2-ambient.CO2)/cbreath;
  print(paste0("Rebreathed %: ",round(rebreathed.pct*100,2)));
  return(rebreathed.pct);
}
makeCO2_Rebreathed_Graph <- function(d){
  d$rebreathed <- unlist(lapply(d$CO2,FUN=percent_rebreathed));
  max <- max(d$rebreathed)+.01;
  d$Time2 <- parse_date_time(d$Time,c('%H:%M:%S %p','%m/%d/%Y %H%M',"'%H:%M:%S'"))
  if (nrow(d) > 1){
    graph <- ggplot(d, aes(x=Time2, y=rebreathed)) +
      geom_line(size=1) +
      scale_y_continuous(limits=c(0,max),breaks=seq(0,1,.005),labels = scales::percent) +
      theme_bw() + xlab("Time") + ylab("Rebreathed %") +
      #scale_color_manual(values=rev(c("#7fc97f", "#beaed4","#fdc086"))) +
      scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
      #geom_hline(yintercept = 800, linetype="dotted", color = "gray", size=.5) + 
      labs(caption=file
      )
    return(graph);
  }
  return(NULL)
}
# Examples to match above ...
#ssCO2 <- calculate_steady_state_co2(n.students=16
#                           ,n.teachers=1
#                           ,classroom.size=800
#                           ,CFMPerPerson=10
#                           ,CFMPerSQFt=.12
#                           ,outsideCO2=420)
#ssCO2.NC <- calculate_steady_state_co2(n.students=16
#                           ,n.teachers=1
#                           ,classroom.size=800
#                           ,CFMPerPerson=7.5
#                           ,CFMPerSQFt=0
#                           ,outsideCO2=420)
#percent_rebreathed(indoor.CO2=5000)
