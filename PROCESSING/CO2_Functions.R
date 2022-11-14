# See https://github.com/Edderic/breathesafe/blob/refactor/python/notebooks/nc-alliance-for-school-equity-and-safety.ipynb
# For Basis of Calculations ...
library(ggplot2);
library(grid);
makeCO2_Graph <- function(d){
  max <- ceiling((max(d$CO2)+250)/100) * 100;
  d$Time2 <- parse_date_time(d$Time,c('%H:%M:%S %p','%m/%d/%Y %H%M',"'%H:%M:%S'"))
  if (nrow(d) > 1){
    graph <- ggplot(d, aes(x=Time2, y=CO2)) +
      geom_line(size=1) +
      scale_y_continuous(limits=c(400,max),breaks=seq(400,3200,200)) +
      theme_bw() + xlab("Time") + ylab("CO2 (PPM)") +
      scale_color_manual(values=rev(c("#7fc97f", "#beaed4","#fdc086"))) +
      scale_x_datetime(limits=c(min(d$Time2)-hours(2),max(d$Time2)),breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
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
makeCO2_Graph_2_CO2_Levels <- function(data, n.students, n.teachers, classroom.size, outsideCO2
                                    , CFMPerPerson1, CFMPerSQFt1
                                    , CFMPerPerson2, CFMPerSQFt2
                                    , COLabel1, COLabel2
                                    , Color01, Color02, Location, File
                                    ) 
{
  line01 = calculate_steady_state_co2(n.students=n.students
                             ,n.teachers=n.teachers
                             ,classroom.size=classroom.size
                             ,CFMPerPerson=CFMPerPerson1
                             ,CFMPerSQFt=CFMPerSQFt1
                             ,outsideCO2=outsideCO2);
  line02 = calculate_steady_state_co2(n.students=n.students
                                              ,n.teachers=n.teachers
                                              ,classroom.size=classroom.size
                                              ,CFMPerPerson=CFMPerPerson2
                                              ,CFMPerSQFt=CFMPerSQFt2
                                              ,outsideCO2=outsideCO2);
  d$Time2 <- parse_date_time(d$Time,c('%H:%M:%S %p','%m/%d/%Y %H%M',"'%H:%M:%S'"))
  g <- makeCO2_Graph(data);
  # Simulation Assumptions
  notes <- paste0(
    "Location: ",Location,"\n"
    ,"File: ",File,"\n"
    ,"------INPUT PARAMETERS------\n"
    ,"# Students: ",n.students,"\n"
    ,"# Teachers: ",n.teachers,"\n"
    ,"Classroom Size (sq ft): ",classroom.size,"\n"
    ,"Ambient CO2 (PPM): ",outsideCO2,"\n"
    ,"---------------------------\n"
    ,"Stady-state Target to Satisfy NC Building Code of 7.5 CFM/person for Classrooms","\n"
    ,"CFM Per Person ",CFMPerPerson2,"\n"
    ,"CFM Per Sq. Ft. ",CFMPerSQFt2,"\n"
    ,"Total CADR ",line02[[3]],"\n"
    ,"Calculated ACH ",line02[[2]],"\n"
    ,"Steady State CO2 Target: ",line02[[1]],"\n"
    ,"---------------------------\n"
    ,"Steady-state Target to Satisfy ASHRAE 62.1-2022 for Classrooms","\n"
    ,"CFM Per Person ",CFMPerPerson1,"\n"
    ,"CFM Per Sq. Ft. ",CFMPerSQFt1,"\n"
    ,"Total CADR ",line01[[3]],"\n"
    ,"Calculated ACH ",line01[[2]],"\n"
    ,"Steady State CO2 Target: ",line01[[1]],"\n"
  )
  
  g <- g + geom_hline(yintercept = line01[[1]], linetype="dotted", color = Color01, size=1) + 
    geom_hline(yintercept = line02[[1]], linetype="dotted", color = Color02, size=1) + 
    theme(plot.margin = unit(c(1,1,2,1), "cm")) + 
    theme(plot.caption = element_text(hjust = 0)) +
    geom_text(label="ASHRAE",y=line01[[1]],x=min(d$Time2)-hours(2),vjust=-1,hjust=0,size=3,  color=Color01) +
    geom_text(label="NC Building Code",y=line02[[1]],x=min(d$Time2)-hours(2),vjust=-1,hjust=0,size=3, color=Color02) +
    #theme(axis.text = element_text(size = 4, colour="gray")) 
    labs(caption = notes)
  return(g);
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
  return(list(ssCO2=round(ssCO2,2),ACH=round(ACH,2), total_cadr=total_cadr, CADR_per_person=CADR_per_person));
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
