# See https://github.com/Edderic/breathesafe/blob/refactor/python/notebooks/nc-alliance-for-school-equity-and-safety.ipynb
# For Basis of Calculations ...

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
