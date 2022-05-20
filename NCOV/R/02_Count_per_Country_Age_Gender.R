library(data.table)
library(openxlsx)
library(dplyr)

DT <- fread("unzip -p ../data/input/NCOV_2.zip")


cols_to_group <- c( "HealthCareWorker", "HealthcareWorkerDetails", "HCWType", "HCWSetting",
"Outcome", "PlaceOfResidence", "Precondition")


group_func <- function(column_group) {
  
  group <-  c("CountryName", "AgeGroup", "Gender", eval(column_group))
  DT_group <- DT[, .(count = .N), by = eval(group)]
  setnames(DT_group, old=eval(column_group), new="label")
  DT_group[,category:=column_group]
  
  DT_group <- DT_group[label!="NULL"]

  return(DT_group)
}


DT_all_counts <-  rbindlist(lapply(cols_to_group, group_func), fill=TRUE)

unique(DT_all_counts$label)


openxlsx::write.xlsx(DT_all_counts, "../data/output/NCOV_DT_all_counts.xlsx")