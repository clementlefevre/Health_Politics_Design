library(data.table)
library(openxlsx)
library(dplyr)

DT <- fread("unzip -p ../data/input/NCOV.zip")
DT[, sum(is.na(.SD)),  .SDcols =names(DT)]
colSums(!is.na(DT))



count_UNK_percent <- function(x){
  ratio <- sum(tolower(x)=="unk")/(sum(!is.na(x)))
  return (round(ratio*100,2))
}
DT_count_UNK_percent <- aggregate(.~CountryName, data=DT,FUN =count_UNK_percent)
setDT(DT_count_UNK_percent)
cols_to_sort <-  names(DT_count_UNK_percent)
cols_to_sort <-  paste0(cols_to_sort, "_UNK_percent")
names(DT_count_UNK_percent) <- cols_to_sort
openxlsx::write.xlsx(DT_count_UNK_percent, "../data/output/NCOV_missing_data_UNK.xlsx")


count_NULL_percent <- function(x){
  ratio <- sum(tolower(x)=="null")/(sum(!is.na(x)))
  return (round(ratio*100,2))
}
DT_count_NULL_percent <- aggregate(.~CountryName, data=DT,FUN =count_NULL_percent)
setDT(DT_count_NULL_percent)
cols_to_sort <-  names(DT_count_NULL_percent)
cols_to_sort <-  paste0(cols_to_sort, "_NULL_percent")
names(DT_count_NULL_percent) <- cols_to_sort
openxlsx::write.xlsx(DT_count_NULL_percent, "../data/output/NCOV_missing_data_NULL.xlsx")

