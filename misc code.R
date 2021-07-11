# misc codes


library(openxlsx)
write.xlsx(data_cleaned, file = "crux_data2015-2019.xlsx",sheetName = "newData", append = FALSE)
# write.xlsx(data_cleaned_old, file = "finalout_combined.xlsx",sheetName = "oldData", append = FALSE)


