# DOWNLOAD WORLD BANK DATA
# 3 min

# Packages
library(WDI)
library(stringr)
library(openxlsx)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
download.metadata = F # If TRUE, download metadata. It takes time; hence the switch.
path.metadata = "../Data/6 Metadata/"
metadata.sheet = "World Bank"
path.data = "../Data/2 Origin csv/World Bank/"
file.metadata = "WDI Metadata.xlsx"
variable.code = "SE.ADT.LITR.ZS" # World Bank variable code
year0 = 1990

# Start stopwatch
t0 = Sys.time()

# Get metadata and save in Excel file
if (download.metadata){
  df_variables = WDIbulk()[[3]]
  df_variables$Topic.1 = substr(df_variables$Topic, 1, str_locate(df_variables$Topic, ":")[,1]-1)
  names(df_variables) = gsub(".", " ", names(df_variables), fixed = T)
  nc = ncol(df_variables)
  df_variables = df_variables[, c(1, nc, 2:(nc-1))]
  hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
  write.xlsx(df_variables, file = paste0(path.metadata, file.metadata), headerStyle = hs, firstRow = T)
} else {
  df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = metadata.sheet)
}

# Get data for variable
df_data = WDI(indicator = variable.code)
df_data = df_data[df_data$year >= year0, ]

# Save metadata and data

variable.name = df_variables[df_variables$Series.Code == variable.code, "Indicator.Name"]
write.table(df_data, file = paste0(path.data, variable.code, " - ", variable.name, ".csv"), sep = "|", dec = ".", row.names = F)

# Show time taken
print(Sys.time() - t0)
