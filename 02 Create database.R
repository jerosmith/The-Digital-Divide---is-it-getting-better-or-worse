# CREATE DATABASE
# 2 sec

# Packages
library(data.table)
library(openxlsx)
library(sqldf)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
path.data = "../Data/2 Origin csv/World Bank/"
path.metadata = "../Data/6 Metadata/"
path.db = "../Databases/"
path.paper = "../Fase 7/Data/"
file.metadata = "Metadata P07.xlsx"
file.db = "WorldBank_Database"
file.rank = "DB_World_Ranking"
pk = c("country", "year")
Pk = c("Country", "Year")
pop.min = 100000
year.rank = 2021 # Year for ranking table

# Load metadata
df_country = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Country", na.strings = "#")

# Create primary key
df_db = data.frame(country = character(), year = integer())
files = list.files(path.data, pattern = "*.csv")
nf = length(files)
for (i in 1:nf){
  print(paste0(round(i/nf*100,0), "%"))
  df = fread(paste0(path.data, files[i]), sep = "|", dec = ".", select = pk)
  df_db = rbind(df_db, df)
  df_db = unique(df_db)
}

# Add variables, one in each column
for (i in 1:nf){
  print(paste0(round(i/nf*100,0), "%"))
  variable.name = gsub(".csv", "", files[i], fixed = T)
  variable.code = unlist(strsplit(variable.name, split = " - "))[1]
  df = fread(paste0(path.data, files[i]), sep = "|", dec = ".", select = c(pk, variable.code))
  df_db = merge(df_db, df)
}

# Clean db dataframe
df_db$country = gsub(";", ",", df_db$country, fixed = T) # Remove semicolon (;) which would break the csv file.
df_db$country = gsub("|", "", df_db$country, fixed = T) # Remove pipe (|) which would break the csv file.
names(df_db)[names(df_db) %in% pk] = Pk # Format pk column names
df_db$IT.NET.BBND.P2 = df_db$IT.NET.BBND.P2/100
df_db$IT.NET.USER.ZS = df_db$IT.NET.USER.ZS/100
df_db$SE.ADT.LITR.ZS = df_db$SE.ADT.LITR.ZS/100

# Get country ISO 3166 codes and regions for plotting.
# Remove territories that are not countries
df_db = merge(df_db, df_country)
df_db$NUTS0.Code = NULL

# Correct some country names
df_db[df_db$Country=="Viet Nam", "Country"] = "Vietnam"
df_db[df_db$Country=="Korea, Rep.", "Country"] = "South Korea"
df_db[df_db$Country=="Korea, Dem. People's Rep.", "Country"] = "North Korea"

# Change World Bank variable names to my more understandable names
names(df_db)[names(df_db) == "IT.NET.USER.ZS"] = "Internet.Use"
names(df_db)[names(df_db) == "IT.NET.BBND.P2"] = "Broadband.Access"
names(df_db)[names(df_db) == "SP.POP.TOTL"] = "Population"
names(df_db)[names(df_db) == "SE.ADT.LITR.ZS"] = "Literacy"

# Calculate digital adoption variable
df_db$Digital.Adoption = sqrt(df_db$Internet.Use*df_db$Broadband.Access)

# Order columns and rows
df_db = df_db[, c("Country", "Country.Code", "Region", "Year", "Population", "Literacy", "Internet.Use", "Broadband.Access", "Digital.Adoption")]
df_db = df_db[order(Digital.Adoption, decreasing = T), ]

# Create ranking table for inclusion in paper
df_rank = as.data.frame(df_db[df_db$Year==year.rank, ])
nr = nrow(df_rank)
df_rank$Rank = 1:nr
df_rank$Region = NULL
df_rank$Literacy = NULL
df_rank$Year = NULL
nc = ncol(df_rank)
df_rank = df_rank[, c(nc, 1:(nc-1))]
names(df_rank) = gsub(".", " ", names(df_rank), fixed = T)

# Save database in both csv and xlsx format, and rank table in xlsx
fwrite(df_db, file = paste0(path.db, file.db, ".csv"), sep = "|", dec = ".")
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
names(df_db) = gsub(".", " ", names(df_db), fixed = T)
write.xlsx(df_db, file = paste0(path.db, file.db, ".xlsx"), firstRow = T, headerStyle = hs)
write.xlsx(df_rank, file = paste0(path.paper, file.rank, ".xlsx"), firstRow = T, headerStyle = hs)

# Show time taken
print(Sys.time()-t0)
