# TIME SERIES PLOT

# Packages
library(data.table)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
path.db = "../Databases/"
file.db = "WorldBank_Database.csv"
year0 = 1995
cols = c("Country", "Year", "Digital.Adoption")
countries = c("South Korea", "China", "Chile", "Vietnam", "Algeria", "Tanzania", "Congo, Dem. Rep.", "World", "Chile")
colours = c("blue", "red", "darkblue", "orange", "green", "cyan", "purple", "darkgrey")

# Load database, from year0
df_db = as.data.frame(fread(paste0(path.db, file.db), header = T, sep = "|", dec = "."))
df_db = df_db[df_db$Year >= year0, cols]

# Time series plot
g = ggplot() + xlab("Year") + ylab("Digital Adoption") +
  scale_x_continuous(breaks = seq(year0, 2025, 5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 100, 10), "%"))
nc = length(countries)
for (i in 1:nc){
  df = df_db[df_db$Country == countries[i], ]
  g = g + geom_line(data = df, mapping = aes(x=Year, y=Digital.Adoption), color=colours[i])  
  if (countries[i]=="Vietnam"){
    g = g + annotate("text", x = 2021, y = max(df$Digital.Adoption, na.rm = T) - 0.03, label = countries[i])
  } else {
    g = g + annotate("text", x = 2021, y = max(df$Digital.Adoption, na.rm = T), label = countries[i])
  }
}
g

