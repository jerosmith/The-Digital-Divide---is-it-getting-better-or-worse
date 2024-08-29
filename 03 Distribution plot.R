# DISTRIBUTION PLOT FOR INTERNET USE AND BROADBAND ACCESS
# Scatterplot showing world distribution of digital divide, defined by the two metrics: internet use and broadband access.
# Each bubble is a country. Its size represents its population, and its colour the region.

# Packages
library(data.table)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
path.db = "../Databases/"
file.db = "WorldBank_Database.csv"
year = 2021 # Year of data for plot

# Load database, only year of plot and without World
df_db = as.data.frame(fread(paste0(path.db, file.db), header = T, sep = "|", dec = "."))
df_db = df_db[df_db$Year==year & df_db$Country != "World", ]

ggplot(data = df_db, mapping = aes(x=Internet.Use, y=Broadband.Access, color=Region, size=Population)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(  limits = c("Sub-Saharan Africa", "Asia", "Latin America & Caribbean", "Middle East & North Africa", "Oceania", "Europe", "North America")
                     , values = c("red", "orange", "green", "yellow","cyan", "blue", "purple")
                     ) +
  scale_size_continuous(range = c(1, 30)) +
  geom_text(mapping = aes(label = Country.Code), color="black", size=2) +
  guides(size = FALSE) +
  theme(legend.position="bottom") +
  xlab("Internet Use") + ylab("Broadband Access") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 100, 10), "%")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 100, 10), "%"))

