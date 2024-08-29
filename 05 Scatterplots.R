# GENERATE SCATTERPLOTS
# Choose path.plots and function for t (t, log(t), sqrt(t)) in ggplot command, depending on the type of scatterplot you want.
# 57 sec

# Packages
library(data.table)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
path.db = "../Databases/"
path.plots = "../Fase 7/Plots/Scatterplots z t/"
file.db = "WorldBank_Database.csv"

# Load database and add z variable
df_db = as.data.frame(fread(paste0(path.db, file.db), header = T, sep = "|", dec = "."))
df_db$z = -log(1/df_db$Digital.Adoption - 1)
df_db = df_db[!is.infinite(df_db$z) & !is.na(df_db$z), ]
year0 = min(df_db$Year)
df_db$t = df_db$Year - year0

# Generate scatterplots of z versus t
country = sort(unique(df_db$Country))
for (c in country){
  print(c)
  df = df_db[df_db$Country==c, ]
  g = ggplot(data = df, mapping = aes(x=Year, y=z)) + geom_point() + ggtitle(c)
  suppressMessages(ggsave(filename = paste0(path.plots, c, " scatterplot.png"), plot = g))
}

# Show time taken
print(Sys.time()-t0)
