# REGRESSION AND PREDICTION
# For each country, chooses the regression model with best fit,
# estimates the parameters and makes two forecasts.
# 50 sec

# Packages
library(openxlsx)
library(data.table)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
path.db = "../Databases/"
path.paper = "../Fase 7/Data/"
path.plots = "../Fase 7/Plots/Prediction/"
file.db = "WorldBank_Database.csv"
file.rank = "DB_World_Ranking.xlsx"
file.prediction.db = "DB_World_Prediction.xlsx"
file.prediction.world.paper = "Paper_World_Prediction.xlsx"
file.prediction.mostpop.paper = "Paper_MostPop_Prediction.xlsx"
min.observations = 3
year0 = 1998
year.rank = 2021 # Year for ranking table
year.prediction = 2030
year.final.prediction = 2100
year.final.plot = 2050
digital.adoption.target = 0.95

# Load database tables
df_db = as.data.frame(fread(paste0(path.db, file.db), header = T, sep = "|", dec = "."))
df_pred = read.xlsx(paste0(path.paper, file.rank))

# Add z variable to df_db and delete countries with less than min.observations
df_db$z = -log(1/df_db$Digital.Adoption - 1)
df_db = df_db[!is.infinite(df_db$z) & !is.na(df_db$z), ]
df_db$t = df_db$Year - year0
obs = as.data.frame(table(df_db$Country))
country.eligible = as.character(obs[obs$Freq >= min.observations, "Var1"])
df_db = df_db[df_db$Country %in% country.eligible, ]

# Add new columns to df_pred
df_pred$Regression.Model = NA
df_pred$R2 = NA
df_pred$a0 = NA
df_pred$a1 = NA
df_pred$Prediction.for.2030 = NA
df_pred$Conf.Interval.2030 = NA
df_pred$Predicted.Growth = NA
df_pred$Prediction.Year = NA
df_pred$Conf.Interval.Year = NA

# Table for different possible regression models
df_model = data.frame(Formula = c("z ~ t", "z ~ log(t+1)", "z ~ sqrt(t)"), R2 = NA)

# Generate regression model and prediction for each country
country = sort(unique(df_db$Country))
for (c in country){
  print(c)
  
  # Get data for country
  df = df_db[df_db$Country==c, c("Year", "t", "Digital.Adoption", "z")]
  
  # Find best model for country
  for (formula in df_model$Formula){
    mod = lm(data = df, formula = as.formula(formula))
    df_model[df_model$Formula==formula, "R2"] = summary(mod)$r.squared
  }
  formula = df_model[df_model$R2==max(df_model$R2), "Formula"]
  
  # With best model do regression, extract coefficients and calculate predictions
  mod = lm(data = df, formula = as.formula(formula))

  # Calculate predictions
  year1 = max(df$Year) + 1 
  df = rbind(df, data.frame(Year = year1:year.final.prediction, t = NA, Digital.Adoption = NA, z = NA))
  df$t = df$Year - year0
  df_z = as.data.frame(predict.lm(mod, df, interval = "confidence"))
  df$z.fit = df_z$fit
  df$z.lwr = df_z$lwr
  df$z.upr = df_z$upr
  df$Digital.Adoption.Prediction = 1/(1 + exp(-df$z.fit))
  df$Digital.Adoption.Lower = 1/(1 + exp(-df$z.lwr))
  df$Digital.Adoption.Upper = 1/(1 + exp(-df$z.upr))
  ypred = df[df$Year==year.prediction, "Digital.Adoption.Prediction"]
  ylwr = df[df$Year==year.prediction, "Digital.Adoption.Lower"]
  yupr = df[df$Year==year.prediction, "Digital.Adoption.Upper"]
  
  # Write coefficients and predictions in df_pred for country
  df_pred[df_pred$Country==c, "Regression.Model"] = formula
  df_pred[df_pred$Country==c, "R2"] = summary(mod)$r.squared
  df_pred[df_pred$Country==c, "a0"] = coefficients(mod)[1]
  df_pred[df_pred$Country==c, "a1"] = coefficients(mod)[2]
  df_pred[df_pred$Country==c, "Prediction.for.2030"] = ypred
  df_pred[df_pred$Country==c, "Conf.Interval.2030"] = paste0(round(ylwr*100,0), "% - ", round(yupr*100, 0), "%")
  df_pred[df_pred$Country==c, "Predicted.Growth"] = ypred - df[df$Year==year.rank, "Digital.Adoption"]
  year.digital.adoption.target = suppressWarnings(min(df[df$Digital.Adoption.Prediction >= digital.adoption.target, "Year"]))
  year.digital.adoption.lower = suppressWarnings(min(df[df$Digital.Adoption.Upper >= digital.adoption.target, "Year"]))
  year.digital.adoption.upper = suppressWarnings(min(df[df$Digital.Adoption.Lower >= digital.adoption.target, "Year"]))
  if (is.infinite(year.digital.adoption.target)){
    df_pred[df_pred$Country==c, "Prediction.Year"] = "> 2100"
  } else {
    df_pred[df_pred$Country==c, "Prediction.Year"] = year.digital.adoption.target
  }
  if (is.infinite(year.digital.adoption.lower)){
    df_pred[df_pred$Country==c, "Conf.Interval.Year"] = "> 2100"
  } else if (is.infinite(year.digital.adoption.upper)){
    df_pred[df_pred$Country==c, "Conf.Interval.Year"] = paste(">", year.digital.adoption.lower)
  } else {
    df_pred[df_pred$Country==c, "Conf.Interval.Year"] = paste(year.digital.adoption.lower, "-", year.digital.adoption.upper)
  }
  
  # Plot of prediction
  df = df[df$Year <= year.final.plot, ]
  df_poly1 = df[order(df$Year), c("Year", "Digital.Adoption.Upper")]
  names(df_poly1) = c("Year", "Digital.Adoption")
  df_poly2 = df[order(-df$Year), c("Year", "Digital.Adoption.Lower")]
  names(df_poly2) = c("Year", "Digital.Adoption")
  df_polygon = rbind(df_poly1, df_poly2)
  g = ggplot(data = df, mapping = aes(x=Year)) +
    ggtitle(paste("Forecast for", c)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 100, 10),"%"), limits = c(0, 1)) +
    ylab("Digital Adoption") +
    geom_point(mapping = aes(y=Digital.Adoption), color="blue") +
    geom_line(mapping = aes(y=Digital.Adoption.Prediction), color="red") +
    geom_polygon(data = df_polygon, mapping = aes(x=Year, y=Digital.Adoption), fill="red", alpha=0.1)
  #suppressMessages(ggsave(filename = paste0(path.plots, c, " Prediction.png")))
  
}

# Order and rank
df_pred = df_pred[order(df_pred$Prediction.for.2030, decreasing = T), ]
nr = nrow(df_pred)
df_pred$Rank = 1:nr

# Extract world prediction table for paper. Eliminate some unnecessary columns.
df_pred_paper = df_pred
df_pred_paper$Country.Code = NULL
df_pred_paper$Region = NULL
df_pred_paper$Population = NULL
df_pred_paper$Internet.Use = NULL
df_pred_paper$Broadband.Access = NULL
df_pred_paper$a0 = NULL
df_pred_paper$a1 = NULL
df_pred_paper$Prediction.for.2030 = NULL
df_pred_paper$Prediction.Year = NULL

# Extract 35 most populous countries prediction table for paper
df_pred_mostpop_paper = df_pred[order(df_pred$Population, decreasing = T), ]
nr = nrow(df_pred_mostpop_paper)
df_pred_mostpop_paper$Rank = 1:nr
df_pred_mostpop_paper = df_pred_mostpop_paper[df_pred_mostpop_paper$Rank <= 35, ]
df_pred_mostpop_paper = df_pred_mostpop_paper[order(df_pred_mostpop_paper$Prediction.for.2030, decreasing = T), ]
nr = nrow(df_pred_mostpop_paper)
df_pred_mostpop_paper$Rank = 1:nr
df_pred_mostpop_paper$Country.Code = NULL
df_pred_mostpop_paper$Region = NULL
df_pred_mostpop_paper$Population = NULL
df_pred_mostpop_paper$Internet.Use = NULL
df_pred_mostpop_paper$Broadband.Access = NULL
df_pred_mostpop_paper$a0 = NULL
df_pred_mostpop_paper$a1 = NULL
df_pred_mostpop_paper$Prediction.for.2030 = NULL
df_pred_mostpop_paper$Prediction.Year = NULL

# Format columns
names(df_pred) = gsub(".", " ", names(df_pred), fixed = T)
names(df_pred_paper) = gsub(".", " ", names(df_pred_paper), fixed = T)
names(df_pred_mostpop_paper) = gsub(".", " ", names(df_pred_mostpop_paper), fixed = T)

# Save df_pred and df_pred_paper
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_pred, file = paste0(path.paper, file.prediction.db), firstRow = T, headerStyle = hs)
write.xlsx(df_pred_paper, file = paste0(path.paper, file.prediction.world.paper), firstRow = T, headerStyle = hs)
write.xlsx(df_pred_mostpop_paper, file = paste0(path.paper, file.prediction.mostpop.paper), firstRow = T, headerStyle = hs)

# Show time taken
print(Sys.time()-t0)
