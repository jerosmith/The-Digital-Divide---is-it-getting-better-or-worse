# REGRESSION AND PREDICTION
# For each country, fits three regression models with data up to 2016,
# chooses the regression model with best fit, estimates the parameters,
# and makes a forecast and confidence interval.
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
path.plots = "../Fase 7/Plots/Testing/"
file.db = "WorldBank_Database.csv"
file.testing.db = "DB_World_Testing.xlsx"
file.testing.world.paper = "Paper_World_Testing.xlsx"
file.testing.mostpop.paper = "Paper_MostPop_Testing.xlsx"
min.observations = 3
year0 = 1998
yearT = 2018 # Maximum year of training data
year.prediction = 2021
year.final.plot = 2025

# Load database table, only year <= 2021 and no missing values for digital adoption
df_db = as.data.frame(fread(paste0(path.db, file.db), header = T, sep = "|", dec = "."))
df_db = df_db[df_db$Year <= year.prediction & !is.na(df_db$Digital.Adoption), ]

# Add z variable to df_db and delete countries that would have less than min.observations for training the model.
df_db$z = -log(1/df_db$Digital.Adoption - 1)
df_db = df_db[!is.infinite(df_db$z) & !is.na(df_db$z), ]
df_db$t = df_db$Year - year0
df_train = df_db[df_db$Year <= yearT, ]
obs = as.data.frame(table(df_train$Country))
country.eligible = as.character(obs[obs$Freq >= min.observations, "Var1"])
df_db = df_db[df_db$Country %in% country.eligible, ]

# Create df_pred
df_pred = df_db[df_db$Year==year.prediction, c("Country", "Population", "Digital.Adoption")]
names(df_pred)[names(df_pred)=="Digital.Adoption"] = "Real.Digital.Adoption"
df_pred$Regression.Model = NA
df_pred$R2 = NA
df_pred$a0 = NA
df_pred$a1 = NA
df_pred$Prediction.for.2021 = NA
df_pred$Conf.Interval.2021 = NA
df_pred$Error = NA
df_pred$In.Out = NA
df_pred = df_pred[order(df_pred$Country), ]

# Table for different possible regression models
df_model = data.frame(Formula = c("z ~ t", "z ~ log(t+1)", "z ~ sqrt(t)"), R2 = NA)

# Generate regression model and prediction for each country
country = sort(unique(df_db$Country))
for (c in country){
  print(c)
  
  # Get data for country
  df = df_db[df_db$Country==c, c("Year", "t", "Digital.Adoption", "z")]
  year.max = max(df$Year) # Get maximum year of data for country. Not all countries have data for year = year.prediction.
  df_train = df[df$Year <= yearT, ]
  
  # Find best model for country
  for (formula in df_model$Formula){
    mod = lm(data = df_train, formula = as.formula(formula))
    df_model[df_model$Formula==formula, "R2"] = summary(mod)$r.squared
  }
  formula = df_model[df_model$R2==max(df_model$R2), "Formula"]
  
  # With best model do regression, extract coefficients and calculate predictions
  mod = lm(data = df_train, formula = as.formula(formula))

  # Calculate predictions
  df_z = as.data.frame(predict.lm(mod, df, interval = "confidence"))
  df$z.fit = df_z$fit
  df$z.lwr = df_z$lwr
  df$z.upr = df_z$upr
  df$Digital.Adoption.Prediction = 1/(1 + exp(-df$z.fit))
  df$Digital.Adoption.Lower = 1/(1 + exp(-df$z.lwr))
  df$Digital.Adoption.Upper = 1/(1 + exp(-df$z.upr))
  ypred = df[df$Year==year.max, "Digital.Adoption.Prediction"]
  ylwr = df[df$Year==year.max, "Digital.Adoption.Lower"]
  yupr = df[df$Year==year.max, "Digital.Adoption.Upper"]
  yreal = df[df$Year==year.max, "Digital.Adoption"]
  error = abs(ypred - yreal)
  if (ylwr<=yreal & yreal<=yupr){
    in.out = "In"
  } else {
    in.out = "Out"
  }
  
  # Write coefficients and predictions in df_pred for country
  df_pred[df_pred$Country==c, "Regression.Model"] = formula
  df_pred[df_pred$Country==c, "R2"] = summary(mod)$r.squared
  df_pred[df_pred$Country==c, "a0"] = coefficients(mod)[1]
  df_pred[df_pred$Country==c, "a1"] = coefficients(mod)[2]
  df_pred[df_pred$Country==c, "Prediction.for.2021"] = ypred
  df_pred[df_pred$Country==c, "Conf.Interval.2021"] = paste0(round(ylwr*100,0), "% - ", round(yupr*100, 0), "%")
  df_pred[df_pred$Country==c, "Error"] = error
  df_pred[df_pred$Country==c, "In.Out"] = in.out
  
  # Plot of testing prediction
  df_poly1 = df[order(df$Year), c("Year", "Digital.Adoption.Upper")]
  names(df_poly1) = c("Year", "Digital.Adoption")
  df_poly2 = df[order(-df$Year), c("Year", "Digital.Adoption.Lower")]
  names(df_poly2) = c("Year", "Digital.Adoption")
  df_polygon = rbind(df_poly1, df_poly2)
  g = ggplot(data = df, mapping = aes(x=Year)) +
    ggtitle(paste("Model Test for", c)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 100, 10),"%"), limits = c(0, 1)) +
    ylab("Digital Adoption") +
    geom_point(mapping = aes(y=Digital.Adoption), color="blue") +
    geom_line(mapping = aes(y=Digital.Adoption.Prediction), color="red") +
    geom_polygon(data = df_polygon, mapping = aes(x=Year, y=Digital.Adoption), fill="red", alpha=0.1)
    suppressMessages(ggsave(filename = paste0(path.plots, c, " Testing.png")))
  
}

# Sort df_pred by country
df_pred = df_pred[order(df_pred$Country), ]

# Extract world testing table for paper. Eliminate some unnecessary columns.
df_pred_paper = df_pred
df_pred_paper$Country.Code = NULL
df_pred_paper$Region = NULL
df_pred_paper$Population = NULL
df_pred_paper$Internet.Use = NULL
df_pred_paper$Broadband.Access = NULL
df_pred_paper$`Regression Model` = NULL
df_pred_paper$R2 = NULL
df_pred_paper$a0 = NULL
df_pred_paper$a1 = NULL

# Extract 35 most populous countries prediction table for paper
df_pred_mostpop_paper = df_pred[order(df_pred$Population, decreasing = T), ]
nr = nrow(df_pred_mostpop_paper)
df_pred_mostpop_paper$Rank = 1:nr
df_pred_mostpop_paper = df_pred_mostpop_paper[df_pred_mostpop_paper$Rank <= 35, ]
df_pred_mostpop_paper = df_pred_mostpop_paper[order(df_pred_mostpop_paper$Country), ]
df_pred_mostpop_paper$Country.Code = NULL
df_pred_mostpop_paper$Region = NULL
df_pred_mostpop_paper$Population = NULL
df_pred_mostpop_paper$Internet.Use = NULL
df_pred_mostpop_paper$Broadband.Access = NULL
df_pred_mostpop_paper$`Regression Model` = NULL
df_pred_mostpop_paper$R2 = NULL
df_pred_mostpop_paper$a0 = NULL
df_pred_mostpop_paper$a1 = NULL

# Format columns
names(df_pred) = gsub(".", " ", names(df_pred), fixed = T)
names(df_pred_paper) = gsub(".", " ", names(df_pred_paper), fixed = T)
names(df_pred_mostpop_paper) = gsub(".", " ", names(df_pred_mostpop_paper), fixed = T)

# Save df_pred and df_pred_paper
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_pred, file = paste0(path.paper, file.testing.db), firstRow = T, headerStyle = hs)
write.xlsx(df_pred_paper, file = paste0(path.paper, file.testing.world.paper), firstRow = T, headerStyle = hs)
write.xlsx(df_pred_mostpop_paper, file = paste0(path.paper, file.testing.mostpop.paper), firstRow = T, headerStyle = hs)

# Show time taken
print(Sys.time()-t0)
