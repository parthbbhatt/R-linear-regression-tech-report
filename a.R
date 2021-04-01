
data <- readxl::read_excel("Data.xlsx", sheet = "All Stores with Stores Block")

colnames(data) <- gsub(" ", "_", colnames(data))
colnames(data) <- gsub("\\(|\\)", "", colnames(data))


create_formula <- function(response, predictors) {
  return(as.formula(
    paste(
      response,
      paste(        predictors, collapse = " + "),
      sep = " ~ ")))
}

create_store_model <- function(
  response,
  predictors,
  data,
  store
) {
  f <- create_formula(response, predictors)
  model <- lm(f, data=subset(data, Store == store))
  return(model)
}

create_store_plot <- function(response, predictors, store, model) {
  f <- create_formula(response, predictors)
  
  plot(
    model, 
    main=paste(
      response,
      paste(predictors, paste(store), sep = " for "),
      sep = " ~ "))
  return()
}

create_model_and_plot <- function(response, predictors, data, store) {
  par(mfrow=c(2,2))
  model <- create_store_model(response, predictors, data=data, "Walmart")
  create_store_plot(response, predictors, store, model)
  return(model)
}

stores = c("Walmart", "Zehrs", "Metro", "No-Frills")
predictors = c("Calories_per_Z_mL", "Fat_g_per_Z_mL",
               "Sodium_mg_per_Z_mL", "Carbohydrates_g_per_Z_mL", 
               "Sugars_g_per_Z_mL", "Protein_g_per_Z_mL")

for (store in stores) {
  print(store)
  for (predictor in predictors) {
    print(predictor)
    model <- create_model_and_plot(response="Price_per_Z_mL ", predictors=c(predictor), data=data, store)
    # summary(x)
  }
}

