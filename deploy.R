library(rsconnect)
library(dotenv)

dotenv::load_dot_env(".env")

# Print a list of app dependencies. Libraries need to be loaded
# before publishing so deployApp() knows what is necessary.
error_on_missing_name <- function(name){
  var <- Sys.getenv(name, unset=NA)
  if(is.na(var)){
    stop(paste0("cannot find ",name),call. = FALSE)
  }
  gsub("\"", '',var)
}

# Set the account info for deployment.
setAccountInfo(name   = error_on_missing_name("SHINY_ACC_NAME"),
               token  = error_on_missing_name("TOKEN"),
               secret = error_on_missing_name("SECRET"))

# Deploy the application.
deployApp(
  appFiles = c("app.R",
              "power_analysis_functions.R",
              "www/all_ICC_samp_calc_df.csv",
              "www/all_wp_anova_df.csv",
              "www/all_wp_correlation_df.csv",
              "www/all_wp_t_df.csv",
              "www/all_wp_t_df_in.csv",
              "www/all_bin_class_samp_calc_df.csv",
              "www/all_regression_samp_calc_df.csv",
              "www/custom_v2.css"
  ),
  appName = "jmspwr",
  appTitle = "shinyapplication")