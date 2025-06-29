#1
# Load libraries
library(readxl)
library(dplyr)

# Read the data
data <- read_excel("~/Desktop/income_democracy.xlsx", sheet = "Data")

# Clean data
data_clean <- data %>% filter(!is.na(dem_ind))

# Average democracy index by country
country_avg <- data_clean %>%
  group_by(country) %>%
  summarize(avg_dem = mean(dem_ind)) %>%
  arrange(desc(avg_dem))

# Top 3 countries (highest average democracy index)
top_3 <- country_avg %>% slice_max(order_by = avg_dem, n = 3, with_ties = FALSE)

# Bottom 3 countries (lowest average democracy index)
bottom_3 <- country_avg %>% slice_min(order_by = avg_dem, n = 3, with_ties = FALSE)

# Overall average democracy index
overall_avg <- mean(data_clean$dem_ind, na.rm = TRUE)

# Print results
print(top_3)
print(bottom_3)
print(overall_avg)

#c
# Load libraries
library(readxl)
library(dplyr)
library(sandwich)
library(lmtest)
library(stargazer)

# Load the data
data <- read_excel("~/Desktop/income_democracy.xlsx", sheet = "Data")

# Clean the data
data_clean <- data %>%
  filter(!is.na(dem_ind), !is.na(log_gdppc), !is.na(country))

# Run pooled OLS
model_pooled <- lm(dem_ind ~ log_gdppc, data = data_clean)

# Clustered + heteroskedasticity-robust (HC3) standard errors
cluster_hc3_se <- vcovCL(model_pooled, cluster = ~country, type = "HC3")

# Extract overall R²
overall_r2 <- summary(model_pooled)$r.squared

# Stargazer output
stargazer(model_pooled, type = "text",
          se = list(sqrt(diag(cluster_hc3_se))),
          title = "Pooled OLS with Clustered (Country) and HC3 Robust Standard Errors",
          dep.var.labels = "Democracy Index",
          covariate.labels = "Log GDP per Capita",
          digits = 3,
          no.space = TRUE,
          add.lines = list(c("Overall R-squared", round(overall_r2, 3))))

#f)
# Load libraries
library(readxl)
library(dplyr)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)

# Read data
data <- read_excel("~/Desktop/income_democracy.xlsx", sheet = "Data")

# Clean data
data_clean <- data %>%
  filter(!is.na(dem_ind), !is.na(log_gdppc), !is.na(country), !is.na(year))

# Set up pdata.frame for plm
pdata <- pdata.frame(data_clean, index = c("country", "year"))

# Run country fixed effects model
model_country_fe <- plm(dem_ind ~ log_gdppc,
                        data = pdata,
                        model = "within",
                        effect = "individual")

# Clustered + HC3 robust SEs
clustered_se <- sqrt(diag(vcovHC(model_country_fe, type = "HC3", cluster = "group")))

# Extract within R²
r2_within <- round(summary(model_country_fe)$r.squared["rsq"], 3)

# Manually compute overall R²
y <- data_clean$dem_ind
yhat <- model_country_fe$model[[1]] + model_country_fe$residuals  # fitted values
ssr <- sum(model_country_fe$residuals^2)
sst <- sum((y - mean(y))^2)
r2_overall <- round(1 - ssr / sst, 3)

# Output in stargazer
stargazer(model_country_fe, type = "text",
          se = list(clustered_se),
          title = "Country Fixed Effects (Clustered HC3 SE)",
          dep.var.labels = "Democracy Index",
          covariate.labels = "Log GDP per Capita",
          digits = 3,
          no.space = TRUE,
          add.lines = list(c("Within R-squared", r2_within),
                           c("Overall R-squared", r2_overall)))


#two way fixed effect
# Load libraries
library(readxl)
library(dplyr)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)

# Load and clean data
data <- read_excel("~/Desktop/income_democracy.xlsx", sheet = "Data")

data_clean <- data %>%
  filter(!is.na(dem_ind), !is.na(log_gdppc), !is.na(country), !is.na(year))

# Convert to panel data format
pdata <- pdata.frame(data_clean, index = c("country", "year"))

# Two-way fixed effects regression
model_twfe <- plm(dem_ind ~ log_gdppc,
                  data = pdata,
                  model = "within",
                  effect = "twoways")

# Clustered and heteroskedasticity-robust SEs (clustered by country)
clustered_se_twfe <- sqrt(diag(vcovHC(model_twfe, type = "HC3", cluster = "group")))

# Extract within R²
r2_within_twfe <- round(summary(model_twfe)$r.squared["rsq"], 3)

# Compute overall R² manually
y <- data_clean$dem_ind
yhat <- model_twfe$model[[1]] + model_twfe$residuals
ssr <- sum(model_twfe$residuals^2)
sst <- sum((y - mean(y))^2)
r2_overall_twfe <- round(1 - ssr / sst, 3)

# Output using stargazer
stargazer(model_twfe, type = "text",
          se = list(clustered_se_twfe),
          title = "Two-Way Fixed Effects (Country + Year) with Clustered HC3 SE",
          dep.var.labels = "Democracy Index",
          covariate.labels = "Log GDP per Capita",
          digits = 3,
          no.space = TRUE,
          add.lines = list(c("Within R-squared", r2_within_twfe),
                           c("Overall R-squared", r2_overall_twfe)))


#g
# Load libraries
library(readxl)
library(dplyr)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)

# Load and clean data
data <- read_excel("~/Desktop/income_democracy.xlsx", sheet = "Data")

data_clean <- data %>%
  filter(!is.na(dem_ind), !is.na(log_gdppc), !is.na(country), !is.na(year),
         !is.na(age_2), !is.na(age_3), !is.na(age_4), !is.na(age_5),
         !is.na(educ), !is.na(log_pop))

# Convert to panel format
pdata <- pdata.frame(data_clean, index = c("country", "year"))

# Run two-way fixed effects regression with controls
model_twfe_controls <- plm(dem_ind ~ log_gdppc + age_2 + age_3 + age_4 + age_5 + educ + log_pop,
                           data = pdata,
                           model = "within",
                           effect = "twoways")

# Clustered HC3 robust SEs
clustered_se_controls <- sqrt(diag(vcovHC(model_twfe_controls, type = "HC3", cluster = "group")))

# Within R²
r2_within_controls <- round(summary(model_twfe_controls)$r.squared["rsq"], 3)

# Overall R² (manual)
y <- data_clean$dem_ind
yhat <- model_twfe_controls$model[[1]] + model_twfe_controls$residuals
ssr <- sum(model_twfe_controls$residuals^2)
sst <- sum((y - mean(y))^2)
r2_overall_controls <- round(1 - ssr / sst, 3)

# Display regression table
stargazer(model_twfe_controls, type = "text",
          se = list(clustered_se_controls),
          title = "Two-Way Fixed Effects with Controls",
          dep.var.labels = "Democracy Index",
          covariate.labels = c("Log GDP per Capita", "Age 2", "Age 3", "Age 4", "Age 5", "Education", "Log Population"),
          digits = 3,
          no.space = FALSE,
          add.lines = list(c("Within R-squared", r2_within_controls),
                           c("Overall R-squared", r2_overall_controls)))


#combine
# Combine all 4 models into one stargazer table
stargazer(model_pooled, model_country_fe, model_twfe, model_twfe_controls,
          type = "text",
          se = list(sqrt(diag(cluster_hc3_se)),
                    clustered_se,
                    clustered_se_twfe,
                    clustered_se_controls),
          title = "Regression Results: Democracy Index and Log GDP per Capita",
          column.labels = c("Pooled OLS", "Country FE", "TWFE", "TWFE + Controls"),
          dep.var.labels = "Democracy Index",
          covariate.labels = c("Log GDP per Capita", "Age 2", "Age 3", "Age 4", "Age 5", "Education", "Log Population"),
          digits = 3,
          no.space = FALSE,
          add.lines = list(
            c("Within R-squared", "", r2_within, r2_within_twfe, r2_within_controls),
            c("Overall R-squared", round(overall_r2, 3), r2_overall, r2_overall_twfe, r2_overall_controls)
          ))


#h
# Load libraries
library(readxl)
library(dplyr)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)

# Load and clean data, excluding Belarus
data <- read_excel("~/Desktop/income_democracy.xlsx", sheet = "Data")

data_no_belarus <- data %>%
  filter(country != "Belarus",           # Remove Belarus
         !is.na(dem_ind), 
         !is.na(log_gdppc), 
         !is.na(country), 
         !is.na(year))

# Convert to panel format
pdata_no_belarus <- pdata.frame(data_no_belarus, index = c("country", "year"))

# Run TWFE regression (country + year fixed effects)
model_twfe_nobelarus <- plm(dem_ind ~ log_gdppc,
                            data = pdata_no_belarus,
                            model = "within",
                            effect = "twoways")

# Clustered HC3 robust SEs
clustered_se_nobelarus <- sqrt(diag(vcovHC(model_twfe_nobelarus, type = "HC3", cluster = "group")))

# Within R²
r2_within_nobelarus <- round(summary(model_twfe_nobelarus)$r.squared["rsq"], 3)

# Overall R²
y <- data_no_belarus$dem_ind
yhat <- model_twfe_nobelarus$model[[1]] + model_twfe_nobelarus$residuals
ssr <- sum(model_twfe_nobelarus$residuals^2)
sst <- sum((y - mean(y))^2)
r2_overall_nobelarus <- round(1 - ssr / sst, 3)

# Output
stargazer(model_twfe_nobelarus, type = "text",
          se = list(clustered_se_nobelarus),
          title = "Two-Way Fixed Effects (Excluding Belarus)",
          dep.var.labels = "Democracy Index",
          covariate.labels = "Log GDP per Capita",
          digits = 3,
          no.space = TRUE,
          add.lines = list(c("Within R-squared", r2_within_nobelarus),
                           c("Overall R-squared", r2_overall_nobelarus)))


