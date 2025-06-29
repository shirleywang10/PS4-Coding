#a)
# 1. Install packages (run once only if not already installed)
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer")

# 2. Load libraries
library(readxl)
library(sandwich)
library(lmtest)
library(stargazer)

# 3. Load the data (adjust the file name/path if needed)
data <- read_excel("/Users/shirleywang/Desktop/wage_panel.xlsx")

# 4. Run the regression
model <- lm(lwage ~ union + educ + black + hisp + exper + expersq + married, data = data)

# 5. Get robust standard errors
robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))

# 6. Display the results
stargazer(model,
          type = "text",
          se = list(robust_se),
          title = "OLS Regression: log(wages) on Union and Controls",
          dep.var.labels = "log(wage)",
          covariate.labels = c("Union", "Education", "Black", "Hispanic", "Experience", "Experience²", "Married"),
          no.space = TRUE)

table(data$union)

#d)
# 1. Install required packages if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("plm", quietly = TRUE)) install.packages("plm")
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer")

# 2. Load libraries
library(readxl)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)

# 3. Load data
data <- read_excel("/Users/shirleywang/Desktop/wage_panel.xlsx")

# 4. Convert to panel data format (worker ID = 'nr', time = 'year')
pdata <- pdata.frame(data, index = c("nr", "year"))

# 5. Run fixed effects regression (within estimator)
fe_model <- plm(lwage ~ union + educ + black + hisp + exper + expersq + married, 
                data = pdata, 
                model = "within")

# 6. Calculate clustered robust SE (clustered at worker level)
cluster_se_entity <- sqrt(diag(vcovHC(fe_model, method = "arellano", type = "HC3", cluster = "group")))

# 7. Output the summary to double-check dropped variables
print(summary(fe_model))  # You should NOT see educ, black, or hisp here

# 8. Stargazer output for Fixed Effects model with robust SE
stargazer(fe_model,
          type = "text",
          se = list(cluster_se_entity),
          title = "Fixed Effects Regression: log(wage) on Union and Controls (Worker FE)",
          dep.var.labels = "log(wage)",
          covariate.labels = c("Union", "Experience", "Experience²", "Married"),
          no.space = TRUE)


#e)
# 1. Run two-way fixed effects regression
fe_tw_model <- plm(lwage ~ union + educ + black + hisp + exper + expersq + married,
                   data = pdata,
                   model = "within",
                   effect = "twoways")  # ← this adds time fixed effects too

# 2. Clustered robust SE (clustered by individual)
cluster_se_tw <- sqrt(diag(vcovHC(fe_tw_model, method = "arellano", type = "HC3", cluster = "group")))

# 3. View summary to confirm time-invariant variables are dropped
summary(fe_tw_model)

# Display two-way FE regression results in stargazer format with clustered SE
stargazer(fe_tw_model,
          type = "text",
          se = list(cluster_se_tw),  # your clustered robust SEs
          title = "Two-Way Fixed Effects Regression: log(wage) on Union and Controls",
          dep.var.labels = "log(wage)",
          covariate.labels = c("Union", "Experience²", "Married"),
          no.space = FALSE)


#f)
# Run OLS
ols_model <- lm(lwage ~ union + educ + black + hisp + exper + expersq + married, data = data)
ols_robust_se <- sqrt(diag(vcovHC(ols_model, type = "HC1")))

# Run one-way fixed effects (worker FE)
fe_model <- plm(lwage ~ union + educ + black + hisp + exper + expersq + married,
                data = pdata, model = "within")
fe_se <- sqrt(diag(vcovHC(fe_model, method = "arellano", type = "HC3", cluster = "group")))

# Run two-way fixed effects (worker + time FE)
fe_tw_model <- plm(lwage ~ union + educ + black + hisp + exper + expersq + married,
                   data = pdata, model = "within", effect = "twoways")
fe_tw_se <- sqrt(diag(vcovHC(fe_tw_model, method = "arellano", type = "HC3", cluster = "group")))

# Combine into one table
stargazer(ols_model, fe_model, fe_tw_model,
          type = "text",
          se = list(ols_robust_se, fe_se, fe_tw_se),
          title = "Comparison of OLS, One-Way FE, and Two-Way FE Regressions",
          column.labels = c("OLS", "FE (Worker)", "FE (Worker + Time)"),
          dep.var.labels = "log(wage)",
          covariate.labels = c("Union", "Education", "Black", "Hispanic", 
                               "Experience", "Experience²", "Married"),
          no.space = FALSE)




























