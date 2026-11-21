simulate_bdat602_project <- function(n = 100000, seed = 602) {
  library(dplyr)
  library(lubridate)
  library(knitr)
  
  set.seed(seed)
  
  # ── SECTION 1: IDENTIFIERS (4 variables) ────────────────────────────────────
  customer_id   <- 1:n
  branch_id     <- sample(1:300, n, replace = TRUE)
  relationship_manager_id <- sample(1:800, n, replace = TRUE)
  account_open_date <- as.Date("2024-12-31") -
    days(round(rlnorm(n, meanlog = 6.5, sdlog = 1.0)))
  
  
  # ── SECTION 2: DEMOGRAPHICS (8 variables) ───────────────────────────────────
  age <- pmax(18, pmin(85, round(rnorm(n, mean = 41, sd = 13))))
  
  gender <- sample(c("Male", "Female", "Other"), n, replace = TRUE,
                   prob = c(0.48, 0.50, 0.02))
  
  region <- sample(
    c("Greater Accra", "Ashanti", "Western", "Eastern",
      "Northern", "Volta", "Central", "Bono"),
    n, replace = TRUE,
    prob = c(0.30, 0.20, 0.12, 0.10, 0.10, 0.08, 0.06, 0.04)
  )
  
  education <- sample(
    c("None", "Primary", "Secondary", "Tertiary", "Postgraduate"),
    n, replace = TRUE,
    prob = c(0.04, 0.12, 0.32, 0.38, 0.14)
  )
  
  employment_status <- sample(
    c("Salaried", "Self-Employed", "Business Owner", "Student", "Unemployed", "Retired"),
    n, replace = TRUE,
    prob = c(0.42, 0.20, 0.13, 0.08, 0.07, 0.10)
  )
  
  marital_status <- sample(
    c("Single", "Married", "Divorced", "Widowed"),
    n, replace = TRUE,
    prob = c(0.42, 0.45, 0.08, 0.05)
  )
  
  dependents <- pmax(0, round(rpois(n, lambda = ifelse(
    marital_status == "Married", 2.0, 0.5
  ))))
  
  # Monthly income: structured by education + employment + region + noise
  base_income <- case_when(
    education == "None"          ~  600,
    education == "Primary"       ~ 1000,
    education == "Secondary"     ~ 2200,
    education == "Tertiary"      ~ 4500,
    education == "Postgraduate"  ~ 8000,
    TRUE ~ 2200
  )
  
  monthly_income <- round(
    base_income *
      case_when(
        employment_status == "Salaried"       ~ 1.00,
        employment_status == "Self-Employed"  ~ 1.15,
        employment_status == "Business Owner" ~ 1.45,
        employment_status == "Student"        ~ 0.25,
        employment_status == "Unemployed"     ~ 0.15,
        employment_status == "Retired"        ~ 0.65,
        TRUE ~ 1.00
      ) *
      ifelse(region %in% c("Greater Accra", "Ashanti"), 1.30, 1.00) *
      rlnorm(n, meanlog = 0, sdlog = 0.40),
    2
  )
  
  
  # ── SECTION 3: ACCOUNT & PRODUCT HOLDINGS (10 binary/categorical) ───────────
  # These binary product-holding flags are the PRIMARY items for
  # ASSOCIATION RULE MINING (Task 1)
  
  account_type <- sample(
    c("Basic", "Standard", "Premium", "Private Banking"),
    n, replace = TRUE,
    prob = c(0.40, 0.35, 0.18, 0.07)
  )
  
  # Product holding flags — probabilities driven by account_type & income
  has_savings_account <- rbinom(n, 1, prob = 0.95)   # almost everyone
  
  has_current_account <- rbinom(n, 1, prob = plogis(
    -1.0 + 0.9 * (account_type %in% c("Standard","Premium","Private Banking")) +
      0.3 * (employment_status %in% c("Salaried","Business Owner"))
  ))
  
  has_fixed_deposit <- rbinom(n, 1, prob = plogis(
    -2.0 + 0.7 * (account_type %in% c("Premium","Private Banking")) +
      0.5 * (log1p(monthly_income) > 8)
  ))
  
  has_credit_card <- rbinom(n, 1, prob = plogis(
    -2.2 + 1.0 * (account_type %in% c("Standard","Premium","Private Banking")) +
      0.4 * (employment_status == "Salaried") +
      0.3 * (age > 28)
  ))
  
  has_personal_loan <- rbinom(n, 1, prob = plogis(
    -2.0 + 0.5 * (employment_status %in% c("Salaried","Business Owner")) +
      0.3 * (dependents > 1) -
      0.4 * (account_type == "Private Banking")
  ))
  
  has_mortgage <- rbinom(n, 1, prob = plogis(
    -3.5 + 0.8 * (age > 30 & age < 60) +
      0.6 * (log1p(monthly_income) > 8.5) +
      0.5 * (marital_status == "Married")
  ))
  
  has_investment_account <- rbinom(n, 1, prob = plogis(
    -2.5 + 1.0 * (account_type %in% c("Premium","Private Banking")) +
      0.5 * (education %in% c("Tertiary","Postgraduate"))
  ))
  
  has_insurance_product <- rbinom(n, 1, prob = plogis(
    -1.8 + 0.5 * (has_mortgage == 1) +
      0.4 * (dependents > 0) +
      0.3 * (account_type != "Basic")
  ))
  
  has_mobile_banking <- rbinom(n, 1, prob = plogis(
    -0.5 + 0.6 * (age < 45) +
      0.4 * (education %in% c("Tertiary","Postgraduate")) +
      0.3 * (region %in% c("Greater Accra","Ashanti"))
  ))
  
  has_overdraft_facility <- rbinom(n, 1, prob = plogis(
    -2.5 + 0.7 * (account_type %in% c("Premium","Private Banking")) +
      0.4 * (employment_status == "Business Owner")
  ))
  
  
  # ── SECTION 4: BALANCES AND TRANSACTIONS (8 variables) ──────────────────────
  # Continuous variables — PRIMARY for CLUSTERING (Task 2)
  
  # Savings balance: right-skewed, driven by income and account type
  savings_balance <- round(
    pmax(0, monthly_income) *
      runif(n, 0.5, 8) *
      case_when(
        account_type == "Basic"           ~ 0.4,
        account_type == "Standard"        ~ 1.0,
        account_type == "Premium"         ~ 2.5,
        account_type == "Private Banking" ~ 6.0,
        TRUE ~ 1.0
      ) *
      rlnorm(n, 0, 0.6),
    2
  )
  
  # Current account balance (only meaningful if has_current_account)
  current_balance <- ifelse(
    has_current_account == 1,
    round(monthly_income * runif(n, 0.2, 2.0) * rlnorm(n, 0, 0.5), 2),
    0
  )
  
  # Average monthly transactions (count)
  avg_monthly_transactions <- rpois(n, lambda = pmax(1,
                                                     3 + 0.002 * monthly_income +
                                                       5 * has_mobile_banking +
                                                       3 * (account_type %in% c("Premium","Private Banking"))
  ))
  
  # Average transaction value
  avg_transaction_value <- round(
    pmax(5, monthly_income / pmax(1, avg_monthly_transactions) *
           rlnorm(n, 0, 0.4)),
    2
  )
  
  # ATM withdrawals per month
  atm_withdrawals_monthly <- rpois(n, lambda = pmax(0.2,
                                                    4 - 1.5 * has_mobile_banking + 0.5 * (age > 50)
  ))
  
  # Online transactions per month
  online_transactions_monthly <- rpois(n, lambda = pmax(0,
                                                        2 + 8 * has_mobile_banking +
                                                          0.001 * monthly_income -
                                                          0.05 * age
  ))
  
  # International transactions per year (rare, right-skewed)
  international_transactions_yearly <- rpois(n, lambda = pmax(0,
                                                              0.2 + 0.0008 * monthly_income +
                                                                1.5 * (account_type %in% c("Premium","Private Banking"))
  ))
  
  # Overdraft usage frequency per year (only if has overdraft facility)
  overdraft_usage_yearly <- ifelse(
    has_overdraft_facility == 1,
    rpois(n, lambda = pmax(0, 2 + 0.3 * (employment_status=="Business Owner"))),
    0
  )
  
  
  # ── SECTION 5: CREDIT & RISK PROFILE (6 variables) ───────────────────────────
  
  # Credit score (300-850 range, FICO-like)
  credit_score_base <- 650 +
    1.5 * (log1p(monthly_income) - 7) * 20 +
    0.3 * age -
    30 * has_overdraft_facility -
    15 * overdraft_usage_yearly +
    rnorm(n, 0, 40)
  
  credit_score <- round(pmax(300, pmin(850, credit_score_base)))
  
  # Number of active loans
  num_active_loans <- has_personal_loan + has_mortgage +
    rbinom(n, 1, prob = plogis(-2 + 0.3*(employment_status=="Business Owner")))
  
  # Total outstanding debt
  total_outstanding_debt <- round(
    pmax(0,
         has_personal_loan * runif(n, 500, 15000) +
           has_mortgage      * runif(n, 20000, 250000) +
           has_overdraft_facility * overdraft_usage_yearly * runif(n, 100, 2000)
    ),
    2
  )
  
  # Debt-to-income ratio (DTI)
  debt_to_income_ratio <- round(
    total_outstanding_debt / pmax(1, monthly_income * 12),
    3
  )
  
  # Number of missed payments in last 12 months
  num_missed_payments <- rpois(n, lambda = pmax(0,
                                                0.3 + 0.02 * debt_to_income_ratio * 10 -
                                                  0.005 * (credit_score - 600)
  ))
  
  # Loan default flag (TARGET for potential supervised follow-up; not used in
  # Tasks 1-2 but available for discussion)
  default_prob <- plogis(
    -3.0 +
      0.04 * num_missed_payments +
      1.5  * (debt_to_income_ratio > 0.5) -
      0.01 * (credit_score - 600) +
      0.3  * (employment_status %in% c("Unemployed","Student"))
  )
  loan_default_flag <- rbinom(n, 1, prob = pmin(default_prob, 0.6))
  
  
  # ── SECTION 6: CUSTOMER BEHAVIOUR & ENGAGEMENT (5 variables) ─────────────────
  
  # Years as a customer
  tenure_years <- round(as.numeric(as.Date("2024-12-31") - account_open_date) / 365.25, 2)
  
  # Customer satisfaction score (1-10)
  satisfaction_score <- pmin(10, pmax(1, round(
    7.0 -
      0.5 * num_missed_payments +
      0.3 * has_mobile_banking +
      0.2 * (account_type %in% c("Premium","Private Banking")) +
      rnorm(n, 0, 1.5)
  )))
  
  # Number of branch visits per year
  branch_visits_yearly <- rpois(n, lambda = pmax(0,
                                                 6 - 4 * has_mobile_banking + 0.05 * age
  ))
  
  # Complaint count in last 12 months
  num_complaints <- rpois(n, lambda = pmax(0,
                                           0.2 + 0.3 * num_missed_payments +
                                             0.4 * (satisfaction_score < 4)
  ))
  
  # Churn flag (closed account / dormant in last 6 months)
  churn_prob <- plogis(
    -2.0 -
      0.3 * (satisfaction_score - 5) +
      0.2 * num_complaints -
      0.3 * has_mobile_banking -
      0.05 * tenure_years +
      0.2 * (account_type == "Basic")
  )
  churned <- rbinom(n, 1, prob = pmin(churn_prob, 0.7))
  
  
  # ── SECTION 7: FREE-TEXT FEEDBACK (1 variable, for optional extension) ──────
  
  feedback_templates <- c(
    "The mobile app keeps crashing whenever I try to make a transfer.",
    "My loan application took far too long to be approved.",
    "Customer service at the branch was excellent and very helpful.",
    "I was charged a fee I did not understand on my statement.",
    "The new online banking interface is confusing to navigate.",
    "Staff at the branch were rude and unprofessional during my visit.",
    "I appreciate the quick response when I reported a lost card.",
    "Interest rates on savings accounts are too low compared to competitors.",
    "The ATM near my home is frequently out of cash.",
    "Setting up the fixed deposit was smooth and well explained.",
    "I have been waiting for a refund on a failed transaction for weeks.",
    "The credit card application process was straightforward and fast.",
    "There is no clear information about overdraft charges.",
    "I love the new budgeting feature in the mobile app.",
    "My relationship manager rarely responds to my emails."
  )
  
  customer_feedback <- sample(feedback_templates, n, replace = TRUE)
  customer_feedback[sample(1:n, size = round(n * 0.15))] <- NA_character_
  
  
  # ── SECTION 8: INTENTIONAL DATA QUALITY ISSUES (for cleaning component) ─────
  
  # 1. Missing values (MAR/MCAR/MNAR mix)
  # ~6% missing monthly_income (sensitive field)
  monthly_income[sample(1:n, size = round(n * 0.06))] <- NA_real_
  
  # ~4% missing credit_score (not all customers have credit history)
  credit_score[sample(1:n, size = round(n * 0.04))] <- NA_real_
  
  # ~3% missing dependents
  dependents[sample(1:n, size = round(n * 0.03))] <- NA_integer_
  
  # ~5% missing satisfaction_score
  satisfaction_score[sample(1:n, size = round(n * 0.05))] <- NA_real_
  
  # 2. Outliers / data entry errors
  # Extreme age values
  age[sample(1:n, size = round(n * 0.001))] <- sample(c(1, 5, 130, 150), round(n*0.001), replace = TRUE)
  
  # Extreme income values
  monthly_income[sample(1:n, size = round(n * 0.001))] <- sample(c(0, -500, 5000000), round(n*0.001), replace = TRUE)
  
  # Extreme savings balance (data entry errors with extra zeros)
  savings_balance[sample(1:n, size = round(n * 0.0015))] <-
    savings_balance[sample(1:n, size = round(n * 0.0015))] * 1000
  
  # Negative debt-to-income ratio (impossible — entry error)
  debt_to_income_ratio[sample(1:n, size = round(n * 0.0008))] <- -abs(
    debt_to_income_ratio[sample(1:n, size = round(n * 0.0008))]
  )
  
  # 3. Inconsistent categorical labels (common real-world issue)
  # Introduce inconsistent capitalisation/spelling in gender and region
  gender_messy <- gender
  messy_idx <- sample(1:n, size = round(n * 0.02))
  gender_messy[messy_idx] <- case_when(
    gender_messy[messy_idx] == "Male"   ~ sample(c("male","MALE","M"), length(messy_idx), replace=TRUE),
    gender_messy[messy_idx] == "Female" ~ sample(c("female","FEMALE","F"), length(messy_idx), replace=TRUE),
    TRUE ~ gender_messy[messy_idx]
  )
  
  region_messy <- region
  messy_idx2 <- sample(1:n, size = round(n * 0.015))
  region_messy[messy_idx2] <- trimws(paste0(" ", region_messy[messy_idx2], "  "))  # whitespace issues
  
  # 4. Duplicate records (exact duplicates of some rows will be added later
  #    in the assembly step by duplicating a sample of customer_ids)
  
  # 5. Implausible dates (account opened in the "future" relative to dataset date)
  bad_date_idx <- sample(1:n, size = round(n * 0.0005))
  account_open_date[bad_date_idx] <- as.Date("2026-01-01") + days(sample(1:300, length(bad_date_idx), replace=TRUE))
  
  
  # ── ASSEMBLE FINAL DATA FRAME ─────────────────────────────────────────────────
  
  df <- data.frame(
    # --- Identifiers (4) ---
    customer_id,
    branch_id,
    relationship_manager_id,
    account_open_date,
    
    # --- Demographics (8) ---
    age,
    gender = gender_messy,
    region = region_messy,
    education,
    employment_status,
    marital_status,
    dependents,
    monthly_income,
    
    # --- Account & product holdings (11) ---
    account_type,
    has_savings_account,
    has_current_account,
    has_fixed_deposit,
    has_credit_card,
    has_personal_loan,
    has_mortgage,
    has_investment_account,
    has_insurance_product,
    has_mobile_banking,
    has_overdraft_facility,
    
    # --- Balances and transactions (8) ---
    savings_balance,
    current_balance,
    avg_monthly_transactions,
    avg_transaction_value,
    atm_withdrawals_monthly,
    online_transactions_monthly,
    international_transactions_yearly,
    overdraft_usage_yearly,
    
    # --- Credit and risk (6) ---
    credit_score,
    num_active_loans,
    total_outstanding_debt,
    debt_to_income_ratio,
    num_missed_payments,
    loan_default_flag,
    
    # --- Behaviour and engagement (5) ---
    tenure_years,
    satisfaction_score,
    branch_visits_yearly,
    num_complaints,
    churned,
    
    # --- Text (1) ---
    customer_feedback,
    
    stringsAsFactors = FALSE
  )
  
  # 6. Add ~0.3% exact duplicate rows (common data integration issue)
  dup_idx <- sample(1:nrow(df), size = round(nrow(df) * 0.003))
  df <- bind_rows(df, df[dup_idx, ])
  
  return(df)
}

#============================================================================
# Generating 100,000 records 
#============================================================================
bank_data <- simulate_bdat602_project(n = 100000, seed = 757)

#===========================================================================
#Step 1: Variable Selection 
#============================================================================

variables <- c("account_type", "education", "age", "monthly_income", 
               "has_current_account", "has_credit_card", "has_fixed_deposit", 
               "has_investment_account", "has_insurance_product", "has_mortgage", 
               "has_mobile_banking" )

##later applied in Data Cleaning Step after dropping duplicates
#===============================================================================
#Step 2: Data Cleaning
#===============================================================================
#Diagnosing the Dataset 
library(skimr)
library(tidyr)
library(tidyverse)

dplyr::glimpse(selected_vars)
skim(selected_vars)
unique(selected_vars$account_type)
#data quality issues 
#duplicates 
#missing values for income
#implausible value for income 
# outliers for age 

# Issue 1: Duplicate rows 
##checking for duplicates 

cat("Duplicate rows:", sum(duplicated(bank_data)), "\n")  # 300 exact duplicate rows

##Dropped duplicates
bank <- bank_data[!duplicated(bank_data), ]

##Applying Variable Selection
bank <- bank[, variables]  


#Issue 2: Missing Values 
##Missing value counts per variable
bank |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  tidyr::pivot_longer(everything(),
                      names_to = "variable",
                      values_to = "n_missing") |>
  filter(n_missing > 0) |>
  mutate(pct_missing = round(100 * n_missing / nrow(selected), 2)) |>
  arrange(desc(pct_missing))

# Impute using the median WITHIN education, since income varies
# substantially by education -- even though the missingness itself is MCAR, a
# group-wise median is a better point estimate than a single global median.

# The missing monthly_income values are MCAR because the function selects 
# about 6% of records completely at random to blank out, giving every customer an equal chance of 
# having a missing value regardless of their characteristics or income. Since 
# the missingness is unrelated to either the observed data or the income value 
# itself, it is MCAR rather than MAR or MNAR.

#Median imputation for income
monthly_income_median <- median(bank$monthly_income, na.rm = TRUE)

bank <- bank |>  
  group_by(education) |> 
  mutate(monthly_income = ifelse(is.na(monthly_income),
                                 median(monthly_income, na.rm = TRUE),
                                 monthly_income)) |>
  ungroup() |> as.data.frame()

# # Issue 3: Outliers / implausible values in monthly_income
# ## (a small fraction of values 0, -500, or 5,000,000)
# 
# # Cap income at the 1st and 99th percentile
# p01_income <-quantile(selected_vars$monthly_income, 0.01, na.rm =
#                         TRUE)
# p99_income <-quantile(selected_vars$monthly_income, 0.99, na.rm =
#                         TRUE)
# selected_vars <- selected_vars |>
#   mutate(
#     monthly_income = pmax(p01_income, pmin(p99_income, monthly_income))
#   )

# Issue 3: implausible and extreme monthly income. Non positive incomes cannot
# be real and are set to missing; the extreme 5,000,000 values are extra zero
# entry errors and are Winsorised at the 99.5th percentile.
nonpos_income <- sum(bank$monthly_income <= 0, na.rm = TRUE)   # 68 values
bank$monthly_income[bank$monthly_income <= 0] <- NA
income_cap <- as.numeric(quantile(bank$monthly_income, 0.995, na.rm = TRUE))
n_income_high <- sum(bank$monthly_income > income_cap, na.rm = TRUE)
bank$monthly_income[!is.na(bank$monthly_income) & 
                      bank$monthly_income > income_cap] <- income_cap

# After the steps above some incomes values are missing.
# We impute the median within education group, because
# education is a strong and observed determinant of income here.
n_income_missing <- sum(is.na(bank$monthly_income))
bank <- bank %>%
  group_by(education) %>%
  mutate(monthly_income = ifelse(is.na(monthly_income),
                                 median(monthly_income, na.rm = TRUE),
                                 monthly_income)) %>%
  ungroup()


# Issue 4: Outliers / implausible values in age
## (a small fraction of values 1, 5, 130 or 150)
## Set implausible ages to NA, then remove affected rows
bank <- bank |>
  dplyr::mutate(age = ifelse(age < 18 | age > 85, NA, age)) |>
  tidyr::drop_na(age)

summary(bank$age)
nrow(bank)

#=============================================================================
# DISCRETISATION of the two continuous variables into banking meaningful bands.
#=============================================================================

# Income bands in Ghana Cedi. Round thresholds close to the sample quartiles
# (roughly 1,434 / 3,130 / 5,859) give three interpretable groups.
bank$income_band <- cut(bank$monthly_income, breaks = c(-Inf, 2000, 6000, Inf),
                     labels = c("Low", "Middle", "High"))


# Age life stage bands. Cut points at 30, 45 and 60 match recognised banking
# life stages and the points where product uptake shifts in this population.
bank$age_band <- cut(bank$age, breaks = c(17, 30, 45, 60, 90),
                  labels = c("18 to 30", "31 to 45", "46 to 60", "Over 60"))


#checking proportional distribution of each band 
age_tab    <- round(prop.table(table(bank$age_band)), 3)
income_tab <- round(prop.table(table(bank$income_band)), 3)

# =============================================================================
# STEP 3(a): CONVERT TO TRANSACTIONS OBJECT
# =============================================================================

# BUILD THE TRANSACTIONS OBJECT (market basket encoding). Each customer is a
# basket. A product becomes an item only when it is held; absent products are
# set to NA and therefore contribute no item. The four profile variables are
# retained as categorical items with all of their levels.

lab <- function(x, name) ifelse(x == 1, name, NA)

transac_df <- data.frame(
  account_type    = bank$account_type,
  education       = bank$education,
  age_band        = bank$age_band,
  income_band     = bank$income_band,
  current_account = lab(bank$has_current_account,    "Current"),
  credit_card     = lab(bank$has_credit_card,        "CreditCard"),
  fixed_deposit   = lab(bank$has_fixed_deposit,      "FixedDeposit"),
  investment      = lab(bank$has_investment_account, "Investment"),
  insurance       = lab(bank$has_insurance_product,  "Insurance"),
  mortgage        = lab(bank$has_mortgage,           "Mortgage"),
  mobile_banking  = lab(bank$has_mobile_banking,     "MobileBanking"),
  stringsAsFactors = TRUE
)


transac <- as(transac_df, "transactions")
n_items <- ncol(items(transac))

# The seven product items. Because the business question is about cross selling
# products, these are the only items allowed on the consequent (right hand side).
prod_items <- c("current_account=Current", "credit_card=CreditCard",
                "fixed_deposit=FixedDeposit", "investment=Investment",
                "insurance=Insurance", "mortgage=Mortgage",
                "mobile_banking=MobileBanking")

#support of each item
prod_support <- round(itemFrequency(transac)[prod_items], 3)
print(prod_support)

##plot of frequent items
itemFrequencyPlot(transac,topN = 20,type = "absolute",
                  main = "Top 20 Most Frequent Items",
                  col = "magenta",las = 2, cex.names = 0.75)

#--- Inspect a few transactions--
inspect(trans_filt[1:5])

# ==============================================================================
# STEP 3(d): SENSITIVITY ANALYSIS ON SUPPORT
# ==============================================================================

# SENSITIVITY ANALYSIS: how the number of rules responds to the support
# threshold at a fixed confidence of 0.25 (consequent restricted to products,
# redundant rules removed, lift greater than one).

supp_grid <- c(0.01, 0.02, 0.03, 0.05, 0.08, 0.1, 0.15)
sens <- sapply(supp_grid, function(s) {
  r <- apriori(transac,
               parameter  = list(supp = s, conf = 0.25, minlen = 2, maxlen = 4),
               appearance = list(rhs = prod_items, default = "lhs"),
               control    = list(verbose = FALSE))
  r <- r[!is.redundant(r)]
  length(subset(r, lift > 1))
})
sens_tab <- data.frame(`Minimum support` = supp_grid,
                       `Rules retained`  = sens, check.names = FALSE)

kable(sens_tab, 
      caption = "Sensitivity Analysis: Rules Retained by Minimum Support",
      col.names = c("Minimum Support", "Rules Retained"),
      align = c("c", "c"))

# =============================================================================
# STEP 3(b)/(c): Applying Mining Algorithm & selecting appropriate minsupp and minconf
# =============================================================================

rules_bank <- apriori(transac,
                     parameter  = list(supp = 0.01, conf = 0.25, minlen = 2, 
                                       maxlen = 4),
                     appearance = list(rhs = prod_items, default = "lhs"),
                     control    = list(verbose = FALSE))

n_bank_rules <- length(rules_bank)
print(n_bank_rules)

#Inspect top 10 rules sorted by lift.
inspect(sort(rules_bank, by = "lift")[1:10])

# Remove redundant rules (a rule is redundant when a more general rule has equal
# or higher confidence), then keep only positively associated rules (lift > 1).
rules_nr <- rules_bank[!is.redundant(rules_bank)]

rules_b <- subset(rules_nr, lift > 1)

#Inspect top 10 non redundant rules sorted by lift.
inspect(sort(rules_b, by = "lift")[1:10])


# Top 10 rules_b (non-redundant) by lift
as(sort(rules_b, by = "lift")[1:10], "data.frame") |>
  dplyr::mutate(across(c(support, confidence, coverage, lift), ~round(., 3))) |>
  kable(caption = "Top 10 Rules by Lift",
        col.names = c("Rule", "Support", "Confidence", "Coverage", "Lift", "Count"),
        row.names = FALSE,
        align = c("l", "c", "c", "c", "c", "c"))

# The minimum support was set to 0.01, meaning a rule must apply to about 990 
# customers to be considered. A lower confidence threshold was used because some 
# products, such as investment accounts (13.3%), fixed deposits (18.5%), and 
# mortgages (8.6%), are owned by relatively few customers. Using a high confidence 
# threshold could remove useful rules involving these products. Since the goal is 
# to identify associations that meaningfully outperform chance rather than ones 
# that merely reflect how common an item is, rules were then ranked using lift (>1). 
# Only those with lift greater than 1 were kept, as these represent associations 
# that are stronger than just chance.  


##Visualization 

library(arulesViz)
#---1. Scatter plot: support vs confidence, coloured by lift--
  plot(rules_b,measure = c("support", "confidence"),
       shading="lift",
       main = "Association Rules: Support vs Confidence (shade =
Lift)",
      # col = colorRampPalette(c("steelblue","gold","red"))(100)
      jitter = 0
       )


  
#---2. Grouped matrix plot--
  plot(rules_b,
       method = "grouped",
       control = list(k = 10),
       main = "Grouped Matrix of Association Rules")

 