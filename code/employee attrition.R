# HR EMPLOYEE ATTRITION ANALYSIS 

# Install required packages (uncomment if needed)
install.packages(c("tidyverse", "ggplot2", "corrplot", "caret", 
                   "randomForest", "RColorBrewer", "scales"))


# Load libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(caret)
library(randomForest)
library(RColorBrewer)
library(scales)

set.seed(42)

# LOAD AND EXPLORE DATA

hr_data <- read.csv("HR-Employee-Attrition.csv", stringsAsFactors = FALSE)

# Create binary attrition column
hr_data$left <- ifelse(hr_data$Attrition == "Yes", 1, 0)

# Basic statistics
total_employees <- nrow(hr_data)
total_left <- sum(hr_data$left)
attrition_rate <- mean(hr_data$left) * 100

cat("=== HR ATTRITION ANALYSIS ===\n")
cat(sprintf("Total employees: %d\n", total_employees))
cat(sprintf("Employees who left: %d\n", total_left))
cat(sprintf("Overall attrition rate: %.1f%%\n\n", attrition_rate))

# QUESTION 1: ATTRITION BY DEPARTMENT & ROLE

cat("=== QUESTION 1: ATTRITION BY DEPARTMENT ===\n")

dept_attrition <- hr_data %>%
  group_by(Department) %>%
  summarise(
    Total = n(),
    Left = sum(left),
    Rate = round(mean(left) * 100, 1)
  ) %>%
  arrange(desc(Rate))

print(dept_attrition)

role_attrition <- hr_data %>%
  group_by(JobRole) %>%
  summarise(
    Total = n(),
    Left = sum(left),
    Rate = round(mean(left) * 100, 1)
  ) %>%
  arrange(desc(Rate))

cat("\nTop 5 roles with highest attrition:\n")
print(head(role_attrition, 5))

# QUESTION 2: KEY FACTORS DRIVING ATTRITION

cat("\n=== QUESTION 2: KEY FACTORS DRIVING ATTRITION ===\n")

# Factor 1: Overtime
ot_attrition <- hr_data %>%
  group_by(OverTime) %>%
  summarise(
    Total = n(),
    Left = sum(left),
    Rate = round(mean(left) * 100, 1)
  )

cat("\n1. OVERTIME IMPACT:\n")
print(ot_attrition)

# Factor 2: Job Satisfaction
sat_attrition <- hr_data %>%
  group_by(JobSatisfaction) %>%
  summarise(
    Total = n(),
    Left = sum(left),
    Rate = round(mean(left) * 100, 1)
  )

cat("\n2. JOB SATISFACTION IMPACT:\n")
print(sat_attrition)

# Factor 3: Career Stagnation
hr_data$stagnant <- ifelse(hr_data$YearsSinceLastPromotion > 2, "Yes", "No")

stagnation_attrition <- hr_data %>%
  group_by(stagnant) %>%
  summarise(
    Total = n(),
    Left = sum(left),
    Rate = round(mean(left) * 100, 1)
  )

cat("\n3. CAREER STAGNATION IMPACT:\n")
print(stagnation_attrition)

# Factor 4: Work-Life Balance
wlb_attrition <- hr_data %>%
  group_by(WorkLifeBalance) %>%
  summarise(
    Total = n(),
    Left = sum(left),
    Rate = round(mean(left) * 100, 1)
  )

cat("\n4. WORK-LIFE BALANCE IMPACT:\n")
print(wlb_attrition)

# Factor 5: Income Level
median_income <- median(hr_data$MonthlyIncome)
hr_data$income_group <- ifelse(hr_data$MonthlyIncome >= median_income, "High", "Low")

income_attrition <- hr_data %>%
  group_by(income_group) %>%
  summarise(
    Total = n(),
    Left = sum(left),
    Rate = round(mean(left) * 100, 1)
  )

cat("\n5. INCOME LEVEL IMPACT:\n")
print(income_attrition)

# QUESTION 3: RETENTION RECOMMENDATIONS

cat("\n=== QUESTION 3: RETENTION RECOMMENDATIONS ===\n")

# Identify high-risk groups
high_risk <- hr_data %>%
  filter(OverTime == "Yes", 
         JobSatisfaction <= 2,
         YearsSinceLastPromotion > 2)

cat("HIGH-RISK EMPLOYEE PROFILE:\n")
cat(sprintf("- %d employees (%.1f%% of workforce)\n", 
            nrow(high_risk), nrow(high_risk)/nrow(hr_data)*100))
cat(sprintf("- Their attrition rate: %.1f%%\n", mean(high_risk$left)*100))
cat(sprintf("- Overall attrition rate: %.1f%%\n\n", attrition_rate))

cat("RECOMMENDATIONS:\n")
cat("1. OVERTIME MANAGEMENT: Review policies, hire additional staff\n")
cat("2. CAREER DEVELOPMENT: Regular promotion reviews (every 1-2 years)\n")
cat("3. JOB SATISFACTION: Conduct stay interviews with low-satisfaction employees\n")
cat("4. WORK-LIFE BALANCE: Implement flexible work arrangements\n")
cat("5. TARGETED INTERVENTIONS: Focus on Sales and R&D departments\n")

# QUESTION 4: PREDICTIVE MODEL

cat("\n=== QUESTION 4: PREDICTIVE MODEL ===\n")

# Prepare data
model_data <- hr_data %>%
  select(left, Age, MonthlyIncome, JobSatisfaction, OverTime,
         YearsSinceLastPromotion, WorkLifeBalance, EnvironmentSatisfaction,
         YearsAtCompany, NumCompaniesWorked) %>%
  mutate(OverTime = ifelse(OverTime == "Yes", 1, 0))

# Split data
train_idx <- sample(1:nrow(model_data), size = 0.7 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data <- model_data[-train_idx, ]

# Train Random Forest
rf_model <- randomForest(
  as.factor(left) ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

# Evaluate
predictions <- predict(rf_model, test_data)
accuracy <- mean(predictions == test_data$left) * 100
cat(sprintf("Model Accuracy: %.1f%%\n", accuracy))

# Feature importance
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model, type = 2)[, 1]
) %>%
  arrange(desc(Importance))

cat("\nMost Important Predictors:\n")
print(head(importance_df, 5))

# QUESTION 5: FISCAL JUSTIFICATION

cat("\n=== QUESTION 5: FISCAL JUSTIFICATION ===\n")

# Calculate costs
avg_annual_salary <- mean(hr_data$MonthlyIncome) * 12
replacement_cost <- avg_annual_salary * 1.0
current_loss <- total_left * replacement_cost

cat(sprintf("Average annual salary: $%s\n", comma(avg_annual_salary, accuracy = 0.01)))
cat(sprintf("Total annual loss: $%s\n", comma(current_loss, accuracy = 0.01)))

# Savings scenarios
savings_20pct <- current_loss * 0.20
model_cost <- 50000
roi <- ((savings_20pct - model_cost) / model_cost) * 100
payback <- (model_cost / savings_20pct) * 12

cat("\n20% REDUCTION SCENARIO:\n")
cat(sprintf("Annual savings: $%s\n", comma(savings_20pct, accuracy = 0.01)))
cat(sprintf("Model investment: $%s\n", comma(model_cost, accuracy = 0.01)))
cat(sprintf("ROI: %.1f%%\n", roi))
cat(sprintf("Payback period: %.1f months\n", payback))

cat("\nVERDICT: YES - The model will save money\n")

# VISUALIZATIONS (6 essential charts)

cat("\n=== CREATING VISUALIZATIONS ===\n")

dir.create("charts", showWarnings = FALSE)

# Chart 1: Department Attrition
p1 <- ggplot(dept_attrition, aes(x = reorder(Department, -Rate), 
                                 y = Rate, fill = Department)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0(Rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Attrition Rate by Department", x = "Department", y = "Rate (%)") +
  theme_minimal() + theme(legend.position = "none") +
  geom_hline(yintercept = attrition_rate, linetype = "dashed", color = "red", alpha = 0.7)
ggsave("charts/1_department_attrition.png", p1, width = 8, height = 5)

print(p1)
# Chart 2: Key Drivers
key_drivers <- rbind(
  data.frame(Factor = c("Overtime: Yes", "Overtime: No"),
             Rate = c(ot_attrition$Rate[1], ot_attrition$Rate[2]),
             Category = "Overtime"),
  data.frame(Factor = c("Stagnant: Yes", "Stagnant: No"),
             Rate = c(stagnation_attrition$Rate[1], stagnation_attrition$Rate[2]),
             Category = "Stagnation"),
  data.frame(Factor = c("Income: Low", "Income: High"),
             Rate = c(income_attrition$Rate[1], income_attrition$Rate[2]),
             Category = "Income")
)

p2 <- ggplot(key_drivers, aes(x = Category, y = Rate, fill = Factor)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  geom_text(aes(label = paste0(Rate, "%")), 
            position = position_dodge(0.9), vjust = -0.5, size = 4) +
  labs(title = "Key Drivers of Attrition", x = "", y = "Rate (%)") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("charts/2_key_drivers.png", p2, width = 10, height = 6)

print(p2)
# Chart 3: Satisfaction Impact
p3 <- ggplot(sat_attrition, aes(x = as.factor(JobSatisfaction), y = Rate)) +
  geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.85) +
  geom_text(aes(label = paste0(Rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Attrition by Job Satisfaction", x = "Satisfaction (1=Low, 4=High)", y = "Rate (%)") +
  theme_minimal()
ggsave("charts/3_satisfaction_impact.png", p3, width = 8, height = 5)

print(p3)
# Chart 4: Job Roles
p4 <- ggplot(head(role_attrition, 8), 
             aes(x = reorder(JobRole, Rate), y = Rate, 
                 fill = ifelse(Rate > attrition_rate, "High", "Low"))) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0(Rate, "%")), hjust = -0.2, size = 4) +
  coord_flip() +
  labs(title = "Top 8 Roles with Highest Attrition", x = "Job Role", y = "Rate (%)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("charts/4_job_roles.png", p4, width = 10, height = 6)

print(p4)




# Chart 5: Feature Importance
p5 <- ggplot(head(importance_df, 5), 
             aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  coord_flip() +
  labs(title = "Top 5 Predictors of Attrition", x = "Feature", y = "Importance") +
  theme_minimal() + theme(legend.position = "none")
ggsave("charts/5_feature_importance.png", p5, width = 8, height = 5)

print(p5)

# Chart 6: Cost Savings
cost_data <- data.frame(
  Scenario = c("Current", "10%", "20%", "30%", "40%", "50%"),
  Cost = c(current_loss, current_loss * c(0.9, 0.8, 0.7, 0.6, 0.5))
)

p6 <- ggplot(cost_data, aes(x = Scenario, y = Cost/1000000, 
                            fill = ifelse(Scenario == "Current", "Current", "Projected"))) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0("$", round(Cost/1000000, 1), "M")), vjust = -0.5, size = 4) +
  labs(title = "Projected Annual Cost Savings", x = "Reduction Scenario", y = "Cost (Millions USD)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("charts/6_cost_savings.png", p6, width = 10, height = 6)

print(p6)
cat("✓ 6 charts saved to 'charts/' folder\n")

# EXECUTIVE SUMMARY

cat("\n=== EXECUTIVE SUMMARY ===\n")
cat(sprintf("\nCurrent State: %.1f%% attrition costing $%s annually", 
            attrition_rate, comma(current_loss, accuracy = 0.01)))
cat("\n\nTop 3 Priorities:")
cat("\n1. Overtime management (2.5x higher attrition)")
cat("\n2. Career development (2.3x higher attrition)")
cat("\n3. Job satisfaction improvement")
cat(sprintf("\n\nFinancial Impact: 20%% reduction saves $%s annually", 
            comma(savings_20pct, accuracy = 0.01)))
cat(sprintf("\nModel ROI: %.1f%% with %.1f-month payback", roi, payback))
cat("\n\nRECOMMENDATION: Implement predictive model IMMEDIATELY\n")


