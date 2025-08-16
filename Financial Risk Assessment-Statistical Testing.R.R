install.packages("dplyr")
library("dplyr")
# Load necessary data
df <- read.csv("C:\\Users\\LENOVO\\Downloads\\financial_risk_assessment.csv")

View(df)
head(df)
tail(df)
summary(df)

# T-test:Assets Value differs between the two employment groups.
#People who are employed may have higher asset values, because they earn and accumulate wealth.

#People who are unemployed may have lower asset values, due to reduced or no income.

df_filtered <- df %>% 
  filter(Employment.Status  %in% c("Employed","Unemployed"))

# Confirm levels
table(df_filtered$Employment.Status )

#run the t-test
result <- t.test(Assets.Value ~ Employment.Status , data = df_filtered)
print(result)
if (result$p.value < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}


#z-test
# Filter only High and Low risk groups
df_filtered <- df %>% filter(Risk.Rating %in% c("High", "Low"))

# Create 2x2 table
tab <- table(df_filtered$Risk.Rating, df_filtered$Previous.Defaults)

# Z-test on proportions
z_result <- prop.test(tab[, "Yes"], rowSums(tab), correct = FALSE)

# Show result
print(z_result)

# Interpretation
if (z_result$p.value < 0.05) {
  print("Statistically significant difference in default history between risk groups.")
} else {
  print("No significant difference in default history between risk groups.")
}

#f_test

# Remove missing values and filter only Male and Female
df_f <- na.omit(df[df$Gender %in% c("Male", "Female"), ])

# Run F-test: Compare variance of Loan Amount between genders
f_result <- var.test(Loan.Amount ~ Gender, data = df_f)

# Display test result
print(f_result)

if (f_result$p.value < 0.05) {
  print("Reject H₀: Variance in Loan Amount is significantly different between Male and Female applicants.\n")
} else {
  print("Fail to reject H₀: No significant difference in Loan Amount variance between genders.\n")
}

#Anova:Does Income Vary Based on Risk Rating?
# Run the ANOVA
result3 <- aov(Income ~ Risk.Rating, data = df)

# Get the ANOVA summary
anova_summary <- summary(result3)

# Extract the p-value correctly
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

# Print the ANOVA summary
print(anova_summary)

# Interpretation
if (p_value < 0.05) {
  print("Significant difference in Income across Risk Ratings.")
} else {
  print(" significant difference in Income across Risk Ratings.")
}


## Chi Square Test:How Education Level Affects Employment Status

# Create a contingency table
table_data <- table(df$Education.Level, df$Employment.Status)
chisq_result <- chisq.test(table_data)

# View result

print(chisq_result)
if (chisq_result$p.value < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}






