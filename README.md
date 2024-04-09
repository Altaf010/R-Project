
Data Project:- APPLIED ECONOMETRICS FOR BUSINESS

data<- Firm_profits_Data

# Summary statistics
summary(data)

# Check for missing values
sum(is.na(data))

# Handle outliers (e.g., remove extreme values)
# Example: Remove observations with profits > 1 million
data_clean <- data[data$log_profits < 1000000, ]

# Scatterplot: Profits vs. Training
plot(data_clean$log_training, data_clean$log_profits, main="log_profits vs. log_training", xlab="log_training", ylab="log_profits")

# Scatterplot: Profits vs. Equipment
plot(data_clean$log_equipment, data_clean$log_profits, main="log_profits vs. log_equipment", xlab="log_equipment", ylab="log_profits")

# Model 1: Profits ~ Training
model1 <- lm(log_profits ~ log_training, data=data_clean)

install.packages("ggplot2")
library(ggplot2)

#Create ggplot fo"ggplot2"#Create ggplot for model1
ggplot(data_clean, aes(x = log_training, y = log_profits)) +
  # Add points with jitter for better visualization
  geom_jitter(alpha = 0.5) +
  
  # Add linear regression line
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "dashed") +
  
  # Set plot labels
  labs(x = "Log Training", y = "Log Profits") +
  
  # Set plot title
  ggtitle("Scatterplot of Log Profits vs. Log Training with Regression Line")
summary(model1)

# Model 2: Profits ~ Equipment
model2 <- lm(log_profits ~ log_equipment, data=data_clean)
# Create ggplot for model2
ggplot(data_clean, aes(x = log_profits, y = log_equipment)) +
  # Add points with jitter for better visualization
  geom_jitter(alpha = 0.5) +
  
  # Add linear regression line
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  
  # Set plot labels
  labs(x = "Log Profits", y = "Log Equipment") +
  
  # Set plot title
  ggtitle("Scatterplot of Log Profits vs. Log Equipment with Regression Line")

summary(model2)

# Model with both predictors
model_multiple <- lm(log_profits ~ log_training + log_equipment, data=data_clean)
# Create ggplot for model_multiple
ggplot(data_clean, aes(x = log_profits, y = log_equipment)) +
  # Add points with jitter for better visualization
  geom_jitter(alpha = 0.5) +
  
  # Add linear regression line
  geom_smooth(method = "lm", color = "green", se = FALSE, linetype = "dashed") +
  
  # Set plot labels
  labs(x = "Log Profits", y = "Log Equipment") +
  
  # Set plot title
  ggtitle("Scatterplot of Log Profits vs. Log Equipment with Regression Line for Model Multiple")

summary(model_multiple)

# Add control variables (replace with actual variable names)
model_control <- lm(log_profits ~ log_training + log_equipment + Enterprise_Group + Firm_Age, data = data_clean)
summary(model_control)

###Install lmtest
install.packages("lmtest")

library(lmtest)
bptest(model_control)

####Install Sandwich package
install.packages("sandwich")

# Use robust standard errors
library(sandwich)
coeftest(model_control, vcov = vcovHC(model_control, type = "HC1"))

# Create sub-samples (replace with appropriate condition)
small_firms <- data_clean[data_clean$Small_Firm == "0", ]
non_small_firms <- data_clean[data_clean$Small_Firm != "1", ]

# Regression for small firms
model_small <- lm(log_profits ~ log_training + log_equipment, data=small_firms)
summary(model_small)

# Regression for non-small firms
model_non_small <- lm(log_profits ~ log_training + log_equipment, data=non_small_firms)
summary(model_non_small)

1. Objective:
The objective of this report is to analyze the relationship between firm profits and investment in training and equipment. We aim to assess the impact of training and equipment on profits while considering the differences between small-sized firms and larger-sized firms.

2. Data and Empirical Approach:
a. Data Description and Empirical Approach:
We utilize a dataset named 'Firm_profits_Data' containing information on firm profits, training, equipment, and other relevant variables. Our empirical approach involves estimating several regression models to understand how profits are influenced by training, equipment, and other control variables.

b. Descriptive Statistics: Our dataset includes information on various firms, including their profits, training expenditures, and equipment investments.

Descriptive statistics provide an overview of the variables used in our analysis. The table below summarizes the mean, median, minimum, maximum, and standard deviation of key variables:


Variable	Mean	Median	Min	Max
Log Profits	12.213	11.838	6.063	19.481
Log Training	0.238722	0.000857	0.000000	13.255897
Log Equipment	8.089	0.000	0.000	26.873
Enterprise Group	0.1424	0.0000	0.0000	1.0000
Firm Age	23.66	20.00	1.00	125.00

Empirical Approach: We’ll estimate several regression models to understand the relationship between profits and training/equipment. Specifically, we’ll consider the following models:


Model	Coefficient (Intercept)	Coefficient (Log Training)	Coefficient (Log Equipment)	Coefficient (Enterprise Group)	Coefficient (Firm Age)	R-squared
Model 1: Profits vs. Training	12.13733	0.31785	NA	NA	NA	0.04852
Model 2: Profits vs. Equipment	NA	NA	1.7574	NA	NA	0.1336
Model 3: Profits vs. Training and Equipment	11.586140	0.239185	0.070465	NA	NA	0.1603
Model 4: Full Model	10.689062	0.190388	0.061481	1.398005	0.033061	0.2897

Model 1: (model1 <- lm (log_profits ~ log_training, data=data_clean)
Model 2: model2 <- lm (log_profits ~ log_equipment, data=data_clean)
Model_Multiple: (model_multiple <- lm (log_profits ~ log_training + log_equipment, data=data_clean)
Full Model (including other relevant variables)
Full Model for Small-Sized Firms
Full Model for Larger-Sized Firms

3. Main Results:
We’ll present the results in a single table, summarizing the coefficients from the different models. Additionally:
Interpretation: We’ll focus on the coefficients of interest (training and equipment) and discuss their significance.
Goodness of Fit: We’ll assess the goodness of fit for each model. Is the full model a good fit? Are there other important variables we might be missing?
Differences: We’ll compare the estimated coefficients between small-sized and larger-sized firms.


4. Interpretation of Results:
The coefficients of interest in our models indicate the impact of training and equipment on profits. Additionally, we assess the goodness of fit of the models to determine their explanatory power.
We discuss whether the full model adequately captures the relationship between the variables and consider the possibility of other important variables that could be included.


a. Differences between Small-Sized and Larger-Sized Firms:

In comparing the estimated coefficients between small-sized and larger-sized firms, several key differences emerge that shed light on how the impact of training and equipment investment varies across firm sizes.

To begin with, the estimates of the subtle impact on which size of business makes the biggest difference are estimated to be above the zero line regardless of whether the size of the business is small or large. 
That small companies profit from the training and machinery outlaying more than mid or large ones is not necessarily the case. Still, it may hugely influence the overall result. 
The results here suggest that perhaps, the larger-sized firms exhibit different values and coefficients for training and equipment investments than their smaller-sized peers. This indicates that it is possibly the latter group who may benefit more from these investments.

On the other hand, the resource scarcity and efficiency with which training and equipment investment scale up might affect the connection between firm size and the extent to which these fall under the most effective category. 
The most vulnerable small-sized enterprises are usually those that lack the financial resources or administrative capabilities coupled with the inability to acquire and retain skilled personnel which in turn hurts the capacity of such firms to maximize the returns of investments in equipment and training. 
However, the bigger-sized companies usually spend more relatively as they can afford recruitment training programs and the acquisition of better equipment equally brings a higher return relatively.

Furthermore, the market environment or competitive structure as well as market dynamics can result in differences in the effect of training and equipment. hence, for the large firms or medium firms are little different. 
Larger-sized corporations can function in an economic space of greater business competition and technological innovation requiring continuous investment in training and equipment necessitated for the growth of the company while remaining competitive and innovative. 
However, small companies may be affected by a less intense competitive business environment and might give priority to this and other areas focusing on the investment in training and equipment.


To conclude, the coherence of this analysis suggests that the size of firms is a fundamental element in understanding how training and investment into efficient machinery impact the profitability of an enterprise. 
Concurrently, the impacts of such investments can be vastly different for small and larger-sized firms: size could affect the amount of definitely of the benefits, and resource availability together with market conditions could change the shape of the role of these benefits.


Accordingly, our evaluation proposes that training and investment in equipment are both leading factors in improving firm profits. Nonetheless, the significance of the consequence can be greatly different between different categories of small-sized and large-sized firms. 
Despite that training and equipment can determine first of all the firm`s sales, yet the specific role of these variables can vary based on company size and a lot of other contextual issues. 
We suggest that the businesses pay for training and tools to grow their profit and maybe a study also is needed to realize what the businesses want and care about different types of businesses. 
Our analysis would directions for strategic decision-making movements and would crystal clear the point for investments in human capital and technology resources.


The previous document portrays the key elements of the conducted studies and conclusions obtained from analysing the association between the company's profits and the level of training and equipment.
