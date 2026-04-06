# Loading the Libraries

install.packages("scales")

library(tidyverse)     # Data manipulation and visualization
library(corrplot)      # Correlation heatmap
library(factoextra)    # Cluster visualization
library(cluster)       # Clustering algorithms
library(skimr)         # Summary statistics
library(DataExplorer)  # EDA automation
library(gridExtra)     # Arrange multiple plots
library(scales)        # Scale formatting

# Getting the folder of the data and pulling it into the system
setwd("C:/Users/Felix/Downloads/Health")

#Verifying the location of the targeted folder been used
getwd()

# Listing all the files in the folder for review
list.files()


# Now using the data
df_hospital <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

df_hospital
View(df_hospital)

glimpse(df_hospital)



# Meta Data thus,Preview of the dataset
cat("=== DATASET DIMENSIONS ===\n")
dim(df_hospital)


cat("\n=== FIRST 6 ROWS ===\n")
head(df_hospital)

cat("\n Last 6 rows\n")
tail(df_hospital)


cat("\n=== COLUMN NAMES ===\n")
names(df_hospital)

cat("\n=== DATA TYPES ===\n")
str(df_hospital)

cat("Row Observation:", nrow(df_hospital))

cat("Column Observation:", ncol(df_hospital))


cat("Target Variable: Diabetes_binary (0 = No Diabetes, 1 = Diabetes/Pre-diabetes)\n")
cat("\nKey Variables:\n")
cat("  - BMI: Body Mass Index\n")
cat("  - HighBP: High Blood Pressure (0=No, 1=Yes)\n")
cat("  - HighChol: High Cholesterol (0=No, 1=Yes)\n")
cat("  - PhysActivity: Physical Activity in past 30 days (0=No, 1=Yes)\n")
cat("  - Smoker: Smoked 100+ cigarettes lifetime (0=No, 1=Yes)\n")
cat("  - Age: Age category (1=18-24, 13=80+)\n")
cat("  - Income: Income scale (1=<$10k, 8=>$75k)\n")
cat("  - GenHlth: General Health (1=Excellent, 5=Poor)\n")


# DATA CLEANING AND PREPROCESSING

cat("\n=== MISSING VALUES ===\n")

clean_dt <- colSums(is.na(df_hospital))
clean_dt

length(clean_dt)



# DUPLICATE CHECK

cat("\n=== DUPLICATE ROWS ===\n")
cat("Number of duplicates:", sum(duplicated(df_hospital)), "\n")

# Remove duplicates
df_clean <- df_hospital[!duplicated(df_hospital), ]

cat("Rows after removing duplicates:", nrow(df_clean), "\n")



# TARGET VARIABLE DISTRIBUTION

cat("\n=== CLASS DISTRIBUTION (Diabetes_binary) ===\n")

table(df_clean$Diabetes_binary)
prop.table(table(df_clean$Diabetes_binary))



# SAMPLING FOR EDA


# For EDA, work with a random sample of 5000 rows
set.seed(123)

df_sample <- df_clean %>% sample_n(5000)

cat("\nWorking sample size:", nrow(df_sample), "\n")




#  SUMMARY STATISTICS

cat("\n=== SUMMARY STATISTICS ===\n")
summary(df_sample)

# Detailed skim
skim(df_sample)




# EXPLORATORY DATA ANALYSIS (EDA)


## 6.1 - Distribution of Target Variable
p1 <- ggplot(df_sample, aes(x = factor(Diabetes_binary),
                            fill = factor(Diabetes_binary))) +
  geom_bar(stat = "count", width = 0.5) +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes/Pre-diabetes")) +
  labs(title = "Distribution of Diabetes Status",
       x = "Diabetes Status", y = "Count", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p1)




## 6.2 - BMI Distribution by Diabetes Status
p2 <- ggplot(df_sample, aes(x = BMI, fill = factor(Diabetes_binary))) +
  geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes")) +
  labs(title = "BMI Distribution by Diabetes Status",
       x = "BMI", y = "Frequency", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p2)


## 6.3 - Age Distribution
p3 <- ggplot(df_sample, aes(x = factor(Age), fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Diabetes Prevalence by Age Group",
       x = "Age Group (1=18-24, 13=80+)",
       y = "Proportion", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p3)


# 6.4 - General Health vs Diabetes
p4 <- ggplot(df_sample, aes(x = factor(GenHlth), fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Diabetes Prevalence by General Health Rating",
       x = "General Health (1=Excellent, 5=Poor)",
       y = "Proportion", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p4)


# 6.5 - High Blood Pressure & High Cholesterol (grouped bar)
df_long <- df_sample %>%
  select(Diabetes_binary, HighBP, HighChol, Smoker, PhysActivity) %>%
  pivot_longer(-Diabetes_binary, names_to = "Variable", values_to = "Value") %>%
  group_by(Diabetes_binary, Variable, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Diabetes_binary, Variable) %>%
  mutate(Prop = Count / sum(Count))

p5 <- ggplot(df_long %>% filter(Value == 1),
             aes(x = Variable, y = Prop, fill = factor(Diabetes_binary))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Risk Factor Prevalence by Diabetes Status",
       x = "Risk Factor", y = "Proportion Positive", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p5)


## 6.6 - BMI Boxplot by Diabetes Status
p6 <- ggplot(df_sample, aes(x = factor(Diabetes_binary), y = BMI,
                            fill = factor(Diabetes_binary))) +
  geom_boxplot(alpha = 0.7, outlier.colour = "grey50") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes")) +
  labs(title = "BMI Distribution by Diabetes Status",
       x = "Diabetes Status", y = "BMI", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p6)


# 6.7 - Income vs Diabetes
p7 <- ggplot(df_sample, aes(x = factor(Income), fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Diabetes Prevalence by Income Level",
       x = "Income (1=<$10k, 8=>$75k)", y = "Proportion", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p7)






# CORRELATION ANALYSIS

# Select numeric variables for correlation
num_vars <- df_sample %>%
  select(BMI, Age, GenHlth, MentHlth, PhysHlth, Income, Education,
         HighBP, HighChol, PhysActivity, Smoker, Diabetes_binary)

cor_matrix <- cor(num_vars, use = "complete.obs")

cat("\n=== CORRELATION MATRIX ===\n")
print(round(cor_matrix, 2))

# Correlation heatmap
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#3498db", "white", "#e74c3c"))(200),
         title = "Correlation Heatmap of Diabetes Health Indicators",
         mar = c(0, 0, 2, 0))



# UNSUPERVISED ML - K-MEANS CLUSTERING

cat("\n=== UNSUPERVISED MACHINE LEARNING: K-MEANS CLUSTERING ===\n")

# Select features for clustering (continuous/meaningful variables)
cluster_vars <- df_sample %>%
  select(BMI, Age, GenHlth, MentHlth, PhysHlth, Income, HighBP, HighChol)

## Normalize (scale) the data — essential for K-Means
cluster_scaled <- scale(cluster_vars)

cat("Variables used for clustering:\n")
cat(names(cluster_vars), "\n")


# 8.1 - Determine Optimal Number of Clusters (Elbow Method)
set.seed(123)

wss <- sapply(1:10, function(k) {
  kmeans(cluster_scaled, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})


kmeans_result <- kmeans(cluster_scaled, centers = 3, nstart = 25)

table(kmeans_result$cluster)



elbow_df <- data.frame(k = 1:10, wss = wss)

p8 <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = "#3498db", linewidth = 1.2) +
  geom_point(color = "#e74c3c", size = 3) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey40") +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster SS") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p8)


# 8.2 - Silhouette Method
set.seed(123)
fviz_nbclust(cluster_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal K") +
  theme_minimal(base_size = 13)


cat("\n=== K-MEANS RESULTS (k=3) ===\n")
cat("Cluster sizes:\n")
print(kmeans_model$size)
cat("\nWithin-cluster sum of squares:", kmeans_model$tot.withinss, "\n")
cat("Between-cluster sum of squares:", kmeans_model$betweenss, "\n")

# Add cluster labels back to sample
df_sample$Cluster <- as.factor(kmeans_model$cluster)


# 8.4 - Visualize Clusters (PCA-based)
p9 <- fviz_cluster(kmeans_model, data = cluster_scaled,
                   geom = "point", ellipse.type = "convex",
                   palette = c("#3498db", "#e74c3c", "#2ecc71"),
                   ggtheme = theme_minimal(),
                   main = "K-Means Cluster Visualization (PCA)")
print(p9)


# 8.5 - Cluster Profiles (Mean of each variable per cluster)
cluster_profiles <- df_sample %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Avg_BMI = round(mean(BMI), 2),
    Avg_Age = round(mean(Age), 2),
    Avg_GenHlth = round(mean(GenHlth), 2),
    Avg_MentHlth = round(mean(MentHlth), 2),
    Avg_PhysHlth = round(mean(PhysHlth), 2),
    Avg_Income = round(mean(Income), 2),
    Pct_HighBP = round(mean(HighBP) * 100, 1),
    Pct_HighChol = round(mean(HighChol) * 100, 1),
    Pct_Diabetic = round(mean(as.numeric(as.character(Diabetes_binary))) * 100, 1)
  )

cat("\n=== CLUSTER PROFILES ===\n")
print(cluster_profiles)


# 8.6 - Diabetes prevalence by cluster
p10 <- ggplot(df_sample, aes(x = Cluster, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"),
                    labels = c("No Diabetes", "Diabetes")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Diabetes Prevalence by Cluster",
       x = "Cluster", y = "Proportion", fill = "Status") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p10)


# 8.7 - BMI by Cluster
p11 <- ggplot(df_sample, aes(x = Cluster, y = BMI, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#3498db", "#e74c3c", "#2ecc71")) +
  labs(title = "BMI Distribution Across Clusters",
       x = "Cluster", y = "BMI") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
print(p11)



# SUMMARY & INTERPRETATION


cat("                    ANALYSIS SUMMARY\n")

cat("Dataset: CDC Diabetes Health Indicators (BRFSS 2015)\n")
cat("Total clean observations:", nrow(df_clean), "\n")
cat("Working sample:", nrow(df_sample), "\n\n")


cat("         KEY EDA FINDINGS:\n")
cat("- Higher BMI is consistently associated with diabetes status.\n")
cat("- Older age groups show significantly higher diabetes prevalence.\n")
cat("- Poor general health strongly correlates with diabetes.\n")
cat("- High blood pressure and high cholesterol co-occur with diabetes.\n")
cat("- Lower income groups show higher diabetes rates.\n\n")

cat("K-MEANS CLUSTERING (k=3) INTERPRETATION:\n")
cat("- Cluster 1: Likely LOWER RISK - younger, lower BMI, better health, higher income\n")
cat("- Cluster 2: Likely MODERATE RISK - middle-aged, moderate BMI, some risk factors\n")
cat("- Cluster 3: Likely HIGHER RISK - older, higher BMI, poor health, high BP & cholesterol\n\n")

cat("NOTE: Cluster labels may vary across runs despite set.seed()\n")
cat("Always reference cluster_profiles table for final interpretation.\n")


