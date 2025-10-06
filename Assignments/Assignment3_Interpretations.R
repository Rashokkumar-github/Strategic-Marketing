rm(list = ls())
# Load required packages
library(tidyverse)
library(factoextra)
library(cluster)
library(ggplot2)

# Load dataset
hotel_customers <- read.csv("/Users/nico/Documents/EUR/Marketing/Assignment/Strategic-Marketing/Assignments/HotelCustomersSubset_sample.csv")

# Quick look at the data
str(hotel_customers)
head(hotel_customers)
summary(hotel_customers)

# Set seed
set.seed(123)

# Convert Age to integer
hotel_customers$Age <- as.numeric(hotel_customers$Age)

# Remove missing and unrealistic age values
hotel_customers <- hotel_customers %>%
  filter(!is.na(Age) & Age >= 18 & Age <= 95)

# Select the variables
selected_variables <- c(
  "Age","AverageLeadTime","DaysSinceCreation",
  "LodgingRevenue","OtherRevenue",
  "PersonsNights","RoomNights",
  "DaysSinceLastStay","DaysSinceFirstStay"
)

# Keep columns of selected variables and check for missing values 
analysis_data <- hotel_customers %>%
  select(all_of(selected_variables)) %>%
  drop_na()

# Standardize all variables
analysis_data_scaled <- scale(analysis_data)

# Check with elbow method
k_opt <- 4

p1 <-  fviz_nbclust(analysis_data_scaled, kmeans, method = "wss", k.max = 13) +
  labs(
    title = "Elbow Method",
    subtitle = paste("Suggested k =", k_opt),
    x = "Number of clusters (k)",
    y = "Within-Cluster Sum of Squares (WSS)"
  ) +
  scale_color_manual(values = c("#2E86AB", "#7FB069"))+
  geom_vline(xintercept = k_opt, linetype = "dashed") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),    
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


# Check with silhouette method
p2 <- fviz_nbclust(analysis_data_scaled, kmeans, method = "silhouette", k.max = 13)+
  labs(
    title = "Silhouette Method",
    subtitle = "Average silhouette width by number of clusters (k)",
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) +
  scale_color_manual(values = c("#2E86AB", "#7FB069"))+
  geom_vline(xintercept = k_opt, linetype = "dashed") +
  theme_classic()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),    
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

print(p1)
print(p2)

# Perform k
kmeans_result <- kmeans(analysis_data_scaled, centers = 4, nstart = 25)

# Add cluster to original data set
hotel_customers$Cluster <- as.factor(kmeans_result$cluster)

# Create data frame
cluster_sizes <- data.frame(
  Cluster = factor(1:4),
  Size = kmeans_result$size
)

# Plot cluster sizes
p3 <- ggplot(cluster_sizes, aes(x = Cluster, y = Size, fill = Cluster)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Cluster Sizes",
    x = "Cluster",
    y = "Number of Customers"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),    
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# Visualize the 4-cluster segmentation solution
p4 <-  fviz_cluster(kmeans_result, data = analysis_data_scaled,
             ellipse.type = "norm",
             geom = "point",
             palette = "Set2",
             main = "K-Means Segmentation Solution") +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),    
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


print(p3)
print(p4)

# Compute the mean of each variable per cluster
cluster_profile <- hotel_customers %>%
  group_by(Cluster) %>%
  summarise(
    Age = mean(Age, na.rm = TRUE),
    AverageLeadTime = mean(AverageLeadTime, na.rm = TRUE),
    DaysSinceCreation = mean(DaysSinceCreation, na.rm = TRUE),
    LodgingRevenue = mean(LodgingRevenue, na.rm = TRUE),
    OtherRevenue = mean(OtherRevenue, na.rm = TRUE),
    PersonsNights = mean(PersonsNights, na.rm = TRUE),
    RoomNights = mean(RoomNights, na.rm = TRUE),
    DaysSinceLastStay = mean(DaysSinceLastStay, na.rm = TRUE),
    DaysSinceFirstStay = mean(DaysSinceFirstStay, na.rm = TRUE)
  ) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Average")

# Bar plot of average variable values per cluster
p5 <- ggplot(cluster_profile, aes(x = Variable, y = Average, fill = Cluster)) +
  geom_col(position = "dodge") +
  labs(
    title = "Overall Segmentation Solution by Mean",
    x = "Variable",
    y = "Average Value"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),    
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

print(p5)

# Compute average and total revenues per cluster
p6 <- hotel_customers %>%
  group_by(Cluster) %>%
  summarise(avg_lodgingrevenue = mean(LodgingRevenue, na.rm = TRUE)) %>%
  ggplot(aes(x = Cluster, y = avg_lodgingrevenue, fill = Cluster)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(avg_lodgingrevenue, 0)), vjust = -0.5) +
  labs(
    title = "Average Lodging Revenue per Customer by Cluster",
    x = "Cluster",
    y = "Average Lodging Revenue (€)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),    
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

p7 <- hotel_customers %>%
  group_by(Cluster) %>%
  summarise(avg_otherrevenue = mean(OtherRevenue, na.rm = TRUE)) %>%
  ggplot(aes(x = Cluster, y = avg_otherrevenue, fill = Cluster)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(avg_otherrevenue, 0)), vjust = -0.5) +
  labs(
    title = "Average Other Revenue per Customer by Cluster",
    x = "Cluster",
    y = "Average Other Revenue (€)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),    
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

print(p6)
print(p7)

this_doc <- "Assignment3_Interpretations.Rmd"
code_path <- knitr::purl(this_doc, documentation = 0, quiet = TRUE)
code <- readLines(code_path, warn = FALSE)

knitr::asis_output(paste0(
  "\\begingroup\\footnotesize\n```r\n",
  paste(code, collapse = "\n"),
  "\n```\n\\endgroup\n"
))
