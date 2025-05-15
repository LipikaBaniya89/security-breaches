#Figure 2
cat("Lipika Baniya", "CMIS 527 – 73D", "assignment-number (01)", "\n")
cat("I Lipika Baniya certify that I did this work on assignment 01 by myself", "\n")

#Import necessary libraries
library(ggplot2)

#set random seed for reproducibility (ensures the same random numbers are generated every time)
set.seed(1492)

# Create a DataFrame with a column named 'varl' containing 5000 random numbers from a standard normal distribution
test.df = data.frame(varl=rnorm(5000))

# Generate a histogram for the 'varl' column with improved styling
ggplot(data = test.df, aes(x = varl)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of 'varl' Column") +
  xlab("Value") +
  ylab("Frequency") +
  theme_minimal()

# Generate a box plot for the 'varl' column
ggplot(data = test.df, aes(y = varl)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red", outlier.shape = 16) +
  ggtitle("Boxplot of 'varl' Column") +
  ylab("Value") +
  theme_minimal()

# Display summary statistics
summary(test.df$varl)


#Listing 2.1
#Create a new dataframe including columns name, os, highvulns
assets.df <- data.frame (
  name = c("danube","gander", "ganges", "mekong", "orinoco"),
  os = c("W2K8", "RHEL5", "W2K8", "RHEL5", "RHEL5"),
  highvulns = c(1,0,2,0,0)
)

#Disply the data structure of the dataframe
str(assets.df)

#Display only the first 5 row 
head(assets.df)

#Only display  os column from the dataframe
head(assets.df$os)

#A new column ip is addedto the dataframe assests.df
assets.df$ip <- c("192.168.1.5", "10.2.7.5", "192.168.1.7", "10.2.7.6", "10.2.7.7")

#Filter out only more than one high vulnerability
head(assets.df[assets.df$highvilns >1,])

#A newn columnn zone is added and puts in either zone 1 or zone 2 based on IP address
assets.df$zones <-  ifelse(grepl("^192", assets.df$ip), "Zone1", "Zone2")

#Display head after adding a new column
head(assets.df)

# Pie chart for visualization of asset distribution across zones
zone_counts <- table(assets.df$zones)  # Count the frequency of each zone

# Plot the pie chart
pie(zone_counts, 
    main = "Distribution of Assets Across Zones",  # Title of the chart
    col = c("lightgreen", "lightcoral"),  # Colors for the zones
    labels = paste(names(zone_counts), "\n", round(100 * zone_counts / sum(zone_counts), 1), "%")  # Display percentage on chart
)