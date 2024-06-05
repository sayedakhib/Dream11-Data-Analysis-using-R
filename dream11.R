#install the required packages
install.packages(c("ggplot2", "dplyr"))

# Load required libraries
library(dplyr)
library(ggplot2)

# Read the CSV file
df <- read.csv("dream11.csv")
print(df)
# Display the first few rows of the data
head(df)
# Get summary statistics
summary(df)

# Check for missing values
sum(is.na(df))

str(df)

# Descriptive statistics
IQR<- IQR(df$Credits,na.rm = TRUE)
var<- var(df$Credits,na.rm = TRUE)
sd<- sd(df$Credits,na.rm = TRUE)

print(paste("IQR of Credits:", IQR))
print(paste("Variance of Credits:", var))
print(paste("Standard Deviation of Credits:", sd))

# Covariance and correlation
cov_matrix <- cov(select(df,Credits))
print(cov_matrix)

#filter
credits<-df%>%filter(Credits>8.0)
print(credits)



# Count of players in each role
role_counts <- table(df$Role)
barplot(role_counts, main="Player Role Distribution", xlab="Role", ylab="Count")

# Summary statistics for credits
summary(df$Credits)

# Histogram of player credits
hist(df$Credits, main="Player Credits Distribution", xlab="Credits")

# Assuming you want to visualize the distribution of Roles
role_distribution <- df %>% 
  group_by(Role) %>%
  summarise(count = n())

#pie chart
ggplot(role_distribution, aes(x = "", y = count, fill = Role)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Distribution of Player Roles")

#line graph
ggplot(df, aes(x = Player, y = Credits, group = Team, color = Team)) +
  geom_line() +
  labs(title = "Player Credits by Team", x = "Player", y = "Credits") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Assuming you want to compare Credits of Players by Role
ggplot(df, aes(x = Role, y = Credits, fill = Team)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Player Credits by Role", x = "Role", y = "Credits") +
  scale_fill_manual(values = c("CSK" = "yellow", "DC" = "blue", "GT" = "green", "KKR" = "purple", "LSG" = "orange", "MI" = "blue", "PBKS" = "red", "RCB" = "red", "RR" = "pink", "SRH" = "orange"))

ggplot(df, aes(x = Credits)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Player Credits", x = "Player Credits", y = "Frequency")


ggplot(df, aes(x = Role, y = Credits, color = Team)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 3) +
  labs(title = "Scatter Plot of Player Credits by Role",
       x = "Player Role",
       y = "Player Credits",
       color = "Team") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
