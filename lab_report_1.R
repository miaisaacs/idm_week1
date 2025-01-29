# LAB REPORT 1

lab1 = read.csv(file.choose(~data/da_LINet.csv))

colnames(lab1)
nrow(lab1)

lab1[2, "Pennsylvania"]

lab1[2, 6]
lab1[30, 5:14]
lab1[, 5]
lab1[36, "New.York.City"]

summary(lab1)


plot(lab1$ New.York.City, type='l', lty=1, main='Influenza-like illness in NYC ', 
     xaxt='n',  # this is telling R not to plot x-axis
     xlab='Date', ylab='ILI per 100,000')
axis(1, at=1:nrow(lab1), labels = lab1$Date)  # add an axis


matplot(1:nrow(lab1), cbind(lab1$New.York.City, lab1$New.York), type='l', lty=1, col=c("blue", "red"),
        main='Influenza-like illness in NYC and NY State', xlab='Date', ylab='ILI per 100,000', xaxt='n')
axis(1, at=1:nrow(lab1), labels=lab1$Date, las=2, cex.axis=0.7)
legend("topright", legend=c("NYC", "NY State"), col=c("blue", "red"), lty=1)


library(tidyverse)

df_long <- lab1 %>%
  gather(key = "State", value = "Value", -Date)  # Convert to long format

# Convert 'Date' to Date type for proper ordering, and remove rows with NA values
df_long <- df_long %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(!is.na(Value))  # Remove rows with NA values

# Convert 'Date' to factor to treat it as categorical for ggplot
df_long$Date <- as.factor(df_long$Date)

# Create a box plot for each date, with values from all states included in the box plot for that date
ggplot(df_long, aes(x = Date, y = Value)) +
  geom_boxplot() +
  labs(title = "Boxplot of Values for Each Date", 
       x = "Date", 
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


quadratic_fn <- function(a, b, c) {
  x1 = (-b + sqrt(b^2 - 4*a*c)) / (2 * a)
  x2 = (-b - sqrt(b^2 - 4*a*c)) / (2 * a)
  return(c(x1, x2))
}

quadratic_fn(a = 2, b = 10, c = 3)
quadratic_fn(a = 5, b = -6, c = 1)






