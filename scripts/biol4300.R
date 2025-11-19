# Libraries
## install.packages("[insert package name here]")
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(ggpubr)

# Load mpg dataset
df <- mpg

# Inspect dataset
head(df)
tail(df)

# How many unique manufacturers?
unique(df$manufacturer)

# Select only land rover
subset <- subset(df, manufacturer == "land rover")
filter <- filter(df, manufacturer == "land rover")

# Let's make a base R plot
plot(x = df$cyl, y = df$cty)

# Let's remake plot in ggplot2
df_plot <- ggplot(data = df, 
       mapping = aes(x = cyl,
                     y = cty)) +
  geom_boxplot(aes(x = factor(cyl))) +
  geom_jitter(aes(x = factor(cyl)), 
              alpha = 0.3, width = 0.2) +
  labs(x = "Number of cylinders",
       y = "City miles per gallon") +
  theme_classic(base_size = 18)
  
df_plot2 <- ggplot(data = df, 
                  mapping = aes(x = cyl,
                                y = hwy)) +
  geom_boxplot(aes(x = factor(cyl))) +
  geom_jitter(aes(x = factor(cyl)), 
              alpha = 0.3, width = 0.2) +
  labs(x = "Number of cylinders",
       y = "Highway miles per gallon") +
  theme_classic(base_size = 18)

png(filename = "biol4300_plot.png", height = 6, width = 12, units = "in", res = 600)
ggarrange(df_plot, df_plot2, labels = c("(a)", "(b)"))
dev.off()

# Some statistics
df2 <- df %>%
  filter(manufacturer == "land rover") %>%
  mutate(anotherColumn = cty + hwy)

df %>%
  group_by(manufacturer) %>%
  summarize(mean_cty = mean(cty, na.rm = TRUE),
            sd_cty = sd(cty, na.rm = TRUE),
            
            mean_hwy = mean(hwy, na.rm = TRUE),
            sd_hwy = sd(hwy, na.rm = TRUE))





