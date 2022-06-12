################################################################################
# Thesis Part 1.
################################################################################
library(readxl)
library(dplyr)
library(plm)
library(ggplot2)
library(stargazer)
library(dplyr)
library(tidyverse)

#Set file directory in advance.

df <- read_excel("RCAS.xlsx")
colnames(df) <- c('Year', 'Product Code', 'Czech Value', "World Value", 
                  'Czech Share', 'World Share','RCA')

df %>% 
  drop_na()


View(df)

for (i in seq(1998,2020,1)) {
  df_cleaned <- df[df$Year == i, ]
  df_cleaned_fit <- df_cleaned[df_cleaned$RCA > 1, ]
  df_cleaned_fit <- df_cleaned_fit %>%
    arrange(desc(RCA))
  
  df_cleaned_fit$Rank <- seq(1,length(df_cleaned_fit$RCA),1)
  df_cleaned_fit$Rank <- df_cleaned_fit$Rank - 0.5
  
  df_cleaned_fit <- df_cleaned_fit %>%
    mutate(RCA = log(RCA)) %>%
    mutate(Rank = log(Rank))
  
  model <- lm(Rank ~ RCA, data = df_cleaned_fit)
  print(cbind(unname(model$coefficients['RCA']), summary(model)$r.squared))
  
  #get standard errors
  #print(round(summary(model)$coef[[4]], 4)) 
  
}





df_panel <- df[df$RCA > 1, ]

df_panel <- df_panel %>%
  arrange(desc(RCA))

View(df_panel)

df_panel$Rank <- seq(1,length(df_panel$RCA),1)
df_panel$Rank <- df_panel$Rank - 0.5

df_panel <- df_panel %>%
  mutate(RCA = log(RCA)) %>%
  mutate(Rank = log(Rank))

df_panel %>%
  arrange(desc(Year)) %>%
  drop_na()

df_panel <- drop_na(df_panel)



table(index(df_panel), useNA = "ifany")

model1 <- plm(Rank ~ RCA, data = df_panel, index = 'Year', model = 'pooling')
summary(model1)
#confint(model1)


plot(df_panel$Rank, df_panel$RCA)


df_panel$density <- predict(prcomp(~Rank+RCA, df_panel))[,1]
ggplot(df_panel, aes(x = Rank, y = RCA, color = density)) + 
  geom_point(shape = 16, size = 2, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  labs(
    x = "log(Rank-0.5)",
    y = "log(RCA)"
  ) +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") + 
  theme(
    axis.title.x = element_text(size = 12, face = "italic"),
    axis.title.y = element_text(size = 12, face = "italic")
  )

stargazer(model1)