# Class data 

Height <- c(70, 70, 72, 70, 73, 63, 65, 70, 64, 70)
WingSpan <- c(69, 69, 73, 71, 73, 62, 66, 69, 64, 70)
Sex <- c(rep("Male", 5), rep("Female", 5))
Name <- c("Tanner", "Kory", "Chandler", "Elliot", "Eddie",
          "Emma", "Jacqueline", "Catherine", "Andi", "Emily")
DF <- data.frame(Name = Name, Height = Height, WingSpan = WingSpan, Sex = Sex)
rm(Name, Height, WingSpan, Sex)

# Graph

library(ggplot2)
ggplot(data = DF, aes(x = Height, y = WingSpan, color = Sex )) + 
  geom_point()

# 
library(dplyr)
Res <- DF %>%
  group_by(Sex) %>%
  summarize(MH = mean(Height), SH = sd(Height))
Res