# Calculation of social class

library(tidyverse)

selfmed <- read.csv("Data/self-medication.csv")

# Coding variables

# Residence

resid <- ifelse(selfmed$Residence == "Urban", 2, 1)
resid

# replacing NAs with 2 (Urban)
resid[is.na(resid)] <- 2

# Occupation
# Father

occFather <- ifelse(selfmed$Father == "Non-working/house wife", 1,
             ifelse(selfmed$Father == "Unskilled manual worker", 2,
             ifelse(selfmed$Father == "Skilled manual worker", 3,
             ifelse(selfmed$Father == "Farmer", 4,
             ifelse(selfmed$Father == "Trades/business", 5, 6)))))
occFather

# replacing NAs with 5 (Trades/business)
occFather[is.na(occFather)] <- 5
occFather

# Mother

occMother <- ifelse(selfmed$Mother == "Non-working/house wife", 1,
             ifelse(selfmed$Mother == "Unskilled manual worker", 2,
             ifelse(selfmed$Mother == "Skilled manual worker", 3,
             ifelse(selfmed$Mother == "Farmer", 4,
             ifelse(selfmed$Mother == "Trades/business", 5, 6)))))
occMother

# replacing NAs with 5 (Trades/business)
occMother[is.na(occMother)] <- 5
occMother

# Education
# Father

eduFather <- ifelse(selfmed$Father2 == "Illiterate", 1,
             ifelse(selfmed$Father2 == "Read and write", 2,
             ifelse(selfmed$Father2 == "Primary", 3,
             ifelse(selfmed$Father2 == "Secondary", 4,
             ifelse(selfmed$Father2 == "University graduate", 5, 6)))))
eduFather

# replacing NAs with 5 (University graduate)
eduFather[is.na(eduFather)] <- 5
eduFather

# Mother

eduMother <- ifelse(selfmed$Mother2 == "Illiterate", 1,
             ifelse(selfmed$Mother2 == "Read and write", 2,
             ifelse(selfmed$Mother2 == "Primary", 3,
             ifelse(selfmed$Mother2 == "Secondary", 4,
             ifelse(selfmed$Mother2 == "University graduate", 5, 6)))))
eduMother

# replacing NAs with 5 (University graduate)
eduMother[is.na(eduMother)] <- 5
eduMother

# Total number of family members
selfmed$Total.number.of.family.members.staying.with.you.in.this.house

# Replacing NAs with 4
selfmed$Total.number.of.family.members.staying.with.you.in.this.house[is.na(selfmed$Total.number.of.family.members.staying.with.you.in.this.house)] <- 4

# Number of rooms
selfmed$Number.of.rooms

# Crowding index
crowd <- ifelse((selfmed$Total.number.of.family.members.staying.with.you.in.this.house/
                   selfmed$Number.of.rooms)> 1, 1, 2)
crowd

# Income
income <- ifelse(selfmed$Total.house.income..month == "Enough", 2, 1)
income

# Score
total <- resid+ occFather+ occMother+ eduFather+ eduMother+ crowd+ income
score <- (total/30)*100
score

# social class
selfmed <- selfmed %>%
          mutate(social_class = ifelse(score >= 75, "High",
                ifelse(score >= 50, "Moderate",
                ifelse(score >= 25, "Low", "Very low"))))
selfmed$social_class

pie_theme <- theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15, face = "bold"),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold"))

selfmed %>%
  group_by(social_class) %>%
   summarise(counts = n())%>%
     mutate(percentage = round((counts/ sum(counts)*100))) %>%
  print() %>%
  ggplot(
    aes(x= "",
        y= percentage,
        fill= social_class)
    )+
  geom_bar(stat = "identity")+ 
  coord_polar(theta = "y")+
  geom_text(
    aes(label = paste(percentage, "%")),
    position = position_stack(vjust = 0.55),
    hjust = 0.4,
    size = 8
    ) +
  labs(fill= "Social Class")+
  pie_theme +
  scale_fill_brewer(palette = "Set2")

