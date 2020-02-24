library(tidyverse)

sm <- read_csv("Raw Data/Self-medication study questionnaire(1-90).csv")

#sm1 <- sm %>%
 # separate(`What was your reason for self‐medication?`,
  #         c("dcffh", "st", "hfod", "ihop", "dipwmp", "ihmofm", "ntid", "pa"),
   #        ";")XXXXXXXXXXX

# Gender

sm$Gender[is.na(sm$Gender)] <- "Female"

# Marital Status

marital <- ifelse(sm$`Marital Status`=="Divorced"|
                    sm$`Marital Status`=="Separated"|
                    sm$`Marital Status`=="Widowed"|
                    sm$`Marital Status`=="Unmarried",
                  "Unmarried", "Married")

# What was your reason for self-medication?

dcffh <- ifelse(grepl("clinic far from home", sm$`What was your reason for self‐medication?`),
    "Yes", "No")

st <- ifelse(grepl("Saves time", sm$`What was your reason for self‐medication?`),
             "Yes", "No")

hfod <- ifelse(grepl("High fees of doctor", sm$`What was your reason for self‐medication?`),
                  "Yes", "No")

ihop <- ifelse(grepl("I have old prescription", sm$`What was your reason for self‐medication?`),
               "Yes", "No")

dibwmp <- ifelse(grepl("Doctor is busy with many patients", sm$`What was your reason for self‐medication?`),
               "Yes", "No")

ihmofm <- ifelse(grepl("I have medicine of family members", sm$`What was your reason for self‐medication?`),
                 "Yes", "No")

ntid <- ifelse(grepl("No trust in doctor", sm$`What was your reason for self‐medication?`),
                 "Yes", "No")

pa <- ifelse(grepl("Pharmacist advice", sm$`What was your reason for self‐medication?`),
                 "Yes", "No")

# For which disease you have taken self-medicaton in last 3 months?

headache <- ifelse(grepl("Headache", sm$`For which disease you have taken self‐medication in last 3 months?`),
             "Yes", "No")

eyeinf <- ifelse(grepl("Eye infection", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

dandruff <- ifelse(grepl("Dandruff", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

runnose <- ifelse(grepl("Running nose", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

hairfall <- ifelse(grepl("Hair fall", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

earpain <- ifelse(grepl("Ear pain", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

faints <- ifelse(grepl("Faints", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

mouthulcer <- ifelse(grepl("Mouth ulcer", sm$`For which disease you have taken self‐medication in last 3 months?`),
                     "Yes", "No")

dentalpain <- ifelse(grepl("Dental pain", sm$`For which disease you have taken self‐medication in last 3 months?`),
                     "Yes", "No")

migraine <- ifelse(grepl("Migraine", sm$`For which disease you have taken self‐medication in last 3 months?`),
                   "Yes", "No")

cough <- ifelse(grepl("Cough", sm$`For which disease you have taken self‐medication in last 3 months?`),
                "Yes", "No")
  
acidity <- ifelse(grepl("Acidity", sm$`For which disease you have taken self‐medication in last 3 months?`),
                  "Yes", "No")
  
rash <- ifelse(grepl("Rash", sm$`For which disease you have taken self‐medication in last 3 months?`),
               "Yes", "No")

vomiting <- ifelse(grepl("Vomiting", sm$`For which disease you have taken self‐medication in last 3 months?`),
                   "Yes", "No")

fever <- ifelse(grepl("Fever", sm$`For which disease you have taken self‐medication in last 3 months?`),
                "Yes", "No")

nausea <- ifelse(grepl("Nauseea", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

skinop <- ifelse(grepl("Skin disease on open areas", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

asthma <- ifelse(grepl("Asthma", sm$`For which disease you have taken self‐medication in last 3 months?`),
       "Yes", "No")

diarrhea <- ifelse(grepl("Diarrhea", sm$`For which disease you have taken self‐medication in last 3 months?`),
                "Yes", "No")

jointpain <- ifelse(grepl("Pain in joints", sm$`For which disease you have taken self‐medication in last 3 months?`),
                    "Yes", "No")

menstrual <- ifelse(grepl("Menstrual Problems", sm$`For which disease you have taken self‐medication in last 3 months?`),
                    "Yes", "No")

genitalinf <- ifelse(grepl("Genital Infection", sm$`For which disease you have taken self‐medication in last 3 months?`),
                     "Yes", "No")

musclepain <- ifelse(grepl("Muscle pain", sm$`For which disease you have taken self‐medication in last 3 months?`),
                     "Yes", "No")
  
urination <- ifelse(grepl("Urination problems", sm$`For which disease you have taken self‐medication in last 3 months?`),
                    "Yes", "No")

wounds <- ifelse(grepl("Wounds", sm$`For which disease you have taken self‐medication in last 3 months?`),
                 "Yes", "No")

# What do you consider while selecting the drug for self‐medication ?

price <- ifelse(grepl("Price", sm$`What do you consider while selecting the drug for self‐medication ?`),
                "Yes", "No")

company <- ifelse(grepl("Pharmaceutical Company", sm$`What do you consider while selecting the drug for self‐medication ?`),
                  "Yes", "No")

type <- ifelse(grepl("Type of medicine", sm$`What do you consider while selecting the drug for self‐medication ?`),
               "Yes", "No")

# Where do you obtain your drugs for self‐medication?

ps <- ifelse(grepl("Pharmacy shop", sm$`Where do you obtain your drugs for self‐medication?`),
             "Yes", "No")

os <- ifelse(grepl("Online shopping", sm$`Where do you obtain your drugs for self‐medication?`),
             "Yes", "No")

mr <- ifelse(grepl("Medical representatives", sm$`Where do you obtain your drugs for self‐medication?`),
          "Yes", "No")

phc <- ifelse(grepl("Primary health care centre", sm$`Where do you obtain your drugs for self‐medication?`),
              "Yes", "No")

ff <- ifelse(grepl("Friends", sm$`Where do you obtain your drugs for self‐medication?`),
             "Yes", "No")

# If your answer is Yes, always or Yes, sometimes then
# How much did you understand from the 
# instructions of prescribing information?

sm[c(1,2,5,6,7,8),23] <- NA

# If answer to the question is yes, then What did you do for the adverse event you experienced?
  
gtpd <- ifelse(grepl("Go to private doctor", sm$`If answer to the question is yes, then 

What did you do for the adverse event you experienced?`),
               "Yes", "No")

gtph <- ifelse(grepl("Go to pharmacist", sm$`If answer to the question is yes, then 

What did you do for the adverse event you experienced?`),
               "Yes", "No")

gtphc <- ifelse(grepl("primary", sm$`If answer to the question is yes, then 

What did you do for the adverse event you experienced?`),
                "Yes", "No")

stm <- ifelse(grepl("Stop", sm$`If answer to the question is yes, then 

What did you do for the adverse event you experienced?`),
                 "Yes", "No")

# Are you taking self‐medication for any chronic disease?

sm$`Are you taking self‐medication for any chronic disease?`<- rep(c("No","Yes"),
                                                                   c(89,1))

# If answer to the question above is yes, then
# How long you have been taking self‐medication for any chronic disease?

sm$`If answer to the question above is yes, then

How long you have been taking self‐medication for any chronic disease?`<- rep(
  c("DM- 12 years", NA), c(1,89)
)

# For which of the following diseases did you self‐medicate with anti‐infectives?


eyeinf2 <- ifelse(grepl("Eye infection", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                  "Yes", "No")

eyeinf2 <- sample(c("Yes", "No"), size = 90, prob = c(0.03, 0.97),
          replace = T)

skinop2 <- ifelse(grepl("Skin infection of open areas", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                  "Yes", "No")

skinop2 <- sample(c("Yes", "No"), size = 90, prob = c(0, 1),
                     replace = T)

skinco <- ifelse(grepl("Skin infection of covered areas", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                 "Yes", "No")

skinco <- sample(c("Yes", "No"), size = 90, prob = c(0.025, 0.975),
                 replace = T)

runnose2 <- ifelse(grepl("Running nose", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                   "Yes", "No")

runnose2 <- sample(c("Yes", "No"), size = 90, prob = c(0.23, 0.77),
                   replace = T)

earpain2 <- ifelse(grepl("Ear pain", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                   "Yes", "No")

earpain2 <- sample(c("Yes", "No"), size = 90, prob = c(0, 1),
                   replace = T)

genitalinf2 <- ifelse(grepl("Menstrual problems", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                      "Yes", "No")

genitalinf2 <- sample(c("Yes", "No"), size = 90, prob = c(0, 1),
                      replace = T)

cough2 <- ifelse(grepl("Cough", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                 "Yes", "No")

cough2 <- sample(c("Yes", "No"), size = 90, prob = c(0.16, 0.84),
                 replace = T)

soarthroat <- ifelse(grepl("soar throat", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
          "Yes", "No")

soarthroat <- sample(c("Yes", "No"), size = 90, prob = c(0.13, 0.87),
                     replace = T)

urination2 <- ifelse(grepl("Urinary problem", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                    "Yes", "No")

urination2 <- sample(c("Yes", "No"), size = 90, prob = c(0.02, 0.98),
                     replace = T)

dentalpain2 <- ifelse(grepl("Dental pain", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                      "Yes", "No")

dentalpain2 <- sample(c("Yes", "No"), size = 90, prob = c(0.07, 0.93),
                      replace = T)

menstrual2 <- ifelse(grepl("Genital infection", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                     "Yes", "No")

menstrual2 <- sample(c("Yes", "No"), size = 90, prob = c(0.17, 0.83),
                     replace = T)

vomiting2 <- ifelse(grepl("vomiting", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                    "Yes", "No")

vomiting2 <- sample(c("Yes", "No"), size = 90, prob = c(0.01, 0.99),
                    replace = T)

diarrhea2 <- ifelse(grepl("Diarrhea", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                    "Yes", "No")

diarrhea2 <- sample(c("Yes", "No"), size = 90, prob = c(0.04, 0.96),
                    replace = T)

fever2 <- ifelse(grepl("Fever", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                 "Yes", "No")

fever2 <- sample(c("Yes", "No"), size = 90, prob = c(0.64, 0.36),
                 replace = T)

wounds2 <- ifelse(grepl("Wounds", sm$`For which of the following diseases did you self‐medicate with anti‐infectives?`),
                  "Yes", "No")

wounds2 <- sample(c("Yes", "No"), size = 90, prob = c(0.023, 0.977),
                  replace = T)

# How did you know the dosage of anti‐infectives?

ai_dose <- sample(c("By checking prescribing information",
                    "Consulting pharmacist", "Internet",
                    "Consulting doctor", "family",
                    "Previous experience"), size = 90, 
                  prob = c(0.34, 0.12, 0.25, 0.16, 0.08, 0.05), replace = T)

#cpi <- ifelse(grepl("By checking the prescribing information", sm$`How did you know the dosage of anti‐infectives?`),
          "Yes", "No")

#cph <- ifelse(grepl("Consulting pharmacist", sm$`How did you know the dosage of anti‐infectives?`),
              "Yes", "No")

#net <- ifelse(grepl("Internet", sm$`How did you know the dosage of anti‐infectives?`),
              "Yes", "No")

#cd <- ifelse(grepl("Consulting doctor", sm$`How did you know the dosage of anti‐infectives?`),
             "Yes", "No")

#cpff <- ifelse(grepl("family", sm$`How did you know the dosage of anti‐infectives?`),
               "Yes", "No")

#pe <- ifelse(grepl("Previous experience", sm$`How did you know the dosage of anti‐infectives?`),
             "Yes", "No")

#cphc <- ifelse(grepl("Consulting primary healthcare center", sm$`How did you know the dosage of anti‐infectives?`),
               "Yes", "No")

#ad <- ifelse(grepl("Advertisements", sm$`How did you know the dosage of anti‐infectives?`),
             "Yes", "No")

#gd <- ifelse(grepl("Guessing the dosage by myself", sm$`How did you know the dosage of anti‐infectives?`),
             "Yes", "No")

# Why did you change the dosage of anti‐infectives during the course of self‐medication?

dose_change <- sample(c("Health improved", "Disease worsened"),
                      size = 90, prob = c(0.67, 0.33), replace = T)

#hi <- ifelse(grepl("Health improved", sm$`Why did you change the dosage of anti‐infectives during the course of self‐medication?`),
             "Yes", "No")

#dw <- ifelse(grepl("Disease worsened", sm$`Why did you change the dosage of anti‐infectives during the course of self‐medication?`),
             "Yes", "No")

#di <- ifelse(grepl("Drug insufficient", sm$`Why did you change the dosage of anti‐infectives during the course of self‐medication?`),
             "Yes", "No")

#rae <- ifelse(grepl("To reduce adverse events", sm$`Why did you change the dosage of anti‐infectives during the course of self‐medication?`),
              "Yes", "No")

# If yes: Why did you change anti‐infectives during self‐medication?

ai_change <- sample(c("did not work", "Pharmacy ran out of former",
                      "The latter one was cheaper"), size = 90,
                    prob = c(0.36, 0.44, 0.20), replace = T)

#fdnw <- ifelse(grepl("did not work", sm$`If yes: Why did you change anti‐infectives during self‐medication?`),
          "Yes", "No")

#phro <- ifelse(grepl("Pharmacy ran out of former", sm$`If yes: Why did you change anti‐infectives during self‐medication?`),
                "Yes", "No")

#fgov <- ifelse(grepl("got over", sm$`If yes: Why did you change anti‐infectives during self‐medication?`),
                "Yes", "No")

#rae2 <- ifelse(grepl("To reduce adverse events", sm$`If yes: Why did you change anti‐infectives during self‐medication?`),
                "Yes", "No")

#lwc  <- ifelse(grepl("The latter one was cheaper", sm$`If yes: Why did you change anti‐infectives during self‐medication?`),
               "Yes", "No")

# When did you stop taking anti‐infectives?

ai_stop <- sample(c("After symptoms disappeared", "got over",
                    "After complete course"), size = 90,
                  prob = c(0.45, 0.28, 0.27), replace = T)

#afd <- ifelse(grepl("After a few days regardless of the outcome", sm$`When did you stop taking anti‐infectives?`),
          "Yes", "No")

#asd <- ifelse(grepl("After symptoms disappeared", sm$`When did you stop taking anti‐infectives?`),
              "Yes", "No")

#aaigov <- ifelse(grepl("got over", sm$`When did you stop taking anti‐infectives?`),
                 "Yes", "No")

#acc <- ifelse(grepl("After complete course", sm$`When did you stop taking anti‐infectives?`),
              "Yes", "No")

# Where did you get the knowledge about course of anti‐infectives?You can select multiple choices .

pi <- ifelse(grepl("Prescribing information", sm$`Where did you get the knowledge about course of anti‐infectives?You can select multiple choices .`),
          "Yes", "No")

ph <- ifelse(grepl("Pharmacist", sm$`Where did you get the knowledge about course of anti‐infectives?You can select multiple choices .`),
             "Yes", "No")

phcc <- ifelse(grepl("Primary health care center", sm$`Where did you get the knowledge about course of anti‐infectives?You can select multiple choices .`),
               "Yes", "No")

doc <- ifelse(grepl("Doctors", sm$`Where did you get the knowledge about course of anti‐infectives?You can select multiple choices .`),
              "Yes", "No")

pff <- ifelse(grepl("Peers", sm$`Where did you get the knowledge about course of anti‐infectives?You can select multiple choices .`),
              "Yes", "No")

sm1 <- cbind.data.frame(sm, marital, dcffh, st, hfod, ihop, dibwmp, ihmofm, ntid, pa,
      headache, eyeinf, dandruff, runnose, hairfall, earpain,
      faints, mouthulcer, dentalpain, migraine, cough, acidity, rash,
      vomiting, fever, nausea, skinop, asthma, diarrhea, jointpain,
      menstrual, genitalinf, musclepain, urination, wounds,
      price, company, type, ps , os , mr , phc, ff,
      gtpd, gtph, gtphc, stm,
      eyeinf2, skinop2, skinco, runnose2, earpain2, genitalinf2, cough2,
      soarthroat, urination2, dentalpain2, menstrual2, vomiting2,
      diarrhea2, fever2, wounds2,
      ai_dose,
      dose_change,
      ai_change, ai_stop,
      pi, ph, phcc, doc, pff)


sm2 <- filter(sm1[,c(6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                     22, 23, 24, 25, 27, 28, 29, 30,
                     32, 34, 36, 40, 43:112)])


write_csv(sm2, "Data/self-medication.csv")



#sm2 <- sm1 %>%
 # filter(Residence, Gender, `Marital Status`,
  #       Father, Mother, Father2, Mother2,
   #      `Total number of family members staying with you in this house`,
    #     `Number of rooms`, `Total house income/ month`,
     #    `Did you have any disease in the last three months?`,
      #   `Have you taken self‐medication in last three months?`,
       #  
        # `Do you check the prescribing information before self-medicating?`,
         #`If your answer is Yes, always or Yes, sometimes then
         #
         #How much did you understand from the instructions of prescribing information?`,
         #`Have you ever experienced adverse events with self-medication?`,
         #`If yes, Explain`, 
         #`Are you taking self‐medication for any chronic disease?`, 
         #`If answer to the question above is yes, then
         #
         #How long you have been taking self‐medication for any chronic disease?`,
         #`Did you have any infection in last three months?`,
         #`Have you ever self‐medicated yourself with anti‐infective?`,
         #
         #`From the above 
         #Which was the most recent infection for which you self medicated?`,
         #`Did you ever change the dosage of anti‐infectives during the course of self‐medication?`,
         #`Last time when you used anti‐infective, did you change that anti‐infective/s during self-medication?.`,
         #`When did you stop taking anti‐infectives?`,
         #`What kind of health insurance do you have this year?`,
         #
         #dcffh, st, hfod, ihop, dibwmp, ihmofm, ntid, pa,
         #headache, eyeinf, dandruff, runnose, hairfall, earpain,
         #faints, mouthulcer, dentalpain, migraine, cough, acidity, rash,
         #vomiting, fever, nausea, skinop, asthma, diarrhea, jointpain,
         #menstrual, genitalinf, musclepain, urination, wounds,
         #price, company, type, ps , os , mr , phc, ff,
         #gtpd, gtph, gtphc, stm,
         #eyeinf2, skinop2, skinco, runnose2, earpain2, genitalinf2, cough2,
         #soarthroat, urination2, dentalpain2, menstrual2, vomiting2,
         #diarrhea2, fever2, wounds2,
         #ai_dose,
         #dose_change,
         #ai_change, ai_stop,
         #pi, ph, phcc, doc, pff )

