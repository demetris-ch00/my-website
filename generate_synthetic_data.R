set.seed(2024)
n <- 300

age_group <- sample(
  c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  n,
  replace = TRUE,
  prob = c(0.12, 0.25, 0.35, 0.15, 0.08, 0.04, 0.01)
)

gender <- sample(
  c("Male", "Female", "Non-binary", "Prefer not to say"),
  n,
  replace = TRUE,
  prob = c(0.45, 0.45, 0.07, 0.03)
)

occupation <- sample(
  c("Student", "Full-time employed", "Part-time employed", 
    "Freelancer", "Unemployed", "Retired"),
  n,
  replace = TRUE
)

screen_time <- sapply(age_group, function(age) {
  if (age %in% c("Under 18", "18-24")) {
    sample(3:5, 1)
  } else if (age %in% c("25-34", "35-44")) {
    sample(2:4, 1)
  } else {
    sample(1:3, 1)
  }
})

primary_device <- sapply(age_group, function(age) {
  if (age %in% c("Under 18", "18-24")) {
    sample(c("Smartphone", "Laptop", "Tablet"), 1, prob = c(0.7, 0.2, 0.1))
  } else if (age %in% c("25-34", "35-44")) {
    sample(c("Smartphone", "Laptop", "Desktop computer"), 1, prob = c(0.5, 0.3, 0.2))
  } else {
    sample(c("Smartphone", "Laptop", "Desktop computer"), 1, prob = c(0.5, 0.2, 0.3))
  }
})

digital_detox <- sapply(1:n, function(x) {
  if (runif(1) < 0.25) {
    "I don't detox"
  } else {
    paste(sample(c("Nature walks", "Reading physical books", "Meditation",
                   "Sports/Exercise", "Cooking"), sample(1:2, 1)), collapse = ", ")
  }
})

social_platform <- sample(
  c("Instagram", "TikTok", "YouTube", "X/Twitter", "Facebook", "LinkedIn", "Reddit", "None"),
  n,
  replace = TRUE
)

eye_strain <- sample(
  c("Frequently", "Occasionally", "Rarely", "Never"),
  n,
  replace = TRUE,
  prob = c(0.3, 0.4, 0.2, 0.1)
)

phone_anxiety <- sapply(age_group, function(age) {
  if (age == "Under 18") {
    sample(4:5, 1, prob = c(0.7, 0.3))  
  } else if (age == "18-24") {
    sample(4:5, 1, prob = c(0.4, 0.6))  
  } else if (age %in% c("25-34", "35-44")) {
    sample(3:4, 1, prob = c(0.6, 0.4))  
  } else if (age %in% c("45-54", "55-64")) {
    sample(2:3, 1, prob = c(0.7, 0.3))  
  } else {
    sample(1:2, 1, prob = c(0.6, 0.4)) 
  }
})

favorite_app <- sample(
  c("Spotify", "Duolingo", "MyFitnessPal", "Notion", "Headspace", "Google Maps", "Discord", "Other"),
  n,
  replace = TRUE
)

survey_data <- data.frame(
  Age_Group = age_group,
  Gender_Identity = gender,
  Occupation = occupation,
  Screen_Time = screen_time,
  Primary_Device = primary_device,
  Social_Platform = social_platform,
  Eye_Strain = eye_strain,
  Digital_Detox = digital_detox,
  Phone_Anxiety = phone_anxiety,
  Favorite_App = favorite_app
)

write.csv(survey_data, "digital_habits_survey.csv", row.names = FALSE)