getwd()

install.packages("writexl")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("dplyr")
install.packages("tidytext")
install.packages("tree")
install.packages("randomForest")
install.packages("foreach")
install.packages("doSNOW")
update.packages("randomForest")
update.packages("foreach")
update.packages("doSNOW")
install.packages("glmnet")
install.packages("gbm")
library(gbm)
library(glmnet)
library(randomForest)
library("foreach")
library("doSNOW")
library(tree)
library(tidyverse)
library(writexl)
library(ggplot2)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(dplyr)
library(tidytext)
library(class)

airtraining <- read.csv("Airbnb_Training.csv")
airtraining <- airtraining[complete.cases(airtraining$avg_rating),]
airtraining <- airtraining[airtraining$index != 16246, ]
airtesting <- read.csv("Airbnb_Testing.csv")
airtesting[775,]<- 0
airtesting[10274,] <- 0

airtraining$testing <- 0
airtesting$testing <- 1
airtesting <- add_column(airtesting, avg_rating = 0, .before = 2)

fullTrain <- rbind(airtraining, airtesting)

testtest <- fullTrain[fullTrain$testing == 1,]
dim(fullTrain)
set.seed(90210)


#####################data cleanup#####################

##index

##avg_rating
#check NA's in Avg rating found 4, 1 data looks scrambled, should probably drop as no
#avg rating can't be used to predict, time to recover 4 rows not really worth unscrambling
#View(fullTrain[is.na(fullTrain$avg_rating),])


##accommodates
#leave as factor or numeric?
table(fullTrain$accommodates)
fullTrain$accommodates <- as.numeric(fullTrain$accommodates)
sum(is.na(fullTrain$accommodates))

##amenities
listofamenities <- read.csv('Amenities.csv')


#Modify the amenities column of the Airbnb dataset, removing the curly brackets and adding commas to the start and end to aid in the searching loop.
fullTrain$amenities <- gsub('"', "", fullTrain$amenities)
fullTrain$amenities <- gsub("[{}]", "", fullTrain$amenities)
fullTrain$amenities <- paste(",", fullTrain$amenities, ",", sep="")

#Loop to create a new column for each amenity and to populate those columns with 1s or 0s depending on if it exists somewhere in the "amenities" column.
#To prevent things like "TV" returning true in "Cable TV" items, the loop is searching for ",TV," instead.
for (i in listofamenities$Amenity){
  j <- paste(",", i, ",", sep="")
  fullTrain[substr(str_replace_all(paste("amen", i, sep=""),"[^a-zA-Z_]",""),-10,10)] <- as.numeric(lengths(regmatches(fullTrain$amenities,gregexpr(j,fullTrain$amenities))))
}

##availability_30 
#leave as character for now.
table(fullTrain$availability_30)
fullTrain$availability_30 <- as.integer(fullTrain$availability_30)


##availability 365 
#was number, should be character if we are leaving the others as characters
table(fullTrain$availability_365)
fullTrain$availability_365 <- as.integer(fullTrain$availability_365)

##availability_60
#leave as character for now strange long string entry, investigate that row
#View(fullTrain[nchar(fullTrain$availability_60) > 5,])
table(fullTrain$availability_60)
fullTrain$availability_60 <- as.integer(fullTrain$availability_60)

##availability 90
#leave as character for now
table(fullTrain$availability_90)
fullTrain$availability_90 <- as.integer(fullTrain$availability_90)

##bathrooms
#leave as character for now
table(fullTrain$bathrooms)
fullTrain$bathrooms <- as.integer(fullTrain$bathrooms)
fullTrain$bathrooms <- ifelse(is.na(fullTrain$bathrooms),0,fullTrain$bathrooms)

##bedrooms
#27 null Most of them were in NY so I'm guessing the correct number is 0 for studio's etc.
#View(fullTrain[fullTrain$bedrooms=='',])
table(fullTrain$bedrooms)
fullTrain$bedrooms <- ifelse(fullTrain$bedrooms=='',0,fullTrain$bedrooms)
fullTrain$bedrooms <- as.integer(fullTrain$bedrooms)

##beds
#there were some na's most common class is 1, so sub that in for NA's 
#convert to character to match other characteristics of the rental, can try numeric later
table(fullTrain$beds)
median(fullTrain[complete.cases(fullTrain$beds),"beds"])
fullTrain$beds<- ifelse(is.na(fullTrain$beds),1,fullTrain$beds)
#fullTrain$beds <- as.character(fullTrain$beds)

##cancellation_policy
#only 1 point of no refunds, could reclassify as strict, leave for now
table(fullTrain$cancellation_policy)
sum(is.na(fullTrain$cancellation_policy))

##city_name
table(fullTrain$city_name)
sum(is.na(fullTrain$city_name))
sum(fullTrain$city_name=='')

##cleaning_fee
#convert all dollars to be numeric will turn non numerics to na
fullTrain$cleaning_fee <- as.numeric(fullTrain$cleaning_fee)
fullTrain$cleaning_fee <- ifelse(is.na(fullTrain$cleaning_fee),0,fullTrain$cleaning_fee)
mean(fullTrain$cleaning_fee)
table(fullTrain$cleaning_fee)

##country
#all but one country is US, could remove for other data methods, leave for trees I guess
table(fullTrain$country) 
fullTrain$country <- NULL

##experiences_offered
#all are none, remove
table(fullTrain$experiences_offered)
fullTrain$experiences_offered <- NULL

##extra people
#extra people, is a monetary charge, should be numeric
#not going to worry about scaling until we move to LASSO or ridge methods
fullTrain$extra_people <-as.numeric(fullTrain$extra_people)
table(fullTrain$extra_people)
sum(is.na(fullTrain$extra_people))
#scale(fullTrain$extra_people,center=FALSE, scale=TRUE)

##first_review
#looks like a serialized date.
#some of the functions might handle a date variable different, so I will convert
#to a date it looks like it uses excel origin.
summary(fullTrain$first_review)
table(fullTrain$first_review)
fullTrain$first_review <- fullTrain$first_review - 39769
fullTrain$first_review <- ifelse(fullTrain$first_review < 0, 0, fullTrain$first_review)
sum(is.na(fullTrain$first_review))

##guests_included
summary(fullTrain$guests_included)
table(fullTrain$guests_included)

##host_acceptance_rate
#most common class is na, second is 1
#I think we can move na's to 1's or .9999, can drop if desired
summary(fullTrain$host_acceptance_rate)
table(fullTrain$host_acceptance_rate)
fullTrain$host_acceptance_rate <- ifelse(is.na(fullTrain$host_acceptance_rate),'n', fullTrain$host_acceptance_rate)
fullTrain$host_acceptance_rate <- NULL

##host_has_profile_pic
#54 empty strings, will convert to n for no response 
#at some point if we do lasso or ridge will need to 
#make numeric
summary(fullTrain$host_has_profile_pic)
table(fullTrain$host_has_profile_pic)
fullTrain$host_has_profile_pic <- ifelse(fullTrain$host_has_profile_pic == ''
                                         ,'0'
                                         ,fullTrain$host_has_profile_pic)
fullTrain$host_has_profile_pic <- ifelse(fullTrain$host_has_profile_pic == "t", 1, 0)

##host_identity_verified
#54 empty strings again, will add to n for no response
summary(fullTrain$host_identity_verified)
table(fullTrain$host_identity_verified)
fullTrain$host_identity_verified <- ifelse(fullTrain$host_identity_verified == ''
                                           ,'n'
                                           ,fullTrain$host_identity_verified)
fullTrain$host_identity_verified <- ifelse(fullTrain$host_identity_verified == "t", 1, 0)

##host_is_superhost
#same 54 missing will change to n for no response
summary(fullTrain$host_is_superhost)
table(fullTrain$host_is_superhost)
fullTrain$host_is_superhost <- ifelse(fullTrain$host_is_superhost == ''
                                      ,'n'
                                      ,fullTrain$host_is_superhost)
fullTrain$host_is_superhost <- ifelse(fullTrain$host_is_superhost == "t", 1, 0)

##host_listings_count
#same 54 missing will convert to n, will have issues if converting to numeric
summary(fullTrain$host_listings_count)
table(fullTrain$host_listings_count)
#View(fullTrain[fullTrain$host_listings_count=='',])
fullTrain$host_listings_count <- ifelse(fullTrain$host_listings_count == ''
                                        ,'n'
                                        ,fullTrain$host_listings_count)
fullTrain$host_listings_count <- ifelse(fullTrain$host_listings_count == "t", 1, 0)

##host_response_rate
#4707 missing will convert to n, will have issues if converting to numeric I think a 0 
#would cause more issues so I will go with n
summary(fullTrain$host_response_rate)
table(fullTrain$host_response_rate)
fullTrain$host_response_rate <- ifelse(fullTrain$host_response_rate == ''
                                       ,0.956
                                       ,fullTrain$host_response_rate)
fullTrain$host_response_rate <- as.numeric(fullTrain$host_response_rate)

##host_response_time
#4707 empty string again
summary(fullTrain$host_response_time)
table(fullTrain$host_response_time)
fullTrain$host_response_time <- ifelse(fullTrain$host_response_time == ''
                                       ,'n'
                                       ,fullTrain$host_response_time)
##host_since
#date value, will convert need to replace 54 empty string values

summary(fullTrain$host_since)
table(fullTrain$host_since)
host_since_explore <- as.numeric(fullTrain$host_since)
host_since_explore <- host_since_explore[c(complete.cases(host_since_explore))]
explore_mean <- mean(host_since_explore)
max(host_since_explore)
min(host_since_explore)
explore_std <- var(host_since_explore)^.5
explore_b <- round(explore_mean - explore_std, 0)
explore_t <- round(explore_mean + explore_std,0)
#mean value is 41826 I think I will just randomize
#between 1 std 671 from the mean so I don't introduce mean bias
#some random number between 41209 and 42443

##save rows to check transformation
missing_rows <- fullTrain$host_since==''

fullTrain$host_since <- ifelse(missing_rows
                               ,sample(explore_b:explore_t,replace=TRUE)
                               ,fullTrain$host_since)
fullTrain$host_since <- as.numeric(fullTrain$host_since)
fullTrain$host_since <- fullTrain$host_since - 39510
fullTrain$host_since <- ifelse(fullTrain$host_since < 0, 0, fullTrain$host_since)
#review and clean up
#View(fullTrain[missing_rows,])
rm(host_since_explore)
rm(explore_b)
rm(explore_t)
rm(explore_mean)
rm(explore_std)
rm(missing_rows)

##host_total_listings_count
#same 54 missing, will replace with n
summary(fullTrain$host_total_listings_count)
table(fullTrain$host_total_listings_count)
fullTrain$host_total_listings_count <- ifelse(fullTrain$host_total_listings_count == ''
                                              ,1
                                              ,fullTrain$host_total_listings_count)
fullTrain$host_total_listings_count <- as.numeric(fullTrain$host_total_listings_count)
sum(is.na(fullTrain$host_total_listings_count))

##host_verifications
#list values, will skip, could have some string manipulations
summary(fullTrain$host_verifications)
listofverifications <- read.csv('Verifications.csv')
fullTrain$host_verifications <- gsub('"', "", fullTrain$host_verifications)
fullTrain$host_verifications <- gsub(" ", "", fullTrain$host_verifications)
fullTrain$host_verifications <- gsub("'", "", fullTrain$host_verifications)
fullTrain$host_verifications <- gsub("[{}]", "", fullTrain$host_verifications)
fullTrain$host_verifications <- gsub("\\[|\\]", "", fullTrain$host_verifications)
fullTrain$host_verifications <- paste(",", fullTrain$host_verifications, ",", sep="")

for (i in listofverifications$Verifications){
  j <- paste(",", i, ",", sep="")
  fullTrain[substr(str_replace_all(paste("ver", i, sep=""),"[^a-zA-Z_]",""),-10,10)] <- as.numeric(lengths(regmatches(fullTrain$host_verifications,gregexpr(j,fullTrain$host_verifications))))
}

sum(fullTrain$host_verifications=='{}')

#table(fullTrain$host_verifications)

##house_rules
#large free form text could check for keywords
#9127 are blank, could create a variable for no rules etc.
summary(fullTrain$house_rules)

sum(fullTrain$house_rules=='')
#table(fullTrain$house_rules)

##instant_bookable
#all f or t nothing to see here
fullTrain$instant_bookable <- ifelse(fullTrain$instant_bookable == ''
                                     ,0
                                     ,fullTrain$instant_bookable)
fullTrain$instant_bookable <- ifelse(fullTrain$instant_bookable == "t", 1, 0)

summary(fullTrain$instant_bookable)
table(fullTrain$instant_bookable)

##is_business_travel_ready
#a lot of empty strings, will convert to n's
summary(fullTrain$is_business_travel_ready)
table(fullTrain$is_business_travel_ready)
fullTrain$is_business_travel_ready <- ifelse(fullTrain$is_business_travel_ready == ''
                                             ,0
                                             ,fullTrain$is_business_travel_ready)
fullTrain$is_business_travel_ready <- ifelse(fullTrain$is_business_travel_ready == "t", 1, 0)

#sum(is.na(fullTrain$is_business_travel_ready))


##is_location_exact
#all f or t, nothing to do
fullTrain$is_location_exact <- ifelse(fullTrain$is_location_exact == ''
                                      ,0
                                      ,fullTrain$is_location_exact)
fullTrain$is_location_exact <- ifelse(fullTrain$is_location_exact == "t", 1, 0)
summary(fullTrain$is_location_exact)
table(fullTrain$is_location_exact)

##latitude
#convert to numeric
summary(fullTrain$latitude)
sum(is.na(fullTrain$latitude))
fullTrain$latitude <- as.numeric(fullTrain$latitude)
fullTrain$latitude <- ifelse(is.na(fullTrain$latitude),37.977,fullTrain$latitude)
##longitude
summary(fullTrain$longitude)
sum(is.na(fullTrain$longitude))
fullTrain$longitude <- as.numeric(fullTrain$longitude)
fullTrain$longitude <- ifelse(is.na(fullTrain$longitude),-94.9389,fullTrain$longitude)

##maximum_nights
#strange max with millions of nights? If there was something in this variable
#It might be lost in the abitrary number of nights for the max. I think I will 
#set a max of 2 years, then if it's something less than that it might be of interest
#I will leave alone for now, maybe people that put 1m days have certain traits I don't want to miss
summary(fullTrain$maximum_nights)
fullTrain$maximum_nights <- ifelse(fullTrain$maximum_nights > 1125,1125,fullTrain$maximum_nights)
fullTrain$maximum_nights <- ifelse(is.na(fullTrain$maximum_nights),670.5865,fullTrain$maximum_nights)
#View(fullTrain[fullTrain$maximum_nights > 1125,])
#sum(fullTrain$maximum_nights>365*2)

##minimum_nights
#leave alone for now
summary(fullTrain$minimum_nights)
fullTrain$minimum_nights <- ifelse(fullTrain$minimum_nights > 365,365,fullTrain$minimum_nights)

##monthly_price
#if no monthly price is available just use 30xprice
#turn all dollars into numeric
fullTrain$monthly_price <- as.numeric(fullTrain$monthly_price)
mean(fullTrain[complete.cases(fullTrain$monthly_price),"monthly_price"])
fullTrain$monthly_price <- ifelse(is.na(fullTrain$monthly_price),30*fullTrain$price,fullTrain$monthly_price)
mean(fullTrain[complete.cases(fullTrain$monthly_price),"monthly_price"])

##neighborhood_overview
#free text explore later

##price
#all dollars will be numeric
fullTrain$price <- as.numeric(fullTrain$price)
summary(fullTrain$price)

##property_type
#add to the other category for values under 30, because I don't want to lose boat!
summary(fullTrain$property_type)
table(fullTrain$property_type)
p_explore <- table(fullTrain$property_type)
p_explore_names <- names(p_explore[p_explore < 100])

#check to see if correct rows will be updated
#View(fullTrain[fullTrain$property_type %in% p_explore_names,])

fullTrain$property_type <- ifelse(fullTrain$property_type %in% p_explore_names
                                  ,'Other'
                                  ,fullTrain$property_type)
table(fullTrain$property_type)

rm(p_explore_names)
rm(p_explore)

##require_guest_phone_verification
#all t and f
summary(fullTrain$require_guest_phone_verification)
fullTrain$require_guest_phone_verification <- ifelse(fullTrain$require_guest_phone_verification == "t", 1, 0)
table(fullTrain$require_guest_phone_verification)

##require_guest_profile_picture
#all t and f
summary(fullTrain$require_guest_profile_picture)
fullTrain$require_guest_profile_picture <- ifelse(fullTrain$require_guest_profile_picture == "t", 1, 0)
table(fullTrain$require_guest_profile_picture)

##requires_license
#all t and f
fullTrain$requires_license <- ifelse(fullTrain$requires_license == "t", 1, 0)
table(fullTrain$requires_license)

##room_type
#3 types no empty strings
summary(fullTrain$room_type)
table(fullTrain$room_type)

##security_deposit
#make numeric will turn all strange entries into na's
#replace security_deposit na's with 0's
#View(fullTrain[is.na(fullTrain$security_deposit),])
fullTrain$security_deposit <- as.numeric(fullTrain$security_deposit)
fullTrain$security_deposit <- ifelse(is.na(fullTrain$security_deposit),0,fullTrain$security_deposit)
table(fullTrain$security_deposit)

##summary
#free text explore later

##transit
#free text explore later

##weekly_price
#make sure it's numeric, fill in NA's with price * 7
fullTrain$weekly_price <- as.numeric(fullTrain$weekly_price)
mean(fullTrain[complete.cases(fullTrain$weekly_price),"weekly_price"])
fullTrain$weekly_price <- ifelse(is.na(fullTrain$weekly_price),7*fullTrain$price,fullTrain$weekly_price)
mean(fullTrain[complete.cases(fullTrain$weekly_price),"weekly_price"])
fullTrain$weekly_price <- ifelse(is.na(fullTrain$weekly_price),1057,fullTrain$weekly_price)


#####################End per variable data cleanup#####################




###################data field creation###################

##house_rules
#1000 char max for house rules. I think a simple way to pull some info out
#is to find the number of characters or words, then I will do some keyword searches
#to find the word count, I'm going to use the number of spaces, for that to work I will replace
#multiple spaces with single spaces then do the count
max(nchar(fullTrain$house_rules))
exp_rules <- lengths(regmatches(fullTrain$house_rules,gregexpr("     ",fullTrain$house_rules))) > 0
#View(fullTrain[exp_rules,])

max(lengths(regmatches(fullTrain$house_rules,gregexpr("   ",fullTrain$house_rules))))

#fix strange spaces
fullTrain$house_rules <- gsub("           "," ",fullTrain$house_rules)
fullTrain$house_rules <- gsub("         "," ",fullTrain$house_rules)
fullTrain$house_rules <- gsub("       "," ",fullTrain$house_rules)
fullTrain$house_rules <- gsub("     "," ",fullTrain$house_rules)
fullTrain$house_rules <- gsub("   "," ",fullTrain$house_rules)
fullTrain$house_rules <- gsub("  "," ",fullTrain$house_rules)

#remove #NAME? errors
sum(fullTrain$house_rules == '#NAME?')
fullTrain$house_rules <- gsub("#NAME?","", fullTrain$house_rules)

#now create word count variable
lengths(regmatches(fullTrain$house_rules,gregexpr(" ",fullTrain$house_rules)))
fullTrain$house_rules_word_count <- lengths(regmatches(fullTrain$house_rules,gregexpr(" ",fullTrain$house_rules)))
#View(head(fullTrain))

rm(exp_rules)
#install.packages("stringr")
#library("stringr")
#?stringr


#possible keywords
#No Smoking, Smoking allowed, 420, marijuana, no pets, pets allowed, late fee
#cancellation fee, smoke free, smoke-free, drug free, drug-free

#Make count of ammeneties

head(fullTrain$amenities)
amen_count <- ifelse(nchar(fullTrain$amenities) == 2,0
                     , lengths(regmatches(fullTrain$amenities,gregexpr(",",fullTrain$amenities)))+1)
fullTrain$amen_count <- amen_count
rm(amen_count)
min(fullTrain$amen_count)
nchar(fullTrain$amenities)
table(fullTrain$amen_count)
#has_wifi or other ammenities

hasWifi <- lengths(regmatches(fullTrain$amenities,gregexpr("Wifi|Wireless Internet",fullTrain$amenities,ignore.case = TRUE)))
fullTrain$hasWifi <- ifelse(hasWifi > 0 ,1,0)
sum(fullTrain$hasWifi)
rm(hasWifi)

noSmoke <- lengths(regmatches(fullTrain$house_rules,gregexpr("nosmok|smokefree"
                                                             ,gsub(" ","",fullTrain$house_rules)
                                                             ,ignore.case = TRUE)))
fullTrain$noSmoke <- ifelse(noSmoke > 0 ,1,0)
sum(fullTrain$noSmoke)
#View(fullTrain[fullTrain$noSmoke==1,])
rm(noSmoke)


noPets <- lengths(regmatches(fullTrain$house_rules,gregexpr("nopet"
                                                            ,gsub(" ","",fullTrain$house_rules)
                                                            ,ignore.case = TRUE)))
fullTrain$noPets <- ifelse(noPets > 0 ,1,0)
sum(fullTrain$noPets)
#View(fullTrain[fullTrain$noPets==1,])
rm(noPets)

##walking distance in Summary

walkingDistance <- lengths(regmatches(fullTrain$summary,gregexpr("walkingdistance"
                                                                 ,gsub(" ","",fullTrain$summary)
                                                                 ,ignore.case = TRUE)))
fullTrain$walkingDistance <- ifelse(walkingDistance > 0 ,1,0)
sum(fullTrain$walkingDistance)
#View(fullTrain[fullTrain$walkingDistance==1,])
rm(walkingDistance)
####################end Variable additions#####################

fullTrain$bathrooms <- ifelse(is.na(fullTrain$bathrooms),0,fullTrain$bathrooms)
fullTrain$latitude <- ifelse(is.na(fullTrain$latitude),37.977,fullTrain$latitude)
fullTrain$longitude <- ifelse(is.na(fullTrain$longitude),-94.9389,fullTrain$longitude)
fullTrain$weekly_price <- ifelse(is.na(fullTrain$weekly_price),1057,fullTrain$weekly_price)
fullTrain$maximum_nights <- ifelse(is.na(fullTrain$maximum_nights),670.5865,fullTrain$maximum_nights)
fullTrain$minimum_nights <- ifelse(is.na(fullTrain$minimum_nights),3.34,fullTrain$minimum_nights)
fullTrain$minimum_nights <- ifelse(is.na(fullTrain$minimum_nights),3.34,fullTrain$minimum_nights)
fullTrain$price <- ifelse(is.na(fullTrain$price),154.58,fullTrain$price)
fullTrain$property_type <- ifelse(is.na(fullTrain$property_type),"Apartment",fullTrain$property_type)
fullTrain$host_identity_verified <- ifelse(is.na(fullTrain$host_identity_verified),1,fullTrain$host_identity_verified)
fullTrain$monthly_price <- ifelse(is.na(fullTrain$monthly_price),1057,fullTrain$monthly_price)


##Analyzing string variables


#fullTrain <- read.csv("C:/Users/mitch/Downloads/fullTrain_Training.csv")


##################FIRST: A COUPLE POTENTIALLY USEFUL VARIABLES###############

#Create 2 vars: popular words, and uncommon words

#First: pop_word_count, which is the count of "popular" words used in the summary
rowCount <- nrow(fullTrain)

tidy_fullTrain <- fullTrain %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words)

pop_words <- tidy_fullTrain %>%
  count(word) %>%
  arrange(desc(n))

pop_words <- head(pop_words,50)
#View(pop_words)
pop_word_count <- rep(0,rowCount)

for (i in 1:50) {
  pop_vec_i <- grepl(pop_words$word[1],fullTrain$summary)
  pop_vec_i <- ifelse(pop_vec_i==FALSE,0,1)
  pop_df <- data.frame(pop_word_count,pop_vec_i)
  pop_df$pop_word_count <- pop_df$pop_word_count + pop_df$pop_vec_i
  pop_df$pop_vec_i <- NULL
  pop_word_count <- pop_df$pop_word_count
}


fullTrain <- cbind(fullTrain,pop_word_count)

#Now uncommon_words, or if the listing doesn't include any of the most popular words in its summary
#fullTrain$uncommon_words <- ifelse((fullTrain$pop_word_count==0 & 
#                                   nchar(fullTrain$summary > 2)), 1, 0)


rm(pop_words)
rm(tidy_fullTest)
rm(i)
rm(pop_vec_i)
rm(pop_word_count)
##################SENTIMENT ANALYSIS AND OTHER STATISTICS###############

#####House Rules#####

###Cleaning
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(fullTrain$house_rules))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

###TDM
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 10 most frequent words
#View(head(dtm_d, 20))
# Plot the most frequent words
#barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
#        col ="lightgreen", main ="Top 5 most frequent words",
#        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
#wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
#          max.words=100, random.order=FALSE, rot.per=0.40, 
#          colors=brewer.pal(8, "Dark2"))


###Sentiment Analysis

##I use 3 different methods, then combine them.
syuzhet_vector <- vector()
bing_vector <- vector()
afinn_vector <- vector()

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="syuzhet")
  syuzhet_vector <- append(syuzhet_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="bing")
  bing_vector <- append(bing_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="afinn")
  afinn_vector <- append(afinn_vector,new_vec)
}

##Combine methods:
#Convert each sentimentality metric to a common measure:
#whether it is postive or negative
vec_comb <- cbind(
  sign(syuzhet_vector),
  sign(bing_vector),
  sign(afinn_vector)
)
vec_comb <- as.data.frame(vec_comb)

vec_comb$sentiment <- (vec_comb$V1 + vec_comb$V2 + vec_comb$V3)/3

#add back into original doc
fullTrain$rules_sentiment <- vec_comb$sentiment
fullTrain$rules_sentiment <- ifelse(fullTrain$rules_sentiment==0,0,
                                    (ifelse(fullTrain$rules_sentiment>0,1,-1)))
rm(vec_comb)
rm(dtm_m)
rm(dtm_d)
rm(dtm_v)

#####"Summary"#####

###Cleaning
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(fullTrain$summary))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

###TDM
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 10 most frequent words
#head(dtm_d, 10)

# Plot the most frequent words
#barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
#        col ="lightgreen", main ="Top 5 most frequent words",
#        ylab = "Word frequencies")


###Sentiment Analysis

##I use 3 different methods, then combine them.
syuzhet_vector <- vector()
bing_vector <- vector()
afinn_vector <- vector()

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="syuzhet")
  syuzhet_vector <- append(syuzhet_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="bing")
  bing_vector <- append(bing_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="afinn")
  afinn_vector <- append(afinn_vector,new_vec)
}

##Combine methods:
#Convert each sentimentality metric to a common measure:
#whether it is postive or negative
vec_comb <- cbind(
  sign(syuzhet_vector),
  sign(bing_vector),
  sign(afinn_vector)
)
vec_comb <- as.data.frame(vec_comb)

vec_comb$sentiment <- (vec_comb$V1 + vec_comb$V2 + vec_comb$V3)/3

#add back into original doc
fullTrain$summary_sentiment <- vec_comb$sentiment
fullTrain$summary_sentiment <- ifelse(fullTrain$summary_sentiment==0,0,
                                      (ifelse(fullTrain$summary_sentiment>0,1,-1)))
rm(vec_comb)
rm(dtm_m)
rm(dtm_d)
rm(dtm_v)

#####Transit#####
###Cleaning
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(fullTrain$transit))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

###TDM
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 10 most frequent words
#head(dtm_d, 10)

# Plot the most frequent words
#barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
#        col ="lightgreen", main ="Top 5 most frequent words",
#        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


###Sentiment Analysis

##I use 3 different methods, then combine them.
syuzhet_vector <- vector()
bing_vector <- vector()
afinn_vector <- vector()

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="syuzhet")
  syuzhet_vector <- append(syuzhet_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="bing")
  bing_vector <- append(bing_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="afinn")
  afinn_vector <- append(afinn_vector,new_vec)
}

##Combine methods:
#Convert each sentimentality metric to a common measure:
#whether it is postive or negative
vec_comb <- cbind(
  sign(syuzhet_vector),
  sign(bing_vector),
  sign(afinn_vector)
)
vec_comb <- as.data.frame(vec_comb)

vec_comb$sentiment <- (vec_comb$V1 + vec_comb$V2 + vec_comb$V3)/3

#add back into original doc
fullTrain$transit_sentiment <- vec_comb$sentiment
fullTrain$transit_sentiment <- ifelse(fullTrain$transit_sentiment==0,0,
                                      (ifelse(fullTrain$transit_sentiment>0,1,-1)))
rm(vec_comb)
rm(dtm_m)
rm(dtm_d)
rm(dtm_v)

#####Neighborhood Overview#####

###Cleaning
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(fullTrain$neighborhood_overview))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

###TDM
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 10 most frequent words
#head(dtm_d, 10)

# Plot the most frequent words
#barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
#        col ="lightgreen", main ="Top 5 most frequent words",
#        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


###Sentiment Analysis

##I use 3 different methods, then combine them.
syuzhet_vector <- vector()
bing_vector <- vector()
afinn_vector <- vector()

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="syuzhet")
  syuzhet_vector <- append(syuzhet_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="bing")
  bing_vector <- append(bing_vector,new_vec)
}

for (i in 1:rowCount) {
  new_vec <- get_sentiment(TextDoc[[i]][["content"]], method="afinn")
  afinn_vector <- append(afinn_vector,new_vec)
}

##Combine methods:
#Convert each sentimentality metric to a common measure:
#whether it is postive or negative
vec_comb <- cbind(
  sign(syuzhet_vector),
  sign(bing_vector),
  sign(afinn_vector)
)
vec_comb <- as.data.frame(vec_comb)

vec_comb$sentiment <- (vec_comb$V1 + vec_comb$V2 + vec_comb$V3)/3

#add back into original doc
fullTrain$neighborhood_sentiment <- vec_comb$sentiment
fullTrain$neighborhood_sentiment <- ifelse(fullTrain$neighborhood_sentiment==0,0,
                                           (ifelse(fullTrain$neighborhood_sentiment>0,1,-1)))
rm(vec_comb)
rm(dtm_m)
rm(dtm_d)
rm(dtm_v)
rm(i)
rm(syuzhet_vector)
rm(toSpace)
rm(bing_vector)
rm(afinn_vector)
rm(tidy_fullTrain)
rm(TextDoc)
rm(TextDoc_dtm)
rm(pop_df)
rm(new_vec)
rm(rowCount)

fullTrainText <- fullTrain





###########Modeling#############



set.seed(90210)

airBNB_regress <- fullTrainText[,c(2,3,5:26,29:36,38:44,47:239)]

#These should be fixed in the 01 data prep, but just in case they're "fixed" again here
airBNB_regress$bathrooms <- ifelse(is.na(airBNB_regress$bathrooms),0,airBNB_regress$bathrooms)
airBNB_regress$latitude <- ifelse(is.na(airBNB_regress$latitude),37.977,airBNB_regress$latitude)
airBNB_regress$longitude <- ifelse(is.na(airBNB_regress$longitude),-94.9389,airBNB_regress$longitude)
airBNB_regress$weekly_price <- ifelse(is.na(airBNB_regress$weekly_price),1057,airBNB_regress$weekly_price)
airBNB_regress$maximum_nights <- ifelse(is.na(airBNB_regress$maximum_nights),670.5865,airBNB_regress$maximum_nights)
airBNB_regress$minimum_nights <- ifelse(is.na(airBNB_regress$minimum_nights),3.34,airBNB_regress$minimum_nights)
airBNB_regress$minimum_nights <- ifelse(is.na(airBNB_regress$minimum_nights),3.34,airBNB_regress$minimum_nights)
airBNB_regress$price <- ifelse(is.na(airBNB_regress$price),154.58,airBNB_regress$price)
airBNB_regress$property_type <- ifelse(is.na(airBNB_regress$property_type),"Apartment",airBNB_regress$property_type)
airBNB_regress$host_identity_verified <- ifelse(is.na(airBNB_regress$host_identity_verified),1,airBNB_regress$host_identity_verified)
airBNB_regress$monthly_price <- ifelse(is.na(airBNB_regress$monthly_price),1057,airBNB_regress$monthly_price)

airBNB_regress <- model.matrix(~.-1,airBNB_regress)
airBNB_regress <- as.data.frame(airBNB_regress)
colnames(airBNB_regress) <- gsub(" ", "_", colnames(airBNB_regress))
colnames(airBNB_regress) <- gsub("-", "_", colnames(airBNB_regress))
colnames(airBNB_regress) <- gsub("&", "_", colnames(airBNB_regress))
colnames(airBNB_regress) <- gsub("/", "_", colnames(airBNB_regress))
testtest <- airBNB_regress[airBNB_regress$testing == 1,]
airBNB_regress <- airBNB_regress[colSums(airBNB_regress) >= 20]

airBNB_testing <- airBNB_regress[airBNB_regress$testing == 1,]
airBNB_training <- airBNB_regress[airBNB_regress$testing == 0,]
airBNB_testing$testing <- NULL
airBNB_training$testing <- NULL

num_obs <- nrow(airBNB_training)
test_obs <- sample(num_obs, 0.25*num_obs)
airBNB_rest <- airBNB_training[-test_obs,]
airBNB_test <- airBNB_training[test_obs,]

#airBNB_rest$avg_rating <- ifelse(airBNB_rest$avg_rating < 40, 40, airBNB_rest$avg_rating)

#LASSO & Ridge

glmnet_lasso.cv <- cv.glmnet(as.matrix(airBNB_rest[,c(2:204)]),airBNB_rest$avg_rating, family = "gaussian", nfolds = 100, alpha=1)
best.lambda <- glmnet_lasso.cv$lambda.min
best.lambda
glmnet_lasso <- glmnet(as.matrix(airBNB_rest[,c(2:204)]),airBNB_rest$avg_rating, family = "gaussian", lambda = best.lambda, alpha=1)

lasso.probs <- predict(glmnet_lasso,s=best.lambda,newx=as.matrix(airBNB_test[,c(2:204)]))
RMSE_lasso <- sqrt(mean((lasso.probs-airBNB_test$avg_rating)^2))
RMSE_lasso

glmnet_ridge.cv <- cv.glmnet(as.matrix(airBNB_rest[,c(2:204)]),airBNB_rest$avg_rating, family = "gaussian", nfolds = 100, alpha=0)
best.lambda2 <- glmnet_ridge.cv$lambda.min
best.lambda2
glmnet_ridge <- glmnet(as.matrix(airBNB_rest[,c(2:204)]),airBNB_rest$avg_rating, family = "gaussian", lambda = best.lambda2, alpha=0)

ridge.probs <- predict(glmnet_ridge,s=best.lambda2,newx=as.matrix(airBNB_test[,c(2:204)]))
RMSE_ridge <- sqrt(mean((ridge.probs-airBNB_test$avg_rating)^2))

RMSE_lasso
RMSE_ridge

#Boosting
bnbBoost1 <- gbm(airBNB_rest$avg_rating~.,data=airBNB_rest[,-1],distribution="gaussian",n.trees=2200,n.cores = 4)
summary(bnbBoost1)

bnbBoostPreds <- predict(bnbBoost1,newdata=airBNB_test)
mean(bnbBoostPreds)
mean(airBNB_test$avg_rating)

RMSE_boost <- sqrt(mean((bnbBoostPreds-airBNB_test$avg_rating)^2))

RMSE_boost


# Random Forest
forest.trees <- randomForest(avg_rating~.,data=airBNB_rest[,],ntree=200,mtry=20,importance=TRUE)
summary(forest.trees)

forest_preds <- predict(forest.trees,newdata=airBNB_test)
forest_preds
RMSE_forest <- sqrt(mean((forest_preds-airBNB_test$avg_rating)^2))
RMSE_forest

importance(forest.trees)
varImpPlot(forest.trees)

RMSE_boost
RMSE_forest
hist(bnbBoostPreds)

#kNN

bnbRest <- data.frame(scale(airBNB_rest[,-1]))
bnbTest <- data.frame(scale(airBNB_test[,-1]))

names(bnbRest)[!(names(bnbRest) %in% names(bnbTest))]

kFullPreds <- knn(bnbRest[,-1], bnbTest[,-1], airBNB_rest$avg_rating, k=17)

table(kFullPreds)

#Percent 100's in full train
sum(fullTrain$avg_rating == 100) / nrow(fullTrain)

kPreds <- data.frame(as.numeric(as.matrix(kFullPreds)))
table(kPreds)

#percent 100's in kpreds
sum(kPreds==100)/nrow(bnbTest)
#percent 100's in test set
sum(bnbTest$avg_rating==100)/nrow(bnbTest)

#remove standardization for RMSE calculation
bnbRest <- airBNB_rest
bnbTest <- airBNB_test

knn_rmse <- (sum ((kPreds - bnbTest$avg_rating) ^2)/nrow(bnbTest))^.5
knn_rmse


#--------------------Ensembling-----------------

airBNB_testing$avg_rating <- NULL

bnbEndPredsBoost <- predict(bnbBoost1,newdata=airBNB_testing)
bnbEndPredsForest <- predict(forest.trees,newdata=airBNB_testing)
bnbEndPredsLasso <- predict(glmnet_lasso,s=best.lambda,newx=as.matrix(airBNB_testing[,c(1:203)]))
bnbEndPredsRidge <- predict(glmnet_ridge,s=best.lambda2,newx=as.matrix(airBNB_testing[,c(1:203)]))
bnbEndPredsY <- c(1:12208)

ridge.probs <- c(ridge.probs)
lasso.probs <- c(lasso.probs)

ensembleDF <- data.frame(avg_rating = airBNB_test$avg_rating, lasso = lasso.probs, ridge = ridge.probs, boost = bnbBoostPreds, forest = forest_preds)

# lm.fit <- lm(avg_rating~., data=ensembleDF)
# summary(lm.fit)


glmnet_lasso.cv2 <- cv.glmnet(as.matrix(ensembleDF[,c(2:5)]),ensembleDF$avg_rating, family = "gaussian", nfolds = 100, alpha=1)
best.lambda2 <- glmnet_lasso.cv2$lambda.min
best.lambda2
glmnet_lasso2 <- glmnet(as.matrix(ensembleDF[,c(2:5)]),ensembleDF$avg_rating, family = "gaussian", lambda = best.lambda2, alpha=1)


bnbEndPredsRidge <- c(bnbEndPredsRidge)
bnbEndPredsLasso <- c(bnbEndPredsLasso)

# ensembleEndDF <- data.frame(lasso = bnbEndPredsLasso, ridge = bnbEndPredsRidge, boost = bnbEndPredsBoost, forest = bnbEndPredsForest)
# ensemblepredict <- predict(lm.fit, newdata = ensembleEndDF)




ensembleEndDF <- data.frame(lasso = bnbEndPredsLasso, ridge = bnbEndPredsRidge, boost = bnbEndPredsBoost, forest = bnbEndPredsForest)
ensemblepredict <- predict(glmnet_lasso2,s=best.lambda2,newx=as.matrix(ensembleEndDF[,c(1:4)]))
#RMSE_lasso2 <- sqrt(mean((ensemblepredict-ensembleDF$avg_rating)^2))

ensemblepredict <- c(ensemblepredict)



mean(ensemblepredict)


newpredict <- ifelse(ensemblepredict > 100, 100, ensemblepredict)

#newpredict <- ifelse(bnbEndPredsForest > 100, 100, bnbEndPredsForest)

newpredict[775] <- 94.57556452
newpredict[10274] <- 94.57556452

mean(newpredict)

#####################convert testing data#####################

submissionFile<-data.frame()
submissionFile <- data.frame(seq(1,length(newpredict)))


submissionFile$Prediction <- data.frame(Prediction=newpredict)
submissionFile$Prediction <- newpredict

write.csv(submissionFile, "airbnbSubmission20a.csv", row.names=F)