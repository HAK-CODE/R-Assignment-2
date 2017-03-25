library(dplyr)
library(tidyr)
library(lubridate)

#loading csv from directory
bufferedDataFrame <- read.csv("hospitaldata.csv", strip.white = T, na.strings = c("-",""," ","\t","\n",NA), stringsAsFactors = F)

#CLEANING DATA
#converting to tbl format 
df <- tbl_df(bufferedDataFrame)

#1. Removing dots(.) from column names
names(df) <- gsub("\\.", "", names(df))

#Removing character from Age Like M
df$Age <- as.numeric(gsub("[^0-9]",'',df$Age))

#Cleaning Date Column
#Converting date factor format to Date class
df$Date <- as.Date(strptime(df$Date, "%a, %B %d, %Y"))

#Cleaning Time Column with formatting
df$Time <- format(strptime(df$Time, format='%I:%M %p'), '%I:%M %p')

#Changing case of Sex to upper case
df$Sex <- toupper(df$Sex)

#Remove Cancelled to NA as no payment is not given
df$TotalCharges <- as.numeric(gsub("cancelled", NA, ignore.case = T,df$TotalCharges))

#Remove Cancelled to NA as no consultation is not given
df$Procedure <- (gsub("cancelled", NA, ignore.case = T,df$Procedure))

#Coverting AmountBalance to numeric
df$AmountBalance <- as.numeric(gsub(",",'',df$AmountBalance))
class(df$AmountBalance)

#Exporting Clean CSV
write.csv(df, "CleanHospitalData.csv")

#View clean data
glimpse(df)
View(df)

#QUESTIONS
#2. Which day of the week is expected to have most visits? 
print(paste("Average age of a person is",weekdays(df$Date[which(table(df$Date) == max(table(df$Date)))])))

#3. What is the average age of patients? 
print(paste("Average age of a person is ",mean(df$Age, na.rm = T)))

#4. How many children were entertained? (Make a Bracket of Age from 1-12) 
total_entertained <- df %>%
                     select(Age) %>%
                     filter(Age >= 1 & Age <= 12, !is.na(.)) %>%
                     count()
paste("Children that were entertained are", total_entertained) 

#5. Which gender type had what kind of procedure in abundance? i.e. Female visit mostly because of Gynae Problem
df %>%
  group_by(Sex, Procedure) %>%
  tally(sort = T) %>%
  filter(!is.na(Sex)) %>%
  View()

#6. Which Doctor is earning highest? 
highest_earning_doctor <-  df %>%
                    group_by(ConsultingDoctor) %>%
                    summarise_each(funs(sum(TotalCharges, na.rm = T))) %>%
                    select(ConsultingDoctor, TotalCharges) %>%
                    arrange(desc(TotalCharges))

paste("Highest paid doctor is",(highest_earning_doctor$ConsultingDoctor[which(highest_earning_doctor$TotalCharges == max(highest_earning_doctor$TotalCharges))]), "with pay of",max(highest_earning_doctor$TotalCharges))

#7. Which procedure type earns more money? 
highest_earning_procedure <-  df %>%
                    group_by(Procedure) %>%
                    summarise_each(funs(sum(TotalCharges, na.rm = T))) %>%
                    select(Procedure, TotalCharges) %>%
                    filter(!is.na(Procedure)) %>%
                    arrange(desc(TotalCharges))
View(highest_earning_procedure)
paste("Highest earning procedure is",(highest_earning_procedure$Procedure[which(highest_earning_procedure$TotalCharges == max(highest_earning_procedure$TotalCharges))]), "worth of",max(highest_earning_procedure$TotalCharges))

#8. Which time of the day has highest frequency of visits by hour? 
high_frequency_time <-  df %>%
                        filter(!is.na(Date), !is.na(Time)) %>%
                        group_by(Date,Time) %>%
                        tally() %>%
                        arrange(desc(n))
View(high_frequency_time)
indexOfMax <- which(high_frequency_time$n == max(high_frequency_time$n))
paste("Highest number of visits were on",weekdays(high_frequency_time$Date[indexOfMax]),"at",format(high_frequency_time$Time[indexOfMax]))

#10. How many patients are repeated visitors?  
visitor <-  df %>%
            group_by(id) %>%
            tally() %>%
            filter(n > 1) %>%
            arrange(desc(n))
View(visitor)
paste("Number of repeated visitors are", count(visitor))

#11. Give us the id of repeated visitors.
#Data is take from above 
View(visitor)
print(visitor$id)

#12. Which patients visited again for the same problem? 
patients_repeated <-  df %>%
                      group_by(id, Procedure) %>%
                      tally() %>%
                      filter(!is.na(Procedure), n > 1) %>%
                      arrange(desc(n))
View(patients_repeated)
#This data show ignoring NA for procedure variable
paste("Patient repeated most for is",(patients_repeated$Procedure[which(patients_repeated$n == max(patients_repeated$n))]))

#13. What is the median age for Females and Males? 
df %>% 
  group_by(Sex) %>%
  summarise(median(Age, na.rm = T))

#14. What is the total amount in balance? 
paste("Total amount in balance is",sum(df$AmountBalance, na.rm = T))

#15. How much money was made by Procedure Type "Consultation"? 
df %>%
  group_by(Procedure) %>%
  summarise(Total_Amount = sum(TotalCharges, na.rm = T)) %>%
  filter(Procedure %in% c("Consultation"))

#16.Is there a relation between Age and Total Charges paid
data <- df %>%
        filter(!is.na(Age), !is.na(TotalCharges))
paste(cor(data$Age, data$TotalCharges),"shows positive linearly directly propotional.")

#17. Which Age group had highest number of visits?
# HIGHEST NUMBER OF VISIT WAS BY AGE UNASIGNED 'NA' THAT IS 28 BUT IT WAS IGNORED
highest_age_visit <- df %>%
                     group_by(Age) %>%
                     tally() %>%
                     filter(!is.na(Age)) %>%
                     arrange(desc(n))
View(highest_age_visit)
paste("Highest number of visit was by age group of",(highest_age_visit$Age[which(highest_age_visit$n == max(highest_age_visit$n))]), "with number of visits of",max(highest_age_visit$n))

#18. What is the total cost earned by Procedure Type X Ray and Scalling together? 
TotalEarning <- df %>%
                filter(Procedure %in% c("X Ray", "Scalling")) %>%
                summarise(Total = sum(TotalCharges, na.rm = T)) 
View(TotalEarning)
paste("Total earning by both X Ray and Scalling is worth",TotalEarning)  
