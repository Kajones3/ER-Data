#Read in the data
R_Data <- read.csv(file.choose(),header = TRUE)

#I attached the data to the frame
attach(R_Data)

#I wanted to combine the columns here
cols <- c("Referred","Owner","AD","Campus","Status")

#Let's change the columns to factors
R_Data[cols] <- lapply(cols, factor)


#Now I want to check my work
sapply(R_Data, class)

#That didn't work. Let's try again.
#Let's change this character column to a factor
R_Data$Referred <- as.factor(R_Data$Referred)

#Let's test and see
str(R_Data)

#Let's change this character column to a factor
R_Data$Referred <- as.factor(R_Data$Referred)

#Let's test and see
str(R_Data)

#Let's change everything
R_Data$Owner <- as.factor(R_Data$Owner)
R_Data$AD <- as.factor(R_Data$AD)
R_Data$Campus <- as.factor(R_Data$Campus)
R_Data$Admin_Status <- as.factor(R_Data$Admin_Status)

#Let's check our work now.
sapply(R_Data, class)
#THAT WORKED!!

#Now I want to see how some of the data performs
y = xtabs(~ Referred + Showed + Enroll, R_Data)
y

#That's not really giving me what I want, so let's try a Table command
table(R_Data$Referred, R_Data$Contact + R_Data$Showed + R_Data$Enroll)


#Let's make some functions that count and calculate percentages.
count <- function(x, n){ length((which(x == n))) }
perc <- function(x, n){ 100*length((which(x == n))) / length(x) }

count(R_Data$Referred,"ER")

#Let's do a frequency distribution
with(R_Data, table(Referred,Showed))

#How Far can we take this?
with(R_Data, 100*(table(Referred, Scheduled)/sum(table(Referred, Scheduled))))


#------------------------------------------------------------

er_data <- read.csv("ER_24_17.csv", header = T, stringsAsFactors = T)
str(er_data)

er_data$Created.Date <- as.Date(er_data$Created.Date, "%m/%d/%Y")
er_data$Contacted.Date <- as.Date(er_data$Contacted.Date, "%m/%d/%Y")
er_data$Scheduled.Date <- as.Date(er_data$Scheduled.Date, "%m/%d/%Y")
er_data$Showed.Date <- as.Date(er_data$Showed.Date, "%m/%d/%Y")
er_data$Enrollment.Date <- as.Date(er_data$Enrollment.Date, "%m/%d/%Y")

str(er_data)

library(tidyverse)

#let's create separate variables and other data frames
er_data$referral[er_data$Referred.By...Name == "ER"] <- "ER"
er_data$referral[er_data$Referred.By...Name != "ER"] <- "Else"
er_data$status[er_data$Created.Date > 0] <- "(0) Created"
er_data$status[er_data$Contacted.Date > 0] <- "(1) Contacted"
er_data$status[er_data$Scheduled.Date > 0] <- "(2) Scheduled"
er_data$status[er_data$Showed.Date > 0] <- "(3) Showed"
er_data$status[er_data$Admissions.Status == "Closed - Enrolled"] <- "(4) Enrolled"


er <- filter(er_data, Referred.By...Name == "ER")
other <- filter(er_data, Referred.By...Name != "ER")


er$status[er$Created.Date > 0] <- "(0) Created"
er$status[er$Contacted.Date > 0] <- "(1) Contacted"
er$status[er$Scheduled.Date > 0] <- "(2) Scheduled"
er$status[er$Showed.Date > 0] <- "(3) Showed"
er$status[er$Admissions.Status == "Closed - Enrolled"] <- "(4) Enrolled"


other$status[other$Created.Date > 0] <- "(0) Created"
other$status[other$Contacted.Date > 0] <- "(1) Contacted"
other$status[other$Scheduled.Date > 0] <- "(2) Scheduled"
other$status[other$Showed.Date > 0] <- "(3) Showed"
other$status[other$Admissions.Status == "Closed - Enrolled"] <- "(4) Enrolled"

other$contact[other$Contacted.Date > 0] <- 1
other$contact[is.na(other$Contacted.Date)] <-0

er$contact[er$Contacted.Date > 0] <- 1
er$contact[is.na(er$Contacted.Date)] <-0

er_data$contact[er_data$Contacted.Date > 0] <- 1
er_data$contact[is.na(er_data$Contacted.Date)] <-0


#Let's look at ER, Other, and both combined

table(er_data$referral,er_data$contact)

eggplot(er_data, aes(status, fill = referral ))+
  geom_bar(aes(y = 100*(..count..)/sum(..count..)))
           
ggplot(other, aes(status))+
  geom_bar(aes(y = 100*(..count..)/sum(..count..)))

ggplot(er, aes(status))+
  geom_bar(aes(y = 100*(..count..)/sum(..count..)))

sum(er$Created.Date < "2017-10-01")
sum(other$Created.Date > "2017-10-01" & other$status == "(4) Enrolled", na.rm = T) / sum(other$Created.Date > "2017-10-01" & other$status != "", na.rm = T)



