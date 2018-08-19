##################EDA Case Study###########################

#Loading the required packages
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

#################Data Cleaning loan data###################

##Reading loan data in R & changing blank values to NA
loan <- read.csv("loan.csv", header=T, na.strings=c("","NA"))

##Structure of loan dataframe to understand the datatypes
summary(loan)
str(loan)

##Check for duplicates for id and member id fields
sum(duplicated(loan$id))
sum(duplicated(loan$member_id))

##To check for NAs
sum(is.na(loan))

##Filtering out the columns which contains only NA's
loan_test<-loan
for(i in 1:length(loan_test)){
  if(sum(is.na(loan_test[,i]))==nrow(loan_test)){
    m<-colnames(loan_test[i])
    loan[m]<-NULL
  }else{
    next
  }
}
remove(loan_test)

##Remove columns with 1 factor level
loan$collections_12_mths_ex_med <- as.factor(loan$collections_12_mths_ex_med)
loan$policy_code <- as.factor(loan$policy_code)
loan$acc_now_delinq <- as.factor(loan$acc_now_delinq)
loan$chargeoff_within_12_mths <- as.factor(loan$chargeoff_within_12_mths)
loan$tax_liens <- as.factor(loan$tax_liens)
loan$delinq_amnt <- as.factor(loan$delinq_amnt) 

loan <- loan[ , -which(names(loan) %in% names(loan[ ,sapply(loan,nlevels) == 1]))]

##Removing columns which are not required for Data Analysis
# emp_title
# url
# desc
# title
# zip_code
loan <- loan[, -which(names(loan) %in% c("emp_title", "url", "desc", "title", "zip_code"))]

##Convert interest rate to number
loan$int_rate <- as.numeric(gsub("%", "", loan$int_rate))

##Rounding off column by 2 decimals for funding_amnt_inv, annual_inc columns
loan$funded_amnt_inv          <- round(loan$funded_amnt_inv, 2)
loan$total_pymnt              <- round(loan$total_pymnt, 2)
loan$total_rec_late_fee       <- round(loan$total_rec_late_fee, 2)
loan$recoveries               <- round(loan$recoveries, 2)
loan$collection_recovery_fee  <- round(loan$collection_recovery_fee, 2)

##Removing "months" from term
loan$term <- as.factor(gsub(" months", "", loan$term))

##Removing "years" and "year" from emp_length
loan$emp_length <- as.factor(gsub(" years", "", loan$emp_length))

##Removing "n/a" string and replacing with NA values for emp_length
loan$emp_length<-gsub("n/a",NA, loan$emp_length)


########################Data Analysis#######################


## Univeriate Analysis of loans based on Loan Status
### About 82.96% of loans are fully paid, 2.87% current and 14.17% Charged Off in the dataset given
ggplot(loan, aes(x="", y=..count../sum(..count..)*100, fill=loan_status)) +  geom_bar() +
geom_text(stat = 'count', aes(label=round(..count../sum(..count..)*100, 2)), position = position_stack(vjust = 0.5), color="white", fontface="bold") + 
coord_polar("y", start=0) +  labs(x="",y="", fill="Loan Status")


##Data distribution for the loan amount
##The loan amount range is 5500(1st Quartile) to 15000(3rd Quartile)
ggplot(loan,aes(x="",y=loan_amnt))+geom_boxplot(col="deepskyblue1")+labs(x="",y="Loan Amount")
summary(loan$loan_amnt)


##Analysis of Defaulters based on credit lines
##Credit lines are the avaiable limit for a customer to borrow money
###So less credit lines indicate customer have less limit to borrow the money
###The plot shows that people who are having less credit lines i.e. ranging from 0-25 are likely to default when compared to higher credit lines.
ggplot(filter(loan,loan_status=="Charged Off"),aes(x=total_acc,fill=loan_status))+
geom_histogram(binwidth = 5)+
geom_freqpoly(binwidth = 5)+theme_bw()


## Analysis of Defaulters based on States
### California has the highest defaulter rate of all states and Florida and New york are marginally higher than other states
ggplot(filter(loan,loan_status=="Charged Off"), aes(x=addr_state, fill=loan_status)) +
geom_bar(position = "dodge") +
labs(x="State",y="Frequency", fill="Loan Status")

##Analysis of Defaulters based on Interest Rates
###The rate of defauters is higher for the loan interest rate if it is more then 10% and less than 15%
###int_rate_seg is derived metric which tells gives us the scale of the interest rates.
loan$int_rate_seg<-case_when( loan$int_rate<="10" ~ "<10%",
                              loan$int_rate>"10" & loan$int_rate<="15" ~ "10%-15%",
                              loan$int_rate>"15" & loan$int_rate<="20" ~ "15%-20%",
                              loan$int_rate>"20" ~ ">20%")
ggplot(filter(loan,loan_status=="Charged Off"),aes(x=int_rate_seg,fill=loan_status))+geom_bar(position = "dodge")+labs(x="Interest Rates", fill="Loan Status")


## Analysis of Defaulters based on Home Ownership
### The rate of defaulters are higher for Rent followed by Mortgage
ggplot(filter(loan,loan_status=="Charged Off"), aes(x=home_ownership, fill=loan_status)) +
geom_bar(position="dodge") +
labs(x="Home Ownership", y="Frequency", fill="Loan Status")

## Analysis of Defaulters based on Years of employment
### People with 10+ years of experience are highly defaulting than other range of experiences
filter(loan,loan_status=="Charged Off")%>%drop_na(emp_length) %>%
ggplot(aes(x=emp_length,fill=loan_status)) + geom_bar() +
labs(x="Employment in Years", y="Frequency", fill="Loan Status")


##Analysis of purpose vs Status of loan
####For the loan purpose of Debt Consolidation there are maximum number of defaulters
ggplot(loan, aes(x=purpose, fill=loan_status)) + geom_bar(position="dodge") +
labs(x="Purpose",y="Frequency", fill="Loan Status")


## Analysis of Grade to find the rate of defaulters##

##################################
### A and B Lower Risk         ###
### C and D Moderate Risk      ###
### E, F and G for Higher Risk ###
##################################

###Grades with comparitively lower and moderate risks are tending to default more than higher and lowest risks.
ggplot(filter(loan,loan_status=="Charged Off"), aes(x=grade, fill=loan_status)) +
  geom_bar(position="dodge") + 
  labs(x="Grades", y="Frequency", fill="Loan Status")

###The sub grades evidently say that the default rates are higher at lower and moderate risky grades.
ggplot(filter(loan,loan_status=="Charged Off"), aes(x=sub_grade, fill=loan_status)) +
  geom_bar(position="dodge") + 
  labs(x="Sub Grades", y="Frequency", fill="Loan Status")
