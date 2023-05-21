#################################
#Loading Packages
#################################

library(readxl)       # package for loading data
library(dplyr)        # package for data manipulation in tables
library(plotly)       # package for plotting
library(ggplot2)      # package for plotting


#################################
# Step 1 - Understanding the data
#################################

# Read Excel file
my_data <- read_excel("Downloads/Visualizing & Analyzing Data with R_Methods & Tools /A1- Business Case Presentation (video submission assignment)/Air France Case Spreadsheet Supplement-1.xls", sheet = 2)
View(my_data)

# View a summary of the data
summary(my_data)

#################################
# Step 2 - Data Massage
#################################

# Create new columns
my_data$CTR <- my_data$Clicks / my_data$Impressions
my_data$ROA <- my_data$`Total Volume of Bookings` / my_data$`Total Cost`
my_data$REV <- my_data$Amount - my_data$`Total Cost`

View(my_data)

# Rename the column
#colnames(my_data)[15] <- "Avg_Cost_per_Click"
#colnames(my_data)[17] <- "Engine_Click_Thru_Rate"
#colnames(my_data)[19] <- "Trans_Conv_Rate"

View(my_data)

# Group by publisher name
#publisher_summary <- data %>% 
#group_by(`Publisher Name`) %>% 
#summarize(Total_Click = sum(Clicks, na.rm = TRUE),
#Total_Impression = sum(Impressions, na.rm = TRUE),
#Total_Cost = sum(`Total Cost`, na.rm = TRUE),
#Total_Bookings = sum(`Total Volume of Bookings`, na.rm = TRUE),
#avg_Cost_per_click = mean(`Avg_Cost_per_Click`, na.rm = TRUE),
#avg_Engine_Click_Thru_Rate = avg(`Engine_Click_Thru_Rate`, na.rm = TRUE),
#avg_Trans_Conv_Rate = avg(`Trans_Conv_Rate`, na.rm = TRUE),
#avg_CTR = mean(CTR, na.rm = TRUE), 
#avg_ROA = mean(ROA, na.rm = TRUE),
#avg_REV = mean(REV,na.rm = TRUE)) %>% 
#arrange(desc(avg_CTR), desc(avg_ROA), desc(avg_REV))

#View(publisher_summary)

# With Kayak Data ver.
# Devide per 52 weeks to marge with Kayak Data
publisher_prep <- my_data %>%
  group_by(`Publisher Name`) %>%
  summarize(Clicks = (sum(Clicks, na.rm = TRUE))/52,
            Total_Cost = (sum(`Total Cost`, na.rm = TRUE))/52,
            Total_Bookings = (sum(`Total Volume of Bookings`, na.rm = TRUE))/52,
            Avg_Ticket = (sum(mean(Amount, na.rm = TRUE)))/52,
            Total_Revenue = (sum(REV, na.rm = TRUE))/52)

View(publisher_prep)

# Rename column
colnames(publisher_prep)[1] ="Publisher_Name"

# Create Kayak data frame
df_Kayak <- data.frame(Publisher_Name = "Kayak",
                       Clicks = 2839,
                       Total_Cost = 3567.13,
                       Total_Bookings = 208,
                       Avg_Ticket = 1123.53,
                       Total_Revenue = 233694.00)

# Rename columns in df_Kayak to match those in publisher_prep
colnames(df_Kayak) <- colnames(publisher_prep)

# Bind df without Kayak and df of Kayak
publisher_global <- rbind(df_Kayak, publisher_prep)
View(publisher_global)

# Change the name of the data frame
#names(With_Kayak) <- "publisher_global"
#publisher_global <- With_Kayak
#View(pubisher_global)

# Create the df for publisher in us by deleting the rows of global
publisher_us <- publisher_global[-c(2,4,6),]
View(publisher_us)

# Create the df for publisher in us by deleting the rows of global
publisher_global2 <- publisher_global[-c(1,3,5,7,8),]
View(publisher_us)

# Group by Keyword
Key_word <- my_data %>%
  group_by(`Category`) %>%
  summarize(Clicks = sum(Clicks, na.rm = TRUE),
            Total_Cost = sum(`Total Cost`, na.rm = TRUE),
            Total_Bookings = sum(`Total Volume of Bookings`, na.rm = TRUE),
            Avg_Ticket = mean(Amount, na.rm = TRUE),
            Total_Revenue = sum(REV, na.rm = TRUE))

View(Key_word)



#################################
# Step 3 - Plotting
#################################
# Format the bar charts
format_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

## Publisher US ##
# Create a bar chart of Total Cost by Publisher_Name
library(ggplot2)
ggplot(publisher_us, aes(x = reorder(`Publisher_Name`, Total_Cost), y = Total_Cost)) +
  geom_bar(stat = "identity", fill = "BLUE") +
  labs(x = "Publisher", y = "Total Cost") +
  scale_y_continuous(labels=format_number) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Create a bar chart of Total Revenue by Publisher_Name
library(ggplot2)
ggplot(publisher_us, aes(x = reorder(`Publisher_Name`, Total_Revenue), y = Total_Revenue)) +
  geom_bar(stat = "identity", fill = "Green") +
  labs(x = "Publisher", y = "Total Revenue") +
  scale_y_continuous(labels=format_number) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Create a bar chart of Clicks by Publisher_Name
library(ggplot2)
ggplot(publisher_us, aes(x = reorder(`Publisher_Name`, Clicks), y = Clicks)) +
  geom_bar(stat = "identity", fill = "Orange") +
  labs(x = "Publisher", y = "Clicks") +
  scale_y_continuous(labels=format_number) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## Publisher Name_Global##
# Create a bar chart of Total Cost by Publisher_Name
library(ggplot2)
ggplot(publisher_global2, aes(x = reorder(`Publisher_Name`, Total_Cost), y = Total_Cost)) +
  geom_bar(stat = "identity", fill = "BLUE") +
  labs(x = "Publisher", y = "Total Cost") +
  scale_y_continuous(labels=format_number) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Create a bar chart of Total Revenue by Publisher_Name
library(ggplot2)
ggplot(publisher_global2, aes(x = reorder(`Publisher_Name`, Total_Revenue), y = Total_Revenue)) +
  geom_bar(stat = "identity", fill = "Green") +
  labs(x = "Publisher", y = "Total Revenue") +
  scale_y_continuous(labels=format_number) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Create a bar chart of Clicks by Publisher_Name
library(ggplot2)
ggplot(publisher_global2, aes(x = reorder(`Publisher_Name`, Clicks), y = Clicks)) +
  geom_bar(stat = "identity", fill = "ORANGE") +
  labs(x = "Publisher", y = "Clicks") +
  scale_y_continuous(labels=format_number) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

########
library(readxl)
Air_france <- read_excel("Downloads/Visualizing & Analyzing Data with R_Methods & Tools /A1- Business Case Presentation (video submission assignment)/Air France Case Spreadsheet Supplement-1.xls", sheet = 2)

View(Air_france)

AF <- is.data.frame(Air_france)
print(AF)

summary(Air_france)
table(Air_france$`Publisher ID`)
row_AF <- nrow(Air_france)     #creating Data Objects
col_AF <- ncol(Air_france)     #creating Data Objects
str(Air_france)
null_check <- is.na(Air_france)
print(null_check)

table(Air_france$`Match Type`)
summary(Air_france$`Search Engine Bid`)
null_check <- is.na(Air_france$`Search Engine Bid`)
print(null_check)

table(Air_france$`Publisher Name`)


##############################################
# Data Massaging
##############################################

Air_france$revenue <- Air_france$Amount - Air_france$`Total Cost`


Air_france$label <- c()

for(i in 1:nrow(Air_france)){
  if(Air_france$revenue[i]> 0 ){
    Air_france$label[i] <- "good"
  }else{
    Air_france$label[i] <- "Bad"
  } #closing the if statement for outstanding
  
} # closing the label for loop with if statement inside
View(Air_france)
table(Air_france$label)

#implementing business logic for the good_bad variable

Air_france$binary <- gsub("good", "1", Air_france$label) #Business success
Air_france$binary  <- gsub("Bad","0", Air_france$binary) #Business failure


table(Air_france$binary)
Air_france$bin <- as.numeric(Air_france$binary)
table(Air_france$bin)

#Purpose of logistic regression is to identify the probability of business success. Prob(business success)/Prob(business failure) = exp(B0+B1x). B = Beta

sum(is.na(Air_france$revenue))
sum(is.na(Air_france$`Search Engine Bid`))
sum(is.na(Air_france$Clicks))
sum(is.na(Air_france$`Click Charges`))
sum(is.na(Air_france$`Avg. Cost per Click`))
sum(is.na(Air_france$Impressions))
sum(is.na(Air_france$`Engine Click Thru %`))
sum(is.na(Air_france$`Avg. Pos.`))
sum(is.na(Air_france$`Trans. Conv. %`))
sum(is.na(Air_france$`Total Cost/ Trans.`))
sum(is.na(Air_france$Amount))
sum(is.na(Air_france$`Total Volume of Bookings`))
sum(is.na(Air_france$`Total Cost`))


#Random sampling

training_AF <- sample(1:nrow(Air_france), size=0.8*nrow(Air_france))

#Step 2
Air_france_train <- Air_france[training_AF,] #Variables or observations?? In this case is observations, then "i" or in this case called "training_idx" goes to the left
Air_france_test <- Air_france[-training_AF,]



#logistic regression
AF_logit <- glm(bin ~ Clicks+`Click Charges`+`Total Volume of Bookings`+`Total Cost/ Trans.`, 
                data = Air_france_train, family = "binomial")

summary(AF_logit)

exp(0.001873)-1   # coefficents for clicks
exp(-0.0119)-1   # coefficents for click charges
exp(10.59)-1    # coefficents for Booking Volumes

#Normalizing variables
normalize <- function(x){
  temp_scale <- (x-min(x))/(max(x)-min(x))
  return(temp_scale)
} #Closing the normalize function

Air_france_train$Clicks_norm <- normalize(x=Air_france_train$Clicks)
Air_france_train$`Click Charges norm` <- normalize(x=Air_france_train$`Click Charges`)
Air_france_train$`Total Volume of Bookings norm` <- normalize(x=Air_france_train$`Total Volume of Bookings`)
Air_france_train$`Total Cost/ Trans. norm` <- normalize(x=Air_france_train$`Total Cost/ Trans.`)


AF_logit_norm <- glm(bin ~ Clicks_norm+`Click Charges norm`+`Total Volume of Bookings norm`+`Total Cost/ Trans. norm`, 
                data = Air_france_train, family = "binomial")

summary

##############################################
# Predictive Analysis
##############################################

install.packages("caret")
library(caret)

AF_prediction <- predict(AF_logit, Air_france_test, type = "response")


confusionMatrix(data=as.factor(as.numeric(AF_prediction>0.5)), #you can increase or decrease this percentage accordigt to business model risk.
                reference =as.factor(as.numeric(Air_france_test$bin)))

library(rpart)
library(rpart.plot)

AF_tree <- rpart(bin ~ Clicks+`Click Charges`+`Total Volume of Bookings`+`Total Cost/ Trans.`, 
                 data = Air_france_train, method = "class", cp = 0.017)
rpart.plot(AF_tree, type =1, extra = 1)

AF_tree_prediction <- predict(AF_tree, Air_france_test,type = "prob")

confusionMatrix(data=as.factor(as.numeric(AF_tree_prediction[,2] >0.5)), #you can increase or decrease this percentage accordigt to business model risk.
                reference =as.factor(as.numeric(Air_france_test$binary)))





