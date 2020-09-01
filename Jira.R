
#Install key dependencies
#install.packages("tidyverse")
#install.packages("dplyr")
#Load the key libraries.
library(tidyverse)
library(dplyr)

datafile <- "Fluid-1Sep.csv"

#Read the CSV file
jira_data <- read.csv(file = datafile , header = TRUE , sep = ",")


#my_iris <- as_tibble(iris)
#my_iris %>%  mutate(sepal_by_petal_l = Sepal.Length/Petal.Length *100) 
#my_data <- my_data %>% mutate(Created_Date = as.Date(Created , "%d/%b/%y"))

#my_data2 <- jira_data %>% mutate_at(c("Created" , "Updated" , "Resolved")  , function (x) as.Date(x , "%d/%b/%y"))

#Pick up the key elements for inspection from the master file
summarised_jira_data <- select (jira_data  , "Issue.key" , "Issue.Type", "Status" , "Created" , "Updated" , "Resolved" , "Priority")

summarised_jira_data <- summarised_jira_data %>% mutate_at(c("Created" , "Updated" , "Resolved")  , function (x) as.Date(x , "%d/%b/%y"))

summarised_jira_data$timeSpent <-  summarised_jira_data$Resolved - summarised_jira_data$Created 

summarised_jira_FromMay <- summarised_jira_data %>% filter(Created > "2020-05-01" & Status=="Done") %>% arrange(Created)

BackLog_count <- summarised_jira_data %>% filter(Status=="Backlog") %>% count()

time_taken_fromMay <- mean(summarised_jira_FromMay$timeSpent)

summarised_jira_data$week_grouping <- cut( summarised_jira_data$Created  , breaks ="2 week")

summarised_jira_data$week_grouping_resolved <- cut(summarised_jira_data$Resolved  , breaks ="2 week")

summarised_jira_data_created <- summarised_jira_data %>% group_by(week_grouping) %>% summarise(number = n())

summarised_jira_data_resolved <- summarised_jira_data %>% group_by(week_grouping_resolved) %>% summarise(number = n())

summarised_jira_data_resolved <- summarised_jira_data_resolved %>% drop_na() 

average_created_2week <- mean(summarised_jira_data_created$number) 

average_created_2week_resolved <- mean(summarised_jira_data_resolved$number) 

merged_result <- merge(summarised_jira_data_created, summarised_jira_data_resolved  , by.x = "week_grouping", by.y = "week_grouping_resolved")

merged_result <- merged_result %>% mutate_at(c("week_grouping" )  , function (x) as.Date(x , "%Y-%m-%d"))

colnames(merged_result) <- c("week_grouping" , "Created_Count", "Resolved_Count")

mean_June_Raised <- with(merged_result , mean(Created_Count[week_grouping >= "2020-05-11"]) )

mean_June_Completed <- with(merged_result , mean(Resolved_Count[week_grouping >= "2020-05-11" ]) )

finalSummary <- data.frame(Items_Backlog = BackLog_count$n , Time_spent_May = time_taken_fromMay , Created = average_created_2week , Resolved =average_created_2week_resolved ,From_June_created = mean_June_Raised , From_June_Resolved  = mean_June_Completed)

rmarkdown::render("Presentation.Rmd")

#DF <- data.frame(matchDate = as.POSIXct(as.Date(sample(5000,100,replace=TRUE), origin="1993-01-01")))
#years <- 1992:2011
#DF$season <- cut(DF$matchDate, breaks=as.POSIXct(paste(years,"-08-01",sep="")), labels=paste(years[-length(years)],years[-length(years)]+1,sep="/"))

