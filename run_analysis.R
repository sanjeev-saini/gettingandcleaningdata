#---------------------------------------------------------------------------------------------------------
#   GETTING THE DATASET
#---------------------------------------------------------------------------------------------------------

# Create a directory 'data' to store the dataset
if(!dir.exists("./data")){
    dir.create("./data")
}

# Download only if dataset not downloaded previously
if(!file.exists("./data/UCI HAR Dataset")){
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                  "./data/UCI_HAR_Dataset.zip",
                  mode = "wb"
    )
}

# unzip the downloaded dataset zip file into 'data' folder
if(!file.exists("./data/UCI HAR Dataset")){
    unzip("./data/UCI_HAR_Dataset.zip", exdir = "./data")
}

#---------------------------------------------------------------------------------------------------------
# READ THE TEST, TRAIN, ACTIVITY, SUBJECT AND FEATURES DATASETS
#---------------------------------------------------------------------------------------------------------

if(!require(data.table)){
    install.packages("data.table")
}

if(!require(dplyr)){
    install.packages("dplyr")
}

library(data.table)
library(dplyr)

# Read activity data file    
tmpActivity <- fread("./data/UCI HAR Dataset/activity_labels.txt",
                     sep = "auto",
                     header = FALSE,
                     stringsAsFactors = TRUE)

my_activity <- as_tibble(tmpActivity)

# Assign descriptive names to the activity variables
names(my_activity) <- c("activityid", "activityname")

rm("tmpActivity")

# Read test - subject association data file    
tmpSubjectTest <- fread("./data/UCI HAR Dataset/test/subject_test.txt",
              sep = "auto",
              header = FALSE,
              stringsAsFactors = TRUE)

my_subjecttest <- as_tibble(tmpSubjectTest)
rm("tmpSubjectTest")

# Read test - activity association data file
tmpActivityTest <- fread("./data/UCI HAR Dataset/test/y_test.txt",
              sep = "auto",
              header = FALSE,
              stringsAsFactors = TRUE)

my_activitytest <- as_tibble(tmpActivityTest)
rm("tmpActivityTest")

# Read test data set
tmpTest <- fread("./data/UCI HAR Dataset/test/X_test.txt",
              sep = " ",
              header = FALSE,
              stringsAsFactors = TRUE)

my_test <- as_tibble(tmpTest)
rm("tmpTest")

# Read train - subject association data file    
tmpSubjectTrain <- fread("./data/UCI HAR Dataset/train/subject_train.txt",
              sep = "auto",
              header = FALSE,
              stringsAsFactors = TRUE)

my_subjecttrain <- as_tibble(tmpSubjectTrain)
rm("tmpSubjectTrain")

# Read train - activity association data file
tmpActivityTrain <- fread("./data/UCI HAR Dataset/train/y_train.txt",
                         sep = "auto",
                         header = FALSE,
                         stringsAsFactors = TRUE)

my_activitytrain <- as_tibble(tmpActivityTrain)
rm("tmpActivityTrain")

# Read train dataset
tmpTrain <- fread("./data/UCI HAR Dataset/train/X_train.txt",
      sep = " ",
      header = FALSE,
      stringsAsFactors = TRUE)

my_train <- as_tibble(tmpTrain)
rm("tmpTrain")

# Read features data file. It contains the column names for test and train datasets
tmpNames <- fread("./data/UCI HAR Dataset/features.txt",
                   sep = "auto",
                   header = FALSE,
                   stringsAsFactors = TRUE)

#---------------------------------------------------------------------------------------------------------
#   ADD FEATURES DATA AS COLUMN NAMES FOR TEST AND TRAIN DATASETS 
#       (FEATURES DATASET HOLDS THE COLUMN NAMES FOR TEST AND TRAIN DATASET)
#---------------------------------------------------------------------------------------------------------

# Make the variable names in features valid for column names in R by removing special characters.
# There are 84 duplicate names. 'unique = TRUE' ensures that unique names are created.
tmpValidNames <- make.names(tmpNames$V2, unique = TRUE, allow_ = TRUE)

my_colnames <- as_tibble(tmpNames)
my_validcolnames <- as_tibble(tmpValidNames)

rm("tmpNames")
rm("tmpValidNames")

# Assign column names to test and train datasets
names(my_train) <- my_validcolnames$value
names(my_test) <- my_validcolnames$value

#---------------------------------------------------------------------------------------------------------
#   ADD COLUMN NAMES FOR SUBJECT AND ACTIVITY DATASETS 
#---------------------------------------------------------------------------------------------------------

names(my_subjecttest) <- "subject"
names(my_activitytest) <- "activity"
names(my_subjecttrain) <- "subject"
names(my_activitytrain) <- "activity"

my_set <- tibble(set = "test")

#---------------------------------------------------------------------------------------------------------
#   BIND COLUMNS subject AND activity TO TEST AND TRAIN DATASETS 
#---------------------------------------------------------------------------------------------------------

my_testset <- cbind(my_subjecttest, my_activitytest, my_set, my_test)
my_testset <- as_tibble(my_testset)

my_set <- tibble(set = "test")

my_trainset <- cbind(my_subjecttrain, my_activitytrain, my_set, my_train)
my_trainset <- as_tibble(my_trainset)

#---------------------------------------------------------------------------------------------------------
#   BIND ROWS OF TEST AND TRAIN DATASETS INTO ONE DATASET
#---------------------------------------------------------------------------------------------------------

my_dataset <- bind_rows(my_testset, my_trainset)

#---------------------------------------------------------------------------------------------------------
#   REPLACE activity with DESCRIPTIVE activityname IN THE DATASET
#---------------------------------------------------------------------------------------------------------

my_dataset <- inner_join(my_dataset, my_activity, by = c("activity" = "activityid"))

#---------------------------------------------------------------------------------------------------------
#   CREATE TIDY DATASET WITH ONLY MEAN AND STD. DEVIATION VARIABLES
#---------------------------------------------------------------------------------------------------------

my_namesMeanAndStd <- grep("(\\.)+(mean|std)(\\.)+", names(my_dataset), value = TRUE)

my_tidyMeanAndStd <- my_dataset %>%
                     select(subject, activityname, set, my_namesMeanAndStd)

#---------------------------------------------------------------------------------------------------------
#   IMPROVE READABILITY OF THE TIDYSET VARIABLE NAMES
#---------------------------------------------------------------------------------------------------------

my_colnames <- names(my_tidyMeanAndStd)

my_colnames <- gsub("mean", "Mean", my_colnames)
my_colnames <- gsub("std", "StdDev", my_colnames)
my_colnames <- gsub("\\.+", "", my_colnames)
my_colnames <- gsub("^t", "time", my_colnames)
my_colnames <- gsub("^f", "freq", my_colnames)
my_colnames <- gsub("BodyBody", "Body", my_colnames)

names(my_tidyMeanAndStd) <- my_colnames

#---------------------------------------------------------------------------------------------------------
#   MAKE NEW TIDY DATASET WHERE EACH ROW IS AN OBSERVATION OF EACH OF THE 6 ACTIVITIES FOR A SUBJECT, 
#   ALONG WITH THE MEAN AND STD. DEVIATION OF 33 BASE VARIABLES
#---------------------------------------------------------------------------------------------------------

my_melt <- melt(my_tidyMeanAndStd, id = c("subject", "set", "activityname"))
my_activitydata <- dcast(my_melt, subject + activityname ~ variable, mean)

#---------------------------------------------------------------------------------------------------------
#   CREATE THE TIDY DATASET FILE IN WORKING DIRECTORY
#---------------------------------------------------------------------------------------------------------

write.table(my_activitydata,
            file = "./MeanAcitivityDetails.txt"
)            

#---------------------------------------------------------------------------------------------------------            
#   CLEAR GLOBAL ENVIRONMENT OF OBJECTS
#---------------------------------------------------------------------------------------------------------

rm(list = ls())
