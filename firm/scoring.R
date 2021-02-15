##### Customization #####
# edit these fields:
base_path <- "//datastore01.psy.miami.edu/Groups/ELosin_Lab/Research/Research_Projects/DPRE-UMiami/Scripts/Scoring Qualtrics Questionnaires/"
document_name <- "test_data.csv" # must be a .csv
first_meaningful <- "Subject" # header of first column you want to keep
header_rows <- 2 # number of extra rows generated with nonsense data at top of CSV... usually 2 with Qualtrics
unique_name <- "Subject" # name of the column (provided in first row of CSV) that contains your subject IDs
##################

##### Function for imputing sum over subjects with missing data #####
imputeSum <- function(x) {
  # takes: vector x
  # returns, for 50%+ complete: sum of vector x, with NA values imputed
  # returns, for less than 50% complete: NA
  
  incomplete_prop = sum(is.na(x)) / length(x)
  if (incomplete_prop <= 0.5)
  {
    mean_x <- mean(x, na.rm=TRUE)
    
    add_on <- (mean_x * sum(is.na(x)))
    
    answer <- sum(x, na.rm=TRUE) + add_on
    return(answer)
  }
  else
  {
    return(NA)
  }
}


##### Reading in raw data #####
raw <- read.csv(paste(base_path, document_name, sep=""), header=T, sep=',', na.strings=c(""," ","NA", "na", "N/A"), stringsAsFactors = FALSE)
drop_rows<- seq(1, header_rows)
drop_cols <- seq(1, which(colnames(raw) == first_meaningful)-1)
raw <- raw[-drop_rows, -drop_cols]

##### Subsetting relevant questionnaire data #####
first_col_name <- "PANAS_S_PANAS_S_1_P"
last_col_name <- "PANAS_S_PANAS_S_20_N"

first_col_num <- which(colnames(raw) == first_col_name)
last_col_num <- which(colnames(raw) == last_col_name)
subj_col_num <- which(colnames(raw) == unique_name)
PANAS_S <- raw[,c(seq(first_col_num, last_col_num), subj_col_num)]
numeric_cols <- c(1:20) # skip none, all numeric
PANAS_S[,numeric_cols] <- sapply(PANAS_S[,numeric_cols], as.numeric)

##### Scoring #####
#score
#Instructions: Positive Affect Score: Add the scores on items 1, 3, 5, 9, 10, 12, 14, 16, 17, and 19.
#Scores can range from 10 - 50, with higher scores representing higher levels of positive affect.
#Mean Scores: Momentary  29.7 ( SD  7.9); Weekly  33.3 ( SD  7.2)

PANAS_S$Pos_Score <- apply(PANAS_S, 1, FUN=function(x){
  imputeSum(as.numeric(x[c(1,3,5,9,10,12,14,16,17,19)]))
})

# Instructions: Negative Affect Score: Add the scores on items 2, 4, 6, 7, 8, 11, 13, 15, 18, and 20.
# Scores can range from 10 - 50, with lower scores representing lower levels of negative affect.
# Mean Score: Momentary  14.8 ( SD  5.4); Weekly  17.4 ( SD  6.2)

PANAS_S$Neg_Score <- apply(PANAS_S, 1, FUN=function(x){
  imputeSum(as.numeric(x[c(2,4,6,7,8,11,13,15,18,20)]))
})