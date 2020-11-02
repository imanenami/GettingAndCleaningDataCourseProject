library(dplyr)

## function that converts dataset variable names
## to descriptive ones

make_nice_name <- function(name) {
  ## detect Time/Frequency Dimension
  dom <- substring(name, 1, 1)[1]
  if (dom == "t") {
    dom <- "Time"
  } else {
    dom <- "Frequency"
  }
  ## detect Variable Name
  var_dir <- substring(name, 2, nchar(name))
  parts <- strsplit(var_dir, "-")
  ## detect aggregation type: mean or std
  if (parts[[1]][2] == "mean()") {
    agg_txt <- "Mean."
  } else {
    agg_txt <- "StdDev."
  }
  ## detect dimension: X, Y, Z or nothing
  dim_txt <- ""
  if (length(parts[[1]]) > 2) {
    dim_txt <- paste(".", parts[[1]][3], ".", "Dimension", sep="")
  }
  ## return a descriptive name
  paste(agg_txt, parts[[1]][1],".",dom,".Domain",dim_txt, sep="")
}

## read features
features <- read.table("features.txt", sep=" ")
mean_features <- grep("mean[(][)]", features[,2], ignore.case=T)
std_features <- grep("std[(][)]", features[,2], ignore.case=T)
target_features <- append(mean_features, std_features)

## read X,y train and test sets
X_train <- read.table(file.path("train", "X_train.txt"))
y_train <- read.table(file.path("train", "y_train.txt"))
X_test <- read.table(file.path("test", "X_test.txt"))
y_test <- read.table(file.path("test", "y_test.txt"))

# read subject 
subj_train <- read.table(file.path("train", "subject_train.txt"))
subj_test <- read.table(file.path("test", "subject_test.txt"))

## merge train and test datasest
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subj <- rbind(subj_train, subj_test)

## select only mean() and std() features from X
X <- X[,target_features]

## make nice feature names!
nice_feature_names <- as.character(sapply(features[target_features, 2], make_nice_name))

# convert y values to descriptive activity labels
lbls_raw <- read.table("activity_labels.txt")
lbls <- sapply(y, function(x) lbls_raw[x,2])

# compose the final DataFrame
df <- cbind(X, factor(lbls), factor(subj[,1]))
names(df) <- append(nice_feature_names, c("Activity.Type", "Subject.ID"))

# calculate average variables for each activity type and subject
avg_df <- df %>% group_by(Activity.Type, Subject.ID) %>% summarise_if(is.numeric, list(AVERAGE = mean))
write.table(avg_df, "out.txt", row.names = F)
avg_df
