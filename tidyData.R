## Automatically Convert Dataset Variable Names
## to Descriptive Names

nice_name <- function(name) {
  dom <- substring(name, 1, 1)[1]
  if (dom == "t") {
    dom <- "Time"
  } else {
    dom <- "Frequency"
  }
  var_dir <- substring(name, 2, nchar(name))
  parts <- strsplit(var_dir, "-")
  if (parts[[1]][2] == "mean()") {
    agg_txt <- "Mean."
  } else {
    agg_txt <- "StdDev."
  }
  dim_txt <- ""
  if (length(parts[[1]]) > 2) {
    dim_txt <- paste(".", parts[[1]][3], ".", "Dimension", sep="")
  }
  paste(agg_txt, parts[[1]][1],".",dom,".Domain",dim_txt, sep="")
}

features <- read.table("features.txt", sep=" ")
mean_features <- grep("mean[(][)]", features[,2], ignore.case=T)
std_features <- grep("std[(][)]", features[,2], ignore.case=T)
target_features <- append(mean_features, std_features)

X_train <- read.table(file.path("train", "X_train.txt"))
y_train <- read.table(file.path("train", "y_train.txt"))
X_test <- read.table(file.path("test", "X_test.txt"))
y_test <- read.table(file.path("test", "y_test.txt"))

X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)

X <- X[,target_features]
nice_feature_names <- as.character(sapply(features[target_features, 2], nice_name))

lbls_raw <- read.table("activity_labels.txt")
lbls = sapply(y, function(x) lbls_raw[x,2])

df <- cbind(X, lbls)
names(df) <- append(nice_feature_names, "Activity.Type")
