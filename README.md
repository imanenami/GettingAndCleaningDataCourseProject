# GettingAndCleaningDataCourseProject

run_analysis.R does the following things: 

- read features from features.txt and creates target_features list containing mean and std features 
- read X and Y and subjects, train and test dataset 
- merges test and train sets 
- subsets X to contain only mean and std features 
- converts variables to nice names using make_nice_name function 
- calculates average of variable for each subject and each activity type 
