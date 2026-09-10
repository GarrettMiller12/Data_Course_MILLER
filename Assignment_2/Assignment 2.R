## Lists all csv files and stores in an object called csv_files
csv_files <- list.files(path = "Data" , pattern = "\\.csv", full.names = TRUE)

## Shows how many files match that description
length(csv_files) 

## Open wingspan_vs_mass.csv file and stores contents on an object named df
df <- read.csv("Data/wingspan_vs_mass.csv")

## Inspects the first 5 lines of this data set
head(df, n = 5) 

## Finds files beginning with b and stores in an object b_files
b_files <- list.files(path = "Data" , pattern = "^b" , full.names = TRUE, recursive = TRUE)

## For loop for displaying the first line of each b file
for(i in 1:length(b_files)) {
  print(readLines(b_files[i], n = 1))
}

## For loop for displaying the first line of each csv file
for(i in 1:length(csv_files)) {
  print(readLines(csv_files[i], n = 1))
}