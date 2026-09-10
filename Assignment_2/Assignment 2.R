csv_files <- list.files(path = "Data" , pattern = "\\.csv", full.names = TRUE)

length(csv_files) 

df <- read.csv("Data/wingspan_vs_mass.csv")

head(df, n = 5) 

b_files <- list.files(path = "Data" , pattern = "^b" , full.names = TRUE, recursive = TRUE)

for(file in b_files) {
  print(readLines(file, n = 1))
}

for(file in csv_files) {
  print(readLines(file, n = 1))
}