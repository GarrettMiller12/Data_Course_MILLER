csv_files <- list.files(path = "Data" , pattern = "\\.csv", full.names = TRUE)

length(csv_files) 

df <- read.csv("Data/wingspan_vs_mass.csv")

head(df, n = 5) 

b_files <- list.files(path = "Data" , pattern = "^b" , full.names = TRUE, recursive = TRUE)

for(i in 1:length(b_files)) {
  print(readLines(b_files[i], n = 1))
}

for(i in 1:length(csv_files)) {
  print(readLines(csv_files[i], n = 1))
}