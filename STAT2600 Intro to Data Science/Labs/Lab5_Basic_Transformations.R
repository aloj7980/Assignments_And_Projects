library(stringr)
library(magrittr)
library(readr)
is_senior_position <- function(jobTitle){
  str_detect(jobTitle, "Principal|Senior|Sr|Lead")
}
is_hourly_wage <- function(salaryRange){
  str_detect(salaryRange, "Per Hour")
}
extract_min_salary <- function(salaryRange){
  str_extract(salaryRange, "(?<=\\$)[[:digit:]]+") %>% as.numeric()
}
extract_max_salary <- function(salaryRange){
  str_extract(salaryRange, "(?<=-\\$)[[:digit:]]+") %>% as.numeric()
}
original <- read_csv("DataScienceJobs.csv")
JobTitle  = original$"Job Title"
IsSrPosition = JobTitle %>% is_senior_position
Salary_Range = original$"Salary Estimate"
MinSalary = rep(0,length(Salary_Range))
MaxSalary = rep(0,length(Salary_Range))
for(n in 1:length(Salary_Range)){
  if(Salary_Range[n] %>% is_hourly_wage){
    MinSalary[n]=extract_min_salary(Salary_Range[n])*52*40/1000
    MaxSalary[n]=extract_max_salary(Salary_Range[n])*52*40/1000
  }else{
    MinSalary[n]=extract_min_salary(Salary_Range[n])
    MaxSalary[n]=extract_max_salary(Salary_Range[n])
  }
}
MedianSalary = (MinSalary + MaxSalary) / 2
df <- data.frame(JobTitle,IsSrPosition,MinSalary,MaxSalary,MedianSalary)
write_csv(df, "lab5.csv")