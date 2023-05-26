require(stringr)
require(readr)
require(dplyr)
collegesDf = read_csv("colleges.csv")
countiesDf = read_csv("counties.csv")
maskDf = read_csv("mask_use_by_county.csv")
nonUniqueColleges = c()
for(college in collegesDf$college){
  count = 0
  for(college1 in collegesDf$college){
    if(college == college1){
      count = count + 1
    }
  }
  if(count > 1){
    nonUniqueColleges = append(nonUniqueColleges,college)
  }
}
uniqueDf = collegesDf %>% filter(college %in% nonUniqueColleges)
uniqueDf = uniqueDf[with(uniqueDf, order(college, state)), ]
uniqueDf = uniqueDf[c(1:3,5)]
View(uniqueDf)
write_csv(uniqueDf,"NonUniqueNamedColleges.csv")

countySummaryDf = countiesDf %>%
  arrange(
    fips,
    date
  ) %>%
  group_by(
    state,
    county,
    fips
  ) %>%
  mutate(
    new_cases = coalesce(cases - lag(cases), cases)
  ) %>% filter(
    between(date, as.Date('2020-07-01'), as.Date('2021-05-31'))
  ) %>% group_by(
    state,county,fips
  ) %>% summarize(
    total_cases = sum(new_cases)
  ) %>% left_join (
    maskDf, c("fips" = "county_fips")
  )

countySummaryDf = countySummaryDf[with(countySummaryDf, order(-total_cases)), ]
View(countySummaryDf)

collegeSummaryDf = collegesDf %>% left_join(
  countySummaryDf, c("state","county")
) %>% mutate(
  percent_college_cases = cases / total_cases * 100
) %>% filter (
  state == "Colorado"
)
collegeSummaryDf = collegeSummaryDf[with(collegeSummaryDf, order(-percent_college_cases)), ]
collegeSummaryDf = collegeSummaryDf[c(1:3,5:6,8:14)]
colnames(collegeSummaryDf)[5] <- "college_cases"
collegeSummaryDf = collegeSummaryDf[,c(1,2,3,4,6,5,12,7,8,9,10,11)]
View(collegeSummaryDf)

write_csv(countySummaryDf,"CountySummary.csv")
write_csv(collegeSummaryDf,"CollegeSummary.csv")

