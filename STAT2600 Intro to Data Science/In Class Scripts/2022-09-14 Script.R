#### HW/Lab Questions ####
## any questions about HW1 or Lab 4? 
source("Homeworks/HW1/hw1.R")
geh_lines = read_text_file("Homeworks/HW1/Hw1TestFiles_practice/GreenEggsAndHam.txt")
geh_words = extract_words_from_lines(geh_lines)
geh_word_counts = table(geh_words)
geh_word_count_df = as.data.frame(geh_word_counts)
colnames(geh_word_count_df) <- c("Word", "Count")

sort(geh_word_count_df[, "Count"])
geh_word_count_df[, "Count"][order(geh_word_count_df[, "Count"])]

sort(geh_word_count_df[, "Count"], decreasing = TRUE)

geh_word_count_df[, "Count"][
    rev(order(geh_word_count_df[, "Count"]))
]

geh_word_count_df[, "Word"][
    rev(order(geh_word_count_df[, "Count"]))
]


geh_word_count_df[
    rev(order(geh_word_count_df[, "Count"])),]

as.data.frame(sort(geh_word_counts, decreasing = TRUE))


## if we didn't care about case, use `tolower`
sort(table(tolower(geh_words)), decreasing = TRUE)

#### Timing Experiments: Making It Fast ####
## Make it work. Make it fast. Make it pretty.


#### Experiment 1: Reading Names ####
library(readr)
library(stringr)
namesFilePath = "Data/p022_names.txt"

startTime1 = Sys.time()
names_raw = read_file(namesFilePath)
checkpoint1 = Sys.time()
names_noQuotes = str_replace_all(
    names_raw, 
    pattern = "\"", 
    replacement = "")
checkpoint2 = Sys.time()
names1 = unlist(str_split(names_noQuotes, pattern = ","))
endTime1 = Sys.time()
endTime1 - startTime1

startTime2 = Sys.time()
names2 = scan(namesFilePath, what = "", sep = ",", na.strings = "", quiet = TRUE)
endTime2 = Sys.time()
endTime2 - startTime2

startTime3 = Sys.time()
names3 = unlist(as.vector(read.csv(namesFilePath, header = FALSE, quote = "\"'", na.strings = "")))
endTime3 = Sys.time()
endTime3 - startTime3

all(names1 == names2)
all(names1 == names3)


#### Experiment 2: Multiplying Numbers ####
# multiplying 3-digit numbers in different ways
twoDigNumbers = 10:99

## `for` loop with appending
start = Sys.time()
products_1 = numeric()
for(first in twoDigNumbers){
    for(second in twoDigNumbers){
        products_1 = c(products_1, first*second)
    }
}
end = Sys.time()
duration_1 = end - start
duration_1

## `for` loop with preallocation
start = Sys.time()
products_2 = numeric(length(twoDigNumbers)^2)
counter = 1
for(first in twoDigNumbers){
    for(second in twoDigNumbers){
        products_2[counter] = first*second
        counter = counter + 1
    }
}
end = Sys.time()
duration_2 = end - start
duration_2


## matrix mutliplication
start = Sys.time()
products_3 = as.vector(
    twoDigNumbers %*% t(twoDigNumbers)
)
end = Sys.time()
duration_3 = end - start


## `expand.grid` and apply()
start = Sys.time()
products_4 = apply(
    expand.grid(twoDigNumbers, twoDigNumbers),
    1, prod
)
end = Sys.time()
duration_4 = end - start

c(
    duration_1,
    duration_2,
    duration_3,
    duration_4
)

