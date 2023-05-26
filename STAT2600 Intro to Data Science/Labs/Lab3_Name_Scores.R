require(stringr)
read_names_txt <- function(namesFilePath){
  scan(namesFilePath,what="",sep=",",na.strings="")
}

scoreName <- function(name) {
  dict = 1:26
  names(dict) = unlist(strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ", ""))
  return(sum(dict[unlist(strsplit(name, ""))]))
}

names = read_names_txt("Data/p022_names.txt")
sum_name_scores <- function(names){
  nlist = sort(names)
  weightedScore = (1:length(nlist))*sapply(nlist,scoreName)
  return(sum(weightedScore))
}

sum_name_scores(read_names_txt("Data/p022_names.txt"))

