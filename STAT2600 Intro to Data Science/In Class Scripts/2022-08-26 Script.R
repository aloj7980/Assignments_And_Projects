### TTP: Ch 1: Program


#### Functions ####
## just like other objects in R, a function is created with an assignment 
randomly_sample_student <- function(){
    class_df = read.csv(file = "ClassRoster.csv")
    student_names = class_df$Name
    n_students = length(student_names)
    student_names[sample(1:n_students, size=1)]  
}

randomly_sample_student()


