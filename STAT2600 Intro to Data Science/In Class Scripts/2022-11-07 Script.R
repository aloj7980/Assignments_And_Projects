# ### Review of project proposals

## final project calendar and schedule; presenting results during final?  
###  final project "hard copy deliverables" due thursday 2022-12-08
### final exam time monday 2022-12-12 13:30-16:00: 
#### optional presentation/demo of projects
### deadline for all non-project assignment resubmissions 
### available and on campus monday 2022-12-12 


## Project Thought/Feedback
## for partner work, submit a single final report/deliverable. please include a breakdown of who did what. 
### for instance just a bulleted list saying "person A did this... person B did this... and we collaborated on these items... " will suffice. 
## in your timelines, be sure to include time to write up your results. 
## for kaggle datasets/competitions, look at what other people are doing. 


### Workflow: using R and RMarkdown simultaneously


head(mtcars)
colnames(mtcars)


carsLm = lm(mpg ~ vs + am + cyl + carb, data = mtcars)
carsLm

summary(carsLm)

