##########################################################################################
# @gilbellosta, 2018-07-02
# Beyond ANOVA
##########################################################################################
setwd("~/Projectes/R/CursBayesianStatistics/session_03")
library(plyr)
library(ggplot2)
library(rstan)

#-----------------------------------------------------------------------------------------
# Schools and students
#-----------------------------------------------------------------------------------------

# We have 300 students
n_schools<-10
n_students <- 300
students <- 1:n_students

# The come from 10 schools

schools <- sample(1:n_schools, n_students, replace = T)

# Each student makes 3 exams and the results are:

n_exams <- 3

mu <- 5
school_effect <- rnorm(n_schools, 0, 0.5)
student_effect <- rnorm(n_students, 0, 2)

exams <- ldply(students, function(id) 
  data.frame(student = id, school = schools[id], exam = 1:n_exams, 
             marks = student_effect[id] + school_effect[schools[id]] + rnorm(n_exams, 0, 1)))

stan_data = list(
  n_obs = nrow(exams),
  n_students = n_students,
  n_schools = n_schools,
  students = exams$student,
  schools  = exams$school,
  marks    = exams$marks
)

fit_alt <- stan(file = "03_anova_stan.stan",
                data = stan_data,
                iter = 10000, warmup = 5000, 
                chains = 1, thin = 10)

# Exercise: are we interested in school or student effects? Change the stan code to reflect that interest 
#   in one or the other effects.

#exercicio que encara no he fet
    
    setwd("~/Projectes/R/CursBayesianStatistics/session_03")
    library(plyr)
    library(ggplot2)
    library(rstan)
    
    #-----------------------------------------------------------------------------------------
    # Schools and students
    #-----------------------------------------------------------------------------------------
    
    # We have 300 students
    n_schools<-10
    n_students <- 300
    students <- 1:n_students
    
    # The come from 10 schools
    
    schools <- sample(1:n_schools, n_students, replace = T)
    
    # Each student makes 3 exams and the results are:
    
    n_exams <- 3
    
    mu <- 5
    school_effect <- rnorm(n_schools, 0, 0.5)
    student_effect <- rnorm(n_students, 0, 2)
    
    exams <- ldply(students, function(id) 
        data.frame(student = id, school = schools[id], exam = 1:n_exams, 
                   marks = student_effect[id] + school_effect[schools[id]] + rnorm(n_exams, 0, 1)))
    
    stan_data = list(
        n_obs = nrow(exams),
        n_students = n_students,
        n_schools = n_schools,
        students = exams$student,
        schools  = exams$school,
        marks    = exams$marks
    )
    
    fit_alt <- stan(file = "03_anova_stanSchool.stan",
                    data = stan_data,
                    iter = 10000, warmup = 5000, 
                    chains = 1, thin = 10)
    

# Exercise: set a school with only 1 student; give extremely high (or low) marks to such student. What would 
#   happen with the school? See how things change if you use a more strict prior on schools/students.






