data {
  int<lower=1> n_obs;      // number of observations
  int<lower=1> n_students;  // number of students
  int<lower=1> n_schools;  // number of schools
  
  int students[n_obs]; // student it
  int schools[n_obs]; // student it
  
  real marks[n_obs];   // exam results
}

parameters {
  vector[n_schools] school; 
  vector[n_students] student;
  real <lower = 0> sigma_exam;   // observation error
}

model {
  student ~ normal(0, 5);
  school  ~ normal(0, 5);
  
  for (i in 1:n_obs){
      marks[i] ~ normal(student[students[i]] + school[schools[i]], sigma_exam);
  }
}