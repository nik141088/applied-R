setwd("C:/Users/nikhi/Dropbox/coursework/Applied R/");
library(data.table);

dt = fread("long_form_returns.csv");

# Question: create month end dates from Jan 2018 to Dec 2020

airquality = as.data.table(datasets::airquality);
iris = as.data.table(datasets::iris);





if(condition_1) {
  # execute your code
} else if(condition_2) {
  # execute your code
} else if(condition_3) {
  # execute your code
} else {
  
}



for(i in c("a", "b", "c")) {
  print(i)
}


x = rnorm(100, mean = 1, sd = 2);

# find mean and then square it and subtract 25% of standard devaition times mean
my_fun = function(x, fraction = 0.25) {
  m = mean(x);
  s = sd(x);
  ans = m^2 - fraction*s*m;
  return(ans);
}

my_fun(x, 0.5)













df1 = data.frame(cust_id = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)));
df2 = data.frame(cust_id = c(2, 4, 7), State = c(rep("Alabama", 2), rep("Ohio", 1)));
dt1 = as.data.table(df1); dt2 = as.data.table(df2);























