# session - 4

for(i in 1:10) {
  cat("9 x ", i, " =  ", 9*i, "\n");
  if(i > 6) {
    break;
  }
}


i = 1;
while(i <= 10) {
  cat("9 x ", i, " =  ", 9*i, "\n");
  i = i + 1;
}



i = 1;
while(TRUE) {
  cat("9 x ", i, " =  ", 9*i, "\n");
  i = i + 1;
  if(i > 10) {
    break;
  }
}








for(i in 1:10) {
  if(i <= 3) {
    next;
  }
  cat("9 x ", i, " =  ", 9*i, "\n");
}











my_mean = function(x) {
  n = length(x);
  ans = sum(x) / n;
  return(ans);
}



my_var = function(x) {
  
}


# Sujith
my_var = function (x) {
  n = length(x);
  m = mean(x);
  ans = sum((x - m)^2) / (n-1);
  return(ans);
}






# Janani
my_var = function (x) { 
  n = length(x);
  return( (sum((x - mean(x))^2) /n ) );
}


# shiffana
variance = function(x){
  return ( mean([x-mean(x)]^2) )
}







x = 10;
y = 20;

if(!(x > 20)) {
  print("Go")
}





nifty = read.csv("C:/Users/nikhi/Downloads/data.csv");
nifty$Date = as.Date(nifty$Date, format = "%d-%b-%Y");

day = as.numeric( format(nifty[,1], format = "%d") );

idx_1 = which( nifty$Close > nifty$Open    & (day < 5)   );

idx_2 = which( !(nifty$Close <= nifty$Open    | (day >= 5)   ));




set.seed(123);
x = sample(1:100, 50, replace = F);
which(x < 30);
x[ which(x < 30) ]




names = c("AAPL", "AAPL", "TSLA", "MSFT", "AAPL", "GOOG");
which( names %in% c("AAPL", "MSFT") )






count = 101:200;
which( count %% 2 == 0 & count %% 3 == 0 & count %% 5 == 0 );

x = 1:10;
y = 6:15;


X = list(a = 1:10, b = rnorm(10, 0, 1), c = runif(10, 9, 91));






x = rnorm(100, 1, 2);

cor(x, x^1)
cor(x, x^2)
cor(x, x^3)



y = rep(NA, 20);
for(i in 1:20) {
  y[i] = cor(x, x^i);
}



y = sapply(1:20, function(i) cor(x, x^i))



install.packages("data.table");
library(data.table);

















