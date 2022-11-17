# Hello

s = rep(c("male", "female"), 5);

f = as.factor(s);


d = data.frame(name = c("a", "b"), weight = c(70, 75),		     height = c(1.78, 1.82));



nifty = read.csv("data.csv");
nifty$Date = as.Date(nifty$Date, format = "%d-%b-%Y");


if(nifty$Date < as.Date("2020-10-31")) {
  nifty$Close[26] = nifty$Close[26] * 10;
}

# else {
#   nifty$Close[26] = nifty$Close[26] * 20;
# }



a = read.table("clipboard");




for(i in 1:10) {
  cat("19 x ", i, " = ", 19*i, "\n");
}



n = nrow(nifty);


my_own_vec = c(1, 12, 63, 51, 56);
for(i in my_own_vec) {
  print(nifty$Date[i]);
}



for(i in 1:n) {
  nifty$Date[i] = nifty$Date[i] + 1;
}


nifty$Date = nifty$Date + 1;







nifty$vol = nifty$High - nifty$Low;

a = 100 * (nifty$vol[-1] - nifty$vol[-n]) / nifty$vol[-n];

nifty$growth = c(NA, a);









