# fib rudimentary -- very slow! O(2^n) time
fib = function(n) {
  if(n <= 2) {
    return(1);
  } else {
    return(fib(n-1) + fib(n-2));
  }
}

# fib better -- much faster! O(n) time
fib2 = function(n) {
  if(n <= 2) {
    return(1);
  }
  a = b = 1;
  for(i in 3:n) {
    f = a + b;
    a = b;
    b = f;
    # cat("a: ", a, " b: ", b, " f: ", f, "\n", sep = "");
  }
  return(f);
}


# fib best -- fastest! O(1) time
phi = (1 + sqrt(5))/2;
phi_hat = (1 - sqrt(5))/2;
fib3 = function(n) {
  return( (phi^n - phi_hat^n)/sqrt(5) );
}


count_1 = seq(10,30,1);
res_1 = time_1 = rep(NA, length(count_1));
cnt = 0;
for(i in count_1) {
  cnt = cnt + 1;
  start = Sys.time();
  res_1[cnt] = fib(i);
  end = Sys.time();
  time_1[cnt] = end - start;
  Sys.sleep(1e-3);
}
plot(time_1, col = "red", lwd = 2, type = "l", xlab = "Fib(i)",
     main = "Fibonacci-1 running time");
# plot(log(time_1), col = "red", lwd = 2, type = "l",
# main = "Log of Fibonacci-1 running time");



count_2 = seq(10, 1000, 10);
res_2 = time_2 = rep(NA, length(count_2));
cnt = 0;
for(i in count_2) {
  cnt = cnt + 1;
  start = Sys.time();
  res_2[cnt] = fib2(i);
  sapply(rep(i, 999), fib2);
  end = Sys.time();
  time_2[cnt] = end - start;
  Sys.sleep(1e-3);
}
time_2 = time_2 / 1000;
plot(time_2, col = "red", lwd = 2, type = "l", xlab = "Fib(i)",
     main = "Fibonacci - 2 running time");
# plot(time_2/count_2, col = "red", lwd = 2, type = "l", 
#      main = "Fibonacci-2 running time divided by n");



count_3 = seq(10, 1000, 10);
res_3 = time_3 = rep(NA, length(count_3));
cnt = 0;
for(i in count_3) {
  cnt = cnt + 1;
  start = Sys.time();
  res_3[cnt] = fib3(i);
  sapply(rep(i, 999), fib3);
  end = Sys.time();
  time_3[cnt] = end - start;
  Sys.sleep(1e-3);
}
time_3 = time_3 / 1000;
plot(time_3, col = "red", lwd = 2, type = "l", xlab = "Fib(i)",
     main = "Fibonacci - 3 running time");




# times for Fib-2 and Fib-3 together
plot(time_2, col = "red", lwd = 2, type = "l", xlab = "Fib(i)", ylim = range(time_2, time_3, 0),
     main = "Fibonacci-2 and Fibonacci-3 running times");
lines(time_3, col = "green", lwd = 2, type = "l");






