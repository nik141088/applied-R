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
  a = 1;
  b = 1;
  for(i in 3:n) {
    f = a + b;
    a = b;
    b = f;
  }
  return(f);
}


# fib best -- fastest! O(1) time
phi = (1 + sqrt(5))/2;
phi_hat = (1 - sqrt(5))/2;
fib3 = function(n) {
  return( (phi^n - phi_hat^n)/sqrt(5) );
}

count_1 = 1:25;
time_1 = rep(NA, length(count_1));
for(i in 1:length(count_1)) {
  start = Sys.time();
  a = fib(i);
  end = Sys.time();
  time_1[i] = end - start;
}
plot(time_1, col = "blue", lwd = 2, type = "l",
     main = "Fibonacci-1 running time");
plot(log(time_1), col = "blue", lwd = 2, type = "l",
     main = "Log of Fibonacci-1 running time");


count_2 = seq(10, 1e4, 10);
time_2 = rep(NA, length(count_2));
for(i in 1:length(count_2)) {
  start = Sys.time();
  a = sapply((i-4):(i+5), fib2);
  end = Sys.time();
  time_2[i] = end - start;
}
plot(time_2, col = "red", lwd = 2, type = "l",
     main = "Fibonacci - 2 running time");
plot(time_2/count_2, col = "red", lwd = 2, type = "l", 
     main = "Fibonacci-2 running time divided by n");


count_3 = seq(1, 1e3, 1);
time_3 = rep(NA, length(count_3));
for(i in 1:length(count_3)) {
  start = Sys.time();
  # a = sapply((i-499):(i+500), fib3);
  a = fib3(i);
  end = Sys.time();
  time_3[i] = end - start;
}
plot(time_3, col = "green", lwd = 2, type = "l",
     main = "Fibonacci - 3 running time");



