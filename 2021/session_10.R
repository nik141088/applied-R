x = 1:100;

y = seq(100, 110, length.out = 100)

z = seq(10, 1, length.out = 100);


plot(x, y, type = "l", col = "blue")

par(new = TRUE);
plot(x, z, type = "l", col = "red", xlab = NA, ylab = NA, axes = F);
axis(4); # makes axis on RHS
mtext(side = 4, line = 3, "z"); # RHS text









x = seq(1, 10, length.out = 1e3);
y = sin(x);
z = sqrt(x);

org_mfrow = par("mfrow");
par(mfrow = c(2,2));

plot(x, y, type = "l", col = "blue", lwd = 2, xlab = "time", ylab = "y");
legend("top", legend = c("sin(x)"), lty = 1, col = c("blue"), lwd = 2);

plot(x, z, type = "l", col = "red", lwd = 2, xlab = "time", ylab = "z");
legend("top", legend = c("sqrt(x)"), lty = 1, col = c("red"), lwd = 2);

par(mfrow = org_mfrow);










n = 1000;
x = rnorm(n, 0, 1);
u = rnorm(n, 0, 0.5);
y = 2*x + u;

# plot(x, y)

f = lfe::felm(y ~ x);
summary(f)

abline(f$coefficients, col = "red")

abline(v = -4:3, col = "blue")
abline(h = seq(-8, 6, 2), col = "orange", lty = 2)

legend("topright", c("hello"), col = "red", bg = NULL)








