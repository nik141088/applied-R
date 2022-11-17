ns = as.data.frame(nasa);

# average pressure for each latitude
ans = aggregate(pressure ~ lat, data = ns, FUN = mean);
plot(ans, type = "l");



# average pressure for each latitude
ans = aggregate(pressure ~ long, data = ns, FUN = mean);
plot(ans, type = "l");



# NA cloudlow enteries
idx = which(is.na(ns$cloudlow));
length(idx);
ns = ns[-idx,];



# level of ozone on lat, long
ans1 = aggregate(lat ~ ozone, data = ns, FUN = mean);
ans2 = aggregate(long ~ ozone, data = ns, FUN = mean);

y_lmt = range(ans1$lat, ans2$long);
plot(ans1, type = "l", col = "blue", lwd = 2, ylim = y_lmt, ylab = "Lat/Long");
lines(ans2, type = "l", col = "red", lwd = 2);

fit = lm(ozone ~ lat + long, data = ns);
summary(fit);



# temperature and surface temperature
ans = aggregate(temperature ~ surftemp, data = ns, FUN = mean);
plot(ans);
fit = lm(temperature ~ surftemp, data = ns);
abline(fit$coefficients, col = "red", lwd = 2);
summary(fit);



# has temperature increased over the years?
ans = aggregate(temperature ~ year, data = ns, FUN = mean);
ans;
ans1 = aggregate(temperature ~ year, data = ns, FUN = sd);
ans1;



# Does May shows least variability in both surftemp and temperature?
ans = aggregate(temperature ~ month, data = ns, FUN = sd);
ans;
ans[order(ans$temperature),];

ans1 = aggregate(surftemp ~ month, data = ns, FUN = sd);
ans1;
ans1[order(ans1$surftemp),];



# relation between surftemp and cloudmid!
ans = aggregate(surftemp ~ cloudmid, data = ns, FUN = mean);
plot(ans);

fit = lm(surftemp ~ cloudmid + I(cloudmid^2), data = ns);
summary(fit);
fit_val = cbind(1, ans$cloudmid, ans$cloudmid^2) %*% fit$coefficients;
lines(ans$cloudmid, fit_val, type = "l", col = "red", lwd = 2);

# what if I ran regression on aggregated values instead?
plot(ans);
fit = lm(surftemp ~ cloudmid + I(cloudmid^2), data = ans);
summary(fit);
lines(ans$cloudmid, fit$fitted.values, type = "l", col = "red", lwd = 2);



# fractional year analysis
ns$frac_yr = ns$year + (ns$month-1)/12;
ans = aggregate(surftemp ~ frac_yr, data = ns, FUN = mean);
plot(ans, type = "l");

