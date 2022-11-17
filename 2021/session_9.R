setwd("C:/Users/nikhi/Dropbox/coursework/Applied R/");


plot_trans = function(x, new_x1 = 1, new_x2 = 100) {
  
  x1 = min(x);
  x2 = max(x);
  
  (x - x1) * ((new_x2 - new_x1)/(x2 - x1)) + new_x1;
  
}






nifty = fread("long_form_returns.csv");
nifty[,  Date := as.Date(Date, format = "%Y-%m-%d")];

setorder(nifty, cusip, Date);
nifty[, cum_ret := cumsum(retadj), by = cusip];

nifty[cusip == "00282410", .(Date, cum_ret)] %>% plot(type = "l");

dt = nifty[cusip == "00282410", .(Date, cum_ret)];

dt[, cum_ret_2 := plot_trans(cum_ret)];
plot(dt$Date, dt$cum_ret, type = "l");
plot(dt$Date, dt$cum_ret_2, type = "l");

plot(dt$Date, plot_trans(dt$cum_ret, 1, 10), type = "l");




# evolution of closing price over time
plot(nifty$Date, nifty$Close);

plot(nifty$Date, nifty$Close, type = "l");


nifty$Close2 = nifty$Close / 1000;

# color of the line
plot(nifty$Date, nifty$Close, type = "l", col = "blue", lwd = 2, lty = 1);
# lwd is for line width
# col is for color
# lty is for line-type (1,2,3,4)















comp = "00282410";
dt = nifty[cusip == comp, .(Date, cum_ret)];

plot(dt$Date, dt$cum_ret, type = "l", col = "blue", lwd = 2, lty = 1, xlab = "Time", ylab = "Nifty Price", cex.axis = 1.1, cex.lab = 0.9);
title(paste0("Time-series of ", comp));
legend("topleft", "cumulative return", col = "blue", lwd = 2, lty = 1, bty = "n");





dt = nifty[cusip %in% c("00282410", "07181310", "88579Y10"), .(cusip, Date, cum_ret)] %>% dcast(Date ~ cusip, value.var = "cum_ret");
dt = dt[1:50];

names(dt) = c("Date", "comp_1", "comp_2", "comp_3");



pdf("plot_1.pdf", width = 6, height = 4);

y_rng = dt[, .(comp_1, comp_2, comp_3)] %>% range(na.rm = T);

plot(dt$Date, dt$comp_1, type = "p", col = "blue", lwd = 1, lty = 1, xlab = "Time", ylab = "Cumulative Return", ylim = y_rng, pch = 1);

lines(dt$Date, dt$comp_2, type = "p", col = "red", lwd = 2, lty = 2, pch = 2);

lines(dt$Date, dt$comp_3, type = "p", col = "green", lwd = 3, lty = 3, pch = 3);

title(paste0("Time-series of two companies"));

legend("topleft", c("comp-1", "comp-2", "comp-3"), col = c("blue", "red", "green"), lwd = c(1,2,3), lty = c(1,2,3), pch = c(1,2,3), ncol = 3);

dev.off();






x = c(1:5, 5:1, 3:7);





dt2 = nifty[cusip %in% c("00282410", "07181310"), .(cusip, Date, retadj)] %>% dcast(Date ~ cusip, value.var = "retadj");











