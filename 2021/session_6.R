setwd("C:/Users/nikhi/Dropbox/coursework/Applied R/");

dt = fread("long_form_returns.csv", na.strings = "");




wide.ret = dcast(dt, Date + industry ~ cusip, value.var = "retadj");


dt.org = melt(wide.ret,
              id.vars = c("Date", "industry"),
              measure.vars = 3:ncol(wide.ret),
              variable.name = "cusip", value.name = "retadj", variable.factor = F, na.rm = T);












date = seq.Date(as.Date("2018-01-01"), by = 1, length.out = 5);
comp = c("A", "B", "C", "D", "E");
all_pairs = merge(comp, date, by = NULL);
# sales data - 15 points set.seed(1);
idx = sample(1:nrow(all_pairs), 15, replace = F);
df1 = data.frame(comp = all_pairs$x[idx], date = all_pairs$y[idx],
                 sales = round(runif(15, min = 1e3, max = 1e5)));
# advertising data - 12 points
idx = sample(1:nrow(all_pairs), 12, replace = F);
df2 = data.frame(comp = all_pairs$x[idx], date = all_pairs$y[idx],
                 adv = round(runif(12, min = 1e2, max = 1e4)));

setDT(df1);
setDT(df2);


# anti join
setkey(df1, comp, date);
setkey(df2, comp, date);

# look-up df1 using df2: df1[df2]

setorder(df1, comp, date);
setorder(df2, comp, date);


df1[, date := day(date)];
df2[, date := day(date)];

setkey(df1, comp, date);
setkey(df2, comp, date);









