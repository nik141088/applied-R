setwd("C:/Users/nikhi/Dropbox/coursework/Applied R");
library(data.table);

dt = fread("long_form_returns.csv", na.strings = "");
dt[, Date := as.yearmon(Date)];

# dt has 4 columns: cusip, Date, retadj and industry

# Suppose we want each firm in a separate column. The below will accomplish that!
# write Date and industry (the coumns for which we need just one column) on left hand side of ~
# write cusip (the colum we need in wide form) on right hand side of ~
# the value in each of new columns goes into value.var
# If there are multiple values, we can also specify a aggregator function
wide.ret = dcast(dt, Date + industry ~ cusip, value.var = "retadj");

# original data from wide.ret
# To generate long form data from wide form, we need to identify the existing long form variables (Date and industry) in id.vars, the list of wide form variables (all companies: 3:N) in measure.vars. Optionally, you can specify the name of company column (cusip) and the value column (retadj).
dt.org = melt(wide.ret, id.vars = c("Date", "industry"), measure.vars = 3:ncol(wide.ret), variable.name = "cusip", variable.factor = F, value.name = "retadj", na.rm = T);




# Next suppose we want firms by Date and industry, i.e. we want to aggregate all firms in an industry
wide.ret = dcast(dt, Date ~ industry, value.var = "retadj", fun.aggregate = mean, na.rm = T);

# original data from wide.ret. Note that since we have done aggregation in the step above, it is impossible to get the original data back. All we can get back is industry avergaed returns
dt.org = melt(wide.ret, id = c("Date"), measure.vars = 2:ncol(wide.ret), variable.name = "industry", variable.factor = F, value.name = "ind_adj_retadj", na.rm = T);




# Now lets widen on the Date var
wide.ret = dcast(dt, cusip ~ Date, value.var = "retadj");

# original data from wide.ret. We do not get industry back here since we did not stored it in wide.ret
dt.org = melt(wide.ret, id = c("cusip"), measure.vars = 2:ncol(wide.ret), variable.name = "Date", variable.factor = F, value.name = "retadj", na.rm = T);



