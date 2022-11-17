# Rolling joins example. X and Y are data.tables
set.seed(123);
X = data.table(comp_id = rep(1:5, 2),
               fiscal_end = seq(as.Date("2010-02-01"), as.Date("2010-11-01"), by = "month") - 1,
               earn = rnorm(10, 1e3, 1e2));

Y = data.table(comp_id = rep(1:5, 2),
               report_date = X$fiscal_end + sample(-5:5, 10),
               price = rnorm(10, 1e2, 1));


X[Y, on = "comp_id"]; # this is simple join. Look-up comp_id colum of Y in X. It is exactly same as merge(X, Y, all.y = T, by = "comp_id").

# There is another way to do join (or lookup) by setting keys.
setkey(X, comp_id);
setkey(Y, comp_id);

X[Y]; # this is same as before.

# Let's change the keys to two vars
setkey(X, comp_id, fiscal_end);
setkey(Y, comp_id, report_date);


# Now X[Y] tries to look-up X using Y. The look-up happens using the keys, i.e. try to look up X[, .(comp_id, fiscal_end)] using Y[, .(comp_id, report_date)]. This is similar to merge(X, Y, all.y = T, by.x = c("comp_id", "fiscal_end"), by.y = c("comp_id", "report_date")). Note that in X[Y] the fiscal_end column actually has dates corresponding to report_date column of Y. We still get the entire Y because we are looking up Y in X. By definition it will report all columns of Y and any matching entries of X's columns.

X[Y, roll = Inf]; # this will lookup Y in X as before with a caveat. The last join column (the second key here) of Y (report_date) is rolled back to "Inf" until a match with the last join column of X (fiscal_end) is achieved. Thus instead of requiring that both keys match exactly, roll = Inf requires that the first key (comp_id) is matched exactly and the second key (report_end) is locf until a match is found with the second key of X (fiscal_end).

X[Y, roll = 5]; # This is same as above with the only difference that Y's second key is rolled only 5 places to see if a match is found. Note that X[Y, roll = Inf] have some un-intended matches where it keeps searching way back. X[Y, roll = 5] limits this search limit to 5 places.

X[Y, roll = -Inf]; # This is the exact opposite of the rolling direction of X[Y, roll = Inf]. Here Y's second key is rolled forward upto "Inf" places to look for a match.

X[Y, roll = "nearest"]; # This is slightly different in the sense that it tries to match Y's second_key to the nearest second key of X. The search is done on both directions.






