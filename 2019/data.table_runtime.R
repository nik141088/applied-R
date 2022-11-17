setwd("C:/Users/nikhil/Downloads/");

# comparing fread and read.csv
data = as.data.table( MASS::mvrnorm(1e5, rep(0, 10), diag(10)) ); # 100k rows of 10 columns

start = Sys.time();
fwrite(data, "delete_this.csv");
end  = Sys.time();
cat("It took ", end - start, " seconds to complete using fwrite\n");

file.remove("delete_this.csv");
start = Sys.time();
write.csv(data, "delete_this.csv");
end  = Sys.time();
cat("It took ", end - start, " seconds to complete using write.csv\n");

start = Sys.time();
dt = fread("delete_this.csv");
end  = Sys.time();
cat("It took ", end - start, " seconds to read using fread\n");

remove(dt);
start = Sys.time();
dt = read.csv("delete_this.csv");
end  = Sys.time();
cat("It took ", end - start, " seconds to read using read.csv\n");







# cumprod using cumsum
my_cumprod = function(x) {
  exp( cumsum( log(x) ) );
}
## Do you see any issues with this defintion?



















# load the nasa dataset
library(tidyverse);
library(dplyr);
library(data.table);
install.packages("stargazer");
require(stargazer);

ns = as.data.table(dplyr::nasa);
ns_DF = as.data.frame(ns);

# Try printing ns and ns_DF and see which one looks more easy to visualize

ns[lat > 0]; # subsetting rows

ns[, ozone]; # select one column
ns[, .(ozone)];
ns[, .(temperature, ozone)];
# is ozone level relatred to temperature?
ns[, .(temperature, ozone)] %>% cor;

# correlation analysis
ns[, .(lat, long, cloudhigh, cloudlow, cloudmid, ozone, pressure, surftemp, temperature)] %>% cor;
ns[, -c("month", "year")] %>% cor; # different way to do the same for the above
ns %>% select(lat, long, cloudhigh:temperature) %>% cor; # different way to do the same for the above

ns[, .(lat, long, cloudhigh, cloudlow, cloudmid, ozone, pressure, surftemp, temperature)] %>% cor(use = "pairwise.complete.obs");
ns[, .(lat, long, cloudhigh, cloudlow, cloudmid, ozone, pressure, surftemp, temperature)] %>% cor(use = "pairwise.complete.obs") %>% print(digits = 2);

# making an actual table out of it
ns[, .(lat, long, cloudhigh, cloudlow, cloudmid, ozone, pressure, surftemp, temperature)] %>% cor(use = "pairwise.complete.obs") %>% stargazer::stargazer(type = "text");
ns[, .(lat, long, cloudhigh, cloudlow, cloudmid, ozone, pressure, surftemp, temperature)] %>% cor(use = "pairwise.complete.obs") %>% stargazer::stargazer(type = "html", out = "cor.html");


ns[lat > 0 & month == 6, .(year, month, temperature)]
ns[temperature %between% c(299, 301)];


# Summarizing
ns[year == 1997, .N]; # num of observation with a row filter
ns[year == 1997] %>% nrow; # same but less efficient
stargazer::stargazer(ns, type = "text", summary = T); # summary table

# Aggregate
ns[, .(.N, sum_temp = sum(temperature), mean_pres = mean(pressure))];

# Grouping and summarizing/aggregating
ns[, .N, by = year]
ns[, .N, by = month];

ns[, .(mean(temperature)), by = year];
ns[lat > 0, .(mean(temperature), mean(pressure), .N), by = year];
ns[, .(mean(temperature)), by = .(odd_mon = month %% 2)];


# special symbols: .N, .I, .SD
ns[, lapply(.SD, mean), by = year];
ns[, c(num_obs = .N, lapply(.SD, mean, na.rm = T)), by = year]

# summarize only few columns
ns[, c(num_obs = .N, lapply(.SD, mean, na.rm = T)), .SDcols = cloudhigh:temperature, by = year]
ns[, c(num_obs = .N, lapply(.SD, mean, na.rm = T)), .SDcols = c(1,2,5:11), by = year]; # diff way of specifying which columns

ns[month %in% c(11,12,1,2), c(num_obs = .N, lapply(.SD, mean, na.rm = T)), .SDcols = cloudhigh:temperature, by = year]; # winter months



# Adding/updating a new column
ns[, year_mon := year + (month-1)/12];

# Removing column
ns[, year := NULL];
ns[, month := NULL];

# Add back year and month
ns[, year := as.integer(year_mon)];
ns[, month := 12*(year_mon - year) + 1]; # this is exactly the reverse of how I created year_mon

# conditional update
ns[lat > 0, year := year*year];
ns[lat > 0, year := year %>% sqrt %>% as.integer]; # revert it back

# Update/create vars by group
ns[, year_mon_lag := dplyr::lag(year_mon, 1), by = .(lat, long)];
setorder(ns, lat, long, year_mon);

ns[, grp_id := .GRP, by = .(lat, long)];

# first and last entry of each group
ns[, .SD[c(1, .N)], by = .(lat, long)];
ns[, .SD[c(1, .N)], by = grp_id]; # same





# Now that we have explored the dataset what kind of questions one may ask?
ns[, mean(temperature), by = year_mon] %>% plot(type = "l");
ns[, mean(ozone), by = year_mon] %>% plot(type = "l");

# does temp vary by lat/long?
ns[, mean(temperature), by = lat] %>% plot(type = "l");
ns[, mean(temperature), by = long] %>% plot(type = "l");

# what about pressure?
ns[, mean(pressure), by = lat] %>% plot(type = "l");
ns[, mean(pressure), by = long] %>% plot(type = "l");


# What should happens to pressure when temperature increases?
ns[, mean(pressure), by = temperature] %>% plot(type = "p");


# Lets try to predict ozone levels using other vars
lm(ozone ~ year_mon + temperature + lat + long, data = ns) %>% summary;


# Finally let's make a regression table
f = vector("list", 5);
f[[1]] = lm(ozone ~ year_mon + temperature, data = ns);
f[[2]] = lm(ozone ~ year_mon + temperature + lat + long, data = ns);
f[[3]] = lm(ozone ~ year_mon + pressure + lat + long, data = ns);
f[[4]] = lm(ozone ~ year_mon + temperature + lat + long + temperature*lat + temperature*long, data = ns);
f[[5]] = lm(ozone ~ year_mon + temperature + lat + long + cloudmid + temperature*lat + temperature*long, data = ns);


stargazer::stargazer(f, type = "text");

stargazer::stargazer(f, type = "html", out = "regression.html");




