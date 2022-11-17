# nasa dataset
library(dplyr);
ns = dplyr::nasa;
ns = as.data.frame(ns);

# relation between temperature and number of reading taken
ns %>% filter(!is.na(cloudlow)) %>% group_by(temperature) %>% summarise(n = n()) %>% arrange(temperature) %>% as.data.frame() %>% plot();

# locations with less than average number of readings
ns %>% filter(!is.na(cloudlow)) %>% group_by(lat, long) %>% summarise(n = n()) %>% arrange(n) %>% as.data.frame() %>% filter(n < mean(n));

# number of readings and % of cloudmid, cloudlow and cloudhigh
ns %>% filter(!is.na(cloudlow)) %>% group_by(cloudmid) %>% summarise(n = n()) %>% arrange(cloudmid) %>% as.data.frame() %>% plot;
ns %>% filter(!is.na(cloudlow)) %>% group_by(cloudlow) %>% summarise(n = n()) %>% arrange(cloudlow) %>% as.data.frame() %>% plot;
ns %>% filter(!is.na(cloudlow)) %>% group_by(cloudhigh) %>% summarise(n = n()) %>% arrange(cloudhigh) %>% as.data.frame() %>% plot;


# does pressure explain temperature? Revise class 12th chemistry: Gay-Lussac law
plot(ns$pressure, ns$surftemp);
# looks like the variability of pressure also increases with temperature

# Lets also look at grouped data to be used later
grouped_ns = ns %>% filter(!is.na(cloudlow)) %>% group_by(pressure) %>% summarise(avg = mean(surftemp), var = sd(surftemp), n = n()) %>% filter(!is.nan(var)) %>% as.data.frame();

fit = lm(data = ns, surftemp ~ pressure);
abline(fit$coefficients, col = "blue", lwd = 2);

# assign temperature variability to each pressure value
temp_var = grouped_ns[,c("pressure", "var")];

# merge with nasa data
colnames(temp_var)[2] = "temp_var";
ns = left_join(ns, temp_var);

# Now run the regression with temp_var as well
fit = lm(data = ns, surftemp ~ pressure*temp_var);
coef = tapply(fit$coefficients, c(1,2,1,2), sum);
abline(coef, col = "red", lwd = 2);


# Lets just see the results from only the grouped_ns data
colnames(grouped_ns)[2:3] = c("temp_avg", "temp_var");
plot(grouped_ns$pressure, grouped_ns$temp_avg);

fit = lm(data = grouped_ns, temp_avg ~ pressure);
abline(fit$coefficients, col = "blue", lwd = 2);

fit = lm(data = grouped_ns, temp_avg ~ pressure*temp_var);
coef = tapply(fit$coefficients, c(1,2,1,2), sum);
abline(coef, col = "red", lwd = 2);

# there seems to be an abrupt change at pressure levels of around 760
grouped_ns$pressure_760_plus = 0;
grouped_ns[grouped_ns$pressure > 760, "pressure_760_plus"] = 1;

fit = lm(data = grouped_ns, temp_avg ~ pressure*pressure_760_plus);
coef = tapply(fit$coefficients, c(1,2,1,2), sum);
abline(coef, col = "green", lwd = 2, lty = 2);

