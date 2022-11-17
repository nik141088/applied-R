setwd("C:/Users/nikhi/Dropbox/coursework/Applied R/");

# CO2 data
datasets::co2; # atmospheric concentration of CO2 (in ppm)

dt = data.table(date = seq.Date(as.Date("1959-01-01"), as.Date("1997-12-01"), by = "month"),
                co2 =  datasets::co2 %>% as.vector);

# has atmospheric co2 increased over time?
dt[, plot(date, co2, type = "l", lwd = 2, col = "red")];

# There seems to be a monthly cycle in concentration
dt[, month := month(date)];
dt[, year := year(date)];

# remove month effects
fit = lm(co2 ~ factor(month), data = dt);
dt[, co2_adj := fit$residuals + fit$coefficients[1]];

# now plot the adjusted series. Note the smoothness!
dt[, plot(date, co2_adj, type = "l", lwd = 2, col = "red")];

# both adjusted and unadjusted together
dt[, plot(date, co2, type = "l", lwd = 2, col = "red", ylim = range(co2, co2_adj))];
dt[, lines(date, co2_adj, type = "l", lwd = 2, col = "blue")];




















# Load a series of datasets from gapminder.com

# CO2 per person
co2 = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/co2_emissions_tonnes_per_person.csv", header = T);
# have a look at the co2 data. It is in wide form
View(co2);
# This is hard to visualize and work. Long form data is much more useful
# wide to long form
co2 = co2 %>% melt(id = "country", variable.name = "year", value.name = "co2", variable.factor = F);
# have a look now
View(co2);
co2[, year := as.integer(year)];

# population data
pop = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/population_total.csv", header = T);
pop = pop %>% melt(id = "country", variable.name = "year", value.name = "population", variable.factor = F);
pop[, year := as.integer(year)];
pop[, population := population / 1e6]; # in millions

# child mortality
child = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/child_mortality_0_5_year_olds_dying_per_1000_born.csv", header = T);
child = child %>% melt(id = "country", variable.name = "year", value.name = "child.mortality", variable.factor = F);
child[, year := as.integer(year)];
child[, child.mortality := child.mortality / 10]; # in percentage

# babies per woman
babies = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/children_per_woman_total_fertility.csv", header = T);
babies = babies %>% melt(id = "country", variable.name = "year", value.name = "num_children", variable.factor = F);
babies[, year := as.integer(year)];

# income (PPP in US $) per person
income = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/income_per_person_gdppercapita_ppp_inflation_adjusted.csv", header = T);
income = income %>% melt(id = "country", variable.name = "year", value.name = "income", variable.factor = F);
income[, year := as.integer(year)];

# life expectancy
life = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/life_expectancy_years.csv", header = T);
life = life %>% melt(id = "country", variable.name = "year", value.name = "life.expectancy", variable.factor = F);
life[, year := as.integer(year)];

# surface area of countries
area = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/surface_area_sq_km.csv", header = T);
area = area %>% melt(id = "country", variable.name = "year", value.name = "area", variable.factor = F);
area[, year := as.integer(year)];
area[, area := area/1e6]; # million sq kms

# agricultural land (fraction of total surface area)
agri = fread("C:/Users/nikhi/Dropbox/coursework/Applied R/agricultural_land_percent_of_land_area.csv", header = T);
agri = agri %>% melt(id = "country", variable.name = "year", value.name = "agri.land", variable.factor = F);
agri[, year := as.integer(year)];






# Merge all datasets
final = merge(co2, pop, by = c("country", "year"), all = T);
final = merge(final, child, by = c("country", "year"), all = T);
final = merge(final, babies, by = c("country", "year"), all = T);
final = merge(final, income, by = c("country", "year"), all = T);
final = merge(final, life, by = c("country", "year"), all = T);
final = merge(final, area, by = c("country", "year"), all = T);
final = merge(final, agri, by = c("country", "year"), all = T);

# remove predictions (year > 2018)
final = final[year <= 2018];


# let's remove NAs for simplicity <-- this mayn't be a good thing to do in general!
final = final %>% na.omit;


# before moving onto analysis let's get some useful derived variables
final[, co2_overall := co2 * population];
final[, gdp_ppp := income*population];
final[, children_living := (100 - child.mortality)*num_children/100];
final[, personal_space := area / population];
final[, co2_per_km2 := co2_overall / area];
final[, co2_per_agri_km2 := co2_overall / (area * agri.land)];
final[, agri_space := area * agri.land / population];







# Let's seek some summaries to understand this data
final[, describe(.SD), .SDcols = -c("country", "year")];

# A more recent summary
final[year %in% 1990:2018, describe(.SD), .SDcols = -c("country", "year")];



# Avg trends by year for all variables? like how has life expectancy changed over years for the globe
final[, lapply(.SD, mean), .SDcols = -"country", by = year][order(year)];

# there are so many years. Lets have a decadal look at trends
final[, decade := floor(year/10)];
final[, decade := paste0(decade*10, "-", decade*10 + 9)];
final[, lapply(.SD, mean), .SDcols = -"country", by = decade][order(decade)];

# how about everything in a nice plot?
# the x-axis will be decade/year while the y-axis could be any nine of co2, population, child.mortality, num_children, income, life.expectancy, co2_overall, gdp_ppp or children_living
pdf("all_together.pdf", width = 15, height = 6);
org_mfrow = par("mfrow");
org_mar = par("mar");

par(mar = c(5, 5, 2, 2));
par(mfrow = c(3,5));
dt = final[, lapply(.SD, mean), .SDcols = -"country", by = decade][order(decade)];

for(i in 3:ncol(dt)) {
  plot(dt$year, dt[, i, with = F] %>% unlist, col = "red", lwd = 2, type = "b", xlab = "year/decade", ylab = dt[, i, with = F] %>% names, cex.axis = 1.5, cex.lab = 2);
}

par(mar = org_mar);
par(mfrow = org_mfrow);
dev.off();



# Now let's think about correlations. In a panel data, it makes sense to avg out the time-series effect of correlations. One way is to report time-series avg. of cross-sectional correlations
final[, -c("country", "year", "decade")] %>% COR; # overall correlations
final[, -c("country", "decade")] %>% COR(ts_avg = "year"); # yearly averages
final[, -c("country", "year")] %>% COR(ts_avg = "decade"); # decadal averages
final[, -c("decade", "year")] %>% COR(ts_avg = "country"); # averaging by country (even more interesting)

# highlight only the high correlations
dt = final[, -c("decade", "year")] %>% COR(ts_avg = "country");
idx = which(abs(dt) <= 0.75);
idx = union(idx, which(dt == 1));
dt[idx] = NA_real_;


# It seems that most of the effects are driven by time F.E. What if we removed that?
vars = c("co2", "population", "child.mortality", "num_children", "income", "life.expectancy", "area", "agri.land", "co2_overall", "gdp_ppp", "children_living", "personal_space", "co2_per_km2", "co2_per_agri_km2", "agri_space");

for(v in vars) {
  fit = lm(as.formula(paste0(v, " ~ factor(year)")), data = final);
  final[, eval(paste0(v, "_adj")) := fit$coefficients[1] + fit$residuals];
}




# now take correlations
dt = final %>% select(country, co2_adj:agri_space_adj) %>% COR(ts_avg = "country");
idx = which(abs(dt) <= 0.5);
idx = union(idx, which(dt == 1));
idx = union(idx, which(lower.tri(dt) == F));
dt[idx] = NA_real_;



# How about a huge plot w.r.t. each country?
dt = final[, -"decade"][, lapply(.SD, mean), .SDcols = co2:agri_space_adj, by = country];
N = nrow(dt);


var = "co2_adj";

pdf("all_countries.pdf", width = 18, height = 9);
org_mfrow = par("mfrow");
org_mar = par("mar");

val = dt[, var, with = F] %>% unlist;
lo = quantile(val, p = 0.1, na.rm = F)
hi = quantile(val, p = 0.9, na.rm = F)

# divide N into 3 equal plots
N_st = seq(1, N, ceiling(N/3));
N_end = c(N_st[-1] - 1, N);
N_len = N_end - N_st + 1;

par(mar = c(10, 5, 2, 2));
par(mfrow = c(3,1));

ylim = range(val);

for(i in 1:3) {
  
  idx = N_st[i]:N_end[i];
  
  plot(NULL, xaxt = "n", xlab = "", ylab = var, xlim = c(1, N_len[i]), ylim = ylim, cex.lab = 2);
  axis(1, at = 1:N_len[i], labels = dt$country[idx], las = 2, cex.axis = 1.5);
  lines(1:N_len[i], val[idx], type = "p", lwd = 3, col = "blue");
  # abline(h = lo, lwd = 1, col = "red");
  abline(h = hi, lwd = 1, col = "red");
  segments(1:N_len[i], min(val), 1:N_len[i], val[idx]);
  
}

par(mar = org_mar);
par(mfrow = org_mfrow);
dev.off();





# Now we can try some regression models
# What are the possible predictors of per person co2 emission?
f = vector("list", 10);

f[[1]] = lfe::felm(co2 ~ life.expectancy, data = final);
f[[2]] = lfe::felm(co2 ~ income, data = final);
f[[3]] = lfe::felm(co2 ~ agri.land, data = final);
f[[4]] = lfe::felm(co2 ~ personal_space, data = final);
f[[5]] = lfe::felm(co2 ~ life.expectancy + income, data = final);
f[[6]] = lfe::felm(co2 ~ life.expectancy + income + agri.land + personal_space, data = final);


# Try fixed effects
f = vector("list", 5);
f[[1]] = lfe::felm(co2 ~ life.expectancy + income + agri.land + personal_space, data = final);
f[[2]] = lfe::felm(co2 ~ life.expectancy + income + agri.land + personal_space | year, data = final);
f[[3]] = lfe::felm(co2 ~ life.expectancy + income + agri.land + personal_space | country, data = final);
f[[4]] = lfe::felm(co2 ~ life.expectancy + income + agri.land + personal_space | year + country, data = final);


# What about overall co2 emissions?
f = vector("list", 5);

f[[1]] = lfe::felm(co2_overall ~ population | year + country, data = final);
f[[2]] = lfe::felm(co2_overall ~ gdp_ppp | year + country, data = final);
f[[3]] = lfe::felm(co2_overall ~ area | year + country, data = final);
f[[4]] = lfe::felm(co2_overall ~ population + gdp_ppp + area | year + country, data = final);
f[[5]] = lfe::felm(co2_overall ~ population + gdp_ppp + area + life.expectancy + income | year + country, data = final);
f[[6]] = lfe::felm(co2_overall ~ population + gdp_ppp + area + life.expectancy + income + personal_space + agri_space | year + country, data = final);












