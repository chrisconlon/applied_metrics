### Loading Packages
#always
library(tidyverse)
# for loading data
library(tidyquant)
# for cleaning up time series
library(timetk)
library(broom)
library(sweep)
library(forecast)
# for AR regression
library(dynlm)
# for testing
library(tseries)
library(lmtest)


# Load and plot the GDP Data
gdp<-tq_get("A191RL1Q225SBEA", get  = "economic.data",
         from='1950-01-1',to='2018-12-31')
tail(gdp)
ggplot(gdp, aes(date, price)) + geom_line() +
    scale_x_date() + ylab("Annualized GDP Growth") + xlab("")


# Compute the autocorrelation function and plot it
result<-tidy(acf(gdp$price))
ggplot(result, aes(x=lag, y=acf)) +
         geom_bar(stat='identity', width=0.1) + theme_bw()


# Run various AR(p) models
ar1<-ar(gdp$price,order=1)
ar2<-ar(gdp$price,order=2)

# automatic lag selection
ar(gdp$price)


# Read in the Steel Data
dt<-read.csv("steel.csv")
# convert to time series
dt2<-ts(dt)

# Run ADL(3,3) Regression
summary(dynlm(output~L(output,1:3)+L(hours,1:3), data=dt2))

# do some granger tests
grangertest(output~ hours, order=3,data=dt2)
grangertest(hours~ output, order=3,data=dt2)


# Plot and generate a random walk
tibble(x = 1:1000, y = cumsum(rnorm(1000, mean = 0))) %>%
  ggplot(aes(x=x,y=y))+
  geom_point()+
  geom_line()


### Dickey-Fuller Test
gdp2<-ts(gdp$price)
# run the regression
tidy(dynlm(d(gdp2)~L(gdp2,1)+L(d(gdp2),2:4)))
# skip to the test
adf.test(gdp2, k=3)

# ARIMA
auto.arima(gdp2)


''' Step 0: Load National Alcohol Expenditure Data from FRED
# This comes from https://cran.r-project.org/web/packages/sweep/vignettes/SW00_Introduction_to_sweep.html
'''

## Alcohol Data
alcohol_sales_tbl <- tq_get("S4248SM144NCEN", 
                            get  = "economic.data", 
                            from = "2007-01-01",to   = "2018-12-31")


# Plot with Exponential Moving Average
alcohol_sales_tbl %>%
    ggplot(aes(x = date, y = price)) +
    geom_line(size = 1, color = palette_light()[[1]]) +
    geom_smooth(method = "loess") +
    labs(title = "US Alcohol Sales: Monthly", x = "", y = "Millions") +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_tq()


'''Step 1: Coerce to a ts object class

The forecast package uses the ts data structure, which is quite a bit different than tibbles that we are currently using. 
Fortunately, it’s easy to get to the correct structure with tk_ts() from the timetk package. 
The start and freq variables are required for the regularized time series (ts) class, and these specify how to treat the time series. 
For monthly, the frequency should be specified as 12. This results in a nice calendar view. The silent = TRUE tells the tk_ts()
function to skip the warning notifying us that the “date” column is being dropped. Non-numeric columns must be dropped for ts
class, which is matrix based and a homogeneous data class.'''

alcohol_sales_ts <- tk_ts(alcohol_sales_tbl, start = 2007, freq = 12, silent = TRUE)
alcohol_sales_ts


'''
Step 2: Modeling a time series

The modeling workflow takes a time series object and applies a model. Nothing new here: we’ll simply use the ets() function
 from the forecast package to get an Exponential Smoothing ETS (Error, Trend, Seasonal) model.
'''

fit_ets <- ets(alcohol_sales_ts)
# or
fit_ets <- alcohol_sales_ts %>%
    ets()

'''sw_tidy

sw_tidy() returns the model parameters.

'''

'''sw_glance

sw_glance() returns the model quality parameters.

'''

'''sw_augment

sw_augment() returns the actual, fitted and residual values.'''

augment_fit_ets <- sw_augment(fit_ets)
augment_fit_ets


# Make the plot of the smoothed fit
# note: yearmon is the time series index
augment_fit_ets %>%
    ggplot(aes(x = index, y = .resid)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_smooth(method = "loess") +
    scale_x_yearmon(n = 10) +
    labs(title = "US Alcohol Sales: ETS Residuals", x = "") + 
    theme_tq()
'''
sw_tidy_decomp

sw_tidy_decomp() returns the time series decomposition (trend, seasonal, residual) of the ETS model.

'''
decomp_fit_ets <- sw_tidy_decomp(fit_ets)
decomp_fit_ets 

decomp_fit_ets %>%
    gather(key = key, value = value, -index) %>%
    mutate(key = forcats::as_factor(key)) %>%
    ggplot(aes(x = index, y = value, group = key)) +
    geom_line(color = palette_light()[[2]]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    facet_wrap(~ key, scales = "free_y") +
    scale_x_yearmon(n = 10) +
    labs(title = "US Alcohol Sales: ETS Decomposition", x = "") + 
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Auto ARIMA (2,10)
fit<-auto.arima(alcohol_sales_ts)



