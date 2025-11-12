################################################################################
# Basic Structural Model (with predictor, outliers and variable TD)
################################################################################

# data (Belgian exports and imports to/from Germany)
y <- ts(c(rjd3toolkit::Exports$Germany, NA), start = c(1995,1), frequency = 12) # add fcast period
y_tr <- log(y)
x <- ts(c(rjd3toolkit::Imports$Germany, 2750), start = c(1995,1), frequency = 12)
x_tr <- log(x)

# build regressors

## trading days
cal_BE <- rjd3toolkit::national_calendar(list(
    rjd3toolkit::fixed_day(7, 21),
    rjd3toolkit::special_day("NEWYEAR"),
    rjd3toolkit::special_day("CHRISTMAS"),
    rjd3toolkit::special_day("MAYDAY"),
    rjd3toolkit::special_day("EASTERMONDAY"),
    rjd3toolkit::special_day("ASCENSION"),
    rjd3toolkit::special_day("WHITMONDAY"),
    rjd3toolkit::special_day("ASSUMPTION"),
    rjd3toolkit::special_day("ALLSAINTSDAY"),
    rjd3toolkit::special_day("ARMISTICE")))
reg_cal <- rjd3toolkit::calendar_td(cal_BE, s = y, groups = c(1, 1, 1, 1, 1, 2, 0), contrast = FALSE)

## outliers
outliers <- c("2008-09-01", "2008-10-01", "2008-11-01", "2008-12-01", "2009-01-01")
reg_outl <- NULL
for (d in outliers) {
    outl <- rjd3toolkit::ao_variable(12, s = y, date = as.Date(d))
    reg_outl <- cbind(reg_outl, outl)
}
colnames(reg_outl) <- paste0("ao", 1:length(outliers))


# model
bsm <- rjd3sts::model()
rjd3sts::add(bsm, rjd3sts::locallineartrend("ll"))
rjd3sts::add(bsm, rjd3sts::seasonal("s", 12, type="HarrisonStevens"))
rjd3sts::add(bsm, rjd3sts::reg(name = "x", x = x_tr, var = 0, fixed  = TRUE)) # fixed coef.
rjd3sts::add(bsm, rjd3sts::reg(name = "cal", x = reg_cal, var = 0.1, fixed = FALSE)) # variable coef.
rjd3sts::add(bsm, rjd3sts::reg(name = "outl", x = reg_outl, var = 0, fixed = TRUE)) # fixed coef.
rjd3sts::add(bsm, rjd3sts::noise("n"))
jrslts <- rjd3sts::estimate(bsm, data = y_tr, marginal = TRUE)
### rjd3toolkit::dictionary(jrslts) # available output
ss <- rjd3toolkit::result(jrslts, "ssf.smoothing.states")
vss <- rjd3toolkit::result(jrslts, "ssf.smoothing.vstates")
ss[nrow(ss),14] / sqrt(vss[nrow(ss),14])

# forecasts
regressors_last <- cbind(x_tr, reg_cal, reg_outl)[length(x_tr),]
coeff_last <- ss[nrow(ss), 14:(ncol(ss) - 1)]
y_tr_fcast <- ss[nrow(ss), 1] + ss[nrow(ss), 3] + crossprod(coeff_last, regressors_last)
(y_fcast <- exp(y_tr_fcast))


