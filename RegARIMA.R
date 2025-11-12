################################################################################
# RegARIMA model (TRAMO)
################################################################################

# data (Belgian exports and imports to/from Germany)
y <- rjd3toolkit::Exports$Germany
x <- ts(c(rjd3toolkit::Imports$Germany, 2750), start = c(1995,1), frequency = 12)

# belgian calendar
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

# define TRAMO specification
spec_init <- rjd3tramoseats::tramo_spec("tr0")
spec_to_use <- rjd3toolkit::set_automodel(spec_init, enable = FALSE) |>
    rjd3toolkit::set_transform(fun = "Auto", outliers = TRUE) |>
    rjd3toolkit::set_arima(
        p = 0,
        d = 1,
        q = 1,
        bp = 0,
        bd = 1,
        bq = 1
    ) |>
    rjd3toolkit::set_tradingdays(
        calendar.name = "cal1",
        test = "Joint_F",
        automatic = "Bic",
        pftd = 0.01,
        autoadjust = TRUE,
        leapyear = "LeapYear"
    ) |>
    rjd3toolkit::set_easter(
        enabled = FALSE
    ) |>
    rjd3toolkit::set_outlier(outliers.type = c("AO", "LS"))

# forecasts

## 1. without explanatory variables

### format BE calendar regressors to make them usable in tramo_forecasts() function
ctxt <- rjd3toolkit::modelling_context(calendars = list(cal1 = cal_BE))

rjd3tramoseats::tramo_forecast(y, spec = spec_to_use, nf = 1, context = ctxt)
# summary(rjd3tramoseats::tramo_fast(y, spec = spec_to_use, context = ctxt))


## 2. with explanatory variables (regARIMA)

spec_to_use <- rjd3toolkit::add_usrdefvar(spec_to_use,
                                          name = "reg1",
                                          regeffect = "Trend")

### format BE calendar and external regressors to make them usable in tramo_forecasts() function
ctxt <- rjd3toolkit::modelling_context(calendars = list(cal1 = cal_BE),
                                       variables = list(reg1 = x))

rjd3tramoseats::tramo_forecast(y, spec = spec_to_use, nf = 1, context = ctxt)
#summary(rjd3tramoseats::tramo_fast(y, spec = spec_to_use, context = ctxt))

