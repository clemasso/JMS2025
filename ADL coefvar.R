################################################################################
# ADL(1,0) with variable coefficient for predictor
################################################################################

# data (Belgian exports and imports to/from Germany)
y <- rjd3toolkit::Exports$Germany
x <- ts(c(rjd3toolkit::Imports$Germany, 2750), start = c(1995,1), frequency = 12)

# pre-treatment
# (make series stationary -> in this case: SA + difflog for both y and x)
jws <- rjd3workspace::jws_open(file = "Workspace JMS 2025.xml")
rws <- rjd3workspace::read_workspace(jws, compute = TRUE)

## y
sa_spec_y <- rws$processing$`SAProcessing-1`$X_DE$estimationSpec |>
    rjd3tramoseats::tramoseats_refresh(policy = "FreeParameters")
sa_rslt_y <- rjd3tramoseats::tramoseats_fast(y, sa_spec_y, context = rws$context)
y_sa <- sa_rslt_y$final$sa$data
y_tr <- diff(log(y_sa), 1)

## x
sa_spec_x <- rws$processing$`SAProcessing-1`$M_DE$estimationSpec |>
    rjd3tramoseats::tramoseats_refresh(policy = "FreeParameters")
sa_rslt_x <- rjd3tramoseats::tramoseats_fast(x, sa_spec_x, context = rws$context)
x_sa <- sa_rslt_x$final$sa$data
x_tr <- diff(log(x_sa), 1)

# define outliers
# in transformed data -> m-o-m growth (detection in GUI)
outliers_yx <- c("2008-09-01", "2008-10-01", "2008-11-01", "2008-12-01", "2009-01-01")
n_outl <- length(outliers_yx)

# build regression dataset
data_to_use <- cbind(y_tr, lag(y_tr, k = -1), x_tr) |>
    stats::window(start = c(1995, 3), end = c(2014, 11))
for (d in outliers_yx) {
    outl <- rjd3toolkit::ao_variable(12, s = data_to_use, date = as.Date(d))
    data_to_use <- cbind(data_to_use, outl)
}
colnames(data_to_use) <- c("y_tr", "Ly_tr", "x_tr", paste0("ao", 1:n_outl))

# model
jmodel <- rjd3sts::model()

## add predictor in the model (with variable coefficient)
rjd3sts::add(jmodel, rjd3sts::reg(name = "x_tr", x = data_to_use[,"x_tr"], var = 0.01, fixed  = FALSE))

## other regressors (with fixed coefficient)
rjd3sts::add(jmodel, rjd3sts::reg(name = "Ly_tr", x = data_to_use[,"Ly_tr"], var = 0, fixed = TRUE))

for(j in 1:n_outl){
    outl_name <- paste0("ao", j)
    rjd3sts::add(jmodel, rjd3sts::reg(name = outl_name, x = data_to_use[, outl_name], var = 0, fixed = TRUE))
}

rjd3sts::add(jmodel, rjd3sts::noise("noise", variance = 0.1, fixed = FALSE))

jrslts <- rjd3sts::estimate(jmodel, data = data_to_use[,"y_tr"], marginal = TRUE)

### rjd3toolkit::dictionary(jrslts) # available output
ss <- rjd3toolkit::result(jrslts, "ssf.smoothing.states")
vss <- rjd3toolkit::result(jrslts, "ssf.smoothing.vstates")
colnames(ss) <- colnames(vss) <- c("x_tr", "Ly_tr", paste0("ao", 1:n_outl), "noise")

# forecasts
n_obs <- nrow(data_to_use)
regressors_last <- c(data_to_use[n_obs, "x_tr"], data_to_use[n_obs, "Ly_tr"],
                     data_to_use[n_obs, grep("^ao", colnames(data_to_use))])
coeff_last <- ss[nrow(ss), -ncol(ss)]
y_tr_fcast <- crossprod(coeff_last, regressors_last)

y_sa_fcast <- y_sa[length(y_sa)] * exp(y_tr_fcast)

(y_fcast <- ifelse(sa_rslt_y$preprocessing$description$log,
                   y_sa_fcast * head(sa_rslt_y$final$s$fcasts, 1),
                   y_sa_fcast + head(sa_rslt_y$final$s$fcasts, 1)))
