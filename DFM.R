# data (Belgian exports and imports to/from Germany)
x_DE <- rjd3toolkit::Exports$Germany
x_FR <- ts(c(rjd3toolkit::Exports$France, 3000), start = c(1995,1), frequency = 12)
x_NL <- ts(c(rjd3toolkit::Exports$Netherlands, 2350), start = c(1995,1), frequency = 12)
x_ES <- ts(c(rjd3toolkit::Exports$Spain, 450), start = c(1995,1), frequency = 12)

# pre-treatment
# (make series stationary -> in this case: SA + difflog for all variables)
jws <- rjd3workspace::jws_open(file = "Workspace JMS 2025.xml")
rws <- rjd3workspace::read_workspace(jws, compute = TRUE)

## X_DE
sa_spec_x_DE <- rws$processing$`SAProcessing-1`$X_DE$estimationSpec |> rjd3tramoseats::tramoseats_refresh(policy = "FreeParameters")
sa_rslt_x_DE <- rjd3tramoseats::tramoseats_fast(x_DE, sa_spec_x_DE, context = rws$context)
x_DE_sa <- sa_rslt_x_DE$final$sa$data
x_DE_tr <- diff(log(x_DE_sa), 1)

## X_FR
sa_spec_x_FR <- rws$processing$`SAProcessing-1`$X_FR$estimationSpec |> rjd3tramoseats::tramoseats_refresh(policy = "FreeParameters")
sa_rslt_x_FR <- rjd3tramoseats::tramoseats_fast(x_FR, sa_spec_x_FR, context = rws$context)
x_FR_sa <- sa_rslt_x_FR$final$sa$data
x_FR_tr <- diff(log(x_FR_sa), 1)

## X_NL
sa_spec_x_NL <- rws$processing$`SAProcessing-1`$X_NL$estimationSpec |> rjd3tramoseats::tramoseats_refresh(policy = "FreeParameters")
sa_rslt_x_NL <- rjd3tramoseats::tramoseats_fast(x_NL, sa_spec_x_NL, context = rws$context)
x_NL_sa <- sa_rslt_x_NL$final$sa$data
x_NL_tr <- diff(log(x_NL_sa), 1)

## X_ES
sa_spec_x_ES <- rws$processing$`SAProcessing-1`$X_ES$estimationSpec |> rjd3tramoseats::tramoseats_refresh(policy = "FreeParameters")
sa_rslt_x_ES <- rjd3tramoseats::tramoseats_fast(x_ES, sa_spec_x_ES, context = rws$context)
x_ES_sa <- sa_rslt_x_ES$final$sa$data
x_ES_tr <- diff(log(x_ES_sa), 1)

data <- cbind(x_DE_tr, x_FR_tr, x_NL_tr, x_ES_tr)

# remove outlier periods from data if necessary (typically crisis figures)
outl_from <- c(2008, 9)
outl_from_dec <- outl_from[1] + (outl_from[2] - 1) / 12
outl_from_pos <- which(abs(time(data) - outl_from_dec) < 1e-4)
outl_to <- c(2009, 3)
outl_to_dec <- outl_to[1] + (outl_to[2] - 1) / 12
outl_to_pos <- which(abs(time(data) - outl_to_dec) < 1e-4)

data_to_use <- data
data_to_use[outl_from_pos:outl_to_pos,] <- NA

# model & forecasts
nfactors <- 1
nlags <- 1
factors_type <- c("M", "M", "M", "M") # note: if business survey indicator (cfr. slides), 'YoY' (or 'M')
factors_loading <- matrix(data = TRUE, ncol(data_to_use), nfactors)

dfm <- rjd3nowcasting::create_model(nfactors,
                                    nlags,
                                    factors_type,
                                    factors_loading,
                                    var_init = "Unconditional")
est <- rjd3nowcasting::estimate_ml(dfm, data_to_use)

dfm_fcasts <- rjd3nowcasting::get_forecasts(est, n_fcst = 1)
#dfm_estimate <- rjd3nowcasting::get_results(est)

x_DE_tr_fcast <- dfm_fcasts$forecasts_only[1,1]
x_DE_sa_fcast <- x_DE_sa[length(x_DE_sa)] * exp(x_DE_tr_fcast)

(x_DE_fcast <- ifelse(sa_rslt_x_DE$preprocessing$description$log,
                   x_DE_sa_fcast * head(sa_rslt_x_DE$final$s$fcasts, 1),
                   x_DE_sa_fcast + head(sa_rslt_x_DE$final$s$fcasts, 1)))
