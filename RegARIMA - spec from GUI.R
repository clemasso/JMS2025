################################################################################
# Hybrid GUI/R approach for RegARIMA model (TRAMO)
################################################################################

# data (Belgian exports and imports to/from Germany)
y <- rjd3toolkit::Exports$Germany
x <- ts(c(rjd3toolkit::Imports$Germany, 2300), start = c(1995,1), frequency = 12)

# load workspace

jws <- rjd3workspace::jws_open(file = "Workspace JMS 2025.xml")
rws <- rjd3workspace::read_workspace(jws, compute = TRUE)

# forecasts

## 1. without explanatory variables

spec_to_use <- rws$processing$`SAProcessing-1`$X_DE$estimationSpec$tramo |>
    rjd3tramoseats::tramo_refresh(policy = "FreeParameters")
ctxt <- rws$context

rjd3tramoseats::tramo_forecast(y, spec = spec_to_use, nf = 1, context = ctxt)
# summary(rjd3tramoseats::tramo_fast(y, spec = spec_to_use, context = ctxt))

## 2. with explanatory variables (regARIMA)

spec_to_use <- rws$processing$`SAProcessing-1`$X_DE_predictor$estimationSpec$tramo |>
    rjd3tramoseats::tramo_refresh(policy = "FreeParameters")
ctxt <- rws$context

rjd3tramoseats::tramo_forecast(y, spec = spec_to_use, nf = 1, context = ctxt)
# summary(rjd3tramoseats::tramo_fast(y, spec = spec_to_use, context = ctxt))
