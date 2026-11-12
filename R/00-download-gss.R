# 00-download-gss.R
# Fetch the 2002 GSS directly from NORC via gssr and cache the needed variables.

library(gssr)
library(dplyr)
library(here)

# Variables used by the original do-files (dataproc.do)
gss_vars <- c(
  "learnjob", "yearsjob",
  "seemovie", "popmusic", "readfict", "drama", "dance", "visitart", "gomusic",
  "educ", "degree", "age", "sex", "racecen1", "wrkstat", "prestg80",
  "marital", "numcntct", "numprobs", "paeduc", "maeduc", "padeg", "madeg"
)

gss2002 <- gss_get_yr(2002)

# Keep only what the analysis needs plus respondent id
analysis_vars <- intersect(c("id", gss_vars), names(gss2002))
missing_vars <- setdiff(gss_vars, names(gss2002))
if (length(missing_vars) > 0) {
  stop("Variables not found in the 2002 GSS file: ",
       paste(missing_vars, collapse = ", "))
}

gss2002_extract <- gss2002 |> select(all_of(analysis_vars))

dir.create(here("data"), showWarnings = FALSE)
saveRDS(gss2002_extract, here("data", "gss2002.rds"))

cat("Saved", nrow(gss2002_extract), "rows,", ncol(gss2002_extract),
    "variables to data/gss2002.rds\n")
