# 01-data-prep.R
# Port of dataproc.do: builds the analysis file for the 2002 GSS.

library(dplyr)
library(tidyr)
library(here)

d <- readRDS(here("data", "gss2002.rds")) |>
  # haven_labelled columns -> plain numeric so recodes and comparisons behave
  mutate(across(everything(), ~ {
    if ("haven_labelled" %in% class(.x)) as.numeric(unclass(.x)) else .x
  })) |>
  filter(!is.na(learnjob))

# --- Demographics / job variables -------------------------------------------
d <- d |>
  mutate(
    black   = as.numeric(racecen1 == 2),
    hisp    = as.numeric(racecen1 == 16),
    married = as.numeric(marital == 1),
    college = as.numeric(degree > 2),
    women   = as.numeric(sex == 2),
    agesq   = age^2 / 100,
    numcntct = ifelse(numcntct == 998, NA, numcntct),
    numprobs = ifelse(numprobs == 98, NA, numprobs),
    numwk   = numcntct - numprobs,
    deg2 = as.numeric(degree == 1),
    deg3 = as.numeric(degree == 2),
    deg4 = as.numeric(degree == 3),
    deg5 = as.numeric(degree == 4)
  )

# --- Recode the job-search-method variable (learnjob) ------------------------
# Stata: recode learnjob 1=. 2/3=1 4=2 5=3 6/8=4 9=5 10=6 11=7 98=., g(learnjob2)
learnjob_map <- c(`1` = NA, `2` = 1, `3` = 1, `4` = 2, `5` = 3,
                  `6` = 4, `7` = 4, `8` = 4, `9` = 5, `10` = 6, `11` = 7,
                  `98` = NA)
d$learnjob2 <- unname(learnjob_map[as.character(d$learnjob)])

lab_learnjob2 <- c("relatives", "close friend", "acquaintance",
                   "emp. service/school", "advertisement/sign",
                   "directly from employer", "called/asked myself")

d <- d |>
  mutate(
    acq  = as.numeric(learnjob2 == 3),
    boss = as.numeric(learnjob2 == 6)
  )

# Helper: port a Stata recode to a recoded numeric vector (values outside the
# map are carried through unchanged, as in Stata's recode)
stata_recode <- function(x, ...) {
  map <- c(...)
  hit <- as.character(x) %in% names(map)
  out <- rep(NA_real_, length(x))
  out[hit] <- unname(map[as.character(x[hit])])
  out[!hit] <- x[!hit]  # Stata recode: uncoded values pass through unchanged
  out
}

# --- Tie-type contrasts (ports of the Stata recode block) --------------------
d <- d |>
  mutate(
    tie_vs_notie = stata_recode(learnjob2,
      `1` = 1, `2` = 1, `3` = 1, `4` = 0, `5` = 0, `6` = 1, `7` = 0),
    acqboss_vs_strong = stata_recode(learnjob2,
      `1` = 0, `2` = 0, `3` = 1, `4` = NA, `5` = NA, `6` = 1, `7` = NA),
    acq_vs_strong = stata_recode(learnjob2,
      `1` = 0, `2` = 0, `3` = 1, `4` = NA, `5` = NA, `6` = NA, `7` = NA),
    boss_vs_strong = stata_recode(learnjob2,
      `1` = 0, `2` = 0, `3` = NA, `4` = NA, `5` = NA, `6` = 1, `7` = NA),
    acq_vs_other = stata_recode(learnjob2,
      `1` = NA, `2` = NA, `3` = 1, `4` = 0, `5` = 0, `6` = NA, `7` = 0),
    boss_vs_other = stata_recode(learnjob2,
      `1` = NA, `2` = NA, `3` = NA, `4` = 0, `5` = 0, `6` = 1, `7` = 0),
    acqboss_vs_other = stata_recode(learnjob2,
      `1` = NA, `2` = NA, `3` = 1, `4` = 0, `5` = 0, `6` = NA, `7` = 0),
    strong_vs_other = stata_recode(learnjob2,
      `1` = 1, `2` = 1, `3` = NA, `4` = 0, `5` = 0, `6` = NA, `7` = 0),
    strong_vs_acq_vs_boss_vs_other = stata_recode(learnjob2,
      `1` = 1, `2` = 1, `3` = 2, `4` = 0, `5` = 0, `6` = 3, `7` = 0),
    strong_and_acqboss_vs_notie = stata_recode(learnjob2,
      `1` = 1, `2` = 1, `3` = 2, `4` = 0, `5` = 0, `6` = 2, `7` = 0)
  )

# --- Cultural participation items --------------------------------------------
# Stata: recode X 2/9=0 — "no" (2) and non-response codes (3-9) count as no.
# Stata: egen omnivore = rsum2(...), anymiss — sum of the 0/1 items, missing
# if any item is missing.
cultural_items <- c("seemovie", "popmusic", "readfict", "drama", "dance",
                    "visitart", "gomusic")

d <- d |>
  mutate(across(all_of(cultural_items), ~ ifelse(.x >= 2 & .x <= 9, 0L, .x)))

omnivore_vals <- as.matrix(d[cultural_items])
any_missing <- apply(omnivore_vals, 1, function(r) any(is.na(r)))
d$omnivore <- ifelse(any_missing, NA_real_, rowSums(omnivore_vals))

# --- Listwise deletion (Stata: drop if X ==.) --------------------------------
# Listwise deletion over the variables in the do-file's drop loop reproduces
# the original analysis sample almost exactly (effective model N 1036 vs 1038
# in the Stata extract). The small residual difference comes from release
# vintage: the modern NORC 2002 file tags some DK/NA/IAP values as missing
# where the 2002-era file stored them as valid numeric codes.
keep_complete <- c(cultural_items, "educ", "degree", "age", "sex", "racecen1",
                   "wrkstat", "prestg80", "marital")
d <- d |>
  drop_na(all_of(keep_complete)) |>
  filter(!is.na(omnivore))

# --- Factor analysis: hi (highbrow) and pop (popular) scores -----------------
# Port of Stata: factor seemovie-gomusic, pcf; rotate, promax; predict hi pop
# pcf = principal-component factoring; psych's principal axis factoring (fm =
# "pa") with promax rotation and regression scores is the closest R analogue.
# As in Stata, scores are computed for the factor estimation sample (rows
# complete on the seven items) and are missing elsewhere.
fa_items <- d[cultural_items]
fa_fit <- psych::fa(fa_items, nfactors = 2, fm = "pa", rotate = "promax",
                    scores = "regression")
scores <- as.data.frame(fa_fit$scores)
names(scores) <- c("hi", "pop")
d <- cbind(d, scores)

saveRDS(d, here("data", "analysis-gss2002.rds"))
cat("Saved", nrow(d), "analytic cases to data/analysis-gss2002.rds\n")
