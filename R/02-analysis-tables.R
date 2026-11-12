# 02-analysis-tables.R
# Port of the table-producing blocks in analysis-SSR-R1.do, -R2.do, -R3.do.
# Stata robust standard errors -> sandwich HC estimators via lmtest::coeftest.

library(dplyr)
library(nnet)
library(MASS)
library(sandwich)
library(lmtest)
library(modelsummary)
library(broom)
library(gt)

# modelsummary does not reliably support nnet::multinom (duplicate-label bug
# across outcome groups), so multinomial models are tabled with gt directly.
gt_multinom <- function(fit, stars = TRUE) {
  tidy(fit) |>
    mutate(
      term = paste(term, sprintf("[%s]", y.level)),
      estimate = sprintf("%.3f", estimate),
      se = sprintf("(%.3f)", std.error),
      star = ifelse(stars & p.value < 0.05, "*", ""),
      estimate = paste0(estimate, star)
    ) |>
    dplyr::select(term, estimate, se) |>
    gt()
}
library(here)

d <- readRDS(here("data", "analysis-gss2002.rds"))

ctrl <- "age + agesq + women + black + hisp + married + deg2 + deg3 + deg4 + deg5"
robust <- function(fit) coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC1"))

dir.create(here("output", "tables"), showWarnings = FALSE)

# ---- R1 Table 1: tie_vs_notie, with and without education controls ----------
m1a <- glm(as.formula(paste("tie_vs_notie ~ omnivore + age + agesq + women +",
                            "black + hisp + married")),
           family = binomial, data = d)
m1b <- glm(as.formula(paste("tie_vs_notie ~ omnivore +", ctrl)),
           family = binomial, data = d)
modelsummary(list(m1a, m1b), vcov = "HC1",
             output = here("output", "tables", "r1-table1.html"),
             stars = TRUE, gof_map = c("nobs", "aic", "ll"))

# ---- R1 Table 2: mlogit strong_and_acqboss_vs_notie ------------------------
m2 <- multinom(as.formula(paste("strong_and_acqboss_vs_notie ~ omnivore +", ctrl)),
               data = d, trace = FALSE)
# Note: sandwich::estfun() has no multinom method, so the multinomial models
# report model-based (Hessian) standard errors; glm models use HC1 robust SEs.
gt_multinom(m2) |> gtsave(here("output", "tables", "r1-table2.html"))

# ---- R1/R2 Table 3: nbreg of network size on omnivore ----------------------
m3 <- glm.nb(as.formula(paste("numcntct ~ omnivore + age + agesq + women +",
                              "black + hisp + married + deg2 + deg3 + deg4 + deg5")),
             data = d)
modelsummary(list(m3), vcov = "HC1",
             output = here("output", "tables", "r1-table3.html"),
             stars = TRUE)

# ---- R1/R2 Table 4: mlogit with squared network size -----------------------
m4 <- multinom(as.formula(paste("strong_and_acqboss_vs_notie ~ omnivore +",
                                "numcntct + I(numcntct^2) +", ctrl)),
               data = d, trace = FALSE)
gt_multinom(m4) |> gtsave(here("output", "tables", "r1-table4.html"))

# ---- R1 Table 5: acqboss_vs_strong ~ numcntct^2 x omnicat ------------------
d <- d |>
  mutate(omnicat = cut(omnivore, breaks = c(-Inf, 0, 3, 5, Inf),
                       labels = c(NA, "1-3 Activities", "4-5 Activities",
                                  "6+ Activities")))
m5 <- glm(as.formula(paste("acqboss_vs_strong ~ numcntct * I(numcntct^2) * omnicat +",
                           "age + agesq + women + black + hisp + married +",
                           "deg2 + deg3 + deg4 + deg5")),
          family = binomial, data = d)
modelsummary(list(m5), vcov = "HC1",
             output = here("output", "tables", "r1-table5.html"),
             stars = TRUE)

# ---- R2/R3 Table 2: logit and mlogit significance tests --------------------
m6 <- glm(as.formula(paste("tie_vs_notie ~ omnivore +", ctrl)),
          family = binomial, data = d)
# Multinomial models cannot share a table with glm models in modelsummary
# (duplicate parameter labels across outcome groups), so the mlogit is tabled
# separately.
modelsummary(list(m1b, m6), vcov = "HC1",
             output = here("output", "tables", "r2-table2-logits.html"),
             stars = TRUE)
gt_multinom(m2) |> gtsave(here("output", "tables", "r2-table2-mlogit.html"))

# ---- R3 memo Table 1: mlogit restricted to working respondents -------------
m7 <- multinom(as.formula(paste("strong_and_acqboss_vs_notie ~ omnivore + yearsjob +",
                                ctrl)),
               data = filter(d, wrkstat == 1), trace = FALSE)
gt_multinom(m7) |> gtsave(here("output", "tables", "r3-memo-table1.html"))

# ---- R3 memo Table 2: mlogit with rescaled factor scores -------------------
d <- d |>
  mutate(
    hi_s  = (hi  - min(hi,  na.rm = TRUE)) / (max(hi,  na.rm = TRUE) - min(hi,  na.rm = TRUE)),
    pop_s = (pop - min(pop, na.rm = TRUE)) / (max(pop, na.rm = TRUE) - min(pop, na.rm = TRUE))
  )
m8 <- multinom(as.formula(paste("strong_vs_acq_vs_boss_vs_other ~ hi_s + pop_s +", ctrl)),
               data = filter(d, !is.na(yearsjob)), trace = FALSE)
gt_multinom(m8) |> gtsave(here("output", "tables", "r3-memo-table2.html"))

cat("Tables written to output/tables/\n")
