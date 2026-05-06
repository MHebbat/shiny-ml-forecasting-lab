# Pure-R smoke for prophet_prepare. Does NOT require the prophet package.
suppressMessages(source("R/utils.R"))
# model_registry.R has heavy deps; only source the prophet helpers we need
# by isolating the relevant functions. Easier: source the file but tolerate
# missing optional packages — the function definitions parse without them.
options(warn = 1)
suppressMessages(suppressWarnings(try(source("R/model_registry.R"), silent = TRUE)))

stopifnot(exists("prophet_prepare"), exists(".assert_prophet_shapes"))

# 1) Basic daily series, linear growth — should rename and order
set.seed(1)
n <- 30
dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = n)
df <- data.frame(date = dates[sample(n)], # shuffled
                 sales = rnorm(n, 100, 5))
out <- prophet_prepare(df, time_col = "date", target = "sales",
                       growth = "linear")
stopifnot(identical(names(out), c("ds", "y")))
stopifnot(inherits(out$ds, "Date"))
stopifnot(!is.unsorted(out$ds))
stopifnot(nrow(out) == n)

# 2) Duplicate ds rows — must dedupe
df2 <- rbind(df, df[1:5, ])
out2 <- prophet_prepare(df2, time_col = "date", target = "sales",
                        growth = "linear")
stopifnot(nrow(out2) == n)
stopifnot(anyDuplicated(out2$ds) == 0)

# 3) NA in target — drop rows
df3 <- df; df3$sales[1:3] <- NA
out3 <- prophet_prepare(df3, time_col = "date", target = "sales",
                        growth = "linear")
stopifnot(nrow(out3) == n - 3)

# 4) Logistic growth without cap should error
err <- tryCatch(prophet_prepare(df, time_col = "date", target = "sales",
                                 growth = "logistic"),
                error = function(e) conditionMessage(e))
stopifnot(grepl("cap", err, ignore.case = TRUE))

# 5) Logistic growth with numeric cap & floor
out5 <- prophet_prepare(df, time_col = "date", target = "sales",
                        growth = "logistic", cap = 200, floor = 0)
stopifnot(all(c("ds","y","cap","floor") %in% names(out5)))
stopifnot(all(out5$cap == 200))

# 6) Logistic growth with cap from column
df6 <- df; df6$mycap <- 250
out6 <- prophet_prepare(df6, time_col = "date", target = "sales",
                        growth = "logistic", cap = "mycap")
stopifnot(all(out6$cap == 250))

# 7) Bad time column -> error
err7 <- tryCatch(prophet_prepare(df, time_col = "nonexistent", target = "sales"),
                 error = function(e) conditionMessage(e))
stopifnot(grepl("not found", err7))

# 8) All-NA dates -> error
df8 <- df; df8$date <- NA
err8 <- tryCatch(prophet_prepare(df8, time_col = "date", target = "sales"),
                 error = function(e) conditionMessage(e))
stopifnot(grepl("could not be parsed", err8))

cat("test_prophet_prepare.R: OK\n")
