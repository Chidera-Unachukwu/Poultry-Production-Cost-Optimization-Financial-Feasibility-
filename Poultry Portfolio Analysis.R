# Poultry Production Cost Optimization & Financial Feasibility
# Portfolio project | Base R only
# IMPORTANT: The 1,000/3,000/5,000 bird outputs are illustrative baseline-equivalent scenarios,
# not forecasts. They scale the 50-bird results linearly and exclude unknown operating costs/capex.
# Keep actual data and model assumptions clearly separated.

baseline <- read.csv("/historical_baseline.csv", na.strings = c("", "NA"), stringsAsFactors = FALSE)
inputs <- read.csv("/model_inputs.csv", na.strings = c("", "NA"), stringsAsFactors = FALSE)

value_of <- function(df, key_col, key, value_col = "value") {
  v <- df[df[[key_col]] == key, value_col]
  if (length(v) != 1 || is.na(v)) stop(paste("Missing input:", key))
  as.numeric(v)
}

get_actual <- function(metric) value_of(baseline, "metric", metric)

placed <- get_actual("Birds placed")
died <- get_actual("Birds died")
sold <- get_actual("Birds sold")
sales_revenue <- get_actual("Total sales revenue")
chicks_cost <- get_actual("Day-old chicks")
starter_cost <- get_actual("Commercial starter feed")
finisher_cost <- get_actual("Commercial finisher feed")
meds_cost <- get_actual("Medication and supplements")

# Other operating expenses are intentionally missing and excluded until collected.

known_costs <- sum(chicks_cost, starter_cost, finisher_cost, meds_cost)
preliminary_surplus <- sales_revenue - known_costs
survival_rate <- sold / placed
mortality_rate <- died / placed
avg_sale_price <- sales_revenue / sold
commercial_feed_total <- starter_cost + finisher_cost
in_house_feed_estimate <- get_actual("In-house feed total estimate")
historical_metrics <- data.frame(
  metric = c("Birds placed", "Birds sold", "Birds died", "Survival rate",
             "Mortality rate", "Sales revenue (NGN)", "Documented costs (NGN)",
             "Preliminary surplus before missing costs (NGN)",
             "Average revenue per bird sold (NGN)",
             "Documented cost per bird sold (NGN)",
             "Commercial feed cost (NGN)", "In-house feed estimate (NGN)",
             "Estimated feed-cost difference (NGN)",
             "Estimated feed-cost difference as % of commercial feed"),
  value = c(placed, sold, died, survival_rate, mortality_rate, sales_revenue,
            known_costs, preliminary_surplus, avg_sale_price, known_costs/sold,
            commercial_feed_total, in_house_feed_estimate,
            commercial_feed_total-in_house_feed_estimate,
            (commercial_feed_total-in_house_feed_estimate)/commercial_feed_total)
)
historical_metrics$value <- format(historical_metrics$value, scientific = FALSE, drop0trailing = TRUE)
cat("\n=== HISTORICAL BASELINE ===\n")
print(historical_metrics, row.names=FALSE)

# Scenario inputs derived from the actual 50-bird batch.
# Other operating costs are intentionally NOT assumed to be zero; they are missing.

chick_per_placed <- chicks_cost / placed
commercial_feed_per_placed <- commercial_feed_total / placed
meds_per_placed <- meds_cost / placed
in_house_feed_per_placed <- in_house_feed_estimate / placed

targets <- c(1000, 3000, 5000)
scale_model <- do.call(rbind, lapply(targets, function(n) {
  expected_sold <- n * survival_rate
  revenue_n <- expected_sold * avg_sale_price
  chick_n <- n * chick_per_placed
  comm_feed_n <- n * commercial_feed_per_placed
  meds_n <- n * meds_per_placed
  in_house_feed_n <- n * in_house_feed_per_placed
  known_cost_comm <- chick_n + comm_feed_n + meds_n
  known_cost_inhouse <- chick_n + in_house_feed_n + meds_n
  data.frame(
    target_birds_placed = n,
    assumed_survival_rate = survival_rate,
    expected_birds_sold_illustrative = expected_sold,
    average_sale_price_NGN = avg_sale_price,
    illustrative_revenue_NGN = revenue_n,
    chick_cost_NGN = chick_n,
    commercial_feed_cost_NGN = comm_feed_n,
    medication_supplement_cost_NGN = meds_n,
    known_costs_commercial_NGN = known_cost_comm,
    preliminary_surplus_commercial_before_missing_costs_NGN = revenue_n-known_cost_comm,
    estimated_in_house_feed_cost_NGN = in_house_feed_n,
    known_costs_in_house_before_missing_costs_NGN = known_cost_inhouse,
    preliminary_surplus_in_house_before_missing_costs_NGN = revenue_n-known_cost_inhouse,
    classification = "Illustrative baseline-equivalent scenario; not a forecast"
  )
}))
scale_model$value <- format(scale_model$value, scientific = FALSE, drop0trailing = TRUE)
cat("\n=== SCALE MODEL: ILLUSTRATION ONLY ===\n")
print(scale_model, row.names = FALSE)

# Sensitivity analysis for the 5,000-bird target.
# Vary price, survival, and known costs; unrecorded expenses and capital expenditure remain excluded.

n <- 5000
sensitivity_assumptions <- data.frame(
  scenario = c("Baseline-equivalent illustration", "Selling price down 10%",
               "Known unit costs up 10%", "Survival falls to 92%",
               "Price down 10% and known costs up 10%",
               "Price down 10%, survival 92%, costs up 10%"),
  price_multiplier = c(1, .9, 1, 1, .9, .9),
  survival = c(survival_rate, survival_rate, survival_rate, .92, survival_rate, .92),
  cost_multiplier = c(1, 1, 1.1, 1, 1.1, 1.1)
)
base_known_cost_5000 <- n * (chick_per_placed + commercial_feed_per_placed + meds_per_placed)
sensitivity <- do.call(rbind, lapply(seq_len(nrow(sensitivity_assumptions)), function(i) {
  a <- sensitivity_assumptions[i, ]
  rev_i <- n * a$survival * avg_sale_price * a$price_multiplier
  cost_i <- base_known_cost_5000 * a$cost_multiplier
  data.frame(
    scenario = a$scenario,
    birds_placed = n,
    survival_rate = a$survival,
    price_multiplier = a$price_multiplier,
    known_cost_multiplier = a$cost_multiplier,
    illustrative_revenue_NGN = rev_i,
    known_costs_only_NGN = cost_i,
    preliminary_surplus_before_missing_costs_and_capex_NGN = rev_i-cost_i,
    classification = "Illustrative only; excludes unknown costs and capex"
  )
}))
sensitivity$value <- format(sensitivity$value, scientific = FALSE, drop0trailing = TRUE)
cat("\n=== SENSITIVITY ANALYSIS ===\n")
print(sensitivity, row.names = FALSE)

write.csv(historical_metrics, "historical_metrics_output.csv", row.names = FALSE)
write.csv(scale_model, "scale_model_output.csv", row.names = FALSE)
write.csv(sensitivity, "sensitivity_output.csv", row.names = FALSE)
cat("\nOutputs saved. Before using this as a business forecast, add other operating costs,\n")
cat("capital expenditure, hatchery assumptions, and validated feed requirements at scale.\n")
