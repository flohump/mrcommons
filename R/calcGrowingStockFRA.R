#' @title calcGrowingStockFRA
#' @description
#' Growing stock of one FRA 2025 forest category at country level, all reporting years.
#' A country without a growing stock entry gets the planted-forest growing stock (plantation,
#' other planted forest) or the whole-forest growing stock (naturally regenerating forest).
#' The weight is the category area; a country still without a value gets no weight.
#' Totals of such countries are 0.
#' The FRA reader fills unreported years of a series with the mean of its reported years. Leading years filled
#' that way get the linear trend from the first reported year to 2020 (where that trend is positive).
#'
#' @param category FRA category: "plantation", "other_planted" or "nat_reg". Several categories give one item
#' per category.
#' @param indicator "per_ha" (m3/ha, area-weighted), "total" (Mm3, absolute) or "measured" (1 where the per-ha
#' series of the category is the country's own entry and changes over the reporting years; weight: the 2020 area
#' of the countries with a value).
#' @param years Years, interpolated between the FRA years (NULL: the FRA years; not used by "measured").
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("GrowingStockFRA", category = "plantation", aggregate = TRUE)
#' }
#'
#' @importFrom magpiesets FRAnames
#' @export

calcGrowingStockFRA <- function(category = "plantation", indicator = "per_ha", years = NULL) {
  if (!all(category %in% c("plantation", "other_planted", "nat_reg"))) {
    stop("Invalid category. Use 'plantation', 'other_planted' or 'nat_reg'.")
  }
  if (!indicator %in% c("per_ha", "total", "measured")) {
    stop("Invalid indicator. Use 'per_ha', 'total' or 'measured'.")
  }
  if (length(category) > 1) {
    parts <- lapply(category, function(k) {
      calcOutput("GrowingStockFRA", category = k, indicator = indicator, years = years,
                 aggregate = FALSE, supplementary = TRUE)
    })
    bind <- function(what) {
      if (is.null(parts[[1]][[what]])) return(NULL)
      return(mbind(lapply(seq_along(parts), function(k) setNames(parts[[k]][[what]], category[k]))))
    }
    return(list(x = bind("x"),
                weight = bind("weight"),
                min = 0,
                unit = parts[[1]]$unit,
                description = paste("FRA 2025 growing stock by forest category,", indicator)))
  }
  stock <- readSource("FRA2025", subtype = "growing_stock", convert = TRUE)
  area <- readSource("FRA2025", subtype = "forest_area", convert = TRUE)
  getNames(area) <- FRAnames(getNames(area))

  perHa <- stock[, , grep("gs_ha_", getNames(stock), value = TRUE, fixed = TRUE)]
  getNames(perHa) <- unname(FRAnames(sub("gs_ha_", "", getNames(perHa), fixed = TRUE)))
  cat <- FRAnames(category)
  raw <- collapseNames(perHa[, , cat])
  rawNat <- collapseNames(perHa[, , FRAnames("nat_reg")])
  # leading years filled with the mean: linear trend from the first reported year to 2020
  fraYears <- getYears(perHa, as.integer = TRUE)
  trend <- function(v) {
    n <- length(v)
    for (k in rev(seq_len(n - 3))) {
      m <- mean(v[(k + 1):n])
      if (m > 0 && all(abs(v[1:k] - m) < 1e-6 * m) && diff(range(v[(k + 1):n])) > 1e-6 * m) {
        first <- fraYears[k + 1]
        b <- v[k + 1] - (v[fraYears == 2020] - v[k + 1]) / (2020 - first) * (first - fraYears[1:k])
        v[1:k][b > 0] <- b[b > 0]
        break
      }
    }
    return(v)
  }
  perHa[] <- aperm(apply(as.array(perHa), c(1, 3), trend), c(2, 1, 3))
  x <- collapseNames(perHa[, , cat])
  weight <- collapseNames(area[, , cat])
  fallback <- collapseNames(perHa[, , ifelse(category == "nat_reg", "Forest", "Managed Forest")])
  fill <- which(x == 0 & fallback > 0)
  x[fill] <- fallback[fill]
  weight[x == 0] <- 0
  derived <- which(x != raw)   # filled or backcast: total from the per-ha value and the area

  if (indicator == "measured") {
    # the category's own entry in the years to 2020: not blank or constant, in no year the naturally regenerating value
    yrs <- getYears(raw, as.integer = TRUE) <= 2020
    v <- as.array(raw)[, yrs, 1]
    forestWide <- rowSums(v > 0 & abs(v - as.array(rawNat)[, yrs, 1]) < 0.5) > 0
    constant <- apply(v, 1, function(z) diff(range(z))) < 0.5
    return(list(x = new.magpie(getItems(raw, 1), NULL, NULL, as.numeric(!(forestWide | constant))),
                weight = setYears(weight[, 2020, ], NULL) + 10^-10,
                min = 0,
                max = 1,
                unit = "1",
                description = paste("Countries whose FRA 2025 growing stock per ha of", cat,
                                    "is a series of its own that changes over the reporting years")))
  }
  if (indicator == "per_ha") {
    unit <- "m3/ha"
    weight <- weight + 10^-10
  } else {
    total <- stock[, , grep("gs_tot_", getNames(stock), value = TRUE, fixed = TRUE)]
    getNames(total) <- unname(FRAnames(sub("gs_tot_", "", getNames(total), fixed = TRUE)))
    total <- collapseNames(total[, , cat])
    total[derived] <- x[derived] * weight[derived]
    x <- total
    weight <- NULL
    unit <- "Mm3"
  }
  if (!is.null(years)) {
    x <- time_interpolate(x, years)
    if (!is.null(weight)) weight <- time_interpolate(weight, years)
  }

  return(list(x = x,
              weight = weight,
              min = 0,
              unit = unit,
              description = paste("Growing stock of", cat, "as reported by FRA 2025,",
                                  "countries without a stock entry filled with planted or whole forest")))
}
