#' @title calcSawlogShare
#' @description Share of sawlogs and veneer logs in industrial roundwood production (FAO forestry
#' production via calcTimberDemand), 1995 to the last FAO year in five-year steps, held constant to 2150.
#' MAgPIE's sawlog floor (module 73) uses it as the regional share of harvest that must be sawlog-grade.
#' @return share (1) with the industrial roundwood production as aggregation weight
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("SawlogShare")
#' }
#' @export
calcSawlogShare <- function() {
  x <- calcOutput("TimberDemand", aggregate = FALSE)
  saw <- collapseNames(x[, , "Sawlogs and veneer logs"][, , "production"])
  irw <- collapseNames(x[, , "Industrial roundwood"][, , "production"])
  years <- getYears(saw, as.integer = TRUE)
  years <- years[years >= 1995 & years %% 5 == 0]
  saw <- saw[, years, ]
  irw <- irw[, years, ]
  share <- saw / irw
  share[is.na(share)] <- 0
  share[share > 1] <- 1
  tAll <- paste0("y", c(seq(1995, 2060, 5), seq(2070, 2110, 10), 2130, 2150))
  share <- toolHoldConstant(share, tAll)
  irw <- toolHoldConstant(irw, tAll)
  return(list(x = share, weight = irw + 1e-10, unit = "1", min = 0, max = 1,
              description = paste("Share of sawlogs and veneer logs in FAO industrial roundwood production,",
                                  "held after the last FAO year")))
}
