penman_co2 <- function (Tmin, Tmax, U2, Ra = NA, lat = NA, Rs = NA, tsun = NA, 
                        CC = NA, ed = NA, Tdew = NA, RH = NA, P = NA, P0 = NA, z = NA, CO2 = NA, crop = "short", na.rm = FALSE) 
{
  if (sum(is.na(Tmin), is.na(Tmax), is.na(U2)) > 0 & na.rm == FALSE) {
    stop("Error: Data must not contain NAs")
  }
  if (((length(Ra) > 1 & sum(is.na(Ra)) > 0) | (length(Rs) > 1 & sum(is.na(Rs)) > 0) | 
       (length(tsun) > 1 & sum(is.na(tsun)) >  0) | (length(CC) > 1 & sum(is.na(CC)) > 0) | 
       (length(ed) >  1 & sum(is.na(ed)) > 0) | (length(Tdew) > 1 & sum(is.na(Tdew)) > 0) | 
       (length(RH) > 1 & sum(is.na(RH)) > 0) | (length(P) > 1 & sum(is.na(P)) > 0) 
       | (length(P0) > 1 & sum(is.na(P0)) > 0)) & na.rm == FALSE) {
    stop("Error: Data must not contain NAs")
  }
  if (is.na(Ra[1]) & is.na(lat[1])) {
    stop("Error: One of Ra or lat must be provided")
  }
  if (!is.na(Ra[1])) {
    warning("Using user-provided (Ra)")
  }
  if (length(Rs) != length(Tmin) & length(tsun) != length(Tmin) & 
      length(CC) != length(Tmin)) {
    stop("Error: One of Rs, tsun or CC must be provided")
  }
  if (length(Tmin) != length(Tmax) | length(Tmin) != length(U2)) {
    stop("Error: Data must be of the same length")
  }
  if (length(P) != length(Tmax) & is.na(z)) {
    stop("Error: Elevation above sea level (z) must be specified if P is not provided.")
  }
  if (is.na(z)) {
    warning("Specifying the elevation above sea level (z) is highly recommended in order to compute the clear-sky solar radiation.")
  }
  ET0 <- Tmin * NA
  if (!is.ts(Tmin)) {
    Tmin <- ts(as.matrix(Tmin), frequency = 12)
  }
  else {
    Tmin <- ts(as.matrix(Tmin), frequency = frequency(Tmin), start = start(Tmin))
  }
  n <- nrow(Tmin)
  m <- ncol(Tmin)
  c <- cycle(Tmin)
  T <- (Tmin + Tmax)/2
  lambda <- 2.501 - 0.002361 * T
  if (nrow(as.matrix(P)) != n) {
    if (length(P0) == n) {
      P <- P0 %*% as.matrix(((293 - 0.0065 * z)/293)^5.26)
    }
    else {
      P0 <- matrix(101.3, n, m)
      P <- P0 %*% as.matrix(((293 - 0.0065 * z)/293)^5.26)
    }
  }
  gamma <- 0.00163 * P/lambda
  etmx <- 0.611 * exp((17.27 * Tmax)/(Tmax + 237.3))
  etmn <- 0.611 * exp((17.27 * Tmin)/(Tmin + 237.3))
  ea <- (etmx + etmn)/2
  Delta <- 4099 * ea/(T + 237.3)^2
  if (length(ed) != n) {
    if (length(Tdew) == n) {
      ed <- 0.611 * exp((17.27 * Tdew)/(Tdew + 237.3))
    }
    else if (length(RH) == n) {
      ed <- RH/((50/etmn) + (50/etmx))
    }
    else {
      ed <- etmn
    }
  }
  if (nrow(as.matrix(Ra)) != n | {
    nrow(as.matrix(Rs)) != n & nrow(as.matrix(tsun)) == n
  }) {
    mlen <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    msum <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273,302, 334) + 15
    J <- msum[c]
    delta <- 0.409 * sin(0.0172 * J - 1.39)
    dr <- 1 + 0.033 * cos(0.0172 * J)
    latr <- lat/57.2957795
    sset <- t(-as.matrix(tan(latr)) %*% tan(delta))
    omegas <- sset * 0
    omegas[sset >= {-1} & sset <= 1] <- acos(sset[sset >= {-1} & sset <= 1])
    omegas[sset < {-1}] <- max(omegas)
  }
  if (nrow(as.matrix(Ra)) != n) {
    Ra <- 37.6 * dr * (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
    Ra <- ifelse(Ra < 0, 0, Ra)
  }
  if (nrow(as.matrix(Rs)) != n) {
    if (nrow(as.matrix(tsun)) == n) {
      N <- 7.64 * omegas
      nN <- tsun/N
    }
    else if (nrow(as.matrix(CC)) == n) {
      nN <- (100 - CC)/100
    }
    as <- 0.25
    bs <- 0.5
    Rs <- (as + bs * (nN)) * Ra
  }
  if (!is.na(z)) {
    Rso <- matrix(0.75 + 2e-05 * z, n, m, byrow = TRUE) * Ra
  }
  else {
    Rso <- (0.75 + 2e-05 * 840) * Ra
  }
  ac <- 1.35
  bc <- -0.35
  a1 <- 0.34
  b1 <- -0.14
  alb <- 0.23
  Rn <- (1 - alb) * Rs - (ac * Rs/Rso + bc) * (a1 + b1 * sqrt(ed)) * 
    4.9e-09 * ((273.15 + Tmax)^4 + (273.15 + Tmin)^4)/2
  Rn[Rs == 0] <- 0
  G <- rep(NA, length(T))
  G[1] <- 0.14 * (T[2] - T[1])
  G[2:{length(T) - 1}] <- 0.07 * (T[3:length(T)] - T[1:{length(T) - 2}])
  G[length(T)] <- 0.14 * (T[length(T)] - T[length(T) - 1])
  if (crop == "short") {
    c1 <- 900
    c2 <- 0.34
  }
  else {
    c1 <- 1600
    c2 <- 0.38
  }
  if (is.na(CO2[1]) & length(CO2) == 1){
    ET0 <- (0.408 * Delta * (Rn - G) + gamma * (c1/(T + 273)) * 
              U2 * (ea - ed))/(Delta + gamma * (1 + c2 * U2))
  }
  else if (length(CO2) > 1 & sum(is.na(CO2)) > 0) {
    stop("Error: Data must not contain NAs")
  }
  else {
    warning("Using user-provided (CO2)")
    ET0 <- (0.408 * Delta * (Rn - G) + gamma * (c1/(T + 273)) * 
              U2 * (ea - ed))/(Delta + gamma * (1 + (c2+2.4*10^-4*CO2-300) * U2))
  }
  ET0 <- ifelse(ET0 < 0, 0, ET0) * mlen[c]
  colnames(ET0) <- rep("ET0_pen", m)
  return(ET0)
}