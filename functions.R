################################################################################################################################################################################
# RESET ALCOHOL PROJECT                                                                                                                                                        #
# PAF/PIF ESTIMATION FUNCTIONS                                                                                                                                                 #              
#                                                                                                                                                                              #
# v.1, 2026/09/06                                                                                                                                                              #
# annibale.cois@uct.ac.za                                                                                                                                                      #
#                                                                                                                                                                              #
################################################################################################################################################################################

################################################################################################################################################################################
# HELPERS                                                                                                                                                                      #
################################################################################################################################################################################

# LOAD AN RData FILE 

loadRData <- function(fileName){     
  load(fileName)
  get(ls()[ls() != "fileName"])
}

################################################################################################################################################################################
# RR                                                                                                                                                                           #
################################################################################################################################################################################

# EXCESS RR FOR FORMER DRINKERS: RRfd
rrfd <- function(x,cause, gender, agecat, outcome, dat, par) {                                                     
  ret <- dat[dat$cause==cause & dat$gender==gender & dat$agecat==agecat & dat$outcome==outcome, "r_fd"][[1]](x,gender,agecat, par)-1
  return(ret) 
}

# EXCESS RR FOR CURRENT NON-BINGE DRINKERS: RRnb(x)
rrnb   <- function(x, cause, gender, agecat, outcome, dat, par) {                                      
  param <- dat[dat$cause==cause & dat$gender==gender & dat$agecat==agecat & dat$outcome==outcome,]
  cap <- param$cap[1]
  if (x>cap) x=cap    # cap the relative risk at x=cap    
  ret <- dat[dat$cause==cause & dat$gender==gender & dat$agecat==agecat & dat$outcome==outcome, "risk"][[1]](x,gender,agecat, par)-1
  return(ret)
}

# EXCESS RR FOR CURRENT BINGE DRINKERS: RRbd(x)
rrbd <- function(x, cause, gender, agecat, outcome, dat, par) {                                                 
  param <- dat[dat$cause==cause & dat$gender==gender & dat$agecat==agecat & dat$outcome==outcome,]
  cap <- param$cap[1]
  if (x>cap) x=cap    # cap the relative risk at x=cap    
  ret <- dat[dat$cause==cause & dat$gender==gender & dat$agecat==agecat & dat$outcome==outcome, "binge_risk"][[1]](x,gender,agecat, par)-1
  return(ret)
}    

################################################################################################################################################################################
# INTEGRATION                                                                                                                                                                  #
################################################################################################################################################################################

# BASE INTEGRAND (SAME RISK FOR BINGE/NON BINGE DRINKERS) 
int1 <- function(x, cause, outcome, gender, agecat, shape, rate, rrnb, dat, par) {                                            
  int <- as.numeric(dgamma(x, shape = shape, rate = rate)*(rrnb(x, cause, gender, agecat, outcome, dat, par)))
  return(int)
}
vint1 <- Vectorize(int1, vectorize.args = "x")   

# INTEGRAND FOR IHD, STROKE, INJURIES (DIFFERENT FOR BINGE/NON BINGE DRINKERS)  
int2 <- function(x, cause, outcome, gender, agecat, shape, rate, rrbd, dat, par) {                                            
  int <- as.numeric(dgamma(x, shape = shape, rate = rate)*(rrbd(x, cause, gender, agecat, outcome, dat, par)))
  return(int)
}  
vint2 <- Vectorize(int2, vectorize.args = "x")

# FALL BACK FUNCTIONS IF THE GAUSS-LEGENDRE FIXED NODES FAILS
safe_integrate <- function(f, lower, upper, cause, outcome, gender, agecat, shape, rate, rrnb, dat, par, rel.tol, subdivisions) {
  res <- tryCatch(
    stats::integrate(f, lower = lower, upper = upper, cause, outcome, gender, agecat, shape, rate, rrnb, dat, par,  rel.tol = rel.tol, 
                     subdivisions = subdivisions, stop.on.error = FALSE),
    error = function(e) list(value = NA_real_, message = conditionMessage(e))
  )
  if (!is.list(res)) {
    return(list(value = NA_real_))
  }
  
  if (!is.null(res$message) && res$message != "OK") {
    return(list(value = NA_real_))
  }
  return(list(value = res$value))
}

################################################################################################################################################################################
# IMPACT FRACTIONS                                                                                                                                                             #
################################################################################################################################################################################

pbat <- function(index, draw, nc, pcd, bd, ub, alpha, beta)  {                                                      
  bat <- pgamma(ub, shape = alpha[index, draw], rate = beta[index, draw]) - 
    pgamma(bd, shape = alpha[index, draw], rate = beta[index, draw])
  return(bat*pcd/nc)
}

# build once, at top level (Golub-Welsch)
gl_nodes <- function(n) {
  i <- seq_len(n - 1)
  b <- i / sqrt(4 * i^2 - 1)
  J <- matrix(0, n, n)
  J[cbind(i, i + 1)] <- b
  J[cbind(i + 1, i)] <- b
  e <- eigen(J, symmetric = TRUE)
  o <- order(e$values)
  list(x = e$values[o], w = 2 * e$vectors[1, o]^2)
}

fast_integrate <- function(f, lower, upper, gl, ...) {
  if (upper <= lower) return(list(value = 0))
  h <- (upper - lower) / 2
  m <- (upper + lower) / 2
  v <- f(m + h * gl$x, ...)
  if (!all(is.finite(v)))                      # fall back if the integrand misbehaves
    return(safe_integrate(f, lower = lower, upper = upper, ...,
                          rel.tol = ITOL, subdivisions = SUBDIV))
  list(value = h * sum(gl$w * v))
}

paf1 <- function(cause, outcome, gender, agecat, pfd, pcd, pbd, shape, rate, nc, lb, ub, rrfd, rrnb, vint1, par1, par2, dat, gl) {
  I1  <- fast_integrate(vint1, lb, ub, gl, cause, outcome, gender, agecat, shape, rate, rrnb, dat, par2)$value
  NUM <- pfd * rrfd(0, cause, gender, agecat, outcome, dat, par1) + pcd / nc * I1
  NUM / (1 + NUM)
}

paf2 <- function(cause, outcome, gender, agecat, pfd, pcd, pbd, pbat, bd, shape, rate, nc, lb, ub, rrfd, rrnb, rrbd, vint1, vint2, par1, par2, par3, dat, gl) {
  NUM1 <- pfd * rrfd(0, cause, gender, agecat, outcome, dat, par1)
  I_nb_low  <- fast_integrate(vint1, lb, bd, gl, cause, outcome, gender, agecat, shape, rate, rrnb, dat, par2)$value
  I_bd_high <- fast_integrate(vint2, bd, ub, gl, cause, outcome, gender, agecat, shape, rate, rrbd, dat, par3)$value
  k <- pcd / nc
  
  if (pbd > pbat) {
    I_bd_low <- fast_integrate(vint2, lb, bd, gl, cause, outcome, gender, agecat, shape, rate, rrbd, dat, par3)$value
    NUM2 <- (pcd - pbd) / (pcd - pbat) * k * I_nb_low + (pbd - pbat) / (pcd - pbat) * k * I_bd_low + k * I_bd_high
  } else {
    NUM2 <- k * I_nb_low + pbd / pbat * k * I_bd_high
  }
  (NUM1 + NUM2) / (1 + NUM1 + NUM2)
}


pif <- function(pifdata, bdm, bdf, lb, ub, gl, radjust) {
  n <- nrow(pifdata)
  out <- numeric(n)

  gender        <- pifdata$gender
  cause         <- pifdata$cause  
  outcome       <- pifdata$outcome
  agecat        <- pifdata$agecat
  r_pfd         <- pifdata$r_pfd
  prisk         <- pifdata$prisk
  pbinge_risk   <- pifdata$pbinge_risk
  wholly        <- pifdata$wholly_attr
  bingea        <- pifdata$bingea
  
  paf           <- pifdata$paf
  
  pdrink_cf     <- pifdata$pdrink_cf
  pbing_cf      <- pifdata$pbing_cf
  alpha_cf      <- pifdata$alpha_cf 
  beta_cf       <- pifdata$beta_cf
  pform_cf      <- pifdata$pform_cf
  
  pdrink_o     <- pifdata$pdrink
  pbing_o      <- pifdata$pbing
  alpha_o      <- pifdata$alpha 
  beta_o       <- pifdata$beta
  pform_o      <- pifdata$pform
  
  # identify rows where counterfactual coincides with observed 
  eps <- 1e-4
  equaldist <- (abs((pdrink_o - pdrink_cf)/pdrink_o) < eps) & (abs((pbing_o - pbing_cf)/pbing_o) < eps) & (abs((alpha_o - alpha_cf)/alpha_o) < eps) & (abs((beta_o - beta_cf)/beta_o) < eps) 
  
  # vectorised pre-computation (one pgamma call per quantity, not per row)
  ebd <- ifelse(gender == "Male", bdm, bdf)
  pUB_cf <- pgamma(ub, alpha_cf, beta_cf)
  pLB_cf <- pgamma(lb, alpha_cf, beta_cf)
  ncc_cf <- pUB_cf - pLB_cf
  pdb_cf <- pdrink_cf * pbing_cf
  # pUB_o <- pgamma(ub, alpha_o, beta_o);  pLB_cf <- pgamma(lb, alpha_o, beta_o)
  # pLB_o <- pgamma(lb, alpha_o, beta_o)
  # ncc_o <- pUB_o - pLB_o
  # pdb_o <- pdrink_o * pbing_o
  
  # epbat only matters for binge rows, but is cheap to do vectorised
  epbat_cf <- pmin((pUB_cf - pgamma(ebd, alpha_cf, beta_cf)) * pdrink_cf / ncc_cf, pdb_cf)
  # epbat_o <- pmin((pUB_o - pgamma(ebd, alpha_o, beta_o)) * pdrink_o / ncc_o, pdb_o)
  
  # 1: Counterfactual distriution coincides with observed
  todo1 <- which(equaldist)
  for (i in todo1) {
    out[i] <- 0
  }
  
  # 2: Wholly attributable with different distributions
  todo2 <- which(!equaldist & wholly)
  for (i in todo2) {
    out[i] <- 1-radjust
  }
  
  # 3: Not wholly attributable and different distributions
  todo3 <- which(!equaldist & (!wholly))
  
  for (i in todo3) {
    d <- pifdata[i, ]
    PAR1 <- r_pfd[[i]]()
    PAR2 <- prisk[[i]]()
    if (bingea[i] == 1) {
      paf_cf <- paf2(cause[i], outcome[i], gender[i], agecat[i], pform_cf[i], pdrink_cf[i], pdb_cf[i],
                     epbat_cf[i], ebd[i], alpha_cf[i], beta_cf[i], ncc_cf[i], lb, ub,
                     rrfd, rrnb, rrbd, vint1, vint2, par1 = r_pfd[[i]](), par2 = prisk[[i]](), par3 = pbinge_risk[[i]](), dat = d, gl = gl)
      # paf_o <- paf2(cause[i], outcome[i], gender[i], agecat[i], pform_o[i], pdrink_o[i], pdb_o[i],
      #                epbat_o[i], ebd[i], alpha_o[i], beta_o[i], ncc_o[i], lb, ub,
      #                rrfd, rrnb, rrbd, vint1, vint2, par1 = r_pfd[[i]](), par2 = prisk[[i]](), par3 = pbinge_risk[[i]](), dat = d, gl = gl)

    } else {
      paf_cf <- paf1(cause[i], outcome[i], gender[i], agecat[i], pform_cf[i], pdrink_cf[i], pdb_cf[i],
                     alpha_cf[i], beta_cf[i], ncc_cf[i], lb, ub, rrfd, rrnb, vint1,
                     par1 = r_pfd[[i]](), par2 = prisk[[i]](), dat = d, gl = gl)
      # paf_o <- paf1(cause[i], outcome[i], gender[i], agecat[i], pform_o[i], pdrink_o[i], pdb_o[i],
      #                alpha_o[i], beta_o[i], ncc_o[i], lb, ub, rrfd, rrnb, vint1,
      #                par1 = r_pfd[[i]](), par2 = prisk[[i]](), dat = d, gl = gl)
    }
   # out[i] <- 1 - (1 - paf_o) / (1 - paf_cf)
    out[i] <- 1 - (1 - paf[i]) / (1 - paf_cf)
  }
  
  out
}

################################################################################################################################################################################
# WRAPPERS                                                                                                                                                                     #
################################################################################################################################################################################

edata <- function(scenario, r0, recalibrate, data1, data2) {
  data <- rbind(data1, data2)
  cf <- left_join(data, scenario)
  cf[,c("mu")] <- cf[,c("mu")] * cf$cons
  cf[,c("beta")] <- cf[,c("beta")] / cf$cons
  cf[,c("pdrink")] <- cf[,c("pdrink")] * cf$curr
  
  R <- sum(cf$pdrink*cf$mu*cf$pop)/sum(data$pdrink*data$mu*data$pop)
  
  if (recalibrate %in% c("curr","cons")) {
    ra <- r0/R 
    if (recalibrate == "curr") {
      scenario$curr <- scenario$curr*ra
    } else {
      scenario$cons <- scenario$cons*ra
    }
    cf <- left_join(data, scenario)
    cf[,c("mu")] <- cf[,c("mu")] * cf$cons
    cf[,c("beta")] <- cf[,c("beta")] / cf$cons
    cf[,c("pdrink")] <- cf[,c("pdrink")] * cf$curr
  } else {
    ra = 1
  }
  
  cf <- cf %>% arrange(gender, agecat) # COUNTERFACTUAL EXPOSURE 
  out = list(expdata = left_join(data, cf  %>% rename(alpha_cf = alpha, beta_cf = beta, pdrink_cf = pdrink, pform_cf = pform, pbing_cf = pbing, mu_cf = mu )), r = R, ra = ra)
  return(out)
  
}  # GENERATE ALTERNATIVE EXPOSURES AND CONSOLIDATE ANALYSIS DATASET

odata <- function(scenario, r0, recalibrate, bdm, bdf, lb, ub, data1, data2, data3, gl, burden) {
  edat <- edata(scenario = scenario, r0 = r0, recalibrate = recalibrate, data1 = data1, data2 = data2)
  pifout <- pif(pifdata = edat$expdata, bdm = bdm, bdf = bdf, lb = lb, ub = ub, gl = gl, radjust = edat$r*edat$ra)
  outdata <- cbind(edat$expdata, pif = pifout) 
  eyll <- cbind(data3, outdata %>% arrange(metric, gender, agecat) %>% dplyr::slice(1:nrow(data3)) %>% dplyr::select(alpha_cf,beta_cf,pdrink_cf,pform_cf,pbing_cf,mu_cf,curr,bing,cons,pif))  
  outdata <- rbind(outdata, eyll) %>% arrange(metric, gender, agecat) 
  if (burden == "gbd") {
    outdata$bod <- outdata$bod2
    outdata$abod <- outdata$abod2
  } 
  outdata$vbod <- outdata$bod*outdata$pif 
  outdata <- outdata[, c("gender","agecat","cat1","cat2","cause","metric","bod","abod","vbod")] 
  outdata[,c("bod","abod","vbod")] <- round(outdata[,c("bod","abod","vbod")],0)
  loutdata <- outdata %>%
    pivot_wider(
      names_from = metric,
      values_from = c(bod, abod, vbod)
    ) %>%
    mutate(bod_DALY =  bod_YLD + bod_YLL, abod_DALY = abod_YLD + abod_YLL, vbod_DALY = vbod_YLD + vbod_YLL) 
  return(loutdata)
  
}  # CALCULATE PIFS, VBOD



