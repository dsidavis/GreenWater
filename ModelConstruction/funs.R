  CropParametersDefine <- function(crop.parameters, cropname) {
    crop.parameters <- crop.parameters[which(crop.parameters$crop==cropname), ]  
    bloom.date <- strptime(paste0(as.character(crop.parameters$bloom.mo), '/', as.character(crop.parameters$bloom.day)), '%m/%d')
    if (cropname=='alfalfa.intermountain' | cropname=='alfalfa.CV' | cropname=='alfalfa.imperial') {
      crop.parameters$Jini <- as.integer(format.Date(bloom.date, '%j')) #this covers the first cutting
      crop.parameters$Jdev <- crop.parameters$Jini + crop.parameters$Lini
      crop.parameters$Jmid <- crop.parameters$Jdev + crop.parameters$Ldev
      crop.parameters$Jlate <- crop.parameters$Jmid + crop.parameters$Lmid
      crop.parameters$Jharv <- crop.parameters$Jlate + crop.parameters$Llate
      return(crop.parameters)
    } else { #these cover the orchard and vine crops
      crop.parameters$Jdev <- as.integer(format.Date(bloom.date, '%j'))
      crop.parameters$Jmid <- crop.parameters$Jdev + crop.parameters$Ldev
      crop.parameters$Jlate <- crop.parameters$Jmid + crop.parameters$Lmid
      crop.parameters$Jharv <- crop.parameters$Jlate + crop.parameters$Llate
      return(crop.parameters)
    }
}


  KcbDefine <- function(doy, crop, crop.parameters) { #find standard value of Kcb by day of year relative to three reference Kcb points defined by crop parameters
    parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
    if (crop=='alfalfa.intermountain' | crop=='alfalfa.CV' | crop=='alfalfa.imperial') {
        cycle.1.length <- sum(parameters[['Lini']], parameters[['Ldev']], parameters[['Lmid']], parameters[['Llate']])
        

    if (crop=='alfalfa.intermountain') {
        alfalfa.Kcb.cycle1(parameters[['Kcb.dorm']], alfalfa.Kcb.cycles(2, parameters[['cycle.length']], alfalfa.Kcb.cycles(3, parameters[['cycle.length']], winter.Kcb.alfalfa())))
      }
      else if (crop=='alfalfa.CV') {
        alfalfa.Kcb.cycle1(parameters[['Kcb.winter']], alfalfa.Kcb.cycles(2, parameters[['cycle.length']], alfalfa.Kcb.cycles(3, parameters[['cycle.length']], alfalfa.Kcb.cycles(4, parameters[['cycle.length']], alfalfa.Kcb.cycles(5, parameters[['cycle.length']], alfalfa.Kcb.cycles(6, parameters[['cycle.length']], CV.alfalfa.last(winter.Kcb.alfalfa())))))))
      } else { #crop is alfalfa.imperial
        alfalfa.Kcb.cycle1(Kcb.winter=NA, alfalfa.Kcb.cycles(2, parameters[['cycle.length']], alfalfa.Kcb.cycles(3, parameters[['cycle.length']], alfalfa.Kcb.cycles(4, parameters[['cycle.length']], alfalfa.Kcb.cycles(5, parameters[['cycle.length']], alfalfa.Kcb.cycles(6, parameters[['cycle.length']], alfalfa.Kcb.cycles(7, parameters[['cycle.length']], alfalfa.Kcb.cycles(8, parameters[['cycle.length']], alfalfa.Kcb.cycles(9, parameters[['cycle.length']], winter.Kcb.alfalfa(cycle.no = 10, final.else = winter.Kcb.alfalfa(cycle.no = 11, final.else=NA)))))))))))
      }
    } else {
        ifelse(doy < parameters[['Jdev']], parameters[['Kcb.dorm']], ifelse(doy < parameters[['Jmid']], parameters[['Kcb.ini']] + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), parameters[['Kcb.dorm']]))))
    }
}


        alfalfa.Kcb.cycle1 <- function(Kcb.winter, final.else) {
         if (crop=='alfalfa.intermountain') {
          ifelse(doy < parameters[['Jdev']], Kcb.winter, ifelse(doy < parameters[['Jmid']], Kcb.winter + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - Kcb.winter), ifelse(doy < parameters[['Jlate']] , parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
        }
        else if (crop=='alfalfa.CV') {
          ifelse(doy < parameters[['Jini']], Kcb.winter, ifelse(doy < parameters[['Jdev']] , Kcb.winter, ifelse(doy < parameters[['Jmid']], Kcb.winter + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - Kcb.winter), ifelse(doy < parameters[['Jlate']] , parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else)))))
        } else { #then covers Imperial valley production just before and after Jan cutting
            ifelse(doy < parameters[['Jini']] - parameters[['Llate']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jini']], parameters[['Kcb.mid']] + (doy - parameters[['Jini']] + parameters[['Llate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), ifelse(doy < parameters[['Jdev']] , parameters[['Kcb.ini']], ifelse(doy < parameters[['Jmid']], parameters[['Kcb.ini']] + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))))
        }
    }



alfalfa.Kcb.cycles <- function(cycle.no, cycle.length, final.else) {
        ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']] + parameters[['Ldev.cycle']], parameters[['Kcb.ini']] + (doy - parameters[['Jini']] - cycle.1.length - cycle.length*(cycle.no - 2) - parameters[['Lini.cycle']]) / parameters[['Ldev.cycle']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']] + parameters[['Ldev.cycle']] + parameters[['Lmid.cycle']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']] + parameters[['Ldev.cycle']] + parameters[['Lmid.cycle']] + parameters[['Llate.cycle']], parameters[['Kcb.mid']] + (doy - parameters[['Jini']] - cycle.1.length - cycle.length*(cycle.no - 2) - parameters[['Lini.cycle']] - parameters[['Ldev.cycle']] - parameters[['Lmid.cycle']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
    }





  CV.alfalfa.last <- function(final.else) {
        ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jini']] - cycle.1.length - parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Llate']], parameters[['Kcb.mid']] + (doy - parameters[['Jini']] - cycle.1.length - parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
    }


  winter.Kcb.alfalfa <- function(cycle.no=1, final.else=NA) { #could add dormancy reduction factor here
        if (crop=='alfalfa.CV') {
          ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jharv']] - cycle.1.length - (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.winter']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Llate']] + parameters[['Llate']], parameters[['Kcb.mid']] + (doy - parameters[['Jharv']] - cycle.1.length - parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / (parameters[['Llate']] + parameters[['Llate']]) * (parameters[['Kcb.winter']] - parameters[['Kcb.mid']]), parameters[['Kcb.winter']]))))
        }
        else if (crop=='alfalfa.intermountain') {
          ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Lfrost.kill']], parameters[['Kcb.mid']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / parameters[['Lfrost.kill']] * (parameters[['Kcb.dorm']] - parameters[['Kcb.mid']]), parameters[['Kcb.dorm']]))))
        } else { #this covers Imperial Valley
          ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] - (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Llate']], parameters[['Kcb.mid']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] - (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
        }
    }











  KcbAdj <- function(Kcb.daily, crop.parameters, crop, U2, RHmin){
    height <- crop.parameters$height[which(crop.parameters$crop==crop)]
    Kcb.climate.adj <- Kcb.daily + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3) ^ 0.3
    Kcb.climate.adj[which(Kcb.climate.adj < 0)] <- 0
    Kc.max <- pmax(1.2 + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3) ^ 0.3, Kcb.climate.adj + 0.05)
    return(as.data.frame(cbind(Kcb.climate.adj, Kc.max)))
  }
  #calculate fraction of cover ('fc') for specific day of year relative to three points defined in crop parameters
  fcCalc <- function(doy, crop.parameters, crop) {
    parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
    if (crop == 'alfalfa.CV' | crop == 'alfalfa.imperial') {
      Kcb.std/parameters[['Kcb.mid']]
    }
    else if (crop == 'alfalfa.intermountain') {
      ifelse(doy < parameters[['Jdev']], 0, ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Lfrost.kill']], Kcb.std/parameters[['Kcb.mid']], 0))
    } else {
        ifelse(doy < parameters[['Jdev']], parameters[['fc.ini']], ifelse(doy < parameters[['Jmid']], parameters[['fc.ini']] + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['fc.mid']] - parameters[['fc.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['fc.mid']], ifelse(doy < parameters[['Jharv']], parameters[['fc.mid']] + (doy-parameters[['Jlate']]) / parameters[['Llate']] * (parameters[['fc.end']] - parameters[['fc.mid']]), parameters[['fc.ini']]))))
    }
  }
  #calculate fraction of soil wetted by both irrigation and precipitation and exposed to rapid drying (fewi) and fraction of soil exposed to rapid drying and is wetted by precipitation only (fewp).  These are dependent upon fraction of cover calculation above
  fwSelect <- function(irr.parameters, irr.type) {
    irr.parameters$fw[which(irr.parameters$irrigation.type==irr.type)]
  }
  fewiCalc <- function(fc, fw) { #see p.147 of FAO-56 Chp.7 re: drip
    fewi.temp <- pmin(1 - fc, fw)
    fewi.temp[which(fewi.temp < 0.001)] <- 0.001 #lower limit on fewi for numeric stability
    fewi.temp[which(fewi.temp > 1)] <- 1 #upper limit on fewi for numeric stability
    return(fewi.temp)
  }
  fewpCalc <- function(fc, fewi) {
    fewp.temp <- 1 - fc - fewi
    fewp.temp[which(fewp.temp < 0.001)] <- 0.001 #lower limit on fewp for numeric stability
    fewp.temp[which(fewp.temp > 1)] <- 1 #upper limit on fewp for numeric stability
    return(fewp.temp)
  }
  # TEWCalc <- function(ThetaFC, ThetaWP, REW, Ze=0.10) { #Ze is depth of upper soil 
  #   #layer where evaporation occurs in meters
  #   result <- 1000 * (ThetaFC - 0.5 * ThetaWP) * Ze
  #   if (result < REW) {
  #     stop(print('Hay una problema con TEW')) #could change to next and print or 
  #     #save model code
  #   } 
  #   return(result)
  # }
  DepInitialCalc <- function(Dep.end, P, i) {
    max(Dep.end[i-1] - P[i], 0) # DON'T RUN THIS IF i=1
  }
  DeiInitialCalc <- function(Dei.end, P, Ir, fw, i) { #DON'T RUN THIS WHEN i=1
    max(Dei.end[i - 1] - P[i] - Ir[i - 1] / fw, 0) #have to use irrigation from 
    #previous day because current day irrigation decision is dependent on this calc
  }
  KrCalc <- function(TEW, REW, De.initial, i) { #can be used to calculate Kri or Krp
    max(0, if(De.initial[i] < REW) { #could initialize this in vector above
      1
    } else if(TEW==REW) {
      0
    } else {
      (TEW - De.initial[i]) / (TEW - REW)
    })
  }
  WCalc <- function(TEW, Dei.initial, Dep.initial, fewp, fewi, i) {
    1 / (1 + (fewp[i] / fewi[i]) * max(TEW - Dep.initial[i], 0.001) / max((TEW - Dei.initial[i]), 0.001))
  }
  KeiCalc <- function(Kri, W, Kcmax, Kcb, fewi, TEW, Dei.end, DPei, P, ETo, i, Ir, fw) {
    Kei.est <- min(Kri[i] * W[i] * (Kcmax[i] - Kcb[i]), fewi[i] * Kcmax[i])
    #print(Kei.est)
    TEW.check <- max(Dei.end[i - 1] - P[i] - Ir[i - 1] / fw + Kei.est * ETo[i] / fewi[i] + DPei[i], 0)
    #print(TEW.check)
    if (TEW.check > TEW) {
      return(fewi[i] * (TEW - Dei.end[i - 1] + P[i] + Ir[i - 1] / fw - DPei[i]) / ETo[i]) #as revised Kep estimate
    } else {
      return(Kei.est)
    }
  }
  KepCalc <- function(Krp, W, Kcmax, Kcb, fewp, TEW, Dep.end, DPep, P, ETo, i) {
    Kep.est <- min(Krp[i] * (1 - W[i]) * (Kcmax[i] - Kcb[i]), fewp[i] * Kcmax[i])
    #print(Kep.est)
    TEW.check <- max(Dep.end[i - 1] - P[i] + (Kep.est * ETo[i]) / fewp[i] + DPep[i], 0)
    #print(TEW.check)
    if (TEW.check > TEW) {
      return(fewp[i] * (TEW - Dep.end[i - 1] + P[i] - DPep[i]) / ETo[i]) #as revised Kep estimate
    } else {
        return(Kep.est)
    }
  }
  EpCalc <- function(ETo, Kep, i) {
    ETo[i] * Kep[i]
  }
  EiCalc <- function(ETo, Kei, i) {
    ETo[i] * Kei[i]
  }
  DPepCalc <- function(P, Dep.end, i) { #DON'T RUN THIS FOR i=1
    max(P[i] - Dep.end[i-1], 0)
  }
  DepEndCalc <- function(Dep.end, P, Ep, fewp, DPep, i) { #DON'T RUN THIS FOR i=1
    Dep.end.est <- max(Dep.end[i - 1] - P[i] + Ep[i] / fewp[i] + DPep[i], 0)
  } #ignores transpiration and runoff from upper layer
  DPeiCalc <- function(P, Ir, fw, Dei.end, i) { #DON'T RUN THIS FOR i=1
    max(0, P[i] + Ir[i - 1] / fw - Dei.end[i-1])
  }
  DeiEndCalc <- function(Dei.end, P, Ir, fw, fewi, Ei, DPei, i) { #DON'T RUN THIS FOR i=1
    max(Dei.end[i - 1] - P[i] - Ir[i - 1] / fw + Ei[i] / fewi[i] + DPei[i], 0)
  } #again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21)
  KcnsCalc <- function(Kcb, Kei, Kep, i) {
    Kcb[i] + Kei[i] + Kep[i]
  }
  ETcnsCalc <- function(Kc.ns, ETo, i) {
    Kc.ns[i] * ETo[i]
  }
  DrInitialCalc <- function(Dr.end, ETc.ns, P, Ir, i) { #DON'T RUN THIS FOR i=1
    Dr.end[i - 1] - P[i] - Ir[i-1] + ETc.ns[i]
}


  DaysNoIrr <- function(P, ETo, Kcb.adjusted, AD, doys.model, years, Jmid, Jharv, cropname, buffer.days=30) {
    if (cropname=='alfalfa.imperial') {
      return(0)
    }
    else if (cropname == 'alfalfa.CV' | cropname == 'alfalfa.intermountain') {
      last.irr.possible <- harvest.days[length(harvest.days)] + buffer.days
      df <- data.frame(cbind(Kcb.adjusted, ETo, P, doys.model, years))
      df <- df[which(df$doys.model >= 200 & df$doys.model <= last.irr.possible), ]
      df$ETcb <- df$Kcb.adjusted * df$ETo
      daily.ETcb <- tapply(df$ETcb, df$doys.model, mean)
      daily.P <- tapply(df$P, df$doys.model, mean)
      daily.WB <- daily.ETcb - daily.P
      for (i in 1:length(daily.WB)) {
        if (sum(daily.WB) < AD) {
          last.irr <- as.integer(names(daily.WB[1]))
          while(last.irr %in% harvest.days) {
            last.irr <- last.irr + 1
          }
          return(last.irr.possible - last.irr)
        } else {
            daily.WB <- daily.WB[-1]
        }
      }
    }
    else if (cropname=='grapes.wine') {
      return(30)
    } else {
      df <- data.frame(cbind(Kcb.adjusted, ETo, P, doys.model, years))
      df <- df[which(df$doys.model >= Jmid & df$doys.model <= Jharv), ]
      df$ETcb <- df$Kcb.adjusted * df$ETo
      daily.ETcb <- tapply(df$ETcb, df$doys.model, mean)
      daily.P <- tapply(df$P, df$doys.model, mean)
      daily.WB <- daily.ETcb - daily.P
      for (i in 1:length(daily.WB)) {
        if (sum(daily.WB) < AD) {
          return(Jharv - as.integer(names(daily.WB[1])))
        } else {
          daily.WB <- daily.WB[-1]
        }
      }
    }
}


  GrapeLastIrr <- function() {
    if (cropname == 'grapes.wine') {
      df <- data.frame(cbind(Kcb.adjusted, ETo, P, doys.model))
      df <- df[which(df$doys.model >= Jharv - days.no.irr & df$doys.model <= Jharv), ]
      df$ETcb <- df$Kcb.adjusted * df$ETo
      daily.ETcb <- tapply(df$ETcb, df$doys.model, mean)
      daily.P <- tapply(df$P, df$doys.model, mean)
      last.irr <- Dr.initial[i] + sum(daily.ETcb) - sum(daily.P) - AD
      if (last.irr > Dr.initial[i]) {
        return(Dr.initial[i])
      } else {
        return(max(last.irr, 0))
      }
    }
}


  HarvestDays <- function(crop) { #assume 4 day buffer on either end of each harvest
    if (crop == 'alfalfa.imperial') {
      parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
      harvest.dates <- c(parameters[['Jini']], seq(from=parameters[['Jharv']], to=parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']]-3), by=parameters[['cycle.length']]), parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']]-3) + (parameters[['Jharv']] - parameters[['Jini']]))
      harvest.dates.all <- c(harvest.dates, harvest.dates-1, harvest.dates-2, harvest.dates-3, harvest.dates-4, harvest.dates+1, harvest.dates+2, harvest.dates+3, harvest.dates+4)
      harvest.dates.all[order(harvest.dates.all)]
    }
    else if (crop == 'alfalfa.CV') {
      parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
      harvest.dates <- c(seq(from=parameters[['Jharv']], to=parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2), by=parameters[['cycle.length']]), parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + (parameters[['Jharv']] - parameters[['Jini']]))
      harvest.dates.all <- c(harvest.dates, harvest.dates - 1, harvest.dates - 2, harvest.dates - 3, harvest.dates - 4, harvest.dates + 1, harvest.dates + 2, harvest.dates + 3, harvest.dates + 4)
      harvest.dates.all[order(harvest.dates.all)]
    } else { #this is for intermountain alfalfa
      parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
      harvest.dates <- seq(from=parameters[['Jharv']], to=parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']], by=parameters[['cycle.length']])
      harvest.dates.all <- c(harvest.dates, harvest.dates - 1, harvest.dates - 2, harvest.dates - 3, harvest.dates - 4, harvest.dates + 1, harvest.dates + 2, harvest.dates + 3, harvest.dates + 4)
      harvest.dates.all[order(harvest.dates.all)]
    }
}


  IrCalc <- function(Ks, RDI.min, AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr, i, cropname, buffer.days=30) {
    if (cropname == 'alfalfa.imperial') {
      if (Dr.initial[i] > AD & !(doys.model[i] %in% harvest.days)) {
        return(Dr.initial[i])
      } else {
          return(0)
        }
    } else if (cropname == 'alfalfa.CV' | cropname == 'alfalfa.intermountain') {
        if (doys.model[i] < crop.parameters$Jini[which(crop.parameters$crop==cropname)] | doys.model[i] %in% harvest.days | doys.model[i] > harvest.days[length(harvest.days)] + buffer.days - days.no.irr) {
          return(0)
        } else if(Dr.initial[i] > AD | doys.model[i] == harvest.days[length(harvest.days)] + buffer.days - days.no.irr) {
        return(Dr.initial[i])
      } else {
          return(0)
      }
    } else if (doys.model[i] < Jdev | doys.model[i] > Jharv - days.no.irr) {
      return(0)
    } else if (cropname == 'grapes.wine' & doys.model[i] < Jharv - days.no.irr & Ks[i] < RDI.min) {
      return(Dr.initial[i] - AD)
    } else if (Dr.initial[i] > AD & cropname != 'grapes.wine') {
      return(Dr.initial[i])
    } else if (doys.model[i] == Jharv - days.no.irr & cropname=='grapes.wine') {
      return(GrapeLastIrr())
    } else if (doys.model[i] == Jharv - days.no.irr) {
      return(Dr.initial[i])
    } else {
      return(0)
    }
}


  DPrCalc <- function(P, Ir, ETc.ns, Dr.end, i) { #DON'T RUN THIS FOR i=1
    max(P[i] + Ir[i-1] - ETc.ns[i] - Dr.end[i - 1], 0)
  }
  KsCalc <- function(Dr.initial, PAW, stress.point=0.5*PAW, i) {
    if (Dr.initial[i] > stress.point) {
      (PAW - Dr.initial[i])/(PAW - stress.point)
    } else { 
      return(1)
    }
  }
  KcactCalc <- function(Ks, Kcb, Kei, Kep, i) {#equation 4 in Allen et al. 2005
    Ks[i] * Kcb[i] + Kei[i] + Kep[i]
  }
  ETcactCalc <- function(Kc.act, ETo, i) {#equation 3 in Allen et al. 2005
    Kc.act[i] * ETo[i]
  }
  DrEndCalc <- function(Dr.end, P, Ir, Kc.act, ETo, DPr, i) { #NOT TO BE USED for i=1
    Dr.end[i - 1] - P[i] - Ir[i-1] + Kc.act[i] * ETo[i] + DPr[i]
  }



  #results function to summarize each model's results
  #find time to first and last irrigation
  IrDateCalc <- function(df, days.no.irr, Jdev, Jharv) { #as.Date('1900-01-01) is a proxy for NA
    first.irr.index <- if (length(which(df$Ir >0)) > 1 & df$doys.model[1] <= Jdev) {
      head(which(df$Ir > 0), 1)} else {
        if (length(which(df$Ir >0)) == 1 & df$doys.model[1] <= Jdev) {
          which(df$Ir > 0)} else {vector()}
      }
    last.irr.index <- if (length(which(df$Ir >0)) > 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
      tail(which(df$Ir > 0), 1)} else {
        if (length(which(df$Ir >0)) == 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
          which(df$Ir > 0)} else {vector()}
      }
    if (length(first.irr.index) == 0 & length(last.irr.index) == 0) {
      data.frame(Irr.1=as.Date('1900-01-01'), Irr.Last=as.Date('1900-01-01'))
    }
    else if (length(first.irr.index) == 0) {
      data.frame(Irr.1=as.Date('1900-01-01'), Irr.Last=df$dates[last.irr.index])
    }
    else if (length(last.irr.index) == 0) {
      data.frame(Irr.1=df$dates[first.irr.index], Irr.Last=as.Date('1900-01-01'))
    } else {
      data.frame(Irr.1=df$dates[first.irr.index], Irr.Last=df$dates[last.irr.index])
    }
}



  #calculate Green Water utilized
  #this asssumes that residual irrigation water storage from previous fall will not contribute to the following year's growing season ET for the purpose of calculating green water utilization.  However a correction is applied to the growing season ET for residual irrigation water storage to correctly estimate green water utilization within the same year.
  WaterBalanceCalc <- function(df, AD, days.no.irr, Jdev, Jharv, PAW) { #concept is to run by year on a data.frame trimmed to Jdev-Jharv each year
    #df <- model.result[which(model.result$years==2004),]
    last.irr.index <- if (length(which(df$Ir >0)) > 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
      tail(which(df$Ir > 0), 1)} else {
        if (length(which(df$Ir >0)) == 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
          which(df$Ir > 0)} else {vector()}
      }
    jharv_index <- which(df$doys.model==Jharv)
    jdev_index <- which(df$doys.model==Jdev)
    if (df$doys.model[1] > Jdev | df$doys.model[nrow(df)] < Jharv) { #if there is not a complete growing season, don't report any data
      if (length(jharv_index)==0) { #data begins after leaf-drop or ends before leaf-drop; either way, can't get entire season or end season data
        data.frame(RAW.end.season = NA, PAW.end.season = NA, Dr.end.season = NA, P.end.season=NA, Irr.end.storage = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA, H2O.stress=NA)
      }
      else if (length(last.irr.index)==0) { #implies no data when last irrigation occurred but there is data at Jharv; thus can get end of season storage data
        data.frame(RAW.end.season = max(AD - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = NA, Irr.end.storage = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA, H2O.stress=NA)
      } else { #implies data exists from at least when last irrigation occurred to Jharv, so can get everything but entire growing season data
        data.frame(RAW.end.season = max(AD - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = sum(df$P[last.irr.index:jharv_index]), Irr.end.storage = max(AD - df$Dr.end[jharv_index] - sum(df$P[last.irr.index:jharv_index]), 0), GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = df$Ir[last.irr.index], ET.growing = NA, E.growing = NA, T.growing = NA, H2O.stress=NA)
      }
    } else { #entire season's coverage available
      if (length(last.irr.index)==0) { #but no final irrigation occurred
        data.frame(RAW.end.season = max(AD - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = NA, Irr.end.storage = NA, GW.ET.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ir[jdev_index:jharv_index]), Irr.app.total = sum(df$Ir), Irr.app.last = NA, ET.growing = sum(df$ETc.act[jdev_index:jharv_index]), E.growing = sum(df$Ei[jdev_index:jharv_index], df$Ep[jdev_index:jharv_index]), T.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ei[jdev_index:jharv_index] - df$Ep[jdev_index:jharv_index]), H2O.stress=sum(df$ETc.ns - df$ETc.act))
      } else {
        irr_storage <- max(AD - df$Dr.end[jharv_index] - sum(df$P[last.irr.index:jharv_index]), 0)
      data.frame(RAW.end.season = max(AD - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = sum(df$P[last.irr.index:jharv_index]), Irr.end.storage = irr_storage, GW.ET.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ir[jdev_index:jharv_index]) + irr_storage, Irr.app.total = sum(df$Ir), Irr.app.last = df$Ir[last.irr.index], ET.growing = sum(df$ETc.act[jdev_index:jharv_index]), E.growing = sum(df$Ei[jdev_index:jharv_index], df$Ep[jdev_index:jharv_index]), T.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ei[jdev_index:jharv_index] - df$Ep[jdev_index:jharv_index]), H2O.stress=sum(df$ETc.ns - df$ETc.act))
      }
    }
}



  ##this splits the overall results data.frame into subsets by year and then runs the GreenWaterCalc on each subset via lapply.  The result from each year is then bound together via rbind called by do.call; AD minus Dr.end at leaf-drop is the readily available water remaining in storage at leaf-drop; the source of this readily available water is minus precip since the last irrigation is Irr.End.Storage
  GreenWaterIrr1Calc <- function(df, Jdev) { #works on a data.frame split by year
    #df <- model.result[model.result$years==2004,]
    first.irr.index <- if (length(which(df$Ir >0)) > 1 & df$doys.model[1] <= Jdev) {
      head(which(df$Ir > 0), 1)} else {
        if (length(which(df$Ir >0)) == 1 & df$doys.model[1] <= Jdev) {
          which(df$Ir > 0)} else {vector()}
      }
    jdev_index <- which(df$doys.model==Jdev)
    #print(df$years[1])
    if (df$doys.model[1] > Jdev | length(first.irr.index)==0) {
      data.frame(GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA)
    } else {
      data.frame(GW.ET.to.Irr1 = sum(df$ETc.act[jdev_index:first.irr.index]), GW.E.to.Irr1 = sum(df$Ei[jdev_index:first.irr.index] + df$Ep[jdev_index:first.irr.index]), GW.T.to.Irr1 = sum(df$Kcb.adjusted[jdev_index:first.irr.index] * df$Ks[jdev_index:first.irr.index] * df$ETo[jdev_index:first.irr.index]))
    }
  }
      #do.call(rbind, lapply(split(model.result, model.result$years), GreenWaterIrr1Calc))



  #determine deep percolation and annual water balance using subsetting by year
  DeepPercCalc <- function(df, days.no.irr, Jdev, Jharv) { #assumes Jharv index is after 10/1
    #df <- model.result[which(model.result$years==2017), ]
    #print(df$years[1])
    jan.1.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-01-01')))
    first.irr.index <- if (length(which(df$Ir >0)) > 1 & df$doys.model[1] <= Jdev) {
      head(which(df$Ir > 0), 1)} else {
        if (length(which(df$Ir >0)) == 1 & df$doys.model[1] <= Jdev) {
          which(df$Ir > 0)} else {vector()}
      }
    last.irr.index <- if (length(which(df$Ir >0)) > 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
      tail(which(df$Ir > 0), 1)} else {
        if (length(which(df$Ir >0)) == 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
          which(df$Ir > 0)} else {vector()}
      }
    jul.1.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-07-01')))
    dec.31.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-12-31')))
    #print(df$years[1])
    if (df$dates[1] > as.Date(paste0(as.character(df$years[1]), '-01-01')) | df$dates[nrow(df)] < as.Date(paste0(as.character(df$years[nrow(df)]), '-12-31'))) { #this means they ain't a whole year's data
      if (length(last.irr.index) == 0 & length(first.irr.index) == 0) { #not sufficient coverage to calc anything
        data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=NA, fall.deep.perc = NA)
      }
      else if (length(jan.1.index) != 0 & length(first.irr.index) != 0 & length(jul.1.index) == 0) { #winter deep perc only
        data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc=NA, fall.deep.perc = NA)
      }
      else if (length(jan.1.index) != 0 & length(jul.1.index) != 0 & length(dec.31.index) == 0) { #winter and post Irr deep perc
        data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc=if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])}else{NA}, fall.deep.perc = NA)
      }
      else if (length(jan.1.index) == 0 & length(first.irr.index) != 0 & length(jul.1.index) != 0 & length(last.irr.index) == 0) { #only Spring deep perc available
        data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])}else{NA}, fall.deep.perc = NA)
      }
      else if (length(jan.1.index) == 0 & length(first.irr.index) != 0 & length(dec.31.index) != 0) { #spring and fall deep perc
        data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])}else{NA}, fall.deep.perc =  sum(df$DPr[last.irr.index:dec.31.index]))
      } else {#fall perc only
        data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=NA,  fall.deep.perc =  sum(df$DPr[last.irr.index:(dec.31.index-1)]))
      }
    } else { #entire annual coverage available
      if (length(first.irr.index)==0 & length(last.irr.index)==0) { #but no irrigations
        data.frame(ET.annual = sum(df$ETc.act), E.annual = sum(df$Ei, df$Ep), T.annual = sum(df$ETc.act, -df$Ei, -df$Ep), deep.perc.annual = sum(df$DPr), winter.deep.perc = NA, post.Irr1.deep.perc = NA, fall.deep.perc = NA)
      } else {
        data.frame(ET.annual = sum(df$ETc.act), E.annual = sum(df$Ei, df$Ep), T.annual = sum(df$ETc.act, -df$Ei, -df$Ep), deep.perc.annual = sum(df$DPr), winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc = if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])}else{NA}, fall.deep.perc = sum(df$DPr[last.irr.index:(dec.31.index)]))
      }
    }
  }
      #do.call(rbind, lapply(split(model.result, model.result$years), DeepPercCalc))



  #determine green water capture from leaf-drop to flowering
  GreenWaterCaptureCalc <- function(df, Jdev, Jharv, Jmid, cropname) {
    if (cropname=='alfalfa.imperial') {
      return(data.frame(GW.capture.net = NA))
    } else if (df$doys.model[1] > Jharv | df$doys.model[nrow(df)] < Jmid) {
        return(data.frame(GW.capture.net = NA))
    } else {
        return(data.frame(GW.capture.net = df$Dr.end[which(df$doys.model == Jharv)] - df$Dr.end[which(df$doys.model == Jdev)]))
    }
}

