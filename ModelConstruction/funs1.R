loopMain =
function(AD, cropname, days.no.irr, doys.model, ETo, fewi, fewp, fw, Jdev, Jharv, Kcb.adjusted, Kcmax, model.length, P, PAW, RDI.min, REW.parameter, TEW.parameter, Dei.end, Ir, Dei.initial, Dep.end, Dep.initial, Kri, Krp, W, DPep, DPei, Kei, Kep, Ep, Ei, Kc.ns, ETc.ns, Dr.end, Dr.initial, Ks, DPr, Kc.act, ETc.act)
{    
   for (i in 2:model.length) { #now for days 2...model.length after initialization
      Dei.initial[i] <- DeiInitialCalc(Dei.end, P, Ir, fw, i)
      Dep.initial[i] <- DepInitialCalc(Dep.end, P, i)
      Kri[i] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial, i)
      Krp[i] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial, i)
      W[i] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi, i)
      DPep[i] <- DPepCalc(P, Dep.initial, i)
      DPei[i] <- DPeiCalc(P, Ir, fw, Dei.initial, i)
      Kei[i] <- KeiCalc(Kri, W, Kcmax, Kcb.adjusted, fewi, TEW.parameter, Dei.end, DPei, P, ETo, i, Ir, fw)
      Kep[i] <- KepCalc(Krp, W, Kcmax, Kcb.adjusted, fewp, TEW.parameter, Dep.end, DPep, P, ETo, i)
      Ep[i] <- EpCalc(ETo, Kep, i)
      Ei[i] <- EiCalc(ETo, Kei, i)
      Dep.end[i] <- DepEndCalc(Dep.end, P, Ep, fewp, DPep, i)
      Dei.end[i] <- DeiEndCalc(Dei.end, P, Ir, fw, fewi, Ei, DPei, i)
      Kc.ns[i] <- KcnsCalc(Kcb.adjusted, Kei, Kep, i)
      ETc.ns[i] <- ETcnsCalc(Kc.ns, ETo, i)
      Dr.initial[i] <- DrInitialCalc(Dr.end, ETc.ns, P, Ir, i)
      Ks[i] <- KsCalc(Dr.initial=Dr.initial, PAW=PAW, i=i) #corrected on 10/18/17
      Ir[i] <- IrCalc(Ks, RDI.min, AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr, i, cropname)
      DPr[i] <- DPrCalc(P, Ir, ETc.ns, Dr.end, i)
      Kc.act[i] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep, i)
      Dr.end[i] <- DrEndCalc(Dr.end, P, Ir, Kc.act, ETo, DPr, i)
      ETc.act[i] <- ETcactCalc(Kc.act, ETo, i) #could take this out of loop
  }

  list(Dei.initial,
      Dep.initial,
      Kri,
      Krp,
      W,
      DPep,
      DPei,
      Kei,
      Kep,
      Ep,
      Ei,
      Dep.end,
      Dei.end,
      Kc.ns,
      ETc.ns,
      Dr.initial,
      Ks,
      Ir,
      DPr ,
      Kc.act,
      Dr.end,
      ETc.act) 
}
