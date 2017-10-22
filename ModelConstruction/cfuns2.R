if(TRUE) {
dyn.load("cimpl2.so")
xEiCalc =  getNativeSymbolInfo("EiCalc")
xKepCalc =  getNativeSymbolInfo("KepCalc")
xDepEndCalc =  getNativeSymbolInfo("DepEndCalc")
xDPepCalc =  getNativeSymbolInfo("DPepCalc")
}

EiCalc = EpCalc =
function(ETo, Kei, i)
  .Call(xEiCalc, ETo, Kei, as.integer(i))

DPepCalc =
function(P, Dep_end, i)
    .Call(xDPepCalc, P, Dep_end, as.integer(i))

DepEndCalc =
function(Dep_end, P, Ep, fewp, DPep, i)
   .Call(xDepEndCalc, Dep_end, P, Ep, fewp, DPep, as.integer(i))


KepCalc =
function(Krp, W, Kcmax, Kcb, fewp, TEW, Dep_end, DPep, P, ETo, i)
   .Call(xKepCalc, Krp, W, Kcmax, Kcb, fewp, TEW, Dep_end, DPep, P, ETo, as.integer(i))

