if(TRUE) {
dyn.load("cimpl.so")
xEiCalc =  getNativeSymbolInfo("EiCalc")
xKepCalc =  getNativeSymbolInfo("KepCalc")
xDepEndCalc =  getNativeSymbolInfo("DepEndCalc")
xDPepCalc =  getNativeSymbolInfo("DPepCalc")
}

EiCalc = EpCalc =
function(ETo, Kei, i)
   .C(xEiCalc, ETo, Kei, as.integer(i), ans = numeric(1))$ans

DPepCalc =
function(P, Dep_end, i)
    .C(xDPepCalc, P, Dep_end, as.integer(i), ans = numeric(1))$ans

DepEndCalc =
function(Dep_end, P, Ep, fewp, DPep, i)
   .C(xDepEndCalc, Dep_end, P, Ep, fewp, DPep, as.integer(i), ans = numeric(1))$ans


KepCalc =
function(Krp, W, Kcmax, Kcb, fewp, TEW, Dep_end, DPep, P, ETo, i)
   .C(xKepCalc, Krp, W, Kcmax, Kcb, fewp, TEW, Dep_end, DPep, P, ETo, as.integer(i), ans = numeric(1))$ans

