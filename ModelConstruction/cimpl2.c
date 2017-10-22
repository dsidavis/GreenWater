#define MAX(a, b)  (a) < (b) ? (b) : (a)
#define MIN(a, b)  (a) < (b) ? (a) : (b)

#include <Rdefines.h>

// EpCalc is the same as EiCalc.
SEXP
EiCalc(SEXP ETo, SEXP Kei, SEXP i)
{
    return(ScalarReal( REAL(ETo)[INTEGER(i)[0]] * REAL(Kei)[INTEGER(i)[0]]));
}

SEXP
DPepCalc(SEXP P, SEXP Dep_end, SEXP i)
{
    return(ScalarReal(MAX(REAL(P)[INTEGER(i)[0]] - REAL(Dep_end)[INTEGER(i)[0]-1], 0)));
}

SEXP
DepEndCalc(SEXP Dep_end, SEXP P, SEXP Ep, SEXP fewp, SEXP DPep, SEXP i)
{
    return(ScalarReal(MAX(REAL(Dep_end)[INTEGER(i)[0] - 1] - REAL(P)[INTEGER(i)[0]] + REAL(Ep)[INTEGER(i)[0]]/REAL(fewp)[INTEGER(i)[0]] + REAL(DPep)[INTEGER(i)[0]], 0)));
}

SEXP
KepCalc(SEXP Krp, SEXP W, SEXP Kcmax, SEXP Kcb, SEXP fewp, SEXP TEW, SEXP Dep_end, SEXP DPep, SEXP P, SEXP ETo, SEXP i)
{
    double Kep_est = MIN(REAL(Krp)[INTEGER(i)[0]] * (1 - REAL(W)[INTEGER(i)[0]]) *(REAL(Kcmax)[INTEGER(i)[0]] - REAL(Kcb)[INTEGER(i)[0]]), REAL(fewp)[INTEGER(i)[0]] * REAL(Kcmax)[INTEGER(i)[0]]);
    double TEW_check = MAX(REAL(Dep_end)[INTEGER(i)[0]-1] - REAL(P)[INTEGER(i)[0]] + (Kep_est * REAL(ETo)[INTEGER(i)[0]]) / REAL(fewp)[INTEGER(i)[0]] + REAL(DPep)[INTEGER(i)[0]], 0);
    double ans;
    if(TEW_check > *REAL(TEW))
	ans = REAL(fewp)[INTEGER(i)[0]] * (*REAL(TEW) - REAL(Dep_end)[INTEGER(i)[0]-1] + REAL(P)[INTEGER(i)[0]] - REAL(DPep)[INTEGER(i)[0]])/REAL(ETo)[INTEGER(i)[0]];
    else
	ans = Kep_est;
    return(ScalarReal(ans));
}


SEXP
KeiCalc(SEXP Kri, SEXP W, SEXP Kcmax, SEXP Kcb, SEXP fewi, SEXP TEW, SEXP Dei_end, SEXP DPei, SEXP P, SEXP ETo, SEXP i, SEXP Ir, SEXP fw, SEXP ans)
{    
    double Kei_est =  MIN(REAL(Kri)[INTEGER(i)[0]] * REAL(W)[INTEGER(i)[0]] * (REAL(Kcmax)[INTEGER(i)[0]] - REAL(Kcb)[INTEGER(i)[0]]), REAL(fewi)[INTEGER(i)[0]] * REAL(Kcmax)[INTEGER(i)[0]]);
    double TEW_check = MAX(REAL(Dei_end)[INTEGER(i)[0] - 1] - REAL(P)[INTEGER(i)[0]] - REAL(Ir)[INTEGER(i)[0] - 1] / *REAL(fw) + Kei_est * REAL(ETo)[INTEGER(i)[0]] / REAL(fewi)[INTEGER(i)[0]] + REAL(DPei)[INTEGER(i)[0]], 0);

    if (TEW_check > *REAL(TEW) )
	return(ScalarReal(REAL(fewi)[INTEGER(i)[0]] * (*REAL(TEW) - REAL(Dei_end)[INTEGER(i)[0] - 1] + REAL(P)[INTEGER(i)[0]] + REAL(Ir)[INTEGER(i)[0] - 1] / *REAL(fw) - REAL(DPei)[INTEGER(i)[0]]) / REAL(ETo)[INTEGER(i)[0]]));
     else 
	 return(ScalarReal(Kei_est));
}
