#include <Rdefines.h>

#include "rawcimpl.c"

SEXP
loopMain(SEXP r_AD, SEXP r_cropname, SEXP r_days_no_irr, SEXP r_doys_model, SEXP r_ETo, SEXP r_fewi, SEXP r_fewp, SEXP r_fw, SEXP r_Jdev, SEXP r_Jharv, SEXP r_Kcb_adjusted, SEXP r_Kcmax, SEXP r_model_length, SEXP r_P, SEXP r_PAW, SEXP r_RDI_min, SEXP r_REW_parameter, SEXP r_TEW_parameter, SEXP r_Dei_end, SEXP r_Ir, SEXP r_Dei_initial, SEXP r_Dep_end, SEXP r_Dep_initial, SEXP r_Kri, SEXP r_Krp, SEXP r_W, SEXP r_DPep, SEXP r_DPei, SEXP r_Kei, SEXP r_Kep, SEXP r_Ep, SEXP r_Ei, SEXP r_Kc_ns, SEXP r_ETc_ns, SEXP r_Dr_end, SEXP r_Dr_initial, SEXP r_Ks, SEXP r_DPr, SEXP r_Kc_act, SEXP r_ETc_act, SEXP harvest_days)
{
    int i, model_length = INTEGER(r_model_length)[0];

double * Dei_end = REAL(r_Dei_end);
double * P = REAL(r_P);
double * Ir = REAL(r_Ir);
double * fw = REAL(r_fw);
double * Dei_initial = REAL(r_Dei_initial);
double * Dep_end = REAL(r_Dep_end);
double * TEW_parameter = REAL(r_TEW_parameter);
double * REW_parameter = REAL(r_REW_parameter);
double * Dep_initial = REAL(r_Dep_initial);
double * fewp = REAL(r_fewp);
double * fewi = REAL(r_fewi);
double * Kri = REAL(r_Kri);
double * W = REAL(r_W);
double * Kcmax = REAL(r_Kcmax);
double * Kcb_adjusted = REAL(r_Kcb_adjusted);
double * DPei = REAL(r_DPei);
double * ETo = REAL(r_ETo);
double * Krp = REAL(r_Krp);
double * DPep = REAL(r_DPep);
double * Kei = REAL(r_Kei);
double * Kep = REAL(r_Kep);
double * Ep = REAL(r_Ep);
double * Dr_initial = REAL(r_Dr_initial);
double * Ks = REAL(r_Ks);
double * Ei = REAL(r_Ei);
double * PAW = REAL(r_PAW);
double * Kc_ns = REAL(r_Kc_ns);
double * ETc_ns = REAL(r_ETc_ns);
int * RDI_min = INTEGER(r_RDI_min);
int * doys_model = INTEGER(r_doys_model);
int * Jdev = INTEGER(r_Jdev);
int * Jharv = INTEGER(r_Jharv);
int * days_no_irr = INTEGER(r_days_no_irr);
const char * cropname = CHAR(STRING_ELT(r_cropname, 0));
double * Kc_act = REAL(r_Kc_act);
double * Dr_end = REAL(r_Dr_end);
double * AD = REAL(r_AD);
double * DPr = REAL(r_DPr);
double * ETc_act = REAL(r_ETc_act);
    
    for(i = 1; i < model_length; i++) {
      Dei_initial[i] = DeiInitialCalc(Dei_end, P, Ir, *fw, i);
      Dep_initial[i] = DepInitialCalc(Dep_end, P, i);
      Kri[i] = KrCalc(*TEW_parameter, *REW_parameter, Dei_initial, i);
      Krp[i] = KrCalc(*TEW_parameter, *REW_parameter, Dep_initial, i);
      W[i] = WCalc(*TEW_parameter, Dei_initial, Dep_initial, fewp, fewi, i);
      DPep[i] = DPepCalc(P, Dep_initial, i);
      DPei[i] = DPeiCalc(P, Ir, *fw, Dei_initial, i);
      Kei[i] = KeiCalc(Kri, W, Kcmax, Kcb_adjusted, fewi, *TEW_parameter, Dei_end, DPei, P, ETo, i, Ir, *fw);
      Kep[i] = KepCalc(Krp, W, Kcmax, Kcb_adjusted, fewp, *TEW_parameter, Dep_end, DPep, P, ETo, i);
      Ep[i] = EpCalc(ETo, Kep, i);
      Ei[i] = EiCalc(ETo, Kei, i);
      Dep_end[i] = DepEndCalc(Dep_end, P, Ep, fewp, DPep, i);
      Dei_end[i] = DeiEndCalc(Dei_end, P, Ir, *fw, fewi, Ei, DPei, i);
      Kc_ns[i] = KcnsCalc(Kcb_adjusted, Kei, Kep, i);
      ETc_ns[i] = ETcnsCalc(Kc_ns, ETo, i);
      Dr_initial[i] = DrInitialCalc(Dr_end, ETc_ns, P, Ir, i);
      Ks[i] = KsCalc(Dr_initial, *PAW, i) ;
      Ir[i] = IrCalc(Ks, *RDI_min, *AD, Dr_initial, doys_model, *Jdev, *Jharv, *days_no_irr, i, cropname, harvest_days);
      DPr[i] = DPrCalc(P, Ir, ETc_ns, Dr_end, i);
      Kc_act[i] = KcactCalc(Ks, Kcb_adjusted, Kei, Kep, i);
      Dr_end[i] = DrEndCalc(Dr_end, P, Ir, Kc_act, ETo, DPr, i);
      ETc_act[i] = ETcactCalc(Kc_act, ETo, i) ;
    }

    return(R_NilValue);
}
