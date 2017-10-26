#define MAX(a, b)  (a) < (b) ? (b) : (a)
#define MIN(a, b)  (a) < (b) ? (a) : (b)

// EpCalc is the same as EiCalc.
#define EpCalc EiCalc

#include <Rdefines.h>

/*

x DeiEndCalc
x DeiInitialCalc
x DepEndCalc
x DepInitialCalc
x DPeiCalc
x DPepCalc
x DPrCalc
x DrEndCalc
x DrInitialCalc
x EiCalc
x EpCalc
x ETcactCalc
x ETcnsCalc
IrCalc
x KcactCalc
x KcnsCalc
x KeiCalc
x KepCalc
x KrCalc
x KsCalc
x WCalc
*/

double
EiCalc(double *ETo, double *Kei, int i)
{
    return(ETo[i] * Kei[i]);
}

double
DPepCalc(double *P, double *Dep_end, int i)
{
    return(MAX(P[i] - Dep_end[i-1], 0));
}

double
DepEndCalc(double *Dep_end, double *P, double *Ep, double *fewp, double *DPep, int i)
{
    return MAX(Dep_end[i - 1] - P[i] + Ep[i]/fewp[i] + DPep[i], 0);
}

double
KepCalc(double *Krp, double *W, double *Kcmax, double *Kcb, double *fewp, double TEW, double *Dep_end, double *DPep, double *P, double *ETo, int i)
{
    double Kep_est = MIN(Krp[i] * (1 - W[i]) *(Kcmax[i] - Kcb[i]), fewp[i] * Kcmax[i]);
    double TEW_check = MAX(Dep_end[i-1] - P[i] + (Kep_est * ETo[i]) / fewp[i] + DPep[i], 0);
    if(TEW_check > TEW)
	return fewp[i] * (TEW - Dep_end[i-1] + P[i] - DPep[i])/ETo[i];
    else
	return  Kep_est;
}


double
KeiCalc(double *Kri, double *W, double *Kcmax, double *Kcb, double *fewi, double TEW, double *Dei_end, double *DPei, double *P, double *ETo, int i, double *Ir, double fw)
{    
    double Kei_est =  MIN(Kri[i] * W[i] * (Kcmax[i] - Kcb[i]), fewi[i] * Kcmax[i]);
    double TEW_check = MAX(Dei_end[i - 1] - P[i] - Ir[i - 1] / fw + Kei_est * ETo[i] / fewi[i] + DPei[i], 0);

    if (TEW_check > TEW) 
	return fewi[i] * (TEW - Dei_end[i - 1] + P[i] + Ir[i - 1] / fw - DPei[i]) / ETo[i];
     else 
	return Kei_est;

}

double
DeiEndCalc(double *Dei_end, double *P, double *Ir, double fw, double *fewi, double *Ei, double *DPei, int i) {
    return(MAX(Dei_end[i - 1] - P[i] - Ir[i - 1] / fw + Ei[i] / fewi[i] + DPei[i], 0));
} 

double
WCalc(double TEW, double *Dei_initial, double *Dep_initial, double *fewp, double *fewi, int i) {
    return( 1. / (1. + (fewp[i] / fewi[i]) * MAX(TEW - Dep_initial[i], 0.001) / MAX((TEW - Dei_initial[i]), 0.001)) );
}


double
KrCalc(double TEW, double REW, double *De_initial, int i)
{
    double v;
    if(De_initial[i] < REW) { 
	v = 1;
    } else if(TEW==REW) {
	v = 0;
    } else {
	v = (TEW - De_initial[i]) / (TEW - REW);
    }
    return(MAX(0, v));
}


double
KsCalc(double *Dr_initial, double PAW, int i)
{
    double stress_point=0.5*PAW ;
    if(Dr_initial[i] > stress_point) 
	return((PAW - Dr_initial[i])/(PAW - stress_point));
    else 
	return(1.);
}

double
DeiInitialCalc(double *Dei_end, double *P, double *Ir, double fw, int i)
{ 
    return(MAX(Dei_end[i - 1] - P[i] - Ir[i - 1] / fw, 0)) ;
}

double
DepInitialCalc(double *Dep_end, double *P, int i) {
    return(MAX(Dep_end[i-1] - P[i], 0));
}

double
DPeiCalc(double *P, double *Ir, double fw, double *Dei_end, int i) {
    return(MAX(0, P[i] + Ir[i - 1] / fw - Dei_end[i-1]));
}

double
DPrCalc(double *P, double *Ir, double *ETc_ns, double *Dr_end, int i) { 
    return(MAX(P[i] + Ir[i-1] - ETc_ns[i] - Dr_end[i - 1], 0));
}

double
DrEndCalc(double *Dr_end, double *P, double *Ir, double *Kc_act, double *ETo, double *DPr, int i) { 
    return( Dr_end[i - 1] - P[i] - Ir[i-1] + Kc_act[i] * ETo[i] + DPr[i]);
}


double
DrInitialCalc(double *Dr_end, double *ETc_ns, double *P, double *Ir, int i) {
    return(Dr_end[i - 1] - P[i] - Ir[i-1] + ETc_ns[i]);
}

double
KcactCalc(double *Ks, double *Kcb, double *Kei, double *Kep, int i) {
    return( Ks[i] * Kcb[i] + Kei[i] + Kep[i]);
}


double
KcnsCalc(double *Kcb, double *Kei, double *Kep, int i)
{
    return(Kcb[i] + Kei[i] + Kep[i]);
}


double
ETcactCalc(double *Kc_act, double *ETo, int i) {
    return( Kc_act[i] * ETo[i]);
}

double
ETcnsCalc(double *Kc_ns, double *ETo, int i)
{
    return(Kc_ns[i] * ETo[i]);
}


#include <string.h>


int in(int val, SEXP table)
{
    int *vals = INTEGER(table);
    int n = Rf_length(table);
    for(int i = 0; i < n; i++)
	if(val == vals[i])
	    return(1);
    
    return(0);
}



double
IrCalc(double *Ks, int RDI_min, double AD, double *Dr_initial, int *doys_model, int Jdev, int Jharv, int days_no_irr, int i, const char *cropname, SEXP harvest_days)
{

    int buffer_days = 30;
    
    if (strcmp(cropname, "alfalfa.imperial") == 0) {
	if (Dr_initial[i] > AD && ! in(doys_model[i], harvest_days)) 
	    return(Dr_initial[i]);
        else 
	  return(0);
    } else if (strcmp(cropname, "alfalfa_CV") == 0 || strcmp(cropname, "alfalfa_intermountain") == 0) {
#if 0	
        if (doys_model[i] < crop_parameters$Jini[which(crop_parameters$crop==cropname)] || in(doys_model[i], harvest_days) || doys_model[i] > (harvest_days[length(harvest_days)] + buffer_days - days_no_irr) )
	    return(0);
        else if(Dr_initial[i] > AD | doys_model[i] == (harvest_days[length(harvest_days)] + buffer_days - days_no_irr)) 
	    return(Dr_initial[i]);
        else 
	    return(0);
#else
	PROBLEM "not implemented yet" ERROR;
#endif	
     } else if (doys_model[i] < Jdev | doys_model[i] > (Jharv - days_no_irr)) {
	  return(0);
    } else if (strcmp(cropname,"grapes_wine") == 0 && doys_model[i] < (Jharv - days_no_irr) & Ks[i] < RDI_min) {
	  return(Dr_initial[i] - AD);
      } else if (Dr_initial[i] > AD & strcmp(cropname,  "grapes_wine") != 0) {
	  return(Dr_initial[i]);
    } else if (doys_model[i] == (Jharv - days_no_irr) && strcmp(cropname, "grapes_wine") == 0) {
#if 0	
	return(GrapeLastIrr());
#else
	PROBLEM "GrapeLastIrr() is not implemented yet in the C code"
	    ERROR;
#endif	
    } else if (doys_model[i] == Jharv - days_no_irr) {
	  return(Dr_initial[i]);
    } else {
	  return(0);
    }
}
