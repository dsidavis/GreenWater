#define MAX(a, b)  (a) < (b) ? (b) : (a)
#define MIN(a, b)  (a) < (b) ? (a) : (b)

// EpCalc is the same as EiCalc.
void
EiCalc(double *ETo, double *Kei, int *i, double *ans)
{
    ans[0] = ETo[*i] * Kei[*i];
}

void
DPepCalc(double *P, double *Dep_end, int *i, double *ans)
{
    ans[0] = MAX(P[*i] - Dep_end[*i-1], 0);
}

void
DepEndCalc(double *Dep_end, double *P, double *Ep, double *fewp, double *DPep, int *i, double *ans)
{
    ans[0] = MAX(Dep_end[*i - 1] - P[*i] + Ep[*i]/fewp[*i] + DPep[*i], 0);
}

void
KepCalc(double *Krp, double *W, double *Kcmax, double *Kcb, double *fewp, double *TEW, double *Dep_end, double *DPep, double *P, double *ETo, int *i, double *ans)
{
    double Kep_est = MIN(Krp[*i] * (1 - W[*i]) *(Kcmax[*i] - Kcb[*i]), fewp[*i] * Kcmax[*i]);
    double TEW_check = MAX(Dep_end[*i-1] - P[*i] + (Kep_est * ETo[*i]) / fewp[*i] + DPep[*i], 0);
    if(TEW_check > *TEW)
	ans[0] = fewp[*i] * (*TEW - Dep_end[*i-1] + P[*i] - DPep[*i])/ETo[*i];
    else
	ans[0] = Kep_est;
}


void
KeiCalc(double *Kri, double *W, double *Kcmax, double *Kcb, double *fewi, double *TEW, double *Dei_end, double *DPei, double *P, double *ETo, int *i, double *Ir, double *fw, double *ans)
{    
    double Kei_est =  MIN(Kri[*i] * W[*i] * (Kcmax[*i] - Kcb[*i]), fewi[*i] * Kcmax[*i]);
    double TEW_check = MAX(Dei_end[*i - 1] - P[*i] - Ir[*i - 1] / *fw + Kei_est * ETo[*i] / fewi[*i] + DPei[*i], 0);

    if (TEW_check > *TEW) 
	ans[0] = fewi[*i] * (*TEW - Dei_end[*i - 1] + P[*i] + Ir[*i - 1] / *fw - DPei[*i]) / ETo[*i];
     else 
	ans[0] = Kei_est;

}
