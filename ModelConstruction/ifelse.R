a = parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']]

ifelse(doy < a,
       parameters[['Kcb.ini']],
       ifelse(doy < a + parameters[['Ldev.cycle']],
              parameters[['Kcb.ini']] + (doy - a) / parameters[['Ldev.cycle']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]),
              ifelse(doy < a + parameters[['Ldev.cycle']] + parameters[['Lmid.cycle']],
                     parameters[['Kcb.mid']],
                     ifelse(doy < a + parameters[['Ldev.cycle']] + parameters[['Lmid.cycle']] + parameters[['Llate.cycle']],
                            parameters[['Kcb.mid']] + (doy - a - parameters[['Ldev.cycle']] - parameters[['Lmid.cycle']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]),
                            final.else))))
