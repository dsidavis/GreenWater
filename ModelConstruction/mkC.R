fixName =
function(x)
    gsub("([A-Za-z])\\.([A-Za-z])", "\\1_\\2", x)

ll = readLines("txt")
ll = gsub("#.*", "", ll)
ll = gsub("<-", "=", ll)
ll = fixName(ll)
ll = gsub("$", ";", ll)


h = "loopMain(AD, cropname, days.no.irr, doys.model, ETo, fewi, fewp, fw, Jdev, Jharv, Kcb.adjusted, Kcmax, model.length, P, PAW, RDI.min, REW.parameter, TEW.parameter, Dei.end, Ir, Dei.initial, Dep.end, Dep.initial, Kri, Krp, W, DPep, DPei, Kei, Kep, Ep, Ei, Kc.ns, ETc.ns, Dr.end, Dr.initial, Ks, DPr, Kc.act, ETc.act)"

h = gsub("(\\(|, )", "\\1SEXP r_", h)
h = fixName(h)


ll = readLines("Info")
ll = ll[ 1:(grep("#END", ll)-1)]
info = strsplit(ll, "( - | )") 
info = as.data.frame(do.call(rbind, info), stringsAsFactors = FALSE)
names(info) = c("name", "type", "length")
info[[3]] = as.integer(info[[3]])
info[[1]] = fixName(info[[1]])

info = info[info[[1]]  != "i", ]

info$RCtype = c("numeric" = "REAL", logical = "INTEGER", integer = "INTEGER", character = "CHAR(STRING_ELT(")[info$type]
info$Ctype = c("numeric" = "double", logical = "int", integer = "int", character = "const char")[info$type]

v = sprintf("%s * %s = %s(r_%s);", info$Ctype, info$name, info$RCtype,  info$name)
cat(v, sep = "\n")
