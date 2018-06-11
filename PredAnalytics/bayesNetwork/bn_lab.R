library(bnlearn)

vname = c("Pol", "Smk", "Can", "Xry", "Dys")

e = empty.graph(vname)

arc.set = matrix(c("Pol", "Smk", "Can", "Can", "Can", "Can", "Xry", "Dys"), 
                 ncol = 2, dimnames = list(NULL, c("from", "to")))
arcs(e) = arc.set

cptPol = matrix(c(0.9, 0.1), ncol = 2, dimnames = list(NULL, c("low", "high")))
cptSmk = matrix(c(0.3, 0.7), ncol = 2, dimnames = list(NULL, c("true", "false")))
cptCan = c(0.30, 0.70, 0.001, 0.999, 0.95, 0.05, 0.2, 0.8)
dim(cptCan) = c(2,2,2)

dimnames(cptCan) = list("Can" = c("true", "false"), "Smk" = c("true", "false"),
                        "Pol" = c("low", "high"))
cptXry = c(0.9, 0.1, 0.2, 0.8)
dim(cptXry) = c(2,2)

dimnames(cptXry) = list("Xry" = c("true", "false"), "Can" = c("true", "false"))
cptDys = c(0.65, 0.35, 0.3, 0.7)
dim(cptDys) = c(2,2)
dimnames(cptDys) = list("Dys" = c("true", "false"), "Can" = c("true", "false"))

dfit = custom.fit(e, dist = list(Pol = cptPol, Smk = cptSmk, Can = cptCan, Xry= cptXry, Dys = cptDys))

cpquery(dfit, event=(Pol == "low"), evidence=(Can == "true"))
cpquery(dfit, event=(Pol == "low"), evidence=(Can == "true") & (Dys=="false"))
cpquery(dfit, event=(Xry == "true"), evidence=(Can == "true") & (Dys=="false"))
cpquery(dfit, event=(Xry == "false"), evidence=(Can == "true") & (Dys=="false"))
cpquery(dfit, event=(Xry == "false"), evidence=(Pol == "high") & (Smk=="true"))
cpquery(dfit, event=(Xry == "true"), evidence=(Pol == "high") & (Smk=="true"))
