library(bnlearn)

vname = c("S", "W", "R", "WG", "SR")

e = empty.graph(vname)

arc.set = matrix(c("W", "W", "S", "R", "R", "S", "R", "WG", "WG", "SR"), 
                 ncol = 2, dimnames = list(NULL, c("from", "to")))

arcs(e) = arc.set

cptW = matrix(c(0.6, 0.4), ncol = 2, dimnames = list(NULL, c("true", "false")))

cptR = matrix(c(0.8, 0.2, 0.1, 0.9))              
dim(cptR) = c(2,2)
dimnames(cptR) = list("R" = c("true", "false"), "W" = c("true", "false"))

cptS = matrix(c(0.2, 0.8, 0.75, 0.25))
dim(cptS) = c(2,2)
dimnames(cptS) = list("S" = c("true", "false"), "W" = c("true", "false"))

cptWG = matrix(c(0.95, 0.05, 0.9, 0.1, 0.8, 0.2, 0, 1))
dim(cptWG) = c(2,2,2)
dimnames(cptWG) = list("WG" = c("true", "false"), "R" = c("true", "false"),
                       "S" = c("true", "false"))

cptSR = matrix(c(0.7, 0.3, 0, 1))
dim(cptSR) = c(2,2)
dimnames(cptSR) = list("SR" = c("true", "false"), "R" = c("true", "false"))

dfit = custom.fit(e, dist = list(W = cptW, S = cptS, R = cptR, WG = cptWG, SR = cptSR))

cpquery(dfit, event=(SR == "false"), evidence=(W == "true"))
cpquery(dfit, event=(WG == "true"), evidence=(S == "true") & W == "true")
cpquery(dfit, event=(SR == "true"), evidence=(WG == "true") & S == "false")
cpquery(dfit, event=(WG == "false"), evidence=(SR == "false") & W == "true")
