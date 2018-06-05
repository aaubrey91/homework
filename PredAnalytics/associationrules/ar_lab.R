library(arules)
data = read.transactions("groceries.csv", format="basket", sep=",")

rules <- apriori(data, parameter = list(supp = 0.001, conf=0.8))

summary(rules)

options(digits=2)
inspect(rules[1:5])

rules <- sort(rules, by="confidence", decreasing=TRUE)

rules <- apriori(data=data, parameter = list(supp=0.001, conf=0.08, minlen=3), 
                 appearance = list(default="lhs", rhs="whole milk"), control=list(verbose=F))
rules <- sort(rules, decreasing=TRUE, by="confidence")
inspect(rules[1:5])

rules <- apriori(data=data, parameter=list(supp=0.001, conf=0.15, minlen=3), 
                 appearance=list(default="rhs", lhs=c("butter","sugar")),
                 control = list(verbose=F))
rules <- sort(rules, decreasing=TRUE, by="confidence")
inspect(rules)

rules <- sort(rules, decreasing=TRUE, by="lift")
inspect(rules)
