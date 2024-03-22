library(arules)
library(arulesViz)
data(Groceries)
itemsets <- apriori(Groceries, parameter = list(minlen=1, maxlen=1, 
                    support=0.02, target="frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets, by="support"),10))

itemsets <- apriori(Groceries, parameter = list(minlen=2, maxlen=2, 
                    support=0.02, target="frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets, by="support"),10))

itemsets <- apriori(Groceries, parameter = list(minlen=3, maxlen=3, 
                    support=0.02, target="frequent itemsets"))
summary(itemsets)
inspect(sort(itemsets, by="support"))

itemsets <- apriori(Groceries, parameter = list(minlen=4, maxlen=4, 
                    support=0.02, target="frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets, by="support"),10))

itemsets <- apriori(Groceries, parameter = list(minlen=2, maxlen=2,
                    support=0.02, target="frequent itemsets"))

rules <- apriori(Groceries, parameter = list(support=0.001, 
                 confidence=0.6, target="rules"))
summary(rules)

plot(rules, jitter = 0.1)  
par(mar = c(5, 5, 2, 2))  
png("rules_quality_plot.png", width = 800, height = 600)  # Adjust dimensions as needed
plot(rules@quality)
dev.off()

slope <- sort(round(rules@quality$lift / rules@quality$confidence, 2))
unlist(lapply(split(slope, f=slope), length))
inspect(head(sort(rules, by="lift"),10))

confidentRules <- rules[quality(rules)$confidence > 0.9]
confidentRules
plot(confidentRules, method = "matrix", measure=c("lift", "confidence"),
     control=list(reorder='none'))

highLiftRules <- head(sort(rules, by="lift"),10)
plot(highLiftRules, method="graph", control=list(type="items"))

