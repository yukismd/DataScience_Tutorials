# F1, F0.1, F2値の比較
# 2021/2/8

library(ggplot2)

predict_1 <- 1:9/10
actual_1 <- 1:9/10

table <- expand.grid(predict_1, actual_1)
names(table) <- c("predict_1", "actual_1")

N <- 100

table['N'] <-  N

table['TP'] <- table['predict_1'] * table['actual_1']
table['FP'] <- table['predict_1'] - table['TP']
table['FN'] <- table['actual_1'] - table['TP']
table['TN'] <- 1 - (table['TP']+table['FP']+table['FN'])
table['TP'] <- table['TP'] * N
table['FP'] <- table['FP'] * N
table['FN'] <- table['FN'] * N
table['TN'] <- table['TN'] * N

table['Precision'] <- table['TP'] / (table['TP']+table['FP'])
table['Recall'] <- table['TP'] / (table['TP']+table['FN'])

table['F1'] <- 2*table['Precision']*table['Recall'] / (table['Precision']+table['Recall'])
table['F0.5'] <- 1.25*table['Precision']*table['Recall'] / (0.25*table['Precision']+table['Recall'])
table['F5'] <- 5*table['Precision']*table['Recall'] / (4*table['Precision']+table['Recall'])

table

summary(table)


# "Precision","Recall","F1","F0.5","F5"

## F1
ggplot(table, aes(x = Precision, y = Recall, fill=F1)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_gradient(low = "white" , high = "black")

## F0.5
ggplot(table, aes(x = Precision, y = Recall, fill=F0.5)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_gradient(low = "white" , high = "black")

## F5
ggplot(table, aes(x = Precision, y = Recall, fill=F5)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_gradient(low = "white" , high = "black")

