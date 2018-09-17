#Plot with multiple axis
predict_xgb <- predict(xgb_model, newdata= completed_data, type="prob")
predict_xgb$values <- completed_data$alive_1_year.1

predict_xgb$index <- rownames(predict_xgb)
colnames(predict_xgb) <- c("alive", "dead", "values", "index")

predict_lasso <- predict(lasso_1, newdata= completed_data, type="prob")
predict_lasso$values <- completed_data$alive_1_year.1
predict_lasso$index <- rownames(predict_lasso)
colnames(predict_lasso) <- c("alive", "dead", "values", "index")


predict_mlp <-  predict(mlpweight, newdata= completed_data, type="prob")
predict_mlp$values <- completed_data$alive_1_year.1
rownames(predict_mlp) <- 1:nrow(predict_mlp)
predict_mlp$index <- rownames(predict_mlp)
colnames(predict_mlp) <- c("alive", "dead", "values", "index")



mlpfinal <- predict_mlp %>%
  mutate(prob = case_when((values==0 ) ~ alive, 
                          (prob = values==1) ~ dead)) 

xgbfinal <- predict_xgb %>%
  mutate(prob = case_when((values==0 ) ~ alive, 
                          (prob = values==1) ~ dead))

lassofinal <- predict_lasso %>%
  mutate(prob = case_when((values==0 ) ~ alive, 
                          (prob = values==1) ~ dead))


xgb_mlp <- xgbfinal %>%
  left_join(mlpfinal, by='index') %>%
  select(values.x, prob.x, prob.y)

colnames(xgb_mlp) <- c("values", "xgb", "mlp")


fill_label = c("Alive", "Dead")

plot_xgb_mlp <- ggplot(xgb_mlp, aes(x=xgb, y=mlp, group = factor(values)))+
  geom_point(aes(color=values, alpha=0.5)) +
  scale_x_continuous(name="Extreme Gradient Boosting probabilities") + 
  scale_y_continuous(name="Neural network probabilities") +
  scale_fill_discrete(name= "Real values", labels = c("Alive", "Dead"))+
  scale_color_hue(labels = c("Alive", "Dead")) +
  ggtitle("Predictions XGB vs. ANN")



xgb_lasso <- xgbfinal %>%
  left_join(lassofinal, by='index') %>%
  select(values.x, prob.x, prob.y)

colnames(xgb_lasso) <- c("values", "xgb", "lasso")


fill_label = c("Alive", "Dead")

plot_xgb_lasso <- ggplot(xgb_lasso, aes(x=xgb, y=lasso, group = factor(values)))+
  geom_point(aes(color=values, alpha=0.5)) +
  scale_x_continuous(name="Extreme Gradient Boosting probabilities") + 
  scale_y_continuous(name="Lasso regression probabilities") +
  scale_fill_discrete(name= "Real values", labels = fill_label)+
  ggtitle("Predictions XGB vs. Lasso")



lasso_mlp <- lassofinal %>%
  left_join(mlpfinal, by='index')%>%
  select(values.x, prob.x, prob.y)

colnames(lasso_mlp) <- c("values", "lasso", "mlp")


plot_lasso_mlp <- ggplot(lasso_mlp, aes(x=lasso, y=mlp, group = factor(values)))+
  geom_point(aes(color=values,alpha=0.5)) +
  scale_x_continuous(name="Lasso regression probabilities") + 
  scale_y_continuous(name="Neural network probabilities") +
  scale_fill_discrete(name= "Real values", labels = fill_label)+
  ggtitle("Predictions Lasso vs. ANN")


install.packages("plot3D")
library("plot3D")


allplot <- xgb_lasso %>%
  left_join(xgb_mlp, by="index") %>%
  select(values.x, xgb.x, lasso, mlp)

colnames(allplot) <- c("values", "xgb", "lasso", "mlp")

xgb <- allplot$xgb
lasso <- allplot$lasso
mlp <- allplot$mlp
values <- allplot$values

scatter3D(xgb, lasso, mlp, col.panel = "steelblue")


values <- gl(5, 200)
cols <- 1:2

library(rgl)
plot3d <- plot3d(xgb, lasso, mlp, col.panel = "steelblue", col=cols[values], angle=100)

colkey= colkey(col= values, side = 2, clim = c(0, 1), add = FALSE, clab = "z", 
               col.clab = "red", adj.clab = 0)

group = sample(letters[1:3],10, replace = TRUE)

colkey <- c(values) 
plot3d(x = x, y = y, z = z, col = cols)


install.packages("scatterplot3d")
library(scatterplot3d)
{
  s3d <- with(allplot, scatterplot3d(xgb, lasso, mlp, color = rainbow(8)[as.numeric(values)], pch = 19, angle=100, main="Comparing performance between models",
                                     xlab="XGB probabilities",
                                     ylab="Lasso probabilities",
                                     zlab="ANN probabilities", box=FALSE, alpha=0.001 ))
  legend("top", inset=0.05,      # location and inset
         bty="n", cex=.7,              # suppress legend box, shrink text 50%
         title="Real values",
         c("alive", "dead"), fill=c("red", "yellow"))
}
