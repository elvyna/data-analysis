library(ggplot2)
library(ggthemes)
theme_set(theme_classic())

student <- matrix(
  data = c(1,2,3,3,4.5,5,6,0.5,2,2.5,
           60,65,78,70,72,80,85,60,90,80),
  byrow = FALSE,
  nrow = 10,
  ncol = 2
  )
student <- as.data.frame(student)
colnames(student) <- c('hour','score')

## simple linear model
model.linear <- lm(score ~ hour, data = student)
pred.score <- predict(model.linear, newdata=student)

john <- as.data.frame(matrix(data=5))
colnames(john) <- c('hour')
jane <- as.data.frame(matrix(data=16))
colnames(jane) <- c('hour')

pred.john <- predict(model.linear, newdata = john)
pred.jane <- predict(model.linear, newdata = jane)

p.lm <- ggplot(data = student) +
  geom_point(aes(
    x = hour,
    y = score
  )) +
  scale_x_continuous(
    name = 'study duration (hour)'
  ) +
  scale_y_continuous(
    name = 'exam score'
  ) +
  geom_line(
    aes(
      x = hour,
      y = pred.score
    ),
    col = 'red',
    linetype = 'solid'
  ) +
  geom_point(
    aes(
      x = john$hour,
      y = pred.john
    ),
    col = 'blue'
  ) + annotate(
  'text', 
  x = john$hour + .5, 
  y = pred.john - 2, 
  label = paste("Predicted score:", round(pred.john)),
  col = 'blue'
) +
  geom_point(
    aes(
      x = jane$hour,
      y = pred.jane
    ),
    col = 'blue'
  ) + annotate(
    'text', 
    x = jane$hour - 1.5, 
    y = pred.jane - 2, 
    label = paste("Predicted score:", round(pred.jane)),
    col = 'blue'
  ) + ggtitle('Simple linear model')

p.lm

summary(model.linear)
plot(model.linear)

## GLM
score.matrix.binom <- matrix(data = c(student$score, 100-student$score), nrow=10)
model.logit <- glm(
  score.matrix.binom ~ hour, 
  data = student,
  family = binomial(link = 'logit')
)
plot(model.logit)
summary(model.logit)
print(paste("R squared:", with(summary(model.logit), 1 - deviance/null.deviance)))

pred.score.logit <- predict(model.logit, newdata=student)
pred.john.logit <- predict(model.logit, newdata = john)
pred.jane.logit <- predict(model.logit, newdata = jane)

john$score <- 100 * 1/(1+exp(-pred.john.logit))
jane$score <- 100 * 1/(1+exp(-pred.jane.logit))

p.glm <- ggplot(data = student) +
  geom_point(aes(
    x = hour,
    y = score
  )) +
  scale_x_continuous(
    name = 'study duration (hour)'
  ) +
  scale_y_continuous(
    name = 'exam score'
  ) +
  geom_line(
    aes(
      x = hour,
      y = 100 * 1/(1+exp(-pred.score.logit)),
    ),
    col = 'red',
    linetype = 'solid'
  ) +
  geom_point(
    aes(
      x = john$hour,
      y = 100 * 1/(1+exp(-pred.john.logit))
    ),
    col = 'blue'
  ) + annotate(
    'text', 
    x = john$hour + .5, 
    y = 100 * 1/(1+exp(-pred.john.logit)) - 2, 
    label = paste("Predicted score:", round(100 * 1/(1+exp(-pred.john.logit)))),
    col = 'blue'
  ) +
  geom_point(
    aes(
      x = jane$hour,
      y = 100 * 1/(1+exp(-pred.jane.logit))
    ),
    col = 'blue'
  ) + annotate(
    'text', 
    x = jane$hour - 1.5, 
    y = 100 * 1/(1+exp(-pred.jane.logit)) - 2, 
    label = paste("Predicted score:", round(100 * 1/(1+exp(-pred.jane.logit)))),
    col = 'blue'
  ) + ggtitle('Logistic regression')
p.glm

model.logit$coefficients
exp(0.52059)/(1+exp(0.52059))
exp(0.1859114)

library(gridExtra)
grid.arrange(p.lm, p.glm)