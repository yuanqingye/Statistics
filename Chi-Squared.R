df <- read.csv("https://goo.gl/j6lRXD")
table(df$treatment, df$improvement)
chisq.test(df$treatment, df$improvement, correct=FALSE)


table(mtcars$carb, mtcars$cyl)
chisq.test(mtcars$carb, mtcars$cyl,correct = FALSE)
