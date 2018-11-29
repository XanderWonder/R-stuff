install.packages("ggplot2")
#Install the ggplot2 package

library(ggplot2)
#Use ggplot2

qplot(Petal.Length, Sepal.Length, data = iris, colour = Species)
#Use the ggplot2's qplot to plot the iris data.
#Note how many fewer arguments are needed compared to 'plot' and
#how a key is created by default

qplot(Petal.Length, Sepal.Length, data = iris, color = Species,
      xlab = "Sepal Length", ylab = "Petal Length",
      main = "Sepal vs. Petal Length in Fisher's Iris data")
#As above, with axis labels and main title

ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()
#Comparing temperature and pressure from the pressure dataset
#Appending 'geom_[feature]' adds extra display features

ggplot(pressure, aes(x=temperature, y=pressure)) + geom_point() + geom_line()
#Many features can be appended together
#If no features are appended, the graph will be created with no features

ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) + geom_density2d()
#The ggplot has many different kinds of features
#See the cheat sheet link below for an overview of the options

#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf