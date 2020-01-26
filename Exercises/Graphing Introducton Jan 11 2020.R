#Graphing Introduction Jan 11 2020

#Notes from R for Data Scientists - Wickham
#https://r4ds.had.co.nz/

#Feb. 2 2019
install.packages('tidyverse')

#If we want to make it clear what package an object comes from, we’ll use the package name followed by two colons, like dplyr::mutate(), or
#nycflights13::flights. This is also valid R code.

library(tidyverse)

#Do cars with big engines use more fuel than cars with small engines? 
#We will check a preloaded dataset in R called mpg
#run the following line
mpg
nrow(mpg)
ncol(mpg)
colnames(mpg)
#To learn more about mpg and the definitions of the variables, open its help page by running ?mpg.
?mpg

#You get the picture.

#Definitions:
#displ, a car’s engine size, in litres.
#hwy, a car’s fuel efficiency on the highway, in miles per gallon (mpg). 
#A car with a low fuel efficiency consumes more fuel than a car with a high fuel efficiency when they travel the same distance.

#Use ggplot
?ggplot

#This is one of the main graphics programs in R. Here's how it works:

#Create a ggplot on mpg engine displacement and highway miles per gallon
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#3.2.3 A graphing template
#Let’s turn this code into a reusable template for making graphs with ggplot2. 
#To make a graph, replace the bracketed sections in the code below with a dataset, a geom function, or a collection of mappings.
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

#using color to distinguish class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#MPG categorical = manufacturer model cyl trans drv fl class
#continuous = disply cty hwy
#Map a continuous variable to color, size, and shape. 
#How do these aesthetics behave differently for categorical vs. continuous variables?
#Answer - they do not come up in discrete blocks by on a spectrum range

#Color by manufacturer
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color =manufacturer))

#Color and Size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color =manufacturer, size=manufacturer))


#adding size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class, color = class))

#What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class, stroke=20))

#What happens if you map an aesthetic to something other than a variable name, 
#like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

#It gives you a true false by color

#Here's a quick demo to show you the types of things you can do in R

demo(topic="graphics")

demo(graphics)

#-----------------------------------------------------------
#That's enough. Stop here
  
#-----------------------------------------------------------  
install.packages(c("nycflights13", "gapminder", "Lahman"))

 require(datasets)

 require(grDevices); require(graphics)

 ## Here is some code which illustrates some of the differences between
 ## R and S graphics capabilities.  Note that colors are generally specified
 ## by a character string name (taken from the X11 rgb.txt file) and that line
 ## textures are given similarly.  The parameter "bg" sets the background
 ## parameter for the plot and there is also an "fg" parameter which sets
 ## the foreground color.
 
 
 x <- stats::rnorm(50)

 opar <- par(bg = "white")

 plot(x, ann = FALSE, type = "n")
Hit <Return to see next plot: 
  
 abline(h = 0, col = gray(.90))

 lines(x, col = "green4", lty = "dotted")

 points(x, bg = "limegreen", pch = 21)

 title(main = "Simple Use of Color In a Plot",
        +       xlab = "Just a Whisper of a Label",
        +       col.main = "blue", col.lab = gray(.8),
        +       cex.main = 1.2, cex.lab = 1.0, font.main = 4, font.lab = 3)

 ## A little color wheel.	 This code just plots equally spaced hues in
 ## a pie chart.	If you have a cheap SVGA monitor (like me) you will
 ## probably find that numerically equispaced does not mean visually
 ## equispaced.  On my display at home, these colors tend to cluster at
 ## the RGB primaries.  On the other hand on the SGI Indy at work the
 ## effect is near perfect.
 
 par(bg = "gray")

 pie(rep(1,24), col = rainbow(24), radius = 0.9)
Hit <Return to see next plot: 
  
 title(main = "A Sample Color Wheel", cex.main = 1.4, font.main = 3)

 title(xlab = "(Use this as a test of monitor linearity)",
        +       cex.lab = 0.8, font.lab = 3)

 ## We have already confessed to having these.  This is just showing off X11
 ## color names (and the example (from the postscript manual) is pretty "cute".
 
 pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)

 names(pie.sales) <- c("Blueberry", "Cherry",
                        + 		      "Apple", "Boston Cream", "Other", "Vanilla Cream")

 pie(pie.sales,
      +     col = c("purple","violetred1","green3","cornsilk","cyan","white"))
Hit <Return to see next plot: 
  
 title(main = "January Pie Sales", cex.main = 1.8, font.main = 1)

 title(xlab = "(Don't try this at home kids)", cex.lab = 0.8, font.lab = 3)

 ## Boxplots:  I couldn't resist the capability for filling the "box".
 ## The use of color seems like a useful addition, it focuses attention
 ## on the central bulk of the data.
 
 par(bg="cornsilk")

 n <- 10

 g <- gl(n, 100, n*100)

 x <- rnorm(n*100) + sqrt(as.numeric(g))

 boxplot(split(x,g), col="lavender", notch=TRUE)
Hit <Return to see next plot: 
  
 title(main="Notched Boxplots", xlab="Group", font.main=4, font.lab=1)

 ## An example showing how to fill between curves.
 
 par(bg="white")

 n <- 100

 x <- c(0,cumsum(rnorm(n)))

 y <- c(0,cumsum(rnorm(n)))

 xx <- c(0:n, n:0)

 yy <- c(x, rev(y))

 plot(xx, yy, type="n", xlab="Time", ylab="Distance")
Hit <Return to see next plot: 
  
 polygon(xx, yy, col="gray")

 title("Distance Between Brownian Motions")

 ## Colored plot margins, axis labels and titles.	 You do need to be
 ## careful with these kinds of effects.	It's easy to go completely
 ## over the top and you can end up with your lunch all over the keyboard.
 ## On the other hand, my market research clients love it.
 
 x <- c(0.00, 0.40, 0.86, 0.85, 0.69, 0.48, 0.54, 1.09, 1.11, 1.73, 2.05, 2.02)

 par(bg="lightgray")

 plot(x, type="n", axes=FALSE, ann=FALSE)
Hit <Return to see next plot: 
  
 usr <- par("usr")

 rect(usr[1], usr[3], usr[2], usr[4], col="cornsilk", border="black")

 lines(x, col="blue")

 points(x, pch=21, bg="lightcyan", cex=1.25)

 axis(2, col.axis="blue", las=1)

 axis(1, at=1:12, lab=month.abb, col.axis="blue")

 box()

 title(main= "The Level of Interest in R", font.main=4, col.main="red")

 title(xlab= "1996", col.lab="red")

 ## A filled histogram, showing how to change the font used for the
 ## main title without changing the other annotation.
 
 par(bg="cornsilk")

 x <- rnorm(1000)

 hist(x, xlim=range(-4, 4, x), col="lavender", main="")
Hit <Return to see next plot: 
  
 title(main="1000 Normal Random Variates", font.main=3)

 ## A scatterplot matrix
 ## The good old Iris data (yet again)
 
 pairs(iris[1:4], main="Edgar Anderson's Iris Data", font.main=4, pch=19)
Hit <Return to see next plot: 
  
 pairs(iris[1:4], main="Edgar Anderson's Iris Data", pch=21,
          +       bg = c("red", "green3", "blue")[unclass(iris$Species)])
Hit <Return to see next plot: 
  
 ## Contour plotting
 ## This produces a topographic map of one of Auckland's many volcanic "peaks".
 
 x <- 10*1:nrow(volcano)

 y <- 10*1:ncol(volcano)

 lev <- pretty(range(volcano), 10)

 par(bg = "lightcyan")

 pin <- par("pin")

 xdelta <- diff(range(x))

 ydelta <- diff(range(y))

 xscale <- pin[1]/xdelta

 yscale <- pin[2]/ydelta

 scale <- min(xscale, yscale)

 xadd <- 0.5*(pin[1]/scale - xdelta)

 yadd <- 0.5*(pin[2]/scale - ydelta)

 plot(numeric(0), numeric(0),
       +      xlim = range(x)+c(-1,1)*xadd, ylim = range(y)+c(-1,1)*yadd,
       +      type = "n", ann = FALSE)
Hit <Return to see next plot: 
  
 usr <- par("usr")

 rect(usr[1], usr[3], usr[2], usr[4], col="green3")

 contour(x, y, volcano, levels = lev, col="yellow", lty="solid", add=TRUE)

 box()

 title("A Topographic Map of Maunga Whau", font= 4)

 title(xlab = "Meters North", ylab = "Meters West", font= 3)

 mtext("10 Meter Contour Spacing", side=3, line=0.35, outer=FALSE,
        +       at = mean(par("usr")[1:2]), cex=0.7, font=3)

 ## Conditioning plots
 
 par(bg="cornsilk")

 coplot(lat ~ long | depth, data = quakes, pch = 21, bg = "green3")
Hit <Return to see next plot: 
  
 par(opar)
 
#Tutorial
#Import Data, Create Dataframe, Rename Columns
snowdata <- rio::import("data/BostonChicagoNYCSnowfalls.csv")
bostonsnow <- select(snowdata, Winter, Boston)
names(bostonsnow)[2] <- "TotalSnow"

#Doing the same thing but with pipe function
bostonsnow2 <- select(snowdata, Winter, Boston) %>%
  rename(TotalSnow = Boston)

#Doing the same thing but more efficiently
bostonsnow3 <- select(snowdata, Winter, TotalSnow = Boston) 

#Basic graphs
plot(bostonsnow$TotalSnow)

hist(bostonsnow$TotalSnow)
boxplot(bostonsnow$TotalSnow)
barplot(bostonsnow$TotalSnow)
barplot(sort(bostonsnow$TotalSnow, decreasing = TRUE))

#qplot
qplot(data=bostonsnow, y = TotalSnow)
qplot(y = bostonsnow$TotalSnow)

#basic ggplot2 - boxplot
ggplot(data=snowdata) + 
  geom_boxplot(aes(x = "Boston", y = Boston))

#dual box plots
ggplot(data=snowdata) + 
  geom_boxplot(aes(x = "Boston", y = Boston)) +
  geom_boxplot(aes(x = "Chicago", y = Chicago))

#bring in snowdata tidy
snowdata_tidy <- rio::import("data/snowdata_tidy.csv")

#view a tidy table
View(snowdata_tidy)

#Boxplot with ggplot
ggplot(snowdata_tidy, aes(x = City, y = TotalSnow)) +
  geom_boxplot()

#Line graphs
ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City)) +
  geom_line()

#ggplot with colors and points

ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line()


#ggplot with colors and points
ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line() +
  geom_point() 

#Filtered for two years, 1999 and 2000
snowdata_tidy21 <- filter(snowdata_tidy, Winter >= "1999-2000")
ggplot(snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line() +
  geom_point()

#Barplots
ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_col() 

#Not so ugly bars
ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, fill = City)) +
  geom_col(position = "dodge") 

test <- ggplot(human_rights)+
  aes(x = year, y = n, fill = name)+
  scale_y_continuous(labels = scales::comma) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "'Human Rights' in Economic News, 2000-2018", 
       subtitle = "NYT, WP, WSJ, NYT, Inside US Trade 2000-2018",
       caption = "Source: ProQuest, 4,334 articles. Graphic by Rob Wells",
       x="Month",
       y="Frequency of 'Human Rights'")

#Save high resolution
ggsave("test.png", width = 6.94, height = 4.79, dpi = 1000)

