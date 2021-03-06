library(readxl)
expectations <- read_excel("expectations.xlsx")
x11()
par(mfrow = c(2, 2))
hist(expectations$Total, main = "total")
hist(expectations$Lead, main = "lead")
hist(expectations$Participated, main = "participate")
plot(expectations$Participated, expectations$Lead)
savePlot("Expectations.pdf", type = "pdf")
savePlot("Expectations.wmf", type = "wmf")


#file edited