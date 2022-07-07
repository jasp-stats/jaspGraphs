x <- "A"
y <- -0.188
ciLower <- y - .1
ciUpper <- y + .1
descriptivesPlot(x, y, ciLower, ciUpper, horizontalLine = -.1)


x <- c("Fast", "Slow")
y <- c(15, 22)
ciLower <- y - 3
ciUpper <- y + 3
xName <- "Condition"
yName <- "Response time"

descriptivesPlot(x, y, ciLower, ciUpper, xName, yName, horizontalLine = 18.5)

x <- c("0", "1", "0", "1")
y <- c(15, 22, 16, 21)
ciLower <- y - 3
ciUpper <- y + 3
xName <- "contBinom"
group <- c("m", "m", "f", "f")
groupName <- "facGender"
descriptivesPlot(x, y, ciLower, ciUpper, xName, group = group, groupName = groupName,
                 horizontalLine = 19)

set.seed(42)
kx <- 5
kg <- 20
n  <- kx * kg
x <- as.character(rep(1:kx, kg))
y <- rnorm(n, 10 + 10 * as.numeric(x), 2)
ciLower <- y - rnorm(n, 3 * as.numeric(x), 2)
ciUpper <- y + rnorm(n, 3 * as.numeric(x), 2)
group <- rep(seq_len(kg), each = kx)
xName <- "Groups"
yName <- "Performance"
groupName <- "Legend title"
descriptivesPlot(x, y, ciLower, ciUpper, xName, yName, group = group, groupName = groupName,
                 breaksAtExtremaOnly = FALSE, connectedPoints = FALSE)
