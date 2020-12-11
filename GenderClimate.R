library(ade4)
library(vegan)


## -------------------------------------------------------------------------------------------
tab <- read.csv("GennovateWheatMaize.csv", 
                row.names = 1, stringsAsFactors = TRUE)
dim(tab)
names(tab)


## -------------------------------------------------------------------------------------------
table(tab$Country)
table(tab$crop)


## -------------------------------------------------------------------------------------------
#Set the color per crop
colCrop <- c("chartreuse3", "darkgoldenrod1")
barplot(table(tab$crop), col=colCrop,
        xlab="Crop", ylab="Number of villages")


## ---- width=8-------------------------------------------------------------------------------
#Compute the contingency table
table(tab$crop, tab$Country)

#Visualize the contingency table
barplot(table(tab$crop, tab$Country), col=colCrop,
        xlab="Crop", ylab="Number of villages",
        cex.names = 0.9)


## -------------------------------------------------------------------------------------------
summary(tab$Pop)


## -------------------------------------------------------------------------------------------
boxplot(tab$Pop, ylab="Population")


## -------------------------------------------------------------------------------------------
tapply(tab$Pop,tab$crop, summary)
boxplot(tab$Pop~tab$crop, ylab="Population", xlab="Crop")


## -------------------------------------------------------------------------------------------
gendervar <- c("W_Mkt", "W_Agri_Jobs", "W_All_Jobs", 
           "Ctrl_Wsales", "Perm_Mig", "Phone_owner", 
           "GC_Sec_Edu", "Elections", "Active_Disc", 
           "Violence", "Physical_Mob", "PF", 
           "Ctrl_Comm", "Ctrl_Subs", "Inheritance")
#Subset variables for gender climate
gc <- tab[,gendervar]
dim(gc)


## -------------------------------------------------------------------------------------------
#multivariate analysis
mvar.gc <- dudi.hillsmith(gc, scannf = FALSE, nf=3)


## -------------------------------------------------------------------------------------------
barplot(mvar.gc$eig/sum(mvar.gc$eig)*100,
        ylab="% variance explained",
        xlab="Principal components")


## -------------------------------------------------------------------------------------------
dotchart(sort(mvar.gc$co[,1]), pch=16, cex = 0.6,
         labels = row.names(mvar.gc$co)[order(mvar.gc$co[,1])],
         xlab="PC1 loadings")
abline(v=0)


## -------------------------------------------------------------------------------------------
#Define color per country-crop
colCC <- rainbow(nlevels(tab$Country))
boxplot(mvar.gc$li[,1]~tab$Country, 
        ylab="Gender climate", xlab="",
        col=colCC, las=2)
abline(h=0)


## -------------------------------------------------------------------------------------------
csvar <- c("Irrigation", "Pop", "Pop_Growth", 
            "HHMkt", "Mkt", "U_Land", 
           "Town", "Preschool", "U_Secondary", 
           "Secondary", "Clinic", "Training", 
           "Bus", "Electricity", "Internet", 
           "Land", "Farmer_Org")
#Subset variables for complementary scores
cs <- tab[,csvar]
dim(cs)


## -------------------------------------------------------------------------------------------
#multivariate analysis
mvar.cs <- dudi.hillsmith(cs, scannf = FALSE, nf=3)


## -------------------------------------------------------------------------------------------
barplot(mvar.cs$eig/sum(mvar.cs$eig)*100,
        ylab="% variance explained",
        xlab="Principal components")


## ---- echo=FALSE----------------------------------------------------------------------------
mvar.cs$co[,1] <- -mvar.cs$co[,1]
mvar.cs$li[,1] <- -mvar.cs$li[,1]


## -------------------------------------------------------------------------------------------
dotchart(sort(mvar.cs$co[,1]), pch=16, xlab="PC1 loadings",
         labels = row.names(mvar.cs$co)[order(mvar.cs$co[,1])],
         cex = 0.5)
abline(v=0)


## -------------------------------------------------------------------------------------------
boxplot(mvar.cs$li[,1]~tab$Country, 
        ylab="Opportunity", xlab="",
        col=colCC, las=2)
abline(h=0)


## -------------------------------------------------------------------------------------------
dotchart(sort(mvar.cs$co[,2]), pch=16, 
         xlab="PC2 loadings", cex = 0.5, 
         labels = row.names(mvar.cs$co)[order(mvar.cs$co[,2])])
abline(v=0)


## -------------------------------------------------------------------------------------------
boxplot(mvar.cs$li[,2]~tab$Country, 
        ylab="Connectivity", xlab="",
        col=colCC, las=2)
abline(h=0)


## -------------------------------------------------------------------------------------------
comp <- data.frame(
  "Gender climate"=mvar.gc$li[,1],
  "Opportunity"=mvar.cs$li[,1],
  "Connectivity"=mvar.cs$li[,2],
  "Country"=tab$Country
)


## -------------------------------------------------------------------------------------------
plot(comp$Gender.climate, comp$Opportunity, 
     xlab="Gender climate", ylab="Opportunity")
cor.test(comp$Gender.climate, comp$Opportunity)


## -------------------------------------------------------------------------------------------
par(oma=c(1,1,0,1))
s.class(comp, fac = comp$Country,
        col = colCC, xax=1, yax=2, grid=FALSE)
mtext("Gender", side = 1, line = 0, outer = TRUE)
mtext("Opportunity", side = 2, line = 0, outer = TRUE)


## -------------------------------------------------------------------------------------------
cor.test(comp$Gender.climate, comp$Opportunity)

par(oma=c(1,1,0,1))
s.class(comp, fac = comp$Country,
        col = colCC, xax=1, yax=3, grid=FALSE)
mtext("Gender", side = 1, line = 0, outer = TRUE)
mtext("Connectivity", side = 2, line = 0, outer = TRUE)



## -------------------------------------------------------------------------------------------
var <- varpart(comp$Gender.climate, ~Opportunity, ~Connectivity, 
               ~Country, data=comp)


## -------------------------------------------------------------------------------------------
par(cex=0.6)
plot(var, Xnames=c("Opportunity", "Connectivity", "Country"),
     bg=rainbow(4))

