### R code from vignette source 'barsurf.Rnw'

###################################################
### code chunk number 1: barsurf.Rnw:37-42
###################################################
options(continue="  ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 1.35, 1.6), cex=0.7, cex.main=1)))

set.seed (1)


###################################################
### code chunk number 2: barsurf.Rnw:96-98
###################################################
library (barsurf)
library (misc3d)


###################################################
### code chunk number 3: barsurf.Rnw:105-106
###################################################
set.bs.options (rendering.style="pdf", theme="blue")


###################################################
### code chunk number 4: barsurf.Rnw:122-124
###################################################
n <- 10
p <- 0.5


###################################################
### code chunk number 5: barsurf.Rnw:127-131
###################################################
x <- y <- 0:10
f <- function (x, y, n, p)
    dbinom (x, n, p) * dbinom (y, n, p)
fv <- outer (x, y, f, n, p)


###################################################
### code chunk number 6: barsurf.Rnw:136-137
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_dfield (x, y, fv)


###################################################
### code chunk number 7: barsurf.Rnw:140-141
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_bar (,,fv)


###################################################
### code chunk number 8: barsurf.Rnw:157-158
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield (rotated.sinc, c (-15.5, 15.5), fb=0, n=40, hcv=TRUE)


###################################################
### code chunk number 9: barsurf.Rnw:161-162
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_surface (rotated.sinc, c (-15.5, 15.5), n=40)


###################################################
### code chunk number 10: barsurf.Rnw:188-190
###################################################
f <- function (w1, w2, w3 = 1 - w1 - w2)
    (w1 - 1 / 3)^2 + (w2 - 1 / 3)^2 + (w3 - 1 / 3)^2


###################################################
### code chunk number 11: barsurf.Rnw:194-195
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_tricontour (f, xlab="w1", ylab="w2")


###################################################
### code chunk number 12: barsurf.Rnw:218-221
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_isosurface (bispherical.dist,
    c (-3, 3),, c (-2, 2), nsurfaces=3,
    ref.arrows=FALSE, pconstants = c (1, 1, 0) )


###################################################
### code chunk number 13: barsurf.Rnw:240-241
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield3 (bispherical.dist, c (-3, 3),, c (-2, 2), emph="l")


###################################################
### code chunk number 14: barsurf.Rnw:258-259
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_vec2 (circular.field, c (-1.6, 1.6), c (-1.1, 1.1) )


###################################################
### code chunk number 15: barsurf.Rnw:276-278
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_vec3 (plughole.field, c (-1.5, 1.5),, c (-0.35, 1.3),
    ref.arrows=FALSE)


###################################################
### code chunk number 16: barsurf.Rnw:316-317
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.litmus () )


###################################################
### code chunk number 17: barsurf.Rnw:319-320
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (green.litmus () )


###################################################
### code chunk number 18: barsurf.Rnw:322-323
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.litmus.hcv () )


###################################################
### code chunk number 19: barsurf.Rnw:325-326
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (green.litmus.hcv () )


###################################################
### code chunk number 20: barsurf.Rnw:328-329
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (rainbow.litmus () )


###################################################
### code chunk number 21: barsurf.Rnw:331-332
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (heat.litmus () )


###################################################
### code chunk number 22: barsurf.Rnw:338-339
###################################################
u <- rnorm (30, 4, 1)^3


###################################################
### code chunk number 23: barsurf.Rnw:343-344
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.litmus.fit.hcv (u) )


###################################################
### code chunk number 24: barsurf.Rnw:368-371
###################################################
.colvs <- cbind (c (c (100, 150, 200, 250) ), 35, 75)
rownames (.colvs) <- c ("first color", "(2nd)", "(3rd)", "last color")
colnames (.colvs) <- c ("H", "C", "L")


###################################################
### code chunk number 25: barsurf.Rnw:374-375
###################################################
.colvs


###################################################
### code chunk number 26: barsurf.Rnw:383-385
###################################################
my.litmus <- function (a=0, b=1)
    litmus (a, b, .colvs, color.space="HCL")


###################################################
### code chunk number 27: barsurf.Rnw:389-390
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (my.litmus () )


###################################################
### code chunk number 28: barsurf.Rnw:396-398
###################################################
my.litmus.fit <- function (x, ...)
    litmus.fit (x, .colvs, color.space="HCL", ...)


###################################################
### code chunk number 29: barsurf.Rnw:402-403
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (my.litmus.fit (u) )


###################################################
### code chunk number 30: barsurf.Rnw:428-431
###################################################
x <- y <- seq (-15.5, 15.5, length.out=60)
fv <- outer (x, y, rotated.sinc)
colf <- blue.litmus.fit.hcv (fv)


###################################################
### code chunk number 31: barsurf.Rnw:435-444
###################################################
getOption("SweaveHooks")[["fig"]]()
#larger heatmap
plot_cfield (x, y, fv,
    contours=FALSE, colf=colf)
#smaller heatmap
plotf_cfield (rotated.sinc, c (-4, 4),
    add=TRUE, contours=FALSE, colf=colf, n=60)
#contour lines
plot_cfield (x, y, fv,
    add=TRUE, fb=0, heatmap=FALSE)


###################################################
### code chunk number 32: barsurf.Rnw:455-456
###################################################
f <- function (x, y) x + x^3 + y + y^3


###################################################
### code chunk number 33: barsurf.Rnw:460-462
###################################################
tempff <- function (x)
    hot.and.cold.fit (x, t = c (-0.2, 0.2) )


###################################################
### code chunk number 34: barsurf.Rnw:467-471
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_surface (f, c (-1, 1),
    grid.color="grey65",
    gradient.shading=FALSE,
    colff=tempff)


###################################################
### code chunk number 35: barsurf.Rnw:478-481
###################################################
tempff2 <- function (x)
    hot.and.cold.fit (x, t = c (-0.2, 0.2),
    hot.hue=100, cold.hue=60)


