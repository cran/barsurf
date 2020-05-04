### R code from vignette source 'barsurf.Rnw'

###################################################
### code chunk number 1: barsurf.Rnw:37-42
###################################################
options(continue="   ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 1.35, 1.6), cex=0.7, cex.main=1)))

set.seed (1)


###################################################
### code chunk number 2: barsurf.Rnw:87-89
###################################################
library (barsurf)
library (misc3d)


###################################################
### code chunk number 3: barsurf.Rnw:96-97
###################################################
set.bs.options (rendering.style="e", theme="blue")


###################################################
### code chunk number 4: barsurf.Rnw:117-119
###################################################
n <- 10
p <- 0.5


###################################################
### code chunk number 5: barsurf.Rnw:122-126
###################################################
x <- y <- 0:10
f <- function (x, y, n, p)
    dbinom (x, n, p) * dbinom (y, n, p)
fv <- outer (x, y, f, n, p)


###################################################
### code chunk number 6: barsurf.Rnw:131-132
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_dfield (x, y, fv)


###################################################
### code chunk number 7: barsurf.Rnw:135-136
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_bar (,,fv)


###################################################
### code chunk number 8: barsurf.Rnw:145-147
###################################################
args (plot_dfield)
args (plotf_dfield)


###################################################
### code chunk number 9: barsurf.Rnw:168-169
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield (rotated.sinc, c (-15.5, 15.5), fb=0, n=60, hcv=TRUE)


###################################################
### code chunk number 10: barsurf.Rnw:172-173
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_surface (rotated.sinc, c (-15.5, 15.5), n=40)


###################################################
### code chunk number 11: barsurf.Rnw:199-200
###################################################
M <- matrix (sample (1:50), 5, 10)


###################################################
### code chunk number 12: barsurf.Rnw:204-207
###################################################
getOption("SweaveHooks")[["fig"]]()
p0 <- matrix.margins ()
plot_matrix (,,M, bin.labels=TRUE)
par (p0)


###################################################
### code chunk number 13: barsurf.Rnw:237-242
###################################################
getOption("SweaveHooks")[["fig"]]()
nested_isosurfaces (bispherical.dist,
    c (-3, 3),, c (-2, 2), fb = c (0.5, 1, 1.75),
    base.contours=TRUE, rear.contours=TRUE,
    arrows=FALSE,
    pconstants = c (1, 1, 0) )


###################################################
### code chunk number 14: barsurf.Rnw:261-263
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield_3d (bispherical.dist, c (-3, 3),, c (-2, 2),
    fb = c (0.5, 1, 1.75, 2.5), emph="l")


###################################################
### code chunk number 15: barsurf.Rnw:284-286
###################################################
f <- function (w1, w2, w3)
    (w1 - 1 / 3)^2 + (w2 - 1 / 3)^2 + (w3 - 1 / 3)^2


###################################################
### code chunk number 16: barsurf.Rnw:294-295
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_tricontour (f, xlab="w1", ylab="w2")


###################################################
### code chunk number 17: barsurf.Rnw:297-298
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_trisurface (f, xlab="w1", ylab="w2")


###################################################
### code chunk number 18: barsurf.Rnw:319-321
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_vecfield (concentric.field, c (-1.6, 1.6), c (-1.1, 1.1),
    m = c (80, 60) )


###################################################
### code chunk number 19: barsurf.Rnw:332-334
###################################################
set.bs.theme ("heat")
set.bs.theme ("blue")


###################################################
### code chunk number 20: barsurf.Rnw:350-352
###################################################
colf <- heat.barface ()
colf (c (TRUE, FALSE) )


###################################################
### code chunk number 21: barsurf.Rnw:355-357
###################################################
colf <- heat.litmus ()
colf (c (0.25, 0.75) )


###################################################
### code chunk number 22: barsurf.Rnw:378-379
###################################################
colf <- barface (c (0, 0, 1) )


###################################################
### code chunk number 23: barsurf.Rnw:383-384
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 24: barsurf.Rnw:390-391
###################################################
colf <- barface (c (90, 42.5, 60), c (90, 35, 67.5), color.space="HCL")


###################################################
### code chunk number 25: barsurf.Rnw:395-396
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 26: barsurf.Rnw:403-404
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.barface () )


###################################################
### code chunk number 27: barsurf.Rnw:406-407
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (green.barface () )


###################################################
### code chunk number 28: barsurf.Rnw:423-426
###################################################
colvs <- cbind (c (c (100, 150, 200, 250) ), 35, 75)
rownames (colvs) <- c ("first color", "(2nd)", "(3rd)", "last color")
colnames (colvs) <- c ("H", "C", "L")


###################################################
### code chunk number 29: barsurf.Rnw:429-430
###################################################
colvs


###################################################
### code chunk number 30: barsurf.Rnw:438-440
###################################################
colf <- litmus (,,colvs, color.space="HCL")
colf


###################################################
### code chunk number 31: barsurf.Rnw:444-445
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 32: barsurf.Rnw:452-453
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (heat.litmus () )


###################################################
### code chunk number 33: barsurf.Rnw:455-456
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.litmus () )


###################################################
### code chunk number 34: barsurf.Rnw:458-459
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (green.litmus () )


###################################################
### code chunk number 35: barsurf.Rnw:461-462
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (rainbow.litmus () )


###################################################
### code chunk number 36: barsurf.Rnw:475-476
###################################################
colf <- mlitmus (blue.litmus (-1, 0, reverse=TRUE), green.litmus (0, 1) )


###################################################
### code chunk number 37: barsurf.Rnw:480-481
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 38: barsurf.Rnw:488-489
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (hot.and.cold () )


###################################################
### code chunk number 39: barsurf.Rnw:497-500
###################################################
colf1 <- glass.rainbow (0, 1.25, 1, start=-200, end=200)
colf2 <- glass.rainbow (1.25, 1.75, c (1, 0.7, 0.4, 0.1), start=200, end=220)
colf3 <- glass.rainbow (1.75, 5, 0.1, start=220)


###################################################
### code chunk number 40: barsurf.Rnw:506-507
###################################################
colf <- mlitmus (colf1, colf2, colf3)


###################################################
### code chunk number 41: barsurf.Rnw:515-516
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 42: barsurf.Rnw:524-525
###################################################
z <- c (-2, -1.6, -1.2, -0.5, -0.1667, 0.1667, 0.5, 1.2, 1.6, 2)


###################################################
### code chunk number 43: barsurf.Rnw:529-531
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield_3d (bispherical.dist, c (-3, 3), z=z,
    fb = c (0.5, 1, 1.75, 2.5), color.function=colf)


###################################################
### code chunk number 44: barsurf.Rnw:544-548
###################################################
custom.litmus.fit <- function (x)
{   colvs <- cbind (c (c (100, 150, 200, 250) ), 35, 75)
    litmus.fit (x, colvs, color.space="HCL")
}


###################################################
### code chunk number 45: barsurf.Rnw:552-553
###################################################
x <- runif (10)


###################################################
### code chunk number 46: barsurf.Rnw:557-559
###################################################
range (x)
custom.litmus.fit (x)


###################################################
### code chunk number 47: barsurf.Rnw:563-564
###################################################
blue.litmus.fit (x)


###################################################
### code chunk number 48: barsurf.Rnw:571-574
###################################################
x <- y <- seq (0, 1,, 30)
fv <- outer (x, y)
fv <- fv^3


###################################################
### code chunk number 49: barsurf.Rnw:579-581
###################################################
colf.simple <- blue.litmus.fit (fv, equalize=0)
colf.equalized <- blue.litmus.fit (fv, equalize=1)


###################################################
### code chunk number 50: barsurf.Rnw:587-588
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf.simple)


###################################################
### code chunk number 51: barsurf.Rnw:595-596
###################################################
fb <- c (0.0014, 0.0881)


###################################################
### code chunk number 52: barsurf.Rnw:600-601
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_cfield (,,fv, fb=fb, color.function=colf.simple)


###################################################
### code chunk number 53: barsurf.Rnw:608-609
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf.equalized)


###################################################
### code chunk number 54: barsurf.Rnw:616-617
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_cfield (,,fv, fb=fb, color.function=colf.equalized)


###################################################
### code chunk number 55: barsurf.Rnw:646-649
###################################################
x <- y <- seq (-15.5, 15.5, length.out=60)
fv <- outer (x, y, rotated.sinc)
colf <- blue.litmus.fit.hcv (fv)


###################################################
### code chunk number 56: barsurf.Rnw:653-662
###################################################
getOption("SweaveHooks")[["fig"]]()
#larger heatmap
plot_cfield (x, y, fv,
    contours=FALSE, color.function=colf)
#smaller heatmap
plotf_cfield (rotated.sinc, c (-4, 4),
    add=TRUE, contours=FALSE, color.function=colf, n=60)
#contour lines
plot_cfield (x, y, fv,
    add=TRUE, fb=0, heatmap=FALSE)


