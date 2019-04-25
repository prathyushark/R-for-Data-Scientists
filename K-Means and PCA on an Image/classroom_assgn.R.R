library(jpeg)
library(RCurl)

########### K-MEANS ################

url <-"http://utdallas.edu/~txc163430/yellowStone.jpg"
readImage <- readJPEG(getURLContent(url, binary=TRUE))
dm <- dim(readImage)
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

plot(y ~ x, data=rgbImage, main="Yellowstone",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")

getPlots <- function(kval) {
  kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                   centers = kval)
  clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])
  plot(y ~ x, data=rgbImage, main="Yellowstone",
       col = clusterColour, asp = 1, pch = ".",
       axes=FALSE, ylab="", 
       xlab=paste("k-means cluster analysis of ", kval, " colours"))
}

lapply(2:10, getPlots)

wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(rgbImage) 


########## PCA #############

url <-"http://utdallas.edu/~txc163430/yellowStone.jpg"
img_new <- readJPEG(getURLContent(url, binary=TRUE))
r <- img_new[,,1]
g <- img_new[,,2]
b <- img_new[,,3]

img_new.r.pca <- prcomp(r, center = FALSE)
img_new.g.pca <- prcomp(g, center = FALSE)
img_new.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(img_new.r.pca, img_new.g.pca, img_new.b.pca)

for (i in seq.int(3, round(nrow(img_new) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('compressed/img_new_', round(i,0), '_components.jpg', sep = ''))
}

img_dwn <- download.file('http://utdallas.edu/~txc163430/yellowStone.jpg', destfile = 'yellowstone.jpg')
original <- file.info('yellowstone.jpg')$size / 1000
imgs <- dir('compressed/')

for (i in imgs) {
  full.path <- paste('compressed/', i, sep='')
  print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}
