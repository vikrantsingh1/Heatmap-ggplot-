setwd("Desktop/")
library(gplots)
library(RColorBrewer)
df<-read.table("test.txt",header=T,stringsAsFactor=F,sep="\t",comment="")
rnames <- df[,1]  
my_data <- data.matrix(df[,2:ncol(df)])
rownames(my_data) <- rnames 
hclustfunc <- function(x) hclust(x, method="complete")
distfunc <- function(x) dist(x,method="euclidean")

d <- distfunc(my_data)
fit <- hclustfunc(d)
clusters <- cutree(fit, h=100)
nofclust.height <-  length(unique(as.vector(clusters)));

# Colorings
hmcols <- rev(greenred(2750))
selcol <- colorRampPalette(brewer.pal(12,"Set3"))
selcol2 <- colorRampPalette(brewer.pal(9,"Set1"))
clustcol.height = selcol2(nofclust.height);
pdf("heatmap.pdf",width=10, height=15)
heatmap.2(as.matrix(my_data),
          density.info="none",
         # cellnote= my_data,
          trace='none',
          dendrogram='row',
          key=T,
          Colv=F,
          scale='row',
         hclust=hclustfunc, distfun=distfunc,
         col=hmcols,
        symbreak=T,
        margins=c(9,12), keysize=1,
        #lmat=rbind(c(5,0,4,0),c(3,1,2,0)), lhei=c(2.0,5.0),
        #lwid=c(1.5,0.2,2.5,2.5),
       lmat=NULL, 
       lwid=NULL, lhei=NULL,
          labRow=rownames(my_data),
          labCol =colnames(my_data),
          #ColSideColors=hmcols;
          #RowSideColors=clustcol.height[clusters],
         cexCol = 0.9
    )
dev.off()

#####################################


