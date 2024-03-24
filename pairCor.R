pair.cor <- function(x, test=c("pearson", "kendall", "spearman"), p.adj.method=c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")){
  mat <- as.matrix(x)
  n <- length(mat[1,])
  N <- n*(n-1)/2
  cor.stats <- matrix(nrow=length(mat[1,]),ncol=length(mat[1,]))
  p.value <- matrix(nrow=length(mat[1,]),ncol=length(mat[1,]))
  p.adj <- matrix(nrow=length(mat[1,]),ncol=length(mat[1,]))
  for( i in 1:length(mat[1,]) ){
    v1 <- mat[,i]
    cor <- vector()
    p <- vector()
    padj <- vector()
    for( j in 1:length(mat[1,]) ){
      v2 <- mat[,j]
      COR <- cor.test(v1,v2, method=test)
      cor <- append(cor,COR$estimate)
      p <- append(p,COR$p.value)
      padj <- append(padj,p.adjust(p[j], method = p.adj.method, n=N ))
    }
    cor.stats[i,] <- cor
    p.value[i,] <- p
    p.adj[i,] <- padj
    colnames(cor.stats)<-colnames(mat)
    rownames(cor.stats)<-colnames(mat)
    colnames(p.value)<-colnames(mat)
    rownames(p.value)<-colnames(mat)
    colnames(p.adj)<-colnames(mat)
    rownames(p.adj)<-colnames(mat)
  }
  result <- list(stats=cor.stats, p.values=p.value, p.adjust=p.adj)
  return(result)
}
