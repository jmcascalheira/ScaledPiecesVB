barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("122", "95", "66", "11", "69", "7", "36", "27", "101", "100", 
"109", "110", "70")
plot.MCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = c('var', 'quali'), title = '', cex = cex)
wilks.p <-
c(RawMaterial = 0.000225130888006352)
wilks.p
sample = sample(rownames(res$call$X), length(rownames(res$call$X)))
res$call$X = res$call$X[sample,]
res$ind$coord = res$ind$coord[sample[!sample %in% rownames(res$ind.sup$coord)],]
res$ind.sup$coord = res$ind.sup$coord[sample[sample %in% rownames(res$ind.sup$coord)],]
drawn <-
c("122", "95", "66", "11", "69", "7", "36", "27", "101", "100", 
"109", "110", "70")
hab <-
"RawMaterial"
plotellipses(res, axes = 1:2, invisible = c('var', 'quali'), select = drawn, keepvar = hab, title = '', cex = cex)
drawn <-
c("ScarArrangement_Isolated/Isolated", "ScarDistribution_Total/Total", 
"ScarExtension_Marginal/Marginal", "ScarArrangement_Other", "ScarDistribution_Central/Total", 
"ScarDistribution_Central/Central", "ScarExtension_Other", "ScarArrangement_Aligned/Isolated"
)
plot.MCA(res, selectMod = drawn, axes = 1:2, choix = 'ind', invisible = 'ind', title = '', cex = cex)
drawn <-
c("26", "43", "68", "97", "49", "83", "65", "15", "28", "107", 
"25", "69", "7")
plot.MCA(res, select = drawn, axes = 3:4, choix = 'ind', invisible = c('var', 'quali'), title = '', cex = cex)
wilks.p <-
c(RawMaterial = 0.285066928942493)
wilks.p
sample = sample(rownames(res$call$X), length(rownames(res$call$X)))
res$call$X = res$call$X[sample,]
res$ind$coord = res$ind$coord[sample[!sample %in% rownames(res$ind.sup$coord)],]
res$ind.sup$coord = res$ind.sup$coord[sample[sample %in% rownames(res$ind.sup$coord)],]
drawn <-
c("26", "43", "68", "97", "49", "83", "65", "15", "28", "107", 
"25", "69", "7")
hab <-
"RawMaterial"
plotellipses(res, axes = 3:4, invisible = c('var', 'quali'), select = drawn, keepvar = hab, title = '', cex = cex)
drawn <-
c("ScarEdgeDelineation_Other", "ScarEdgeDelineation_Straight/Straight", 
"ScarEdgeDelineation_Irregular/Straight", "Unifacial/Bifacial", 
"ScarArrangement_Aligned/Isolated", "ScarExtension_Invasive/Invasive", 
"ScarExtension_Other", "Unifacial/Unifacial")
plot.MCA(res, selectMod = drawn, axes = 3:4, choix = 'ind', invisible = 'ind', title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("122", "95", "66", "11", "69", "7", "36", "27", "101", "100", 
"109", "110", "70")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
