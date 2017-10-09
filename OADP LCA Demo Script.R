library(MplusAutomation)

createModels("C:/Users/tuf22063/Dropbox/R Talk/LCA/Create Models Syntax.txt")

runModels("C:/Users/tuf22063/Dropbox/R Talk/LCA", recursive=FALSE)

OADP_LCA_Summary<- readModels("C:/Users/tuf22063/Dropbox/R Talk/LCA", recursive=TRUE)

HTMLSummaryTable(OADP_LCA_Summary, filename="C:/Users/tuf22063/Dropbox/R Talk/LCA/OADP LCA Summary.html", display=TRUE, keepCols=c("Title", "LL", "AIC", "BIC", "aBIC", "AICC", "Parameters", "T11_VLMR_2xLLDiff", "T11_VLMR_ParamDiff", "T11_VLMR_PValue", "T11_LMR_Value", "T11_LMR_PValue"), sortBy="Parameters")

library(ggplot2)

cl2modelParams <- extractModelParameters("C:/Users/tuf22063/Dropbox/R Talk/LCA/oadp lca demo - 2 class model.out")$unstandardized
cl2modelParams <- subset(cl2modelParams, paramHeader=="Thresholds" & LatentClass !="Categorical.Latent.Variables", select=c("LatentClass", "param", "est", "se"))
cl2limits <- aes(ymax = est + se, ymin=est - se)
cl2fmmMeanPlot <- ggplot(cl2modelParams, aes(x=param, y=est)) +
  geom_pointrange(cl2limits) +
  scale_x_discrete("") +
  geom_hline(yintercept=0, color="grey50") +
  facet_grid(LatentClass ~ .) +
  theme_bw() +
  ylab("Mean Value") +
  coord_flip()
print(cl2fmmMeanPlot)

cl3modelParams <- extractModelParameters("C:/Users/tuf22063/Dropbox/R Talk/LCA/oadp lca demo - 3 class model.out")$unstandardized
cl3modelParams <- subset(cl3modelParams, paramHeader=="Thresholds" & LatentClass !="Categorical.Latent.Variables", select=c("LatentClass", "param", "est", "se"))
cl3limits <- aes(ymax = est + se, ymin=est - se)
cl3fmmMeanPlot <- ggplot(cl3modelParams, aes(x=param, y=est)) +
  geom_pointrange(cl3limits) +
  scale_x_discrete("") +
  geom_hline(yintercept=0, color="grey50") +
  facet_grid(LatentClass ~ .) +
  theme_bw() +
  ylab("Mean Value") +
  coord_flip()
print(cl3fmmMeanPlot)

cl4modelParams <- extractModelParameters("C:/Users/tuf22063/Dropbox/R Talk/LCA/oadp lca demo - 4 class model.out")$unstandardized
cl4modelParams <- subset(cl4modelParams, paramHeader=="Thresholds" & LatentClass !="Categorical.Latent.Variables", select=c("LatentClass", "param", "est", "se"))
cl4limits <- aes(ymax = est + se, ymin=est - se)
cl4fmmMeanPlot <- ggplot(cl4modelParams, aes(x=param, y=est)) +
  geom_pointrange(cl4limits) +
  scale_x_discrete("") +
  geom_hline(yintercept=0, color="grey50") +
  facet_grid(LatentClass ~ .) +
  theme_bw() +
  ylab("Mean Value") +
  coord_flip()
print(cl4fmmMeanPlot)

cl5modelParams <- extractModelParameters("C:/Users/tuf22063/Dropbox/R Talk/LCA/oadp lca demo - 5 class model.out")$unstandardized
cl5modelParams <- subset(cl5modelParams, paramHeader=="Thresholds" & LatentClass !="Categorical.Latent.Variables", select=c("LatentClass", "param", "est", "se"))
cl5limits <- aes(ymax = est + se, ymin=est - se)
cl5fmmMeanPlot <- ggplot(cl5modelParams, aes(x=param, y=est)) +
  geom_pointrange(cl5limits) +
  scale_x_discrete("") +
  geom_hline(yintercept=0, color="grey50") +
  facet_grid(LatentClass ~ .) +
  theme_bw() +
  ylab("Mean Value") +
  coord_flip()
print(cl5fmmMeanPlot)

cl6modelParams <- extractModelParameters("C:/Users/tuf22063/Dropbox/R Talk/LCA/oadp lca demo - 6 class model.out")$unstandardized
cl6modelParams <- subset(cl6modelParams, paramHeader=="Thresholds" & LatentClass !="Categorical.Latent.Variables", select=c("LatentClass", "param", "est", "se"))
cl6limits <- aes(ymax = est + se, ymin=est - se)
cl6fmmMeanPlot <- ggplot(cl6modelParams, aes(x=param, y=est)) +
  geom_pointrange(cl6limits) +
  scale_x_discrete("") +
  geom_hline(yintercept=0, color="grey50") +
  facet_grid(LatentClass ~ .) +
  theme_bw() +
  ylab("Mean Value") +
  coord_flip()
print(cl6fmmMeanPlot)

cl7modelParams <- extractModelParameters("C:/Users/tuf22063/Dropbox/R Talk/LCA/oadp lca demo - 7 class model.out")$unstandardized
cl7modelParams <- subset(cl7modelParams, paramHeader=="Thresholds" & LatentClass !="Categorical.Latent.Variables", select=c("LatentClass", "param", "est", "se"))
cl7limits <- aes(ymax = est + se, ymin=est - se)
cl7fmmMeanPlot <- ggplot(cl7modelParams, aes(x=param, y=est)) +
  geom_pointrange(cl7limits) +
  scale_x_discrete("") +
  geom_hline(yintercept=0, color="grey50") +
  facet_grid(LatentClass ~ .) +
  theme_bw() +
  ylab("Mean Value") +
  coord_flip()
print(cl7fmmMeanPlot)

ggsave(filename="cl2fmmMeanPlot.pdf", plot=cl2fmmMeanPlot)
ggsave(filename="cl3fmmMeanPlot.pdf", plot=cl3fmmMeanPlot)
ggsave(filename="cl4fmmMeanPlot.pdf", plot=cl4fmmMeanPlot)
ggsave(filename="cl5fmmMeanPlot.pdf", plot=cl5fmmMeanPlot)
ggsave(filename="cl6fmmMeanPlot.pdf", plot=cl6fmmMeanPlot)
ggsave(filename="cl7fmmMeanPlot.pdf", plot=cl7fmmMeanPlot)