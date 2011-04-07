library(caret)
species.full = read.table("../data/speciesprocessed.csv",header=T,sep=",")
species.features = subset(species.full,select=c("name","sex","age","weight","visits","totsibs","actsibs"))

namefreq = as.data.frame(with(species.features, table(name)))
excludename = as.character(namefreq[namefreq$Freq < 100,"name"])
badnames = as.integer(rownames(species.features[species.features$name %in% excludename,]))
levels(species.features$name) = c(levels(species.features$name),"Other")
species.features[badnames,]$name = "Other"
species.features$name = species.features$name[drop=T]

namemodelmatrix = model.matrix(~ name - 1, data=species.features)
sexmodelmatrix = model.matrix(~ sex - 1, data=species.features)
species.features = subset(species.features,select=c("age","weight","visits","totsibs","actsibs"))
species.features = cbind(species.features, sexmodelmatrix)
species.features = cbind(species.features, namemodelmatrix)

species.targets = subset(species.full, select="species")
species.targets = species.targets$species[drop=TRUE]

set1index = createDataPartition(species.targets, p=.2, list=FALSE, times=1)
species.targets.test = species.targets[set1index]
species.features.test = species.features[set1index,]
species.targets.train = species.targets[-set1index]
species.features.train = species.features[-set1index,]

annmodel = train(species.features.train,species.targets.train,"nnet")

speciesPredictions = extractPrediction(list(annmodel),testX=species.features.test,testY=species.targets.test)
plotObsVsPred(speciesPredictions)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
speciesProbs = extractProb(list(annmodel),testX=species.features.test,testY=species.targets.test)
plotClassProbs(speciesProbs)
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

nbmodel = train(species.features.train,species.targets.train,"nb")

speciesPredictions = extractPrediction(list(nbmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

rfmodel = train(species.features.train,species.targets.train,"rf")

speciesPredictions = extractPrediction(list(rfmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
speciesProbs = extractProb(list(rfmodel),testX=species.features.test,testY=species.targets.test)
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

