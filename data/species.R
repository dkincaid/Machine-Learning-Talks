species.full = read.table("species.csv",header=T,sep=",")
species.full = species.full[species.features$species=="Canine" | species.features$species=="Feline",]
species.full = species.full[species.full$age < 20000,]
species.full = species.full[species.full$age > 0,]
species.full = species.full[species.full$units =="pounds",]
species.full = species.full[species.full$weight < 1000,]
species.features = subset(species.full,select=c("age","weight"))
species.targets = subset(species.full, select="species")
species.targets = species.targets$species[drop=TRUE]

set1index = createDataPartition(species.targets, p=.2, list=FALSE, times=1)
species.targets.test = species.targets[set1index]
species.features.test = species.features[set1index,]
species.targets.train = species.targets[-set1index]
species.features.train = species.features[-set1index]


knnmodel = train(species.features.train,species.targets.train,"knn")

speciesPredictions = extractPrediction(list(knnmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

nbmodel = train(species.features.train,species.targets.train,"nb")

speciesPredictions = extractPrediction(list(nbmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)


svmmodel = train(species.features.train,species.targets.train,"svmLinear")

speciesPredictions = extractPrediction(list(svmmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)
