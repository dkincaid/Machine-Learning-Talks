library(caret)
# Load the csv file and pull out just the columns we are interested in
species.full = read.table("../data/speciesprocessed.csv",header=T,sep=",")
species.features = subset(species.full,select=c("name","sex","age","weight","visits","totsibs","actsibs"))
head(species.features, 10)
str(species.features)

# Create a data frame with counts of the names
namefreq = as.data.frame(with(species.features, table(name)))
head(namefreq[with(namefreq, order(-Freq)),],10)

# Change all the names with less than 100 occurences into "Other"
excludename = as.character(namefreq[namefreq$Freq < 100,"name"])
badnames = as.integer(rownames(species.features[species.features$name %in% excludename,]))
levels(species.features$name) = c(levels(species.features$name),"Other")
species.features[badnames,]$name = "Other"
species.features$name = species.features$name[drop=T]
head(species.features, 10)
str(species.features)

# Create the dummy variables for the categorical features name and sex
matrixdummies = dummyVars( ~ ., data=species.features)
species.features = predict(matrixdummies, species.features)
species.features = as.data.frame(species.features)
head(species.features)
str(species.features)

# Create the list of target species
species.targets = subset(species.full, select="species")
species.targets = species.targets$species[drop=TRUE]
head(species.targets)
str(species.targets)

# Partition the examples into 80% for training and 20% for testing
set1index = createDataPartition(species.targets, p=.2, list=FALSE, times=1)
species.targets.test = species.targets[set1index]
species.features.test = species.features[set1index,]
species.targets.train = species.targets[-set1index]
species.features.train = species.features[-set1index,]
str(species.features.test)
str(species.features.train)

# Running the models
library(doMC)
registerDoMC(cores=8)

# Artificial Neural Network
annmodel = train(species.features.train,species.targets.train,"nnet")

speciesPredictions = extractPrediction(list(annmodel),testX=species.features.test,testY=species.targets.test)
plotObsVsPred(speciesPredictions)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
speciesProbs = extractProb(list(annmodel),testX=species.features.test,testY=species.targets.test)
plotClassProbs(speciesProbs)
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

# Naive Bayes
nbmodel = train(species.features.train,species.targets.train,"nb")

speciesPredictions = extractPrediction(list(nbmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

# Random Forest
rfmodel = train(species.features.train,species.targets.train,"rf")

speciesPredictions = extractPrediction(list(rfmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
speciesProbs = extractProb(list(rfmodel),testX=species.features.test,testY=species.targets.test)
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

