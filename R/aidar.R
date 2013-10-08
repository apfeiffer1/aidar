# --------------------------------------------------------------------------------
# lists the content of a given AIDA file (uncompressed XML)

getFileInfo <- function(fileName) {

	doc   = xmlRoot(xmlTreeParse(fileName, useInternalNodes=TRUE))
	allTypes = names(doc)  # c("histogram1d", "histogram2d", "histogram3d", "profile1d", "profile2d", "tuple", "cloud1d", "cloud2d", "cloud3d")

	content = vector(mode="list", length=length(allTypes))
	names(content) = allTypes
	
	for (type in allTypes ) {
		if ( type != "implementation") {
			aidaObj = doc[type] # getNodeSet(doc, paste("/aida/",type,"[@name]") )
			if ( !is.null( aidaObj ) ) {
				name    = as.character( sapply( aidaObj, xmlGetAttr, "name") )
				title   = as.character( sapply( aidaObj, xmlGetAttr, "title") )
				ann     = getAnnotation(fileName, name)
				if ( c("Entries") %in% ann$keys) {
					entries = ann$values[ann$keys=="Entries"]
				} else {
					# todo: handle case of several tuples in the same file
					entries = length(doc[["tuple"]][["rows"]])
				}
			}
			content[[type]] = data.frame(name, title, entries, stringsAsFactors = FALSE)
		} # ignore "implementation"
	}
	result = content
}

# --------------------------------------------------------------------------------
# retrieves the annotation of a given AIDA object by it's name from the given file:

getAnnotation <- function(fileName, objectName) {

	doc   = xmlRoot(xmlTreeParse(fileName, useInternalNodes=TRUE))

	# find the correct annotation (we need the type, so we loop over what we got)
 	for (hType in names(doc) ) {   # allTypes ) {
 		if ( hType != "implementation") { # ... and ignore that one :)
 			ann   = getNodeSet(doc, paste("/aida/",hType,"[@name=\"",objectName,"\"]/annotation/item", sep="") )
 			if ( !is.null(ann) ) { break } # found it ...
 		}
 	}

	keys   = as.character( sapply( ann, xmlGetAttr,   "key" ) )
	values = as.character( sapply( ann, xmlGetAttr, "value" ) )

	result = data.frame(keys, values, stringsAsFactors = FALSE)
}

# --------------------------------------------------------------------------------
# retrieves a given 1D histogram by it's name from the given file:

getHisto1D <- function(fileName, histoName) {

	doc   = xmlRoot(xmlTreeParse(fileName))
	bins = getNodeSet(doc, paste("//histogram1d[@name=\"",histoName,"\"]/data1d/bin1d", sep=""))

	binNumber    = as.character( sapply(bins, xmlGetAttr, "binNum") )
	entries      = as.double( sapply(bins, xmlGetAttr, "entries") )
	error        = as.double( sapply(bins, xmlGetAttr, "error") )
	height       = as.double( sapply(bins, xmlGetAttr, "height") )
	weightedMean = as.double( sapply(bins, xmlGetAttr, "weightedMean") )

	xAxisNode = getNodeSet( doc, paste("//histogram1d[@name=\"",histoName,"\"]/axis[@direction='x']", sep="") )
	min   = as.numeric( xmlGetAttr(xAxisNode[[1]], "min") )
	max   = as.numeric( xmlGetAttr(xAxisNode[[1]], "max") )
	nBins = as.numeric( xmlGetAttr(xAxisNode[[1]], "numberOfBins") )

	xAxisNode = getNodeSet( doc, paste("//histogram1d[@name=\"",histoName,"\"]/axis[@direction='x']", sep="") )
	binX = getBins(xAxisNode, binNumber)

	result = data.frame(binNumber, binX, entries, error, height, weightedMean)
}

# --------------------------------------------------------------------------------
# retrieves a given 2D histogram by it's name from the given file:

getHisto2D <- function(fileName, histoName) {

	doc   = xmlRoot(xmlTreeParse(fileName))
	bins = getNodeSet(doc, paste("//histogram2d[@name=\"",histoName,"\"]/data2d/bin2d", sep=""))

	binNumberX    = as.character( sapply(bins, xmlGetAttr, "binNumX") )
	binNumberY    = as.character( sapply(bins, xmlGetAttr, "binNumY") )

	entries       = as.double( sapply(bins, xmlGetAttr, "entries") )
	error         = as.double( sapply(bins, xmlGetAttr, "error") )
	height        = as.double( sapply(bins, xmlGetAttr, "height") )
	weightedMeanX = as.double( sapply(bins, xmlGetAttr, "weightedMeanX") )
	weightedMeanY = as.double( sapply(bins, xmlGetAttr, "weightedMeanY") )

	xAxisNode = getNodeSet( doc, paste("//histogram2d[@name=\"",histoName,"\"]/axis[@direction='x']", sep="") )
	binX = getBins(xAxisNode, binNumberX)

	yAxisNode = getNodeSet( doc, paste("//histogram2d[@name=\"",histoName,"\"]/axis[@direction='y']", sep="") )
	binY = getBins(yAxisNode, binNumberY)

	result = data.frame(binNumberX, binNumberY, binX, binY, entries, error, height, weightedMeanX, weightedMeanY)
}

# --------------------------------------------------------------------------------
# retrieves a given 3D histogram by it's name from the given file:

getHisto3D <- function(fileName, histoName) {

	doc   = xmlRoot(xmlTreeParse(fileName))
	bins = getNodeSet(doc, paste("//histogram3d[@name=\"",histoName,"\"]/data3d/bin3d", sep=""))

	binNumberX    = as.character( sapply(bins, xmlGetAttr, "binNumX") )
	binNumberY    = as.character( sapply(bins, xmlGetAttr, "binNumY") )
	binNumberZ    = as.character( sapply(bins, xmlGetAttr, "binNumZ") )

	entries       = as.double( sapply(bins, xmlGetAttr, "entries") )
	error         = as.double( sapply(bins, xmlGetAttr, "error") )
	height        = as.double( sapply(bins, xmlGetAttr, "height") )

	weightedMeanX = as.double( sapply(bins, xmlGetAttr, "weightedMeanX") )
	weightedMeanY = as.double( sapply(bins, xmlGetAttr, "weightedMeanY") )
	weightedMeanZ = as.double( sapply(bins, xmlGetAttr, "weightedMeanZ") )

	xAxisNode = getNodeSet( doc, paste("//histogram3d[@name=\"",histoName,"\"]/axis[@direction='x']", sep="") )
	binX = getBins(xAxisNode, binNumberX)

	yAxisNode = getNodeSet( doc, paste("//histogram3d[@name=\"",histoName,"\"]/axis[@direction='y']", sep="") )
	binY = getBins(yAxisNode, binNumberY)

	zAxisNode = getNodeSet( doc, paste("//histogram3d[@name=\"",histoName,"\"]/axis[@direction='z']", sep="") )
	binZ = getBins(zAxisNode, binNumberY)

	result = data.frame(binNumberX, binNumberY, binNumberZ, binX, binY, binZ, entries, error, height, weightedMeanX, weightedMeanY, weightedMeanZ)
}

# --------------------------------------------------------------------------------
# retrieves a given 1D profile histogram by it's name from the given file:

getProfile1D <- function(fileName, histoName) {

	doc   = xmlRoot(xmlTreeParse(fileName))
	bins = getNodeSet(doc, paste("//profile1d[@name=\"",histoName,"\"]/data1d/bin1d", sep=""))

	binNumber     = as.character( sapply(bins, xmlGetAttr, "binNum") )
	entries       = as.double( sapply(bins, xmlGetAttr, "entries") )
	error         = as.double( sapply(bins, xmlGetAttr, "error") )
	height        = as.double( sapply(bins, xmlGetAttr, "height") )
	rms           = as.double( sapply(bins, xmlGetAttr, "rms") )
	weightedMean = as.double( sapply(bins, xmlGetAttr, "weightedMean") )

	xAxisNode = getNodeSet( doc, paste("//profile1d[@name=\"",histoName,"\"]/axis[@direction='x']", sep="") )
	binX = getBins(xAxisNode, binNumber)

	result = data.frame(binNumber, binX, entries, error, height, rms, weightedMean)
}

# --------------------------------------------------------------------------------
# retrieves a given 2D profile histogram by it's name from the given file:

getProfile2D <- function(fileName, histoName) {

	doc   = xmlRoot(xmlTreeParse(fileName))
	bins = getNodeSet(doc, paste("//profile2d[@name=\"",histoName,"\"]/data2d/bin2d", sep=""))

	binNumberX    = as.character( sapply(bins, xmlGetAttr, "binNumX") )
	binNumberY    = as.character( sapply(bins, xmlGetAttr, "binNumY") )

	entries       = as.double( sapply(bins, xmlGetAttr, "entries") )
	error         = as.double( sapply(bins, xmlGetAttr, "error") )
	height        = as.double( sapply(bins, xmlGetAttr, "height") )
	rms           = as.double( sapply(bins, xmlGetAttr, "rms") )
	weightedMeanX = as.double( sapply(bins, xmlGetAttr, "weightedMeanX") )
	weightedMeanY = as.double( sapply(bins, xmlGetAttr, "weightedMeanY") )

	xAxisNode = getNodeSet( doc, paste("//profile2d[@name=\"",histoName,"\"]/axis[@direction='x']", sep="") )
	binX = getBins(xAxisNode, binNumberX)
	yAxisNode = getNodeSet( doc, paste("//profile2d[@name=\"",histoName,"\"]/axis[@direction='y']", sep="") )
	binY = getBins(yAxisNode, binNumberY)

	result = data.frame(binNumberX, binNumberY, binX, binY, entries, error, height, rms, weightedMeanX, weightedMeanY)
}

# --------------------------------------------------------------------------------
# retrieves a given tuple by it's name from the given file:

getTuple <- function(fileName, tupName) {

	doc   = xmlRoot(xmlTreeParse(fileName))

	columns = getNodeSet(doc, paste("/aida/tuple[@name=\"",tupName,"\"]/columns/column", sep=""))
	colNames = as.character( sapply( columns, xmlGetAttr, "name") )
	
	rows = getNodeSet(doc, paste("/aida/tuple[@name=\"",tupName,"\"]/rows/row", sep=""))
	rowValues = as.numeric( sapply ( rows, getRow ) )

	result = data.frame( matrix( rowValues, ncol=length(colNames), dimnames = list(c(), colNames), byrow=TRUE ) )
}

# --------------------------------------------------------------------------------
# retrieves a given 1D cloud by it's name from the given file:

getCloud1D <- function(fileName, cloudName) {

	doc   = xmlRoot(xmlTreeParse(fileName))

	bins = getNodeSet(doc, paste("//cloud1d[@name=\"",cloudName,"\"]/entries1d/entry1d", sep=""))
	valuesX = as.double( sapply(bins, xmlGetAttr, "valueX") )

	result = data.frame(valuesX)
}

# --------------------------------------------------------------------------------
# retrieves a given 2D cloud by it's name from the given file:

getCloud2D <- function(fileName, cloudName) {

	doc   = xmlRoot(xmlTreeParse(fileName))

	bins = getNodeSet(doc, paste("//cloud2d[@name=\"",cloudName,"\"]/entries2d/entry2d", sep=""))
	valuesX = as.double( sapply(bins, xmlGetAttr, "valueX") )
	valuesY = as.double( sapply(bins, xmlGetAttr, "valueY") )

	result = data.frame(valuesX, valuesY)
}

# --------------------------------------------------------------------------------
# retrieves a given 3D cloud by it's name from the given file:

getCloud3D <- function(fileName, cloudName) {

	doc   = xmlRoot(xmlTreeParse(fileName))

	bins = getNodeSet(doc, paste("//cloud3d[@name=\"",cloudName,"\"]/entries3d/entry3d", sep=""))
	valuesX = as.double( sapply(bins, xmlGetAttr, "valueX") )
	valuesY = as.double( sapply(bins, xmlGetAttr, "valueY") )
	valuesZ = as.double( sapply(bins, xmlGetAttr, "valueZ") )

	result = data.frame(valuesX, valuesY, valuesZ)
}



# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------

# various helper functions follow here

# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------

getRow <- function(rowNode) {
	values = sapply( getNodeSet(rowNode, "//entry") , xmlGetAttr, "value" )
}

getBins <- function(axisNode, binNumber) {
	min   = as.numeric( xmlGetAttr(axisNode[[1]], "min") )
	max   = as.numeric( xmlGetAttr(axisNode[[1]], "max") )
	nBins = as.numeric( xmlGetAttr(axisNode[[1]], "numberOfBins") )
	
	dx = (max-min)/nBins
	binX = sapply ( binNumber, getBinCentre1D, dx, min, max )
}

getBinCentre1D <- function (binNr, dx, min, max) {
	if (binNr == "UNDERFLOW") return( min-dx )
	if (binNr == "OVERFLOW" ) return( max+dx )

	# print ( paste("binNr: ", binNr, " dx = ", dx, 'val = ', val) )
	result = min + as.double(binNr)*dx + dx/2
}

getEntries <- function(node) {
	result = as.numeric( xmlGetAttr(node, "entries") )
}

getXAxisInfo <- function(node) {
	min   = as.numeric( xmlGetAttr(node, "min") )
	max   = as.numeric( xmlGetAttr(node, "max") )
	nBins = as.numeric( xmlGetAttr(node, "numberOfBins") )
	result <- c(min=min, max=max, nBins=nBins)
}

getXStats <- function(node) {
	mean = as.numeric( xmlGetAttr(node, "mean") )
	rms  = as.numeric( xmlGetAttr(node, "rms") )
	result <- c(mean, rms)
}

getYAxisInfo <- function(node) {
	min   = as.numeric( xmlGetAttr(node, "min") )
	max   = as.numeric( xmlGetAttr(node, "max") )
	nBins = as.numeric( xmlGetAttr(node, "numberOfBins") )
	result <- c(min=min, max=max, nBins=nBins)
}

getYStats <- function(node) {
	mean = as.numeric( xmlGetAttr(node, "mean") )
	rms  = as.numeric( xmlGetAttr(node, "rms") )
	result <- c(mean, rms)
}
