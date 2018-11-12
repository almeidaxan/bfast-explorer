import ee
import numpy
import ssl
from datetime import datetime

# Earth Engine API init
ee.Initialize()

# Convert YYYYMMDD format to YYYY-MM-DD
def convertDate(s):
	return datetime.strptime(s, '%Y%m%d').strftime('%Y_%m_%d')

# Function to calculate and add some spectral-band-based indexes (LS 4,5,7)
def getVi(image):
	scalingFactor = 0.0001
	b1 = image.select(['B1']).multiply(scalingFactor)
	b2 = image.select(['B2']).multiply(scalingFactor)
	b3 = image.select(['B3']).multiply(scalingFactor)
	b4 = image.select(['B4']).multiply(scalingFactor)
	b5 = image.select(['B5']).multiply(scalingFactor)
	b7 = image.select(['B7']).multiply(scalingFactor)
	evi = image.expression('2.5*(NIR - R)/(NIR + 6.0*R - 7.5*B + 1)', {'R': b3, 'NIR': b4, 'B': b1})
	evi2 = image.expression('2.5*(NIR - R)/(NIR + 2.4*R + 1)', {'R': b3, 'NIR': b4})
	ndvi = image.normalizedDifference(['B4','B3'])
	ndmi = image.normalizedDifference(['B4','B5'])
	image2 = (
		image.select(['pixel_qa'])
		.addBands(b1.select([0],['b1']))
		.addBands(b2.select([0],['b2']))
		.addBands(b3.select([0],['b3']))
		.addBands(b4.select([0],['b4']))
		.addBands(b5.select([0],['b5']))
		.addBands(b7.select([0],['b7']))
		.addBands(evi.select([0],['evi']))
		.addBands(evi2.select([0],['evi2']))
		.addBands(ndvi.select([0],['ndvi']))
		.addBands(ndmi.select([0],['ndmi']))
	)
	return(image2)

# Function to calculate and add some spectral-band-based indexes (LS 8)
def getVi8(image):
	scalingFactor = 0.0001
	b1 = image.select(['B1']).multiply(scalingFactor)
	b2 = image.select(['B2']).multiply(scalingFactor)
	b3 = image.select(['B3']).multiply(scalingFactor)
	b4 = image.select(['B4']).multiply(scalingFactor)
	b5 = image.select(['B5']).multiply(scalingFactor)
	b7 = image.select(['B7']).multiply(scalingFactor)
	evi = image.expression('2.5*(NIR - R)/(NIR + 6.0*R - 7.5*B + 1)', {'R': b4, 'NIR': b5, 'B': b2})
	evi2 = image.expression('2.5*(NIR - R)/(NIR + 2.4*R + 1)', {'R': b4, 'NIR': b5})
	ndvi = image.normalizedDifference(['B5','B4'])
	ndmi = image.normalizedDifference(['B5','B6'])
	image2 = (
		image.select(['pixel_qa'])
		.addBands(b1.select([0],['b1']))
		.addBands(b2.select([0],['b2']))
		.addBands(b3.select([0],['b3']))
		.addBands(b4.select([0],['b4']))
		.addBands(b5.select([0],['b5']))
		.addBands(b7.select([0],['b7']))
		.addBands(evi.select([0],['evi']))
		.addBands(evi2.select([0],['evi2']))
		.addBands(ndvi.select([0],['ndvi']))
		.addBands(ndmi.select([0],['ndmi']))
	)
	return(image2)

# Create a geometry point with provided coordinates
longCen = float(coords[0])
latCen = float(coords[1])
bounds = ee.Geometry.Point([longCen, latCen])

# Define the image collection
imgCol = ee.ImageCollection('LANDSAT/' + satChoice)

# Get time series from GEE
if satChoice == 'LC08/C01/T1_SR':
	values = imgCol.filterBounds(bounds).map(getVi8).getRegion(bounds, 30)
else:
	values = imgCol.filterBounds(bounds).map(getVi).getRegion(bounds, 30)

# Get time series info and apply cfmask filter
try:
	aux = values.getInfo()
	serie = []
	# for i in range(1, len(aux)):
	# 	print aux[i][4] == None
	for i in range(1, len(aux)):
		serie += [convertDate(str(aux[i][0])[12:20])] + \
		['NA' if v is None else v for v in aux[i][5:15]]
		#numpy.round(aux[i][5:15], 4).tolist()

	# Additional variables to be used in R
	if len(aux) > 0:
		colNames = [u'date'] + aux[0][5:15]
		numCol = len(colNames)
		numRow = len(aux) - 1
except ee.ee_exception.EEException:
	# No data available
	serie = []
