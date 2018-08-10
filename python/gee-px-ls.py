import ee
import numpy
import os
from datetime import datetime
from oauth2client.client import OAuth2Credentials


class FileCredentials(OAuth2Credentials):
  def __init__(self, access_token_file):
    access_token = self._read_access_token(access_token_file)
    super(FileCredentials, self).__init__(access_token, None, None, None, None, None, None)
    self.access_token_file = access_token_file

  def _refresh(self, http):
    self.access_token = self._read_access_token(self.access_token_file)

  def _read_access_token(self, access_token_file):
    with file(access_token_file) as f:
      return f.read()

  def __str__(self):
    return 'FileCredentials(' + self.access_token_file + ')'


# earth engine API init
access_token_file = os.getenv("HOME") + '/.google-access-token'
if os.path.exists(access_token_file):
  ee.Initialize(FileCredentials(access_token_file))
else:
  ee.Initialize()


# convert Julian day to YYYY-MM-DD date
def julianDayToDate(s):
  return datetime.strptime(s, '%Y%m%d').strftime('%Y_%m_%d')


# function to calculate and add some spectral-bands-based indexes (LS 4,5,7)
def getVi(image):
  scalingFactor = 0.0001
  b1 = image.select(['B1']).multiply(scalingFactor)
  b2 = image.select(['B2']).multiply(scalingFactor)
  b3 = image.select(['B3']).multiply(scalingFactor)
  b4 = image.select(['B4']).multiply(scalingFactor)
  b5 = image.select(['B5']).multiply(scalingFactor)
  b7 = image.select(['B7']).multiply(scalingFactor)
  evi = image.expression('2.5 * (NIR - R) / (NIR + 6.0*R - 7.5*B + 1)', {'R': b3, 'NIR': b4, 'B': b1})
  evi2 = image.expression('2.5 * (NIR - R) / (NIR + 2.4*R + 1)', {'R': b3, 'NIR': b4})
  ndvi = image.normalizedDifference(['B4', 'B3'])
  ndmi = image.normalizedDifference(['B4', 'B5'])
  image2 = (
    image.select(['pixel_qa'])
      .addBands(b1.select([0], ['b1']))
      .addBands(b2.select([0], ['b2']))
      .addBands(b3.select([0], ['b3']))
      .addBands(b4.select([0], ['b4']))
      .addBands(b5.select([0], ['b5']))
      .addBands(b7.select([0], ['b7']))
      .addBands(evi.select([0], ['evi']))
      .addBands(evi2.select([0], ['evi2']))
      .addBands(ndvi.select([0], ['ndvi']))
      .addBands(ndmi.select([0], ['ndmi']))
  )
  return (image2)


# function to calculate and add some spectral-bands-based indexes (LS 8)
def getVi8(image):
  scalingFactor = 0.0001
  b1 = image.select(['B1']).multiply(scalingFactor)
  b2 = image.select(['B2']).multiply(scalingFactor)
  b3 = image.select(['B3']).multiply(scalingFactor)
  b4 = image.select(['B4']).multiply(scalingFactor)
  b5 = image.select(['B5']).multiply(scalingFactor)
  # b6 = image.select(['B6']).multiply(scalingFactor)
  b7 = image.select(['B7']).multiply(scalingFactor)
  evi = image.expression('2.5 * (NIR - R) / (NIR + 6.0*R - 7.5*B + 1)', {'R': b4, 'NIR': b5, 'B': b2})
  evi2 = image.expression('2.5 * (NIR - R) / (NIR + 2.4*R + 1)', {'R': b4, 'NIR': b5})
  ndvi = image.normalizedDifference(['B5', 'B4'])
  ndmi = image.normalizedDifference(['B5', 'B6'])
  image2 = (
    image.select(['pixel_qa'])
      .addBands(b1.select([0], ['b1']))
      .addBands(b2.select([0], ['b2']))
      .addBands(b3.select([0], ['b3']))
      .addBands(b4.select([0], ['b4']))
      .addBands(b5.select([0], ['b5']))
      # .addBands(b6.select([0],['b6']))
      .addBands(b7.select([0], ['b7']))
      .addBands(evi.select([0], ['evi']))
      .addBands(evi2.select([0], ['evi2']))
      .addBands(ndvi.select([0], ['ndvi']))
      .addBands(ndmi.select([0], ['ndmi']))
  )
  return (image2)


# create a geometry point with provided coordinates
longCen = float(coords[0])
latCen = float(coords[1])
bounds = ee.Geometry.Point([longCen, latCen])

# define image collection
imgCol = ee.ImageCollection('LANDSAT/' + satChoice)

# get time series from GEE
if satChoice == 'LC08/C01/T1_SR':
  values = imgCol.filterBounds(bounds).map(getVi8).getRegion(bounds, 30)
else:
  values = imgCol.filterBounds(bounds).map(getVi).getRegion(bounds, 30)

# get time series info and apply cfmask filter
try:
  aux = values.getInfo()
  serie = []
  for i in range(1, len(aux)):
    # filtering using band_qa (only clear pixels are kept)
    qa = aux[i][4]
#    if pixel_qa and (pixel_qa & 40 == 0):
    if qa:
      shadow = qa & 8 != 0
      cloud = qa & 32 != 0
      snow = qa & 16 != 0
      high_cloud_conf = qa & 64 != 0 and qa & 128 != 0
      med_cloud_conf = qa & 64 != 0 and qa & 128 == 0
      if not high_cloud_conf or med_cloud_conf or shadow:
        serie += [julianDayToDate(str(aux[i][0])[12:20])] + \
                 numpy.round(aux[i][5:15], 4).tolist()

  # additional variables to be used in R
  if len(aux) > 0:
    colNames = [u'date'] + aux[0][5:15]
    numCol = len(colNames)
    numRow = len(aux) - 1
except ee.ee_exception.EEException:
  # no data available
  serie = []


class FileCredentials(OAuth2Credentials):
  def __init__(self, access_token_file):
    access_token = self._read_access_token(access_token_file)
    super(FileCredentials, self).__init__(access_token, None, None, None, None, None, None)
    self.access_token_file = access_token_file

  def _refresh(self, http):
    self.access_token = self._read_access_token(self.access_token_file)

  def _read_access_token(self, access_token_file):
    with file(access_token_file) as f:
      return f.read()

  def __str__(self):
    return 'FileCredentials(' + self.access_token_file + ')'
