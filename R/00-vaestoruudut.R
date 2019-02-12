# Reittidatan sainkin jo. Vielä tarvitaan ruutudata.

library(gdalUtils)

ogr2ogr("WFS:http://geo.stat.fi/geoserver/vaestoruutu/wfs", "shp/ruudut.shp", layer = "vaestoruutu:vaki2017_1km")
