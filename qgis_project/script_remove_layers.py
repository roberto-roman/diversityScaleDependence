# libraries
from qgis.core import *
from qgis.gui import (
    QgsLayerTreeMapCanvasBridge,
    QgsMapCanvas,
    QgsVertexMarker,
    QgsMapCanvasItem,
    QgsMapMouseEvent,
    QgsRubberBand,
)
from PyQt5.QtWidgets import QApplication
from qgis.core import QgsApplication, QgsProject
from qgis.gui import QgsMapCanvas, QgsLayerTreeMapCanvasBridge

## configuration
app = QApplication([])
QgsApplication.setPrefixPath("C:/Program Files/QGIS 3.22.9", True)
qgs = QgsApplication([], True)
qgs.initQgis()

canvas = QgsMapCanvas()
project = QgsProject.instance()
bridge = QgsLayerTreeMapCanvasBridge(project.layerTreeRoot(), canvas)
project.read('C:/Users/rober/My Drive (robertoromanrespaldo2@gmail.com)/folder_respaldo3/maestria/tesis/bases_datos_plantas/qgis_project/quadrats_interactions.qgz')

## code

# Write your code here to load some layers, use processing
# algorithms, etc.

path_to_layer = "C:/Users/rober/My Drive (robertoromanrespaldo2@gmail.com)/folder_respaldo3/maestria/tesis/bases_datos_plantas/intermedium/quadrats_V06.GeoJSON"

# add layer
Output = QgsVectorLayer(path_to_layer, "Layer_loaded", "ogr")
QgsProject.instance().addMapLayer(Output)

# remove layer
rm = QgsProject().instance().mapLayersByName('google hybrid')
QgsProject.instance().removeMapLayer(rm[0])

#canvas.show()
#app.exec_()

project.write()

# Finally, exitQgis() is called to remove the
# provider and layer registries from memory
# qgs.exitQgis()

# python-qgis-ltr script_remove_layers.py