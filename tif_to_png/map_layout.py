"""
Credits:
    https://opensourceoptions.com/blog/pyqgis-create-and-print-a-map-layout-with-python/

1. Open the raster layer in QGIS
2. Set the symbology to whatever you like
3. Open Python Console and load this script
4. Run the script
5. In the console type something like `main('final_cropped', 'watson')`
6. This will render the layer `final_cropped` to `watson.png` in your home directory

Reference for further improvements:
    https://docs.qgis.org/3.22/en/docs/pyqgis_developer_cookbook/composer.html
"""

def main(name, fn):
    from qgis.PyQt import QtGui

    layers = QgsProject.instance().mapLayersByName(name)
    layer = layers[0]

    project = QgsProject.instance()
    manager = project.layoutManager()
    layoutName = 'Layout1'
    layouts_list = manager.printLayouts()
    # remove any duplicate layouts
    for layout in layouts_list:
        if layout.name() == layoutName:
            manager.removeLayout(layout)
    layout = QgsPrintLayout(project)
    layout.initializeDefaults()
    layout.setName(layoutName)
    manager.addLayout(layout)

    # create map item in the layout
    map = QgsLayoutItemMap(layout)
    map.setRect(20, 20, 20, 20)

    # set the map extent
    ms = QgsMapSettings()
    ms.setLayers([layer]) # set layers to be mapped
    rect = QgsRectangle(ms.fullExtent())
    rect.scale(1.0)
    ms.setExtent(rect)
    map.setExtent(rect)
    map.setBackgroundColor(QColor(255, 255, 255, 0))
    layout.addLayoutItem(map)

    map.attemptMove(QgsLayoutPoint(20, 20, QgsUnitTypes.LayoutMillimeters))
    map.attemptResize(QgsLayoutSize(180, 180, QgsUnitTypes.LayoutMillimeters))

    legend = QgsLayoutItemLegend(layout)
    layerTree = QgsLayerTree()
    layerTree.addLayer(layer)
    legend.model().setRootGroup(layerTree)
    layout.addLayoutItem(legend)
    legend.attemptMove(QgsLayoutPoint(10, 175, QgsUnitTypes.LayoutMillimeters))

    title = QgsLayoutItemLabel(layout)
    title.setText(fn)
    title.setFont(QFont('Arial', 24))
    title.adjustSizeToText()
    layout.addLayoutItem(title)
    title.attemptMove(QgsLayoutPoint(100, 5, QgsUnitTypes.LayoutMillimeters))

    layout = manager.layoutByName(layoutName)
    exporter = QgsLayoutExporter(layout)

    exporter.exportToImage(fn + '.png', QgsLayoutExporter.ImageExportSettings())
