# Learn to use gvis
# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html

install.packages("googleVis")
library(googleVis)

# Line chart
df = data.frame(country = c("US", "GB", "BR"), val1 = c(10, 13, 14), val2 = c(23, 12, 32))
Line = gvisLineChart(df)
plot(Line)
print(Line) # 可以输出html代码, 但是暂时不知道咋用

# Line chart with two axis
Line2 = gvisLineChart(df, "country", c("val1", "val2"),
                      options = list(series = "[{targetAxisIndex:0},{targetAxisIndex:1}]",
                                    vAxes = "[{title:'val1'},{title:'val2'}]"))
plot(Line2)

# Bar chart
Bar = gvisBarChart(df)
plot(Bar)

# Column chart
Column = gvisColumnChart(df)
plot(Column)

# Area chart
Area = gvisAreaChart(df)
plot(Area)

# Stepped Area chart
SteppedArea = gvisSteppedAreaChart(df, xvar = "country", yvar = c("val1", "val2"),
            options = list(isStacked = TRUE))
plot(SteppedArea)

# Combo chart
Combo = gvisComboChart(df, xvar = "country", yvar = c("val1", "val2"),
              options = list(seriesType = "bars", series = '{1: {type:"line"}}'))
plot(Combo)

# Scatter chart
Scatter = gvisScatterChart(women, options = list(
    legend = "none", lineWidth = 2, pointSize = 0, title = "Women",
    vAxis = "{title: 'weight(lbs)'}", hAxis = "{title:'height(in)'}",
    width = 300, height = 300))
plot(Scatter)

# Bubble chart
Bubble = gvisBubbleChart(Fruits, idvar = "Fruit",
         xvar = "Sales", yvar = "Expenses",
         colorvar = "Year", sizevar = "Profit",
        options = list(hAxis = '{minValue:75, maxValue:125}'))
plot(Bubble)

# Customizing Lines
Dashed = gvisLineChart(df, xvar = "country", yvar = c("val1", "val2"),
                       options = list(
                           series = "[
                           {color:'green',targetAxisIndex:0, lineWidth:1, lineDashStyle:[2,2,20,2,20,2]},
                           {color:'blue', targetAxisIndex:1, lineWidth:2, lineDashStyle:[4,1]}
                           ]", vAxes = "[{title:'val1'},{title:'val2'}]"
                       ))
plot(Dashed)

# Customizing points
M = matrix(nrow = 6, ncol = 6)
M[col(M) == row(M)] = 1:6
dat = data.frame(X = 1:6, M)
SC = gvisScatterChart(dat, options = list(
            title = "Customizing points",
            legend = "right",
            pointSize = 30,
            series = "{
                0: {pointShape: 'circle'},
                1: {pointShape: 'triangle'},
                2: {pointShape: 'diamond'},
                3: {pointShape: 'star'},
                4: {pointShape: 'polygon'}
            }"
        ))
plot(SC)

# cat(SC$html$chart, file = "sc.html")
# gvisMotionChart

# Add edit button for on the fly customisation
Line4 = gvisLineChart(df, "country", c("val1", "val2"),
              options = list(gvis.editor = "Edit me!"))
plot(Line4)
# Export as an html file and can be opened in the browser
cat(Line4$html$chart, file = "Line4.html")

# A chart with many options set
Line3 <- gvisLineChart(df, xvar = "country", yvar = c("val1", "val2"),
                        options = list(
                          title = "Hello World",
                          titleTextStyle = "{color:'red', 
                                           fontName:'Courier', 
                                           fontSize:16}",
                          backgroundColor = "#D3D3D3",
                          vAxis = "{gridlines:{color:'red', count:3}}",
                          hAxis = "{title:'Country', titleTextStyle:{color:'blue'}}",
                          series = "[{color:'green', targetAxisIndex: 0}, 
                                   {color: 'orange',targetAxisIndex:1}]",
                          vAxes = "[{title:'val1'}, {title:'val2'}]",
                          legend = "bottom",
                          curveType = "function",
                          width = 500,
                          height = 300
                        ))
plot(Line3)

# Candlestick chart
Candle = gvisCandlestickChart(OpenClose, options = list(legend = 'none'))
plot(Candle)

# Pie chart
Pie = gvisPieChart(CityPopularity)
plot(Pie)

# Gauge
Gauge = gvisGauge(CityPopularity, options = list(
    min = 0, max = 800, greenFrom = 500, greenTo = 800,
    yellowFrom = 300, yellowTo = 500, redFrom = 0, redTo = 300))
plot(Gauge)

# Intensity Map
Intensity = gvisIntensityMap(df)
plot(Intensity)

# Geo Chart
Geo = gvisGeoChart(Exports, locationvar = "Country",
                 colorvar = "Profit",
                 options = list(projection = "kavrayskiy-vii"))
plot(Geo)

# Example showing US data by state
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options = list(region = "US",
                                       displayMode = "regions",
                                       resolution = "provinces",
                                       width = 600, height = 400))
plot(GeoStates)
# Show Hurricane Andrew(1992) storm track with markers
GeoMarker <- gvisGeoChart(Andrew, "LatLong",
                          sizevar = 'Speed_kt',
                          colorvar = "Pressure_mb",
                          options = list(region = "US"))
plot(GeoMarker)

AndrewMap <- gvisMap(Andrew, "LatLong", "Tip",
                     options = list(showTip = TRUE,
                                  showLine = TRUE,
                                  enableScrollWheel = TRUE,
                                  mapType = 'terrain',
                                  useMapTypeControl = TRUE))
plot(AndrewMap)

# Table
Table = gvisTable(Stock, formats = list(Value = "#,###"))
plot(Table)

# Table with pages
PopTable = gvisTable(Population, formats = list(
    Population = "#,###",
    '% of World Population' = '#.##%'),
    options = list(page = 'enable'))
plot(PopTable)

# Org chart
Org <- gvisOrgChart(Regions,
                    options = list(width = 600, height = 250,
                                 size = 'large', allowCollapse = TRUE))
plot(Org)
# Tree map
Tree <- gvisTreeMap(Regions,
                    "Region", "Parent",
                    "Val", "Fac",
                    options = list(fontSize = 16))
plot(Tree)

# Annotation chart
Anno <- gvisAnnotationChart(Stock,
                            datevar = "Date",
                            numvar = "Value",
                            idvar = "Device",
                            titlevar = "Title",
                            annotationvar = "Annotation",
                            options = list(
                              width = 1000, height = 600,
                              fill = 10, displayExactValues = TRUE,
                              colors = "['#0000ff','#00ff00']")
)
plot(Anno)

# Calendar chart
Cal <- gvisCalendar(Cairo,
                    datevar = "Date",
                    numvar = "Temp",
                    options = list(
                      title = "Daily temperature in Cairo",
                      height = 320,
                      calendar = "{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
)
plot(Cal)

# Merging charts
G1 = gvisGeoChart(Exports, "Country", "Profit",
                 options = list(width = 600, height = 600))
G2 = gvisTable(Exports, options = list(width = 400, height = 600))
GT = gvisMerge(G1, G2, horizontal = TRUE)
plot(GT)
cat(GT$html$chart, file = "mergeCharts.html")

Motion = gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")

cat(Motion$html$chart, file = "fruitsMotion.html")

AndrewGeo <- gvisGeoMap(Andrew,
                        locationvar = "LatLong",
                        numvar = "Speed_kt",
                        hovervar = "Category",
                        options = list(height = 350,
                                     region = "US",
                                     dataMode = "markers"))
cat(AndrewGeo$html$chart, file = "andrewGeo.html")