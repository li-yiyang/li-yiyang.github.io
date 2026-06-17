model Model1
  Modelica.Electrical.Analog.Basic.Ground ground1 annotation(Placement(visible = true, transformation(origin = {-80, -40}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Components.HeatCapacitor heatCapacitor1(C = 4861, T.start = 318.15) annotation(Placement(visible = true, transformation(origin = {20, 51.632}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Components.BodyRadiation bodyRadiation1(Gr = 2) annotation(Placement(visible = true, transformation(origin = {3.227, -40}, extent = {{10, -10}, {-10, 10}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = 40) annotation(Placement(visible = true, transformation(origin = {62.49, 50}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Sources.FixedTemperature fixedTemperature1(T = 283.15) annotation(Placement(visible = true, transformation(origin = {-30, -40}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Electrical.Analog.Sources.SineVoltage sineVoltage1(V = 220, freqHz = 50) annotation(Placement(visible = true, transformation(origin = {-120, 0}, extent = {{-10, -10}, {10, 10}}, rotation = -90)));
  Modelica.Blocks.Logical.OnOffController onOffController1(bandwidth = 1) annotation(Placement(visible = true, transformation(origin = {106.793, 17.868}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Resistor resistor1(useHeatPort = true, R = 18.4) annotation(Placement(visible = true, transformation(origin = {-55, 0}, extent = {{-10, -10}, {10, 10}}, rotation = -270)));
  Modelica.Thermal.HeatTransfer.Celsius.TemperatureSensor temperatureSensor annotation(Placement(visible = true, transformation(origin = {65, -0}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Electrical.Analog.Ideal.IdealClosingSwitch switch1 annotation(Placement(visible = true, transformation(origin = {-85, 35}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
equation
  connect(heatCapacitor1.port, bodyRadiation1.port_a) annotation(Line(visible = true, origin = {18.333, -12.789}, points = {{1.667, 54.421}, {1.667, -27.211}, {-5.106, -27.211}}, color = {191, 0, 0}));
  connect(bodyRadiation1.port_b, fixedTemperature1.port) annotation(Line(visible = true, origin = {-13.387, -40}, points = {{6.613, 0}, {-6.613, 0}}, color = {191, 0, 0}));
  connect(sineVoltage1.n, ground1.p) annotation(Line(visible = true, origin = {-100, -22.5}, points = {{-20, 12.5}, {-20, -2.5}, {20, -2.5}, {20, -7.5}}, color = {10, 90, 224}));
  connect(const.y, onOffController1.reference) annotation(Line(visible = true, origin = {82.071, 36.934}, points = {{-8.581, 13.066}, {-2.071, 13.066}, {-2.071, -13.066}, {12.722, -13.066}}, color = {1, 37, 163}));
  connect(resistor1.p, ground1.p) annotation(Line(visible = true, origin = {-67.5, -22.5}, points = {{12.5, 12.5}, {12.5, -2.5}, {-12.5, -2.5}, {-12.5, -7.5}}, color = {10, 90, 224}));
  connect(resistor1.heatPort, heatCapacitor1.port) annotation(Line(visible = true, origin = {-1.667, 13.877}, points = {{-43.333, -13.877}, {21.667, -13.877}, {21.667, 27.755}}, color = {191, 0, 0}));
  connect(heatCapacitor1.port, temperatureSensor.port) annotation(Line(visible = true, origin = {31.667, 13.877}, points = {{-11.667, 27.755}, {-11.667, -13.877}, {23.333, -13.877}}, color = {191, 0, 0}));
  connect(temperatureSensor.T, onOffController1.u) annotation(Line(visible = true, origin = {82.448, 5.934}, points = {{-7.448, -5.934}, {-2.448, -5.934}, {-2.448, 5.934}, {12.345, 5.934}}, color = {1, 37, 163}));
  connect(switch1.n, resistor1.n) annotation(Line(visible = true, origin = {-61.667, 26.667}, points = {{-13.333, 8.333}, {6.667, 8.333}, {6.667, -16.667}}, color = {10, 90, 224}));
  connect(switch1.p, sineVoltage1.p) annotation(Line(visible = true, origin = {-111.667, 26.667}, points = {{16.667, 8.333}, {-8.333, 8.333}, {-8.333, -16.667}}, color = {10, 90, 224}));
  connect(switch1.control, onOffController1.y) annotation(Line(visible = true, origin = {54.264, 40.715}, points = {{-139.264, 1.285}, {-139.264, 34.285}, {75.736, 34.285}, {75.736, -23.504}, {63.529, -23.504}, {63.529, -22.847}}, color = {190, 52, 178}));
  annotation(experiment(StopTime = 200, __Wolfram_NumberOfIntervals = -1), Diagram(coordinateSystem(extent = {{-150, -90}, {150, 90}}, preserveAspectRatio = true, initialScale = 0.1, grid = {5, 5})));
end Model1;
