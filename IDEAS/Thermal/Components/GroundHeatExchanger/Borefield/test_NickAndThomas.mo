within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield;
model test_NickAndThomas

  parameter Integer lenSim=3600*24*365*20
    "Simulation length ([s]). By default = 100 days";

  GroundCoupledHeatPump groundCoupledHeatPump(lenSim=lenSim, redeclare
      Borefield.Data.BorefieldStepResponse.test_NickAndThomas_accurate                                                   bfSteRes)
    annotation (Placement(transformation(extent={{-26,-44},{26,4}})));
  Modelica.Blocks.Sources.Sine QEva(
    amplitude=10000,
    freqHz=3.17E-8,
    offset=8000)
    annotation (Placement(transformation(extent={{-80,20},{-60,40}})));
  Modelica.Blocks.Sources.Pulse TEva(
    amplitude=-10,
    period=3E+7,
    startTime=1.5E+7,
    offset=303.15)
    annotation (Placement(transformation(extent={{80,20},{60,40}})));

equation
  connect(QEva.y, groundCoupledHeatPump.QEva) annotation (Line(
      points={{-59,30},{-15.6,30},{-15.6,5.44}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(TEva.y, groundCoupledHeatPump.T_eva) annotation (Line(
      points={{59,30},{15.6,30},{15.6,5.44}},
      color={0,0,127},
      smooth=Smooth.None));
  annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}}), graphics),
    experiment(StopTime=6.3e+008, __Dymola_NumberOfIntervals=10000),
    __Dymola_experimentSetupOutput(events=false));
end test_NickAndThomas;
