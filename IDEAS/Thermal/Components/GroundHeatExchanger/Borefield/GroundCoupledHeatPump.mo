within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield;
model GroundCoupledHeatPump "Ground coupled heat pump for calculation of COP"

  parameter Integer lenSim "Simulation length ([s]). By default = 100 days";

  MultipleBoreHoles_IDEAS_opt multipleBoreHoles_IDEAS_opt(bfSteRes = bfSteRes, lenSim=lenSim)
    annotation (Placement(transformation(extent={{4,-66},{64,-6}})));
  Modelica.Blocks.Interfaces.RealInput QEva(unit="W") "load to the borefield"
    annotation (Placement(transformation(
        extent={{-20,-20},{20,20}},
        rotation=-90,
        origin={-60,106})));
  Modelica.Blocks.Interfaces.RealOutput COP annotation (Placement(
        transformation(
        extent={{-14,-14},{14,14}},
        rotation=-90,
        origin={-60,-110})));
  Modelica.Blocks.Interfaces.RealOutput T_bf_in(unit="K", displayUnit="degC") annotation (Placement(
        transformation(
        extent={{-14,-14},{14,14}},
        rotation=-90,
        origin={0,-110})));
  Modelica.Blocks.Interfaces.RealOutput T_bf_out(unit="K", displayUnit="degC") annotation (Placement(
        transformation(
        extent={{-14,-14},{14,14}},
        rotation=-90,
        origin={60,-110})));
  Modelica.Blocks.Interfaces.RealInput T_eva(unit="K", displayUnit="degC")
    "Temperature of evaporator"
    annotation (Placement(transformation(
        extent={{-20,-20},{20,20}},
        rotation=-90,
        origin={60,106})));
  Modelica.Blocks.Sources.RealExpression realExpression(y=QCond)
    annotation (Placement(transformation(extent={{-10,-10},{10,10}})));

  Modelica.SIunits.HeatFlowRate QCond "Condensor power";
  Modelica.SIunits.Power PEl "Electrical power";

  replaceable parameter Data.Records.BorefieldStepResponse bfSteRes
    constrainedby Data.Records.BorefieldStepResponse
    annotation (Placement(transformation(extent={{-86,-44},{-66,-24}})));

equation
  COP = 3;
  PEl = QEva * COP;
  QCond = QEva + PEl;

  connect(T_bf_in, T_bf_in) annotation (Line(
      points={{1.77636e-015,-110},{1.77636e-015,-110}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(multipleBoreHoles_IDEAS_opt.T_sou, T_bf_out) annotation (Line(
      points={{55,-36},{60,-36},{60,-110}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(multipleBoreHoles_IDEAS_opt.T_sin, T_bf_in) annotation (Line(
      points={{13,-36},{0,-36},{0,-110}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(realExpression.y, multipleBoreHoles_IDEAS_opt.Q_flow) annotation (
      Line(
      points={{11,0},{34,0},{34,-17.5714}},
      color={0,0,127},
      smooth=Smooth.None));
  annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}}), graphics), Icon(coordinateSystem(
          preserveAspectRatio=false, extent={{-100,-100},{100,100}}), graphics={
        Rectangle(
          extent={{-80,6},{80,-82}},
          lineThickness=0.5,
          fillColor={255,213,170},
          fillPattern=FillPattern.Solid,
          pattern=LinePattern.None),
        Rectangle(extent={{-40,80},{40,20}}, lineColor={0,0,0}),
        Text(
          extent={{-40,78},{40,18}},
          lineColor={0,0,0},
          textString="HP"),
        Line(
          points={{-60,0},{-60,-80},{-40,-80},{-40,-78},{-40,0},{-10,0},{-10,-80},
              {10,-80},{10,0},{40,0},{40,-80},{60,-80},{60,0},{68,0},{68,26},{40,
              26}},
          color={0,0,255},
          smooth=Smooth.None,
          thickness=0.5),
        Line(
          points={{-60,0},{-70,0},{-70,24},{-40,24}},
          color={0,0,255},
          smooth=Smooth.None,
          thickness=0.5)}));
end GroundCoupledHeatPump;
