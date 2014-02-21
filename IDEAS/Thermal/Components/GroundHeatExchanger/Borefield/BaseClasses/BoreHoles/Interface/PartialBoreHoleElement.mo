within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.BoreHoles.Interface;
partial model PartialBoreHoleElement

  parameter Data.Records.SoilData matSoi "Thermal properties of the ground"
    annotation (Placement(transformation(extent={{-46,-116},{-26,-96}})));
  parameter Data.Records.BoreholeFillingData matFil
    "Thermal properties of the filling material"
    annotation (Placement(transformation(extent={{-22,-116},{-2,-96}})));
  parameter Data.Records.BorefieldGeometryData bfGeo
    "Geometric charachteristic of the borehole"
    annotation (Placement(transformation(extent={{2,-116},{22,-96}})));
  parameter Data.Records.Advanced adv(hBor=bfGeo.hBor) "Advanced parameters"
    annotation (Placement(transformation(extent={{-70,-116},{-50,-96}})));
  parameter Data.Records.GenericStepParam genStePar
    annotation (Placement(transformation(extent={{-96,-116},{-76,-96}})));

end PartialBoreHoleElement;
