within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.Data.Records;
record SoilData
  import Buildings;

  extends Buildings.HeatTransfer.Data.Soil.Generic;
  parameter String name="SoilData";
  final parameter Modelica.SIunits.DiffusionCoefficient alp=k/d/c;
end SoilData;
