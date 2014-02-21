within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.Data.Records;
record BoreholeFillingData
  import Buildings;

  extends Buildings.HeatTransfer.Data.BoreholeFillings.Generic;

  parameter String name="BoreholeFillingData";
  final parameter Modelica.SIunits.DiffusionCoefficient alp=k/d/c;

end BoreholeFillingData;
