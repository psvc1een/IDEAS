within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.GroundHX.BaseClasses;
partial function partialBoreFieldTemperature
  import SI = Modelica.SIunits;

  input Data.Records.GenericStepParam genStePar;
  input Data.Records.BorefieldGeometryData bfGeo;
  input Data.Records.SoilData soi;
  input Integer t_d "discrete time at which the temperature is calculated";

  output SI.Temperature T;

end partialBoreFieldTemperature;
