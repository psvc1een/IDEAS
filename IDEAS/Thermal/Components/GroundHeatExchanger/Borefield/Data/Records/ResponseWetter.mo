within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.Data.Records;
record ResponseWetter
  extends Modelica.Icons.Record;
  import SI = Modelica.SIunits;

  parameter String name="ResponseWetter";
  parameter Integer vecLen "vector lenght (=tBre_d)";
  parameter SI.Time[vecLen] tVec "[s] time vector";
  parameter SI.Temperature[vecLen] TResSho
    "Short time temperature step response from Wetter's model";
end ResponseWetter;
