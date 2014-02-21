within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.GroundHX.Examples;
model test_T_rt
  extends Modelica.Icons.Example;

  parameter Data.GenericStepParam.tS3600_tmind1_qSte30 genStePar;
  parameter Data.BorefieldGeometricData.Line3_rB010_h100 bfGeo_mb;
  parameter Data.BorefieldGeometricData.Line1_rB010_h100 bfGeo_sb;

  parameter Data.SoilData.Sandstone soi;
  parameter Data.Records.ResponseWetter resWet=
      Data.ResponseWetter.Sandstone_Line1_rB010_h100_H100qSte30();

  Modelica.SIunits.Temperature T_rt_sb;
  Modelica.SIunits.Temperature T_rt_mb;

equation
  if time < genStePar.t_min_d*genStePar.tStep then
    T_rt_sb = 273.15;
    T_rt_mb = 273.15;
  else
    T_rt_sb = BoreFieldWallTemperature(
      t_d=integer(time/genStePar.tStep),
      r=bfGeo_sb.rBor,
      genStePar=genStePar,
      bfGeo=bfGeo_sb,
      soi=soi);
    T_rt_mb = BoreFieldWallTemperature(
      t_d=integer(time/genStePar.tStep),
      r=bfGeo_mb.rBor,
      genStePar=genStePar,
      bfGeo=bfGeo_mb,
      soi=soi);
  end if;

  annotation (experiment(StopTime=700000, __Dymola_NumberOfIntervals=100),
      __Dymola_experimentSetupOutput);
end test_T_rt;
