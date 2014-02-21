within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.GroundHX.Examples;
model test_T_ft_cor
  extends Modelica.Icons.Example;
  import SI = Modelica.SIunits;

  parameter Data.GenericStepParam.tS3600_tmind1_qSte30 genStePar;
  parameter Data.BorefieldGeometricData.Line3_rB010_h100 bfGeo;
  parameter Data.SoilData.Sandstone soi;
  parameter Data.ResponseWetter.Sandstone_Line1_rB010_h100_H100qSte30 resWet;

  SI.Temperature T_fts_cor;

  Integer timeSca "time step size for simulation";
  Integer timeSca_old;
  Integer i;
  Integer i_old;

  Integer t_old(start=0) "help variable for simulation timestep";
  Integer t_new(start=0) "help variable for simulation timestep";

algorithm
  t_old := t_new;
  t_new := max(t_old, integer(integer(time/timeSca)*timeSca/genStePar.tStep));

algorithm
  when initial() then
    i :=integer(2);
    i_old :=i;
    timeSca :=integer(genStePar.tStep);
    timeSca_old :=timeSca;
  elsewhen sample(genStePar.tStep*10^1,genStePar.tStep*10^i/5) then
    i_old :=i;
    i :=i_old+1;
    timeSca :=integer(timeSca_old*2);
    timeSca_old :=timeSca;
  end when;

equation
  T_fts_cor = HeatCarrierFluidStepTemperature(
    t_d=max(t_old, integer(integer(time/timeSca)*timeSca/genStePar.tStep)),
    genStePar=genStePar,
    bfGeo=bfGeo,
    soi=soi,
    resWet=resWet);

  annotation (experiment(StopTime=720000, __Dymola_NumberOfIntervals=10),
      __Dymola_experimentSetupOutput);
end test_T_ft_cor;
