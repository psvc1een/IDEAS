within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.GroundHX.BaseClasses.Examples;
model test_integrandBh_rt
  extends Modelica.Icons.Example;

  parameter Real samplePeriod=0.01;
  parameter Integer lim=5;
  Real int;

algorithm
  if time < 0.007 then
    int := 0;
  else
    int := integrandBh_rt(
      r=0,
      D=100,
      u=time*lim);
  end if;
  annotation (experiment(
      StopTime=5,
      __Dymola_NumberOfIntervals=100,
      Tolerance=1e-005,
      __Dymola_Algorithm="Dassl"), __Dymola_experimentSetupOutput);
end test_integrandBh_rt;
