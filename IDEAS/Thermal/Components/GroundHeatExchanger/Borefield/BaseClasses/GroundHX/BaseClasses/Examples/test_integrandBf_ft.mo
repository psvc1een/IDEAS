within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.GroundHX.BaseClasses.Examples;
model test_integrandBf_ft
  extends Modelica.Icons.Example;

  parameter Integer lim=5;
  Real int;

algorithm
  if time < 0.00785 then
    int := 0;
  else
    int := integrandBf_bt(D=100, u=time*lim);
  end if;
  annotation (experiment(
      StopTime=1,
      __Dymola_NumberOfIntervals=1000,
      Tolerance=1e-005,
      __Dymola_Algorithm="Dassl"), __Dymola_experimentSetupOutput);
end test_integrandBf_ft;
