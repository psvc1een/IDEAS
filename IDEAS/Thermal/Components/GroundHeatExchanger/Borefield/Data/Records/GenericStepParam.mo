within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.Data.Records;
record GenericStepParam
  extends Modelica.Icons.Record;
  import SI = Modelica.SIunits;

  parameter String name="GenericStepParam";

  parameter SI.Time tStep=3600 "[s] discrete time step";
  parameter Integer t_min_d=1 "[-] Mininum simulation discrete time";
  parameter Integer tSteSta_d=integer(3600*24*365*30/tStep)
    "[-] discrete time to reach steady state (default = 30 years)";
  parameter Integer tBre_d=100
    "[-] discrete time upper boundary for saving results";
  parameter Real q_ste(unit="W/m") = 30
    "Amplitude per length borehole of step load input";
  parameter SI.MassFlowRate m_flow=0.3 "Flow through the pipe";
  parameter SI.Temperature T_ini=273.15 "Initial temperature";

  //  replaceable package Medium = Buildings.Media.ConstantPropertyLiquidWater;
end GenericStepParam;
