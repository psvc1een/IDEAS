within IDEAS.Climate.Time.BaseClasses;
model CalendarTime

  extends Modelica.Blocks.Interfaces.BlockIcon;

  parameter Boolean ifSolCor;

  Modelica.Blocks.Interfaces.RealInput timSim
    annotation (Placement(transformation(extent={{-120,20},{-80,60}})));
  Modelica.Blocks.Interfaces.RealInput delay
    annotation (Placement(transformation(extent={{-120,-60},{-80,-20}})));
  Modelica.Blocks.Interfaces.RealOutput timCal
    annotation (Placement(transformation(extent={{90,30},{110,50}})));
  Modelica.Blocks.Interfaces.RealOutput timCalSol
    annotation (Placement(transformation(extent={{90,-50},{110,-30}})));
  Modelica.Blocks.Interfaces.RealInput startingDay
    annotation (Placement(transformation(extent={{-120,-92},{-80,-52}})));
  Modelica.Blocks.Interfaces.RealOutput[4] timDat
    annotation (Placement(transformation(extent={{90,54},{110,74}})));
equation
  timCal = timSim;
  // - integer(timSim/31536000)*31536000;
  timDat[2] = div(timSim,86400)+startingDay; //day of the year
  timDat[1] = rem(timDat[2],7); //day of the week
  timDat[4] = div(timSim,3600); //Hour of the year
  timDat[3] = rem(timDat[4],24); //Hour of the day
  if ifSolCor then
    timCalSol = timSim + delay;
  else
    timCalSol = timSim;
  end if;

  annotation (Diagram(graphics));
end CalendarTime;
