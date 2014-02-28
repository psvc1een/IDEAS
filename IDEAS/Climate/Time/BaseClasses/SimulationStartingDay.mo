within IDEAS.Climate.Time.BaseClasses;
model SimulationStartingDay

extends Modelica.Blocks.Interfaces.BlockIcon;

parameter Integer startingDay;

Modelica.Blocks.Interfaces.RealOutput timSim
    annotation (Placement(transformation(extent={{90,-10},{110,10}})));
equation
  timSim = startingDay;

  annotation (Diagram(graphics));
end SimulationStartingDay;
