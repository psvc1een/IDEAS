within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield;
package Examples
extends Modelica.Icons.ExamplesPackage;

  function test_T_ft
    input Integer q_max=11;
    input Integer p_max=6;

    input Real[q_max,p_max] QMat=
        Borefield.BaseClasses.Aggregation.aggregateLoad(
              q_max=q_max,
              p_max=p_max,
              rArr=
          Borefield.BaseClasses.Aggregation.BaseClasses.cellWidth(
          q_max, p_max),
              nuMat=
          Borefield.BaseClasses.Aggregation.BaseClasses.nbPulseAtEndEachLevel(
                q_max=q_max,
                p_max=p_max,
                rArr=
            Borefield.BaseClasses.Aggregation.BaseClasses.cellWidth(
            q_max, p_max)),
              lenLoa=12282,
              loaVec=ones(12282));
    input Real[q_max,p_max] kappaMat=
        Borefield.BaseClasses.Aggregation.transientFrac(
              q_max=q_max,
              p_max=p_max,
              genStePar=
          Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30(),
              bfGeo=
          Borefield.Data.BorefieldGeometricData.Line1_rB010_h100(),
              soi=Borefield.Data.SoilData.Sandstone(),
              resWet=
          Borefield.Data.ResponseWetter.SandstoneH100qSte30(),
              nuMat=
          Borefield.BaseClasses.Aggregation.BaseClasses.nbPulseAtEndEachLevel(
                q_max=q_max,
                p_max=p_max,
                rArr=
            Borefield.BaseClasses.Aggregation.BaseClasses.cellWidth(
            q_max, p_max)));
    input Real R_ss=0.09;

    output Real T_fts;

  algorithm
    T_fts := Borefield.T_ft(
      q_max,
      p_max,
      QMat,
      kappaMat,
      R_ss);
  end test_T_ft;

  model test_multipleBoreholes
    extends Modelica.Icons.Example;

    parameter Data.BorefieldStepResponse.SandstoneLine1H100qSte30tSte3600
      bfSteRes
      annotation (Placement(transformation(extent={{-70,52},{-50,72}})));
    parameter Integer lenSim=3600*24*20;

    MultipleBoreHoles_Buildings multipleBoreholes(lenSim=lenSim, bfSteRes=
          bfSteRes)
      annotation (Placement(transformation(extent={{-34,-68},{36,2}})));
    Modelica.Blocks.Sources.RealExpression realExpression(y=30*bfSteRes.bfGeo.hBor)
      annotation (Placement(transformation(extent={{-88,-22},{-68,-2}})));
    Modelica.Blocks.Interfaces.RealOutput T_fts
      annotation (Placement(transformation(extent={{92,-10},{112,10}})));

  equation
    connect(realExpression.y, multipleBoreholes.Q_flow)  annotation (Line(
        points={{-67,-12},{-47.35,-12},{-47.35,-32.3},{-27.7,-32.3}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(multipleBoreholes.T_fts, T_fts)  annotation (Line(
        points={{33.9,-32.3},{62,-32.3},{62,0},{102,0}},
        color={0,0,127},
        smooth=Smooth.None));
    annotation (
      Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}), graphics),
      experiment(StopTime=1.7e+006, __Dymola_NumberOfIntervals=100),
      __Dymola_experimentSetupOutput);
  end test_multipleBoreholes;
end Examples;
