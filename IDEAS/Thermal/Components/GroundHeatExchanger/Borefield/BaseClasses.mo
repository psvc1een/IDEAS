within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield;
package BaseClasses
  extends Modelica.Icons.Package;

  package BoreHoles "Package with borehole heat exchangers"
  extends Modelica.Icons.VariantsPackage;

    model SingleBoreHolesInSerie
      "Single U-tube borehole heat exchanger model. If more than one borehole is given, they are assumed to be connected in series"
      extends Borefield.BaseClasses.BoreHoles.Interface.PartialSingleBoreHole;

      BaseClasses.SingleBoreHole[bfGeo.nbSer] borHol(
        redeclare each final package Medium = Medium,
        each final matSoi=matSoi,
        each final matFil=matFil,
        each final bfGeo=bfGeo,
        each final adv=adv,
        each final genStePar=genStePar,
        each final dp_nominal=10000,
        each final m_flow_nominal=genStePar.m_flow,
        each final T_start=genStePar.T_ini) "Borehole heat exchanger" annotation (
          Placement(transformation(extent={{-16,-16},{16,16}}, rotation=0)));

    equation
      T_wall_ave = sum(borHol[:].T_wall_ave)/bfGeo.nbSer;

      connect(port_a, borHol[1].port_a);
      connect(borHol[bfGeo.nbSer].port_b, port_b);
      for i in 1:bfGeo.nbSer - 1 loop
        connect(borHol[i].port_b, borHol[i + 1].port_a);
      end for;

      annotation (
        Dialog(group="Borehole"),
        Dialog(group="Borehole"),
        defaultComponentName="borehole",
        Icon(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2},
            initialScale=0.5), graphics={
            Rectangle(
              extent={{-68,60},{-60,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Rectangle(
              extent={{-40,60},{-32,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Rectangle(
              extent={{32,60},{40,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Rectangle(
              extent={{60,60},{68,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Rectangle(
              extent={{-40,-72},{-60,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Rectangle(
              extent={{60,-72},{40,-80}},
              lineColor={0,0,0},
              fillColor={192,192,192},
              fillPattern=FillPattern.Backward),
            Rectangle(
              extent={{-56,68},{-52,-72}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-48,68},{-44,-72}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{52,68},{56,-72}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{44,68},{48,-72}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid),
            Rectangle(
              extent={{-2,15},{2,-15}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid,
              origin={-33,66},
              rotation=90),
            Rectangle(
              extent={{-2,15},{2,-15}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid,
              origin={33,66},
              rotation=90),
            Rectangle(
              extent={{-2,15},{2,-15}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid,
              origin={67,66},
              rotation=90),
            Rectangle(
              extent={{-2,15},{2,-15}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid,
              origin={-67,66},
              rotation=90),
            Line(
              points={{0,80},{-20,20},{0,-40},{-12,-82},{-12,-82}},
              color={0,0,0},
              smooth=Smooth.Bezier),
            Line(
              points={{12,80},{-8,20},{12,-40},{0,-82},{0,-82}},
              color={0,0,0},
              smooth=Smooth.Bezier),
            Text(
              extent={{-30,102},{38,66}},
              lineColor={0,0,0},
              fillColor={0,128,255},
              fillPattern=FillPattern.Solid,
              textString="nbSer")}),
        Diagram(coordinateSystem(
            preserveAspectRatio=false,
            extent={{-100,-100},{100,100}},
            grid={2,2},
            initialScale=0.5), graphics={Text(
              extent={{60,72},{84,58}},
              lineColor={0,0,255},
              textString=""),Text(
              extent={{50,-32},{90,-38}},
              lineColor={0,0,255},
              textString="")}),
        Documentation(info="<html>
<p>
Model of a single U-tube borehole heat exchanger. 
The borehole heat exchanger is vertically discretized into <i>n<sub>seg</sub></i>
elements of height <i>h=h<sub>Bor</sub>&frasl;n<sub>seg</sub></i>.
Each segment contains a model for the heat transfer in the borehole, 
for heat transfer in the soil and for the far-field boundary condition.
</p>
<p>
The heat transfer in the borehole is computed using a convective heat transfer coefficient
that depends on the fluid velocity, a heat resistance between the two pipes, and
a heat resistance between the pipes and the circumference of the borehole.
The heat capacity of the fluid, and the heat capacity of the grout, is taken into account.
All thermal mass is assumed to be at the two bulk temperatures of the down-flowing 
and up-flowing fluid.
</p>
<p>
The heat transfer in the soil is computed using transient heat conduction in cylindrical
coordinates for the spatial domain <i>r<sub>bor</sub> &le; r &le; r<sub>ext</sub></i>. 
In the radial direction, the spatial domain is discretized into 
<i>n<sub>hor</sub></i> segments with uniform material properties.
Thermal properties can be specified separately for each horizontal layer.
The vertical heat flow is assumed to be zero, and there is assumed to be 
no ground water flow. 
</p>
<p>
The far-field temperature, i.e., the temperature at the radius 
<i>r<sub>ext</sub></i>, is computed using a power-series solution
to a line-source heat transfer problem. This temperature boundary condition
is updated every <i>t<sub>sample</sub></i> seconds.
</p>
<p>
The initial far-field temperature <i>T<sub>ext,start</sub></i>, which
is the temperature of the soil at a radius <i>r<sub>ext</sub></i>,
is computed 
as a function of the depth <i>z &gt; 0</i>. 
For a depth between <i>0 &le; z &le; z<sub>0</sub></i>, the temperature
is set to <i>T<sub>ext,0,start</sub></i>. 
The value of <i>z<sub>0</sub></i> is a parameter with a default of 10 meters.
However, there is large variability in the depth where the undisturbed soil temperature
starts.
For a depth of <i>z<sub>0</sub> &le; z &le; h<sub>bor</sub></i>,
the temperature is computed as
</p>
<p align=\"center\" style=\"font-style:italic;\">
  T<sup>i</sup><sub>ext,start</sub> = T<sub>ext,0,start</sub> + (z<sup>i</sup> - z<sub>0</sub>)  dT &frasl; dz
</p>
with <i>i &isin; {1, ..., n<sub>ver</sub>}</i>,
where the temperature gradient <i>dT &frasl; dz &ge; 0</i> is a parameter.
As with <i>z<sub>0</sub></i>, there is large variability in 
<i>dT &frasl; dz &ge; 0</i>. The default value is set to <i>1</i> Kelvin per 100 meters.
For the temperature of the grout, the same equations are applied, with
<i>T<sub>ext,0,start</sub></i> replaced with
<i>T<sub>fil,0,start</sub></i>, and 
<i>T<sup>i</sup><sub>ext,start</sub></i> replaced with
<i>T<sup>i</sup><sub>fil,start</sub></i>. 
The default setting uses the same temperature for the soil and the filling material.
</p>
<h4>Implementation</h4>
<p>
Each horizontal layer is modeled using an instance of
<a href=\"modelica://Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.BoreholeSegment\">
Buildings.HeatExchangers.Fluid.Boreholes.BaseClasses.BoreholeSegment</a>.
This model is composed of the model
<a href=\"modelica://Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.HexInternalElement\">
Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.HexInternalElement</a> which computes
the heat transfer in the pipes and the borehole filling,
of the model
<a href=\"modelica://Buildings.HeatTransfer.Conduction.SingleLayerCylinder\">
Buildings.HeatTransfer.Conduction.SingleLayerCylinder</a> which computes
the heat transfer in the soil, and
of the model
<a href=\"modelica://Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.TemperatureBoundaryCondition\">
Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.TemperatureBoundaryCondition</a> which computes
the far-field temperature boundary condition.
</p>
</html>",     revisions="<html>
<ul>
<li>
August 2011, by Pierre Vigouroux:<br>
First implementation.
</li>
</ul>
</html>"));
    end SingleBoreHolesInSerie;

    package BaseClasses "Base classes for Borehole"
    extends Modelica.Icons.BasesPackage;

      model SingleBoreHole "Single U-tube borehole heat exchanger"
        import Buildings;

        extends Borefield.BaseClasses.BoreHoles.Interface.PartialSingleBoreHole;

        BaseClasses.BoreHoleSegmentFourPort borHolSeg[adv.nVer](
          redeclare each final package Medium = Medium,
          each final matSoi=matSoi,
          each final matFil=matFil,
          each final bfGeo=bfGeo,
          each final genStePar=genStePar,
          each final adv=adv,
          final dp_nominal={if i == 1 then dp_nominal else 0 for i in 1:adv.nVer},
          TExt_start=adv.TExt_start,
          TFil_start=adv.TExt_start,
          each final homotopyInitialization=homotopyInitialization,
          each final show_T=show_T,
          each final computeFlowResistance=computeFlowResistance,
          each final from_dp=from_dp,
          each final linearizeFlowResistance=linearizeFlowResistance,
          each final deltaM=deltaM,
          each final energyDynamics=energyDynamics,
          each final massDynamics=massDynamics,
          each final p_start=p_start,
          each T_start=genStePar.T_ini,
          each X_start=X_start,
          each C_start=C_start,
          each C_nominal=C_nominal) "Discretized borehole segments"
          annotation (Placement(transformation(extent={{-18,-10},{2,10}})));

        Modelica.SIunits.Temperature TDown[adv.nVer]
          "Medium temperature in pipe 1";
        Modelica.SIunits.Temperature TUp[adv.nVer]
          "Medium temperature in pipe 2";

      equation
        T_wall_ave = sum(borHolSeg[:].intHEX.port.T)/adv.nVer;

        TDown[:] = borHolSeg[:].intHEX.vol1.heatPort.T;
        TUp[:] = borHolSeg[:].intHEX.vol2.heatPort.T;
        connect(port_a, borHolSeg[1].port_a1) annotation (Line(
            points={{-100,5.55112e-016},{-60,5.55112e-016},{-60,6},{-18,6}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(port_b, borHolSeg[1].port_b2) annotation (Line(
            points={{100,5.55112e-016},{20,5.55112e-016},{20,-40},{-40,-40},{-40,-6},
                {-18,-6}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(borHolSeg[adv.nVer].port_b1, borHolSeg[adv.nVer].port_a2) annotation (
           Line(
            points={{5.55112e-16,6},{10,6},{10,-6},{5.55112e-16,-6}},
            color={0,127,255},
            smooth=Smooth.None));
        for i in 1:adv.nVer - 1 loop
          connect(borHolSeg[i].port_b1, borHolSeg[i + 1].port_a1) annotation (Line(
              points={{2,6},{2,20},{-18,20},{-18,6}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(borHolSeg[i].port_a2, borHolSeg[i + 1].port_b2) annotation (Line(
              points={{2,-6},{2,-20},{-18,-20},{-18,-6}},
              color={0,127,255},
              smooth=Smooth.None));
        end for;
        annotation (
          Dialog(group="Borehole"),
          Dialog(group="Borehole"),
          defaultComponentName="borehole",
          Icon(coordinateSystem(
              preserveAspectRatio=true,
              extent={{-100,-100},{100,100}},
              grid={2,2},
              initialScale=0.5), graphics={
              Rectangle(
                extent={{-70,80},{70,-80}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={95,95,95},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-62,-52},{62,-60}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,0},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-62,58},{62,54}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,0},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-62,6},{62,0}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,0},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{54,92},{46,-88}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-54,-88},{-46,92}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-72,80},{-62,-80}},
                lineColor={0,0,0},
                fillColor={192,192,192},
                fillPattern=FillPattern.Backward),
              Rectangle(
                extent={{62,80},{72,-80}},
                lineColor={0,0,0},
                fillColor={192,192,192},
                fillPattern=FillPattern.Backward)}),
          Diagram(coordinateSystem(
              preserveAspectRatio=false,
              extent={{-100,-100},{100,100}},
              grid={2,2},
              initialScale=0.5), graphics={Text(
                extent={{60,72},{84,58}},
                lineColor={0,0,255},
                textString=""),Text(
                extent={{50,-32},{90,-38}},
                lineColor={0,0,255},
                textString="")}),
          Documentation(info="<html>
<p>
Model of a single U-tube borehole heat exchanger. 
The borehole heat exchanger is vertically discretized into <i>n<sub>seg</sub></i>
elements of height <i>h=h<sub>Bor</sub>&frasl;n<sub>seg</sub></i>.
Each segment contains a model for the heat transfer in the borehole, 
for heat transfer in the soil and for the far-field boundary condition.
</p>
<p>
The heat transfer in the borehole is computed using a convective heat transfer coefficient
that depends on the fluid velocity, a heat resistance between the two pipes, and
a heat resistance between the pipes and the circumference of the borehole.
The heat capacity of the fluid, and the heat capacity of the grout, is taken into account.
All thermal mass is assumed to be at the two bulk temperatures of the down-flowing 
and up-flowing fluid.
</p>
<p>
The heat transfer in the soil is computed using transient heat conduction in cylindrical
coordinates for the spatial domain <i>r<sub>bor</sub> &le; r &le; r<sub>ext</sub></i>. 
In the radial direction, the spatial domain is discretized into 
<i>n<sub>hor</sub></i> segments with uniform material properties.
Thermal properties can be specified separately for each horizontal layer.
The vertical heat flow is assumed to be zero, and there is assumed to be 
no ground water flow. 
</p>
<p>
The far-field temperature, i.e., the temperature at the radius 
<i>r<sub>ext</sub></i>, is computed using a power-series solution
to a line-source heat transfer problem. This temperature boundary condition
is updated every <i>t<sub>sample</sub></i> seconds.
</p>
<p>
The initial far-field temperature <i>T<sub>ext,start</sub></i>, which
is the temperature of the soil at a radius <i>r<sub>ext</sub></i>,
is computed 
as a function of the depth <i>z &gt; 0</i>. 
For a depth between <i>0 &le; z &le; z<sub>0</sub></i>, the temperature
is set to <i>T<sub>ext,0,start</sub></i>. 
The value of <i>z<sub>0</sub></i> is a parameter with a default of 10 meters.
However, there is large variability in the depth where the undisturbed soil temperature
starts.
For a depth of <i>z<sub>0</sub> &le; z &le; h<sub>bor</sub></i>,
the temperature is computed as
</p>
<p align=\"center\" style=\"font-style:italic;\">
  T<sup>i</sup><sub>ext,start</sub> = T<sub>ext,0,start</sub> + (z<sup>i</sup> - z<sub>0</sub>)  dT &frasl; dz
</p>
with <i>i &isin; {1, ..., n<sub>ver</sub>}</i>,
where the temperature gradient <i>dT &frasl; dz &ge; 0</i> is a parameter.
As with <i>z<sub>0</sub></i>, there is large variability in 
<i>dT &frasl; dz &ge; 0</i>. The default value is set to <i>1</i> Kelvin per 100 meters.
For the temperature of the grout, the same equations are applied, with
<i>T<sub>ext,0,start</sub></i> replaced with
<i>T<sub>fil,0,start</sub></i>, and 
<i>T<sup>i</sup><sub>ext,start</sub></i> replaced with
<i>T<sup>i</sup><sub>fil,start</sub></i>. 
The default setting uses the same temperature for the soil and the filling material.
</p>
<h4>Implementation</h4>
<p>
Each horizontal layer is modeled using an instance of
<a href=\"modelica://Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.BoreholeSegment\">
Buildings.HeatExchangers.Fluid.Boreholes.BaseClasses.BoreholeSegment</a>.
This model is composed of the model
<a href=\"modelica://Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.HexInternalElement\">
Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.HexInternalElement</a> which computes
the heat transfer in the pipes and the borehole filling,
of the model
<a href=\"modelica://Buildings.HeatTransfer.Conduction.SingleLayerCylinder\">
Buildings.HeatTransfer.Conduction.SingleLayerCylinder</a> which computes
the heat transfer in the soil, and
of the model
<a href=\"modelica://Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.TemperatureBoundaryCondition\">
Buildings.Fluid.HeatExchangers.Boreholes.BaseClasses.TemperatureBoundaryCondition</a> which computes
the far-field temperature boundary condition.
</p>
</html>",       revisions="<html>
<ul>
<li>
August 2011, by Pierre Vigouroux:<br>
First implementation.
</li>
</ul>
</html>"));
      end SingleBoreHole;

      model BoreHoleSegmentFourPort "Vertical segment of a borehole"
        import Buildings;

        extends
          Borefield.BaseClasses.BoreHoles.Interface.PartialBoreHoleElement;
        extends Buildings.Fluid.Interfaces.PartialFourPortInterface(
          redeclare final package Medium1 = Medium,
          redeclare final package Medium2 = Medium,
          final m1_flow_nominal=adv.m_flow_nominal,
          final m2_flow_nominal=adv.m_flow_nominal,
          final m1_flow_small=adv.m_flow_small,
          final m2_flow_small=adv.m_flow_small,
          final allowFlowReversal1=adv.allowFlowReversal,
          final allowFlowReversal2=adv.allowFlowReversal);
        extends Buildings.Fluid.Interfaces.TwoPortFlowResistanceParameters;
        extends Buildings.Fluid.Interfaces.LumpedVolumeDeclarations(T_start=adv.TFil0_start);
        replaceable package Medium =
            Modelica.Media.Interfaces.PartialMedium "Medium in the component"
                                    annotation (choicesAllMatching=true);

        parameter Modelica.SIunits.Temperature TExt_start=adv.TExt0_start
          "Initial far field temperature"
          annotation (Dialog(tab="Boundary conditions",group="T_start: ground"));
        parameter Modelica.SIunits.Temperature TFil_start=adv.TFil0_start
          "Initial far field temperature"
          annotation (Dialog(tab="Boundary conditions",group="T_start: ground"));

        replaceable SingleUTubeInternalHEX intHEX(
          redeclare final package Medium = Medium,
          final m1_flow_nominal=adv.m_flow_nominal,
          final m2_flow_nominal=adv.m_flow_nominal,
          final dp1_nominal=dp_nominal,
          final dp2_nominal=0,
          final from_dp1=from_dp,
          final from_dp2=from_dp,
          final linearizeFlowResistance1=linearizeFlowResistance,
          final linearizeFlowResistance2=linearizeFlowResistance,
          final deltaM1=deltaM,
          final deltaM2=deltaM,
          final m1_flow_small=adv.m_flow_small,
          final m2_flow_small=adv.m_flow_small,
          final matSoi=matSoi,
          final matFil=matFil,
          final bfGeo=bfGeo,
          final adv=adv,
          final genStePar=genStePar,
          final allowFlowReversal1=adv.allowFlowReversal,
          final allowFlowReversal2=adv.allowFlowReversal,
          final homotopyInitialization=adv.homotopyInitialization,
          final energyDynamics=energyDynamics,
          final massDynamics=massDynamics,
          final p1_start=p_start,
          T1_start=adv.TFil0_start,
          X1_start=X_start,
          C1_start=C_start,
          C1_nominal=C_nominal,
          final p2_start=p_start,
          T2_start=adv.TFil0_start,
          X2_start=X_start,
          C2_start=C_start,
          C2_nominal=C_nominal) constrainedby
          Interface.PartialBoreHoleInternalHEX
          "Internal part of the borehole including the pipes and the filling material"
          annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
        CylindricalGroundLayer soi(
          final material=matSoi,
          final h=adv.hSeg,
          final nSta=adv.nHor,
          final r_a=bfGeo.rBor,
          final r_b=adv.rExt,
          final TInt_start=TFil_start,
          final TExt_start=TExt_start,
          final steadyStateInitial=false) "Heat conduction in the soil"
          annotation (Placement(transformation(extent={{0,-10},{20,10}})));

        Modelica.Thermal.HeatTransfer.Sources.PrescribedTemperature TBouCon(final T=
              adv.TExt0_start) "Thermal boundary condition for the far-field"
          annotation (Placement(transformation(extent={{68,-10},{48,10}})));
      protected
        Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heaFlo
          annotation (Placement(transformation(extent={{-30,-10},{-10,10}})));
      equation
        connect(intHEX.port_b1, port_b1) annotation (Line(
            points={{-50,6.36364},{-40,6.36364},{-40,60},{100,60}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(intHEX.port_a2, port_a2) annotation (Line(
            points={{-50,-4.54545},{-40,-4.54545},{-40,-60},{100,-60}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(intHEX.port_b2, port_b2) annotation (Line(
            points={{-70,-4.54545},{-80,-4.54545},{-80,-60},{-100,-60}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(intHEX.port, heaFlo.port_a) annotation (Line(
            points={{-60,10},{-45,10},{-45,1.22125e-015},{-40,1.22125e-015},{-40,0},{
                -30,0}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(heaFlo.port_b, soi.port_a) annotation (Line(
            points={{-10,0},{-7.5,0},{-7.5,1.22125e-015},{-5,1.22125e-015},{-5,0},{0,
                0}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(port_a1, intHEX.port_a1) annotation (Line(
            points={{-100,60},{-80,60},{-80,6.36364},{-70,6.36364}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(soi.port_b, TBouCon.port) annotation (Line(
            points={{20,0},{48,0}},
            color={191,0,0},
            smooth=Smooth.None));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                  100,100}}), graphics),
          Icon(graphics={
              Rectangle(
                extent={{-72,80},{68,-80}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={95,95,95},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{88,54},{-88,64}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{88,-64},{-88,-54}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid),
              Rectangle(
                extent={{-72,80},{68,68}},
                lineColor={0,0,0},
                fillColor={192,192,192},
                fillPattern=FillPattern.Backward),
              Rectangle(
                extent={{-72,-68},{68,-80}},
                lineColor={0,0,0},
                fillColor={192,192,192},
                fillPattern=FillPattern.Backward)}),
          Documentation(info="<html>
<p>
Horizontal layer that is used to model a U-tube borehole heat exchanger. 
This model combines three models, each simulating a different aspect 
of a borehole heat exchanger. 
</p>
<p>
The instance <code>pipFil</code> computes the heat transfer in the pipes and the filling material. 
This computation is done using the model
<a href=\"modelica://Buildings.Fluid.Boreholes.BaseClasses.HexInternalElement\">
Buildings.Fluid.Boreholes.BaseClasses.HexInternalElement</a>.
</p>
<p>
The instance <code>soi</code> computes transient and steady state heat transfer in the soil using a vertical cylinder.
The computation is done using the model <a href=\"modelica://Buildings.HeatTransfer.Conduction.SingleLayerCylinder\">
Buildings.HeatTransfer.Conduction.SingleLayerCylinder</a>.
</p>
<p>
The model <code>TBouCon</code> computes the far-field temperature boundary condition, i.e., the temperature at the outer
surface of the above cylindrical heat transfer computation.
The computation is done using the model
<a href=\"modelica://Buildings.Fluid.Boreholes.BaseClasses.TemperatureBoundaryCondition\">
Buildings.Fluid.Boreholes.BaseClasses.TemperatureBoundaryCondition</a>.
</p>
</html>",       revisions="<html>
<ul>
<li>
July 28 2011, by Pierre Vigouroux:<br>
First implementation.
</li>
</ul>
</html>"));
      end BoreHoleSegmentFourPort;

      model SingleUTubeInternalHEX
        "Internal part of a borehole for a U-Tube configuration"
        import Buildings;
        extends
          Borefield.BaseClasses.BoreHoles.Interface.PartialBoreHoleInternalHEX;

        extends Buildings.Fluid.Interfaces.FourPortHeatMassExchanger(
          redeclare final package Medium1 = Medium,
          redeclare final package Medium2 = Medium,
          T1_start=TFil_start,
          T2_start=TFil_start,
          final tau1=Modelica.Constants.pi*bfGeo.rTub^2*adv.hSeg*rho1_nominal/
              m1_flow_nominal,
          final tau2=Modelica.Constants.pi*bfGeo.rTub^2*adv.hSeg*rho2_nominal/
              m2_flow_nominal,
          final show_T=true,
          vol1(
            final energyDynamics=energyDynamics,
            final massDynamics=massDynamics,
            final prescribedHeatFlowRate=false,
            final homotopyInitialization=homotopyInitialization,
            final allowFlowReversal=allowFlowReversal1,
            final V=m2_flow_nominal*tau2/rho2_nominal,
            final m_flow_small=m1_flow_small),
          redeclare final Buildings.Fluid.MixingVolumes.MixingVolume vol2(
            final energyDynamics=energyDynamics,
            final massDynamics=massDynamics,
            final prescribedHeatFlowRate=false,
            final homotopyInitialization=homotopyInitialization,
            final V=m1_flow_nominal*tau1/rho1_nominal,
            final m_flow_small=m2_flow_small));

        parameter Modelica.SIunits.Temperature TFil_start=adv.TFil0_start
          "Initial temperature of the filling material"
          annotation (Dialog(group="Filling material"));

        Modelica.Thermal.HeatTransfer.Components.ConvectiveResistor RConv1
          "Pipe convective resistance"
          annotation (Placement(transformation(extent={{-58,40},{-82,16}})));
        Modelica.Thermal.HeatTransfer.Components.ConvectiveResistor RConv2
          "Pipe convective resistance"
          annotation (Placement(transformation(extent={{-56,-40},{-80,-16}})));
        Modelica.Thermal.HeatTransfer.Components.ThermalResistor Rpg1(
          final R=RCondGro_val) "Grout thermal resistance"
          annotation (Placement(transformation(extent={{-50,16},{-26,40}})));
        Modelica.Thermal.HeatTransfer.Components.ThermalResistor Rpg2(
          final R=RCondGro_val) "Grout thermal resistance"
          annotation (Placement(transformation(extent={{-48,-40},{-24,-16}})));
        Modelica.Thermal.HeatTransfer.Components.ThermalResistor Rgb1(
          final R=Rgb_val) "Grout thermal resistance"
          annotation (Placement(transformation(extent={{52,26},{76,50}})));
        Modelica.Thermal.HeatTransfer.Components.ThermalResistor Rgb2(
          final R=Rgb_val) "Grout thermal resistance"
          annotation (Placement(transformation(extent={{52,-40},{76,-16}})));
        Modelica.Thermal.HeatTransfer.Components.ThermalResistor Rgg(
          final R=Rgg_val) "Grout thermal resistance"
          annotation (Placement(transformation(extent={{-12,-12},{12,12}},
              rotation=-90,
              origin={20,2})));

        Modelica.Thermal.HeatTransfer.Components.HeatCapacitor capFil1(final C=Co_fil/2, T(
              start=TFil_start)) "Heat capacity of the filling material" annotation (
            Placement(transformation(
              extent={{-90,36},{-70,16}},
              rotation=0,
              origin={80,0})));

        Modelica.Thermal.HeatTransfer.Components.HeatCapacitor capFil2(final C=Co_fil/2, T(
              start=TFil_start)) "Heat capacity of the filling material" annotation (
            Placement(transformation(
              extent={{-90,-36},{-70,-16}},
              rotation=0,
              origin={80,6})));

      protected
        final parameter Modelica.SIunits.SpecificHeatCapacity cpFil=matFil.c
          "Specific heat capacity of the filling material";
        final parameter Modelica.SIunits.ThermalConductivity kFil=matFil.k
          "Thermal conductivity of the filling material";
        final parameter Modelica.SIunits.Density dFil=matFil.d
          "Density of the filling material";

        parameter Modelica.SIunits.HeatCapacity Co_fil=dFil*cpFil*adv.hSeg*Modelica.Constants.pi
            *(bfGeo.rBor^2 - 2*(bfGeo.rTub + bfGeo.eTub)^2)
          "Heat capacity of the whole filling material";

        parameter Modelica.SIunits.SpecificHeatCapacity cpMed=
            Medium.specificHeatCapacityCp(Medium.setState_pTX(
            Medium.p_default,
            Medium.T_default,
            Medium.X_default)) "Specific heat capacity of the fluid";
        parameter Modelica.SIunits.ThermalConductivity kMed=
            Medium.thermalConductivity(Medium.setState_pTX(
            Medium.p_default,
            Medium.T_default,
            Medium.X_default)) "Thermal conductivity of the fluid";
        parameter Modelica.SIunits.DynamicViscosity mueMed=Medium.dynamicViscosity(
            Medium.setState_pTX(
            Medium.p_default,
            Medium.T_default,
            Medium.X_default)) "Dynamic viscosity of the fluid";

        parameter Real Rgb_val(fixed=false);
        parameter Real Rgg_val(fixed=false);
        parameter Real RCondGro_val(fixed=false);
        parameter Real x(fixed=false);

      public
        Modelica.Blocks.Sources.RealExpression RVol1(y=
          convectionResistance(
          hSeg=adv.hSeg,
          rBor=bfGeo.rBor,
          rTub=bfGeo.rTub,
          kMed=kMed,
          mueMed=mueMed,
          cpMed=cpMed,
          m_flow=m1_flow,
          m_flow_nominal=adv.m_flow_nominal))
          "Convective and thermal resistance at fluid 1"
          annotation (Placement(transformation(extent={{-100,-2},{-80,18}})));
        Modelica.Blocks.Sources.RealExpression RVol2(y=
          convectionResistance(hSeg=adv.hSeg,
          rBor=bfGeo.rBor,
          rTub=bfGeo.rTub,
          kMed=kMed,
          mueMed=mueMed,
          cpMed=cpMed,
          m_flow=m2_flow,
          m_flow_nominal=adv.m_flow_nominal))
          "Convective and thermal resistance at fluid 2"
           annotation (Placement(transformation(extent={{-100,-18},{-80,2}})));

      initial equation
        (Rgb_val, Rgg_val, RCondGro_val, x) =
          singleUTubeResistances(hSeg=adv.hSeg,
          rBor=bfGeo.rBor,
          rTub=bfGeo.rTub,
          eTub=bfGeo.eTub,
          sha=bfGeo.xC,
          kFil=matFil.k,
          kSoi=matSoi.k,
          kTub=bfGeo.kTub);

      equation
        connect(vol1.heatPort, RConv1.fluid) annotation (Line(
            points={{-10,60},{-60,60},{-60,50},{-90,50},{-90,28},{-82,28}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(RConv1.solid, Rpg1.port_a) annotation (Line(
            points={{-58,28},{-50,28}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(Rpg1.port_b, capFil1.port) annotation (Line(
            points={{-26,28},{-20,28},{-20,36},{0,36}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(capFil1.port, Rgb1.port_a) annotation (Line(
            points={{0,36},{26,36},{26,38},{52,38}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(capFil1.port, Rgg.port_a) annotation (Line(
            points={{0,36},{20,36},{20,14}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(Rgb1.port_b, port) annotation (Line(
            points={{76,38},{86,38},{86,100},{0,100}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(RConv2.solid, Rpg2.port_a) annotation (Line(
            points={{-56,-28},{-48,-28}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(Rpg2.port_b, capFil2.port) annotation (Line(
            points={{-24,-28},{-12,-28},{-12,-30},{0,-30}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(RConv2.fluid, vol2.heatPort) annotation (Line(
            points={{-80,-28},{-86,-28},{-86,-46},{20,-46},{20,-60},{12,-60}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(capFil2.port, Rgb2.port_a) annotation (Line(
            points={{0,-30},{26,-30},{26,-28},{52,-28}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(Rgg.port_b, capFil2.port) annotation (Line(
            points={{20,-10},{20,-30},{0,-30}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(Rgb2.port_b, port) annotation (Line(
            points={{76,-28},{86,-28},{86,100},{0,100}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(RVol1.y, RConv1.Rc) annotation (Line(
            points={{-79,8},{-70,8},{-70,16}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(RVol2.y, RConv2.Rc) annotation (Line(
            points={{-79,-8},{-68,-8},{-68,-16}},
            color={0,0,127},
            smooth=Smooth.None));

        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                  -120},{100,100}}),
                              graphics),
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-120},{100,
                  100}}), graphics={Rectangle(
                extent={{88,54},{-88,64}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid), Rectangle(
                extent={{88,-66},{-88,-56}},
                lineColor={0,0,255},
                pattern=LinePattern.None,
                fillColor={0,0,255},
                fillPattern=FillPattern.Solid)}),
          Documentation(info="<html>
<p>Model for the heat transfer between the fluid and within the borehole filling. This model computes the dynamic response of the fluid in the tubes, and the heat transfer between the fluid and the borehole filling, and the heat storage within the fluid and the borehole filling. </p>
<p>This model computes the different thermal resistances present in a single-U-tube borehole using the method of Bauer et al. [1] and computing explicitely the <i>fluid-to-ground</i> thermal resistance <i>Rb</i> and the <i>grout-to-grout </i>resistance <i>Ra</i> as defined by Hellstroem [2] using the multipole method (BaseClasses.singleUTubeResistances). The convection resistance is calculated using the Dittus-Boelter correlation (see BaseClasses.convectionResistance).</p>
<p>The following figure shows the thermal network set up by Bauer et al. [1]</p>
<p><img src=\"modelica://DaPModels/Borefield/Boreholes/BaseClasses/Documentation/Bauer_singleUTube_small.PNG\"/></p>
<p><h4>References</h4></p>
<p>[1] G. Hellstr&ouml;m. <i>Ground heat storage: thermal analyses of duct storage systems (Theory)</i>. Dep. of Mathematical Physics, University of Lund, Sweden, 1991.</p>
<p>[2] D. Bauer, W. Heidemann, H. M&uuml;ller-Steinhagen, and H.-J. G. Diersch. <i>Thermal resistance and capacity models for borehole heat exchangers</i>. INTERNATIONAL JOURNAL OF ENERGY RESEARCH, 35:312&ndash;320, 2010.</p>
</html>",       revisions="<html>
<p><ul>
<li>January 2014, Damien Picard,<br/><i>First implementation.</i></li>
</ul></p>
</html>"));
      end SingleUTubeInternalHEX;

      model CylindricalGroundLayer
        "Heat conduction in a cylinder using the radial descretization as adviced by Eskilson"
        replaceable parameter Borefield.Data.Records.SoilData material
          "Material thermal properties" annotation (choicesAllMatching=true);

        parameter Modelica.SIunits.Height h "Height of the cylinder";
        parameter Modelica.SIunits.Radius r_a "Internal radius";
        parameter Modelica.SIunits.Radius r_b "External radius";
        parameter Integer nSta(min=1) = 10 "Number of state variables";
        parameter Modelica.SIunits.Temperature TInt_start=293.15
          "Initial temperature at port_a, used if steadyStateInitial = false"
          annotation (Dialog(group="Initialization", enable=not steadyStateInitial));
        parameter Modelica.SIunits.Temperature TExt_start=293.15
          "Initial temperature at port_b, used if steadyStateInitial = false"
          annotation (Dialog(group="Initialization", enable=not steadyStateInitial));
        parameter Boolean steadyStateInitial=false
          "true initializes dT(0)/dt=0, false initializes T(0) at fixed temperature using T_a_start and T_b_start"
          annotation (Dialog(group="Initialization"), Evaluate=true);

        parameter Real gridFac(min=1) = 2 "Grid factor for spacing";

        Modelica.SIunits.TemperatureDifference dT "port_a.T - port_b.T";

        Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a port_a(T(start=TInt_start))
          "Heat port at surface a" annotation (Placement(transformation(extent={{-110,
                  -10},{-90,10}}, rotation=0)));
        Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_b port_b(T(start=TExt_start))
          "Heat port at surface b" annotation (Placement(transformation(extent={{90,-10},
                  {110,10}},rotation=0)));

        Modelica.SIunits.Temperature T[nSta](start={TInt_start + (TExt_start -
              TInt_start)/Modelica.Math.log(r_b/r_a)*Modelica.Math.log((r_a + (r_b -
              r_a)/(nSta)*(i - 0.5))/r_a) for i in 1:nSta})
          "Temperature of the states";
        Modelica.SIunits.HeatFlowRate Q_flow[nSta + 1]
          "Heat flow rate from state i to i+1";

        //  Modelica.SIunits.TemperatureSlope der_T[nSta]
        //    "Time derivative of temperature (= der(T))";

        parameter Modelica.SIunits.Radius r[nSta + 1](each fixed=false)
          "Radius to the boundary of the i-th domain";

      protected
        parameter Modelica.SIunits.Radius rC[nSta](each fixed=false)
          "Radius to the center of the i-th domain";

        final parameter Modelica.SIunits.SpecificHeatCapacity c=material.c
          "Specific heat capacity";
        final parameter Modelica.SIunits.ThermalConductivity k=material.k
          "Thermal conductivity of the material";
        final parameter Modelica.SIunits.Density d=material.d
          "Density of the material";

        parameter Modelica.SIunits.ThermalConductance G[nSta + 1](each fixed=false)
          "Heat conductance between the temperature nodes";
        parameter Modelica.SIunits.HeatCapacity C[nSta](each fixed=false)
          "Heat capacity of each state";

        parameter Real gridFac_sum(fixed=false);
        parameter Real gridFac_sum_old(fixed=false);

      initial algorithm
        for i in 0:nSta - 3 - 1 loop
          if i == 0 then
            gridFac_sum := gridFac^i;
            gridFac_sum_old := gridFac_sum;
          else
            gridFac_sum := gridFac_sum_old + gridFac^i;
            gridFac_sum_old := gridFac_sum;
          end if;
        end for;

      initial equation
        assert(r_a < r_b, "Error: Model requires r_a < r_b.");
        assert(0 < r_a, "Error: Model requires 0 < r_a.");

        // ****************** Comments *********************:
        // The layer is divided into nSta segments. The radius of each segment increase exponentially with base 'griFac':
        // r_b - r_a = sum( a * griFac^k - a , k=2..nSta) => from this we find a = (r_b - r_a)/(griFac^(n+1) - griFac)
        // Using this a, we can now find the r[i]:
        // r[1] := r_a
        // r[i] = r[i-1] + a * griFac^i = r[i-1] + griFac^i * (r_b - r_a) / (griFac^(n+1) - griFac)
        //
        // This can also be writing as:
        // r[i]= r[i-1] + ( r_b - r_a)  * (1-griFac)/(1-griFac^(nSta)) * griFac^(i-2);

        r[1] = r_a;
        r[2] = r_a + sqrt(k/c/d*60) "eskilson minimum lengthe";
        r[3] = r_a + 2*sqrt(k/c/d*60);
        r[4] = r_a + 3*sqrt(k/c/d*60);

        for i in 5:nSta + 1 loop
          r[i] = r[i - 1] + (r_b - r[4])/gridFac_sum*gridFac^(i - 5);
        end for;
        assert(abs(r[nSta + 1] - r_b) < 1E-3,
          "Error: Wrong computation of radius. r[nSta+1]=" + String(r[nSta + 1]));

        // Radii at middle of resistance
        for i in 1:nSta loop
          rC[i] = (r[i] + r[i + 1])/2;
        end for;

        // Conductance between nodes (which are in the center of the domain)
        G[1] = 2*Modelica.Constants.pi*k*h/Modelica.Math.log(rC[1]/r_a);
        G[nSta + 1] = 2*Modelica.Constants.pi*k*h/Modelica.Math.log(r_b/rC[nSta]);
        for i in 2:nSta loop
          G[i] = 2*Modelica.Constants.pi*k*h/Modelica.Math.log(rC[i]/rC[i - 1]);
        end for;

        // Heat capacity of each segment
        for i in 1:nSta loop
          C[i] = (d*Modelica.Constants.pi*c*h*((r[i + 1])^2 - (r[i])^2));
        end for;
        // The initialization is only done for materials that store energy.
        if not material.steadyState then
          if steadyStateInitial then
            der(T) = zeros(nSta);
          else
            for i in 1:nSta loop
              T[i] = TInt_start + (TExt_start - TInt_start)/Modelica.Math.log(r_b/r_a)
                *Modelica.Math.log(rC[i]/r_a);
            end for;
          end if;
        end if;
      equation
        dT = port_a.T - port_b.T;
        port_a.Q_flow = +Q_flow[1];
        port_b.Q_flow = -Q_flow[nSta + 1];
        Q_flow[1] = G[1]*(port_a.T - T[1]);
        Q_flow[nSta + 1] = G[nSta + 1]*(T[nSta] - port_b.T);
        for i in 2:nSta loop
          Q_flow[i] = G[i]*(T[i - 1] - T[i]);
          // Q_flow[i] represents the heat flowing between two nodes
        end for;
        if material.steadyState then
          for i in 2:nSta + 1 loop
            Q_flow[i] = Q_flow[1];
          end for;
        else
          for i in 1:nSta loop
            der(T[i]) = (Q_flow[i] - Q_flow[i + 1])/C[i];
          end for;
        end if;
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                  100,100}}), graphics),
          Icon(coordinateSystem(preserveAspectRatio=false,extent={{-100,-100},{100,100}}),
                          graphics={
              Rectangle(
                extent={{-94,4},{92,-4}},
                lineColor={0,0,0},
                fillColor={191,0,0},
                fillPattern=FillPattern.Solid),
              Polygon(
                points={{12,8},{14,8},{16,4},{18,-2},{18,-6},{16,-12},{10,-16},{6,-20},
                    {-2,-22},{-6,-18},{-12,-12},{-14,-2},{-12,4},{-10,8},{-8,10},{-6,
                    12},{-2,14},{2,14},{8,12},{12,8}},
                lineColor={0,0,0},
                smooth=Smooth.None,
                fillColor={215,215,215},
                fillPattern=FillPattern.Solid),
              Polygon(
                points={{-6,-16},{2,-18},{8,-16},{14,-14},{10,-16},{6,-20},{-2,-22},{
                    -8,-20},{-12,-12},{-14,-2},{-12,4},{-10,8},{-8,10},{-10,0},{-10,-8},
                    {-6,-16}},
                lineColor={0,0,0},
                smooth=Smooth.None,
                fillColor={135,135,135},
                fillPattern=FillPattern.Solid),
              Text(
                extent={{-110,-74},{-26,-86}},
                lineColor={0,0,255},
                textString="%r_a"),
              Text(
                extent={{-22,-62},{20,-76}},
                lineColor={0,0,255},
                textString="%nSta"),
              Text(
                extent={{16,-76},{102,-88}},
                lineColor={0,0,255},
                textString="%r_b"),
              Polygon(
                points={{-50,60},{-38,34},{-32,0},{-36,-30},{-50,-60},{-62,-60},{-48,
                    -30},{-44,0},{-50,34},{-62,60},{-50,60}},
                lineColor={0,0,0},
                smooth=Smooth.None,
                fillPattern=FillPattern.Backward,
                fillColor={175,175,175}),
              Polygon(
                points={{52,60},{64,34},{70,0},{66,-30},{52,-60},{40,-60},{54,-30},{
                    58,0},{52,34},{40,60},{52,60}},
                lineColor={0,0,0},
                smooth=Smooth.None,
                fillPattern=FillPattern.Backward,
                fillColor={175,175,175}),
              Text(
                extent={{-100,100},{100,60}},
                lineColor={0,0,255},
                textString="%name")}),
          defaultComponentName="lay",
          Documentation(info="<html>
</p>
Model for radial heat transfer in a hollow cylinder.
</p>
<p>
If the heat capacity of the material is non-zero, then this model computes transient heat conduction, i.e., it
computes a numerical approximation to the solution of the heat equation
</p>
<p align=\"center\" style=\"font-style:italic;\">
   &rho; c ( &part; T(r,t) &frasl; &part;t ) = 
    k ( &part;&sup2; T(r,t) &frasl; &part;r&sup2; + 1 &frasl; r &nbsp;  &part; T(r,t) &frasl; &part;r ),
</p>
<p>
where 
<i>&rho;</i>
is the mass density,
<i>c</i>
is the specific heat capacity per unit mass,
<i>T</i>
is the temperature at location <i>r</i> and time <i>t</i> and
<i>k</i> is the heat conductivity. 
At the locations <i>r=r<sub>a</sub></i> and <i>r=r<sub>b</sub></i>, 
the temperature and heat flow rate are equal to the 
temperature and heat flow rate of the heat ports.
</p>
<p>
If the heat capacity of the material is set to zero, then steady-state heat flow is computed using
</p>
<p align=\"center\" style=\"font-style:italic;\">
   Q = 2 &pi; k (T<sub>a</sub>-T<sub>b</sub>)&frasl; ln(r<sub>a</sub> &frasl; r<sub>b</sub>),
</p></p>
<p>
where
<i>r<sub>a</sub></i> is the internal radius,
<i>r<sub>b</sub></i> is the external radius,
<i>T<sub>a</sub></i> is the temperature at port a and
<i>T<sub>b</sub></i> is the temperature at port b.
</p>
<h4>Implementation</h4>
<p>
To spatially discretize the heat equation, the construction is 
divided into compartments with <code>material.nSta &ge; 1</code> state variables. 
The state variables are connected to each other through thermal conductors. 
There is also a thermal conductor
between the surfaces and the outermost state variables. Thus, to obtain
the surface temperature, use <code>port_a.T</code> (or <code>port_b.T</code>)
and not the variable <code>T[1]</code>.
</p>
<p>
</html>",       revisions="<html>
<ul>
<li>
March 9, 2012, by Michael Wetter:<br>
Removed protected variable <code>der_T</code> as it is not required.
</li>
<li>
April 14 2011, by Pierre Vigouroux:<br>
First implementation.
</li>
</ul>
</html>"));
      end CylindricalGroundLayer;

      function singleUTubeResistances
        "Thermal resistances for single U-tube, according to Bauer et al. (2011)"

        // Geometry of the borehole
        input Modelica.SIunits.Height hSeg "Height of the element";
        input Modelica.SIunits.Radius rBor "Radius of the borehole";
        // Geometry of the pipe
        input Modelica.SIunits.Radius rTub "Radius of the tube";
        input Modelica.SIunits.Length eTub "Thickness of the tubes";
        input Modelica.SIunits.Length sha
          "Shank spacing, defined as the distance between the center of a pipe and the center of the borehole";

        // Thermal properties
        input Modelica.SIunits.ThermalConductivity kFil
          "Thermal conductivity of the grout";
        input Modelica.SIunits.ThermalConductivity kSoi
          "Thermal conductivity of the soi";
        input Modelica.SIunits.ThermalConductivity kTub
          "Thermal conductivity of the tube";

        // Outputs
        output Modelica.SIunits.ThermalResistance Rgb
          "Thermal resistance between grout zone and borehole wall";
        output Modelica.SIunits.ThermalResistance Rgg
          "Thermal resistance between the two grout zones";
        output Modelica.SIunits.ThermalResistance RCondGro
          "Thermal resistance between: pipe wall to capacity in grout";
        output Real x "Capacity location";

      protected
        Boolean test=false "thermodynamic test for R and x value";

        Modelica.SIunits.ThermalResistance Rg
          "Thermal resistance between outer borehole wall and one tube";
        Modelica.SIunits.ThermalResistance Rar
          "Thermal resistance between the two pipe outer walls";
        Modelica.SIunits.ThermalResistance RCondPipe
          "Thermal resistance of the pipe wall";

        Real Rb
          "Fluid-to-grout resistance, as defined by Hellstroem. Resistance from the fluid in the pipe to the borehole wall";
        Real Ra
          "Grout-to-grout resistance (2D) as defined by Hellstroem. Interaction between the different grout part";

        // Help variables
        Real sigma "Help variable as defined by Hellstroem";
        Real beta "Help variable as defined by Hellstroem";
        Real R_1delta_LS
          "One leg of the triangle resistance network, corresponding to the line source solution";
        Real R_1delta_MP
          "One leg of the triangle resistance network, corresponding to the multipole solution";
        Real Ra_LS
          "Grout-to-grout resistance calculated with the line-source approximation";

        Integer i=1 "Loop counter";

      algorithm
        // ********** Rb and Ra from multipole **********
        // Help variables
        RCondPipe :=Modelica.Math.log((rTub + eTub)/rTub)/(2*Modelica.Constants.pi*hSeg*kTub);
        sigma :=(kFil - kSoi)/(kFil + kSoi);
        R_1delta_LS :=1/(2*Modelica.Constants.pi*kFil)*(log(rBor/(rTub + eTub)) + log(rBor/(2*sha)) +
          sigma*log(rBor^4/(rBor^4 - sha^4)));
        R_1delta_MP :=R_1delta_LS - 1/(2*Modelica.Constants.pi*kFil)*((rTub + eTub)^2/
          (4*sha^2)*(1 - sigma*4*sha^4/(rBor^4 - sha^4))^2)/((1 + beta)/(1 - beta) + (
          rTub + eTub)^2/(4*sha^2)*(1 + sigma*16*sha^4*rBor^4/(rBor^4 - sha^4)^2));
        Ra_LS      :=1/(Modelica.Constants.pi*kFil)*(log(2*sha/rTub) + sigma*log((
          rBor^2 + sha^2)/(rBor^2 - sha^2)));

        //Rb and Ra
        beta :=2*Modelica.Constants.pi*kFil*RCondPipe;
        Rb :=R_1delta_MP/2;
        Ra :=Ra_LS - 1/(Modelica.Constants.pi*kFil)*(rTub^2/(4*sha^2)*(1 + sigma*
          4*rBor^4*sha^2/(rBor^4 - sha^4))/((1 + beta)/(1 - beta) - rTub^2/(4*sha^2) +
          sigma*2*rTub^2*rBor^2*(rBor^4 + sha^4)/(rBor^4 - sha^4)^2));

        //Conversion of Rb (resp. Ra) to Rg (resp. Rar) of Bauer:
        Rg  :=2*Rb/hSeg;
        Rar :=Ra/hSeg;

      /* **************** Simplification of Bauer for single U-tube ************************
  //Thermal resistance between: Outer wall and one tube
     Rg := Modelica.Math.acosh((rBor^2 + (rTub + eTub)^2 - sha^2)/(2*rBor*(rTub +
       eTub)))/(2*Modelica.Constants.pi*hSeg*kFil)*(1.601 - 0.888*sha/rBor);

  //Thermal resistance between: The two pipe outer walls
  Rar := Modelica.Math.acosh((2*sha^2 - (rTub + eTub)^2)/(rTub + eTub)^2)/(2*
       Modelica.Constants.pi*hSeg*kFil);
*************************************************************************************** */

        // ********** Resistances and capacity location according to Bauer **********
        while test == false and i <= 10 loop
          // Capacity location (with correction factor in case that the test is negative)
          x := Modelica.Math.log(sqrt(rBor^2 + 2*(rTub + eTub)^2)/(2*(rTub + eTub)))/
            Modelica.Math.log(rBor/(sqrt(2)*(rTub + eTub)))*((15 - i + 1)/15);

          //Thermal resistance between the grout zone and bore hole wall
          Rgb := (1 - x)*Rg;

          //Thermal resistance between the two grout zones
          Rgg := 2*Rgb*(Rar - 2*x*Rg)/(2*Rgb - Rar + 2*x*Rg);

          // Thermodynamic test to check if negative R values make sense. If not, decrease x-value.
          // fixme: the implemented is only for single U-tube BHE's.
          test := ((1/Rgg + 1/2/Rgb)^(-1) > 0);
          i := i + 1;
        end while;
        //Conduction resistance in grout from pipe wall to capacity in grout
        RCondGro := x*Rg + RCondPipe;

        annotation (Diagram(graphics), Documentation(info="<html>
<p>
This model computes the different thermal resistances present in a single-U-tube borehole 
using the method of Bauer et al. [1].
It also computes the fluid-to-ground thermal resistance <i>R<sub>b</sub></i> 
and the grout-to-grout thermal resistance <i>R<sub>a</sub></i> 
as defined by Hellstroem [2] using the multipole method.
</p>
<p>
The figure below shows the thermal network set up by Bauer et al.
</p>
<p align=\"center\">
<img alt=\"image\" src=\"E:/work\\modelica/DaPModels/Images/Documentation/Bauer_singleUTube_small.png\"/>
</p>
<p>
The different resistances are calculated with following equations:</p>
<p align=\"center\">
<img alt=\"image\" src=\"E:/work\\modelica/DaPModels/Images/Documentation/Bauer_resistanceValues.PNG\"/>
</p>
<p>
Notice that each resistance each resistance still needs to be divided by 
the height of the borehole segment <i>h<sub>Seg</sub></i>.
</p>
<p>
The fluid-to-ground thermal resistance <i>R<sub>b</sub></i> and the grout-to-grout resistance <i>R<sub>a</sub></i> 
are calculated with the multipole method (Hellstroem (1991)) shown below.
</p>
<p>
<!-- If this is an equation, it needs to be typed, not an image -->
<img alt=\"image\" src=\"E:/work\\modelica/DaPModels/Images/Documentation/Rb_multipole.png\"/>
</p>
<p>
<!-- If this is an equation, it needs to be typed, not an image -->
<img alt=\"image\" src=\"E:/work\\modelica/DaPModels/Images/Documentation/Ra_multipole.png\"/>
</p>
<p>
where 
<!-- fixme: use greek symbols such as &lambda; -->
<i>lambda<sub>b</sub></i> and <i>lambda</i>are the conductivity of the filling material 
and of the ground respectively, 
<i>r<sub>p</sub></i> and <i>r<sub>b</sub></i> 
are the pipe and the borehole radius, 
<i>D</i> is the shank spacing (center of borehole to center of pipe), 
<i>R<sub>p</sub></i> is resistance from the fluid to the outside wall of the pipe, 
<i>r<sub>c</sub></i> is the radius at which the ground temperature is radially uniform and 
<i>Epsilon</i> can be neglected as it is close to zero.
</p>
<h4>References</h4>
<p>G. Hellstr&ouml;m. 
<i>Ground heat storage: thermal analyses of duct storage systems (Theory)</i>. 
Dept. of Mathematical Physics, University of Lund, Sweden, 1991.
</p>
<p>D. Bauer, W. Heidemann, H. M&uuml;ller-Steinhagen, and H.-J. G. Diersch. 
<i>Thermal resistance and capacity models for borehole heat exchangers</i>. 
International Journal Of Energy Research, 35:312&ndash;320, 2010.</p>
</html>",       revisions="<html>
<p>
<ul>
<li>
February 12, 2014, by Damien Picard:<br/>
Remove the flow dependency of the resistances, as this function calculates the conduction resistances only.
</li>
<li>
January 24, 2014, by Michael Wetter:<br/>
Revised implementation.
</li>
<li>
January 23, 2014, by Damien Picard:<br/>
First implementation.
</li>
</ul></p>
</html>"));
      end singleUTubeResistances;

      function convectionResistance
        "Thermal resistance from the fluid in pipes and the grout zones (Bauer et al. 2011)"
        import Buildings;

        // Geometry of the borehole
        input Modelica.SIunits.Height hSeg "Height of the element";
        input Modelica.SIunits.Radius rBor "Radius of the borehole";
        input Modelica.SIunits.Radius rTub "Tube radius";

        // thermal properties
        input Modelica.SIunits.ThermalConductivity kMed
          "Thermal conductivity of the fluid";
        input Modelica.SIunits.DynamicViscosity mueMed
          "Dynamic viscosity of the fluid";
        input Modelica.SIunits.SpecificHeatCapacity cpMed
          "Specific heat capacity of the fluid";
        input Modelica.SIunits.MassFlowRate m_flow "Mass flow rate";
        input Modelica.SIunits.MassFlowRate m_flow_nominal
          "Nominal mass flow rate";

        // Outputs
        output Modelica.SIunits.ThermalResistance RFlu2pipe
          "Convection resistance (or conduction in fluid if no mass flow)";

      protected
        Modelica.SIunits.CoefficientOfHeatTransfer h
          "Convective heat transfer coefficient of the fluid";

        Real k(unit="s/kg")
          "Coefficient used in the computation of the convective heat transfer coefficient";

      algorithm
        // ********** Convection resistance **********
        // Dittus-Boelter: h = 0.023*k_f*Re*Pr/(2*rTub)
        // Re = rho*v*DTub / mue_f = m_flow/(pi r^2) * DTub/mue_f = 2*m_flow / ( mue*pi*rTub)
        k := 2/(mueMed*Modelica.Constants.pi*rTub);

        // Convection
        h := 0.023*kMed*(cpMed*mueMed/kMed)^(0.35)/(2*rTub)*
               Buildings.Utilities.Math.Functions.regNonZeroPower(
                 x=m_flow*k,
                 n=0.8,
                 delta=0.01*m_flow_nominal*k);
        RFlu2pipe := 1/(2*Modelica.Constants.pi*rTub*hSeg*h);

        annotation (Diagram(graphics), Documentation(info="<html>
<p>This model computes the convection resistance in the pipes of a borehole segment with heigth hSeg.
</p>
<p>
The correlation of Dittus-Boelter is used to find the convection heat transfer coefficient:
</p>
<p align=\"center\" style=\"font-style:italic;\">
  Nu = 0.023 &nbsp; Re<sup>0.8</sup> &nbsp; Pr<sup>n</sup>,
</p>
<p>
where <i>Nu</i> is the Nusselt number, 
<i>Re</i> is the Reynolds number and 
<i>Pr</i> is the Prandlt number.
We selected <i>n=0.35</i> (according to the correlation <i>n=0.4</i> for heating,
<i>0.3</i> for cooling). 
Dittus-Boelter&apos;s correlation is for turbulent flow in cylindrical smooth pipe.
</p>
</html>",       revisions="<html>
<p>
<ul>
<li>
January 24, 2014, by Michael Wetter:<br/>
Revised implementation. 
Changed <code>cpFluid</code> to <code>cpMed</code> to use consistent notation.
Added regularization for computation of convective heat transfer coefficient to
avoid an event and a non-differentiability.
</li>
<li>
January 23, 2014, by Damien Picard:<br/>
First implementation.
</li>
</ul>
</p>
</html>"));
      end convectionResistance;

      package Examples "Example models to test base classes"
      extends Modelica.Icons.ExamplesPackage;

        model BoreholeSegment "Test for the boreholeSegment model"
          import Buildings;
          import DaPModels;
          extends Modelica.Icons.Example;
          inner Modelica.Fluid.System system
            annotation (Placement(transformation(extent={{-80,-80},{-60,-60}})));
          package Medium = Buildings.Media.ConstantPropertyLiquidWater;

          Borefield.BaseClasses.BoreHoles.BaseClasses.BoreHoleSegmentFourPort
            seg(
            redeclare package Medium = Medium,
            dp_nominal=5,
            matSoi=Borefield.Data.SoilData.Sandstone(),
            matFil=Borefield.Data.BoreholeFillingData.Sandstone(),
            bfGeo=
                Borefield.Data.BorefieldGeometricData.Line1_rB010_h100(),
            adv=Borefield.Data.Advanced.Default(),
            TExt_start=273.15,
            TFil_start=273.15) annotation (Placement(transformation(
                extent={{-13,-13},{13,13}},
                rotation=270,
                origin={11,-1})));
          Buildings.Fluid.Sources.Boundary_pT sou_1(
            redeclare package Medium = Medium,
            nPorts=1,
            use_T_in=false,
            p=101340,
            T=303.15) annotation (Placement(transformation(extent={{-60,40},{-40,60}},
                  rotation=0)));
          Buildings.Fluid.Sources.Boundary_pT sin_2(
            redeclare package Medium = Medium,
            use_p_in=false,
            use_T_in=false,
            nPorts=1,
            p=101330,
            T=283.15) annotation (Placement(transformation(extent={{-60,10},{-40,30}},
                  rotation=0)));
        equation
          connect(sou_1.ports[1], seg.port_a1) annotation (Line(
              points={{-40,50},{20,50},{20,12},{18.8,12}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(seg.port_b1, seg.port_a2) annotation (Line(
              points={{18.8,-14},{3.2,-14}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(seg.port_b2, sin_2.ports[1]) annotation (Line(
              points={{3.2,12},{4,12},{4,20},{-40,20}},
              color={0,127,255},
              smooth=Smooth.None));
          annotation (
            __Dymola_Commands(file=
                  "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/BaseClasses/Examples/BoreholeSegment.mos"
                "Simulate and plot"),
            Diagram(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},{100,
                    100}}), graphics),
            experimentSetupOutput,
            Diagram,
            Documentation(info="<html>
This example illustrates modeling a segment of a borehole heat exchanger.
It simulates the behavior of the borehole on a single horizontal section including the ground and the
boundary condition.
</html>",         revisions="<html>
<ul>
<li>
August 30, 2011, by Pierre Vigouroux:<br>
First implementation.
</li>
</ul>
</html>"));
        end BoreholeSegment;

        model singleLayerCylinder_MLB
          "Comparison of the CylindricalGroundLayer with the Modelica Buildings Library"
          extends Modelica.Icons.Example;

          parameter Data.BorefieldStepResponse.Validation_Spitler_DaPWetter
                                                                  bfSteRes
            annotation (Placement(transformation(extent={{-60,76},{-40,96}})));

          Buildings.HeatTransfer.Conduction.SingleLayerCylinder soi_MBL(
            final material=Buildings.HeatTransfer.Data.BaseClasses.ThermalProperties(
             k=bfSteRes.soi.k,c=bfSteRes.soi.c,d=bfSteRes.soi.d),
            final h=bfSteRes.adv.hSeg,
            final nSta=bfSteRes.adv.nHor,
            final r_a=bfSteRes.bfGeo.rBor,
            final r_b=bfSteRes.adv.rExt,
            final steadyStateInitial=false,
            final TInt_start=bfSteRes.adv.TFil0_start,
            final TExt_start=bfSteRes.adv.TExt0_start)
            "Heat conduction in the soil"
                annotation (Placement(transformation(extent={{-10,-62},{10,-42}})));

          Buildings.HeatTransfer.Sources.PrescribedHeatFlow prescribedHeatFlow1
            annotation (Placement(transformation(extent={{-66,-62},{-46,-42}})));
          Buildings.HeatTransfer.Sources.PrescribedTemperature prescribedTemperature1
            annotation (Placement(transformation(extent={{44,-62},{64,-42}})));
          Modelica.Blocks.Sources.Constant const3(k=bfSteRes.adv.TFil0_start)
            annotation (Placement(transformation(extent={{0,-92},{20,-72}})));

          CylindricalGroundLayer soi(
            final material=bfSteRes.soi,
            final h=bfSteRes.adv.hSeg,
            final nSta=bfSteRes.adv.nHor,
            final r_a=bfSteRes.bfGeo.rBor,
            final r_b=bfSteRes.adv.rExt,
            final steadyStateInitial=false,
            final TInt_start=bfSteRes.adv.TFil0_start,
            final TExt_start=bfSteRes.adv.TExt0_start)
            "Heat conduction in the soil" annotation (Placement(
                transformation(extent={{-12,16},{8,36}})));
          Buildings.HeatTransfer.Sources.PrescribedTemperature prescribedTemperature2
            annotation (Placement(transformation(extent={{42,16},{62,36}})));
          Modelica.Blocks.Sources.Constant const1(k=bfSteRes.adv.TFil0_start)
            annotation (Placement(transformation(extent={{-2,-14},{18,6}})));
          Modelica.Blocks.Sources.Step     const4(
            height=120,
            offset=0,
            startTime=1000)
            annotation (Placement(transformation(extent={{-94,18},{-74,38}})));
          Buildings.HeatTransfer.Sources.PrescribedHeatFlow prescribedHeatFlow2
            annotation (Placement(transformation(extent={{-64,18},{-44,38}})));
          Modelica.Blocks.Sources.Step     const2(
            height=120,
            offset=0,
            startTime=1000)
            annotation (Placement(transformation(extent={{-96,-62},{-76,-42}})));
        equation
          connect(prescribedHeatFlow1.port,soi_MBL. port_a) annotation (Line(
              points={{-46,-52},{-10,-52}},
              color={191,0,0},
              smooth=Smooth.None));
          connect(soi_MBL.port_b, prescribedTemperature1.port) annotation (Line(
              points={{10,-52},{24,-52},{24,-28},{80,-28},{80,-52},{64,-52}},
              color={191,0,0},
              smooth=Smooth.None));
          connect(const3.y, prescribedTemperature1.T) annotation (Line(
              points={{21,-82},{32,-82},{32,-52},{42,-52}},
              color={0,0,127},
              smooth=Smooth.None));
          connect(soi.port_b, prescribedTemperature2.port) annotation (Line(
              points={{8,26},{22,26},{22,50},{78,50},{78,26},{62,26}},
              color={191,0,0},
              smooth=Smooth.None));
          connect(const1.y, prescribedTemperature2.T) annotation (Line(
              points={{19,-4},{30,-4},{30,26},{40,26}},
              color={0,0,127},
              smooth=Smooth.None));
          connect(const4.y, prescribedHeatFlow2.Q_flow) annotation (Line(
              points={{-73,28},{-64,28}},
              color={0,0,127},
              smooth=Smooth.None));
          connect(prescribedHeatFlow2.port, soi.port_a) annotation (Line(
              points={{-44,28},{-28,28},{-28,26},{-12,26}},
              color={191,0,0},
              smooth=Smooth.None));
          connect(const2.y, prescribedHeatFlow1.Q_flow) annotation (Line(
              points={{-75,-52},{-66,-52}},
              color={0,0,127},
              smooth=Smooth.None));
          annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{100,100}}), graphics),
            experiment(StopTime=187200),
            __Dymola_experimentSetupOutput);
        end singleLayerCylinder_MLB;
      end Examples;
    annotation (preferedView="info", Documentation(info="<html>
<p>
This package contains base classes that are used to construct the models in
<a href=\"modelica://Buildings.Fluid.HeatExchangers.Boreholes\">Buildings.Fluid.HeatExchangers.Boreholes</a>.
</p>
</html>"));
    end BaseClasses;

    package Interface
    extends Modelica.Icons.InterfacesPackage;

      partial model PartialBoreHoleElement

        parameter Data.Records.SoilData matSoi
          "Thermal properties of the ground"
          annotation (Placement(transformation(extent={{-46,-116},{-26,-96}})));
        parameter Data.Records.BoreholeFillingData matFil
          "Thermal properties of the filling material"
          annotation (Placement(transformation(extent={{-22,-116},{-2,-96}})));
        parameter Data.Records.BorefieldGeometryData bfGeo
          "Geometric charachteristic of the borehole"
          annotation (Placement(transformation(extent={{2,-116},{22,-96}})));
        parameter Data.Records.Advanced adv(hBor=bfGeo.hBor)
          "Advanced parameters"
          annotation (Placement(transformation(extent={{-70,-116},{-50,-96}})));
        parameter Data.Records.GenericStepParam genStePar
          annotation (Placement(transformation(extent={{-96,-116},{-76,-96}})));

      end PartialBoreHoleElement;

      partial model PartialBoreHoleInternalHEX
        extends
          Borefield.BaseClasses.BoreHoles.Interface.PartialBoreHoleElement;

        replaceable package Medium =
            Modelica.Media.Interfaces.PartialMedium "Medium in the component"
                                    annotation (choicesAllMatching=true);

        Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a port
          "Heat port that connects to filling material" annotation (Placement(
              transformation(extent={{-10,90},{10,110}}), iconTransformation(extent={
                  {-10,90},{10,110}})));

        annotation (Diagram(coordinateSystem(extent={{-100,-120},{100,100}})), Icon(
              coordinateSystem(extent={{-100,-120},{100,100}})));
      end PartialBoreHoleInternalHEX;

      partial model PartialSingleBoreHole "Single borehole heat exchanger"
        import Buildings;

        extends
          Borefield.BaseClasses.BoreHoles.Interface.PartialBoreHoleElement;
        replaceable package Medium =
            Modelica.Media.Interfaces.PartialMedium "Medium in the component"
                                    annotation (choicesAllMatching=true);
        extends Buildings.Fluid.Interfaces.PartialTwoPortInterface(show_T=true,
            redeclare package Medium = Medium);
        extends Buildings.Fluid.Interfaces.TwoPortFlowResistanceParameters(final
            computeFlowResistance=false, final linearizeFlowResistance=false);
        extends Buildings.Fluid.Interfaces.LumpedVolumeDeclarations;

        Modelica.SIunits.Temperature T_wall_ave "Average borehole temperature";

      end PartialSingleBoreHole;
    end Interface;

    package Examples "Example files for borehole heat exchanger"
    extends Modelica.Icons.ExamplesPackage;

      replaceable package Medium = Modelica.Media.Interfaces.PartialMedium
        "Medium in the component" annotation (choicesAllMatching=true);

      model SingleBoreHole_MBL "Model that tests the borehole model"
        import Buildings;
        extends Modelica.Icons.Example;
        package Medium = Buildings.Media.ConstantPropertyLiquidWater;
        inner Modelica.Fluid.System system
          annotation (Placement(transformation(extent={{-100,-100},{-80,-80}})));

        Borefield.Data.BorefieldStepResponse.SandstoneLine1H100qSte30tSte3600
          bfSteRes annotation (Placement(transformation(extent={{80,-98},{100,
                  -78}})));

        Borefield.BaseClasses.BoreHoles.BaseClasses.SingleBoreHole
          borHol(
          redeclare each package Medium = Medium,
          matSoi=bfSteRes.soi,
          matFil=bfSteRes.bhFil,
          bfGeo=bfSteRes.bfGeo,
          adv=bfSteRes.adv,
          dp_nominal=10000,
          m_flow_nominal=0.3) "Borehole heat exchanger" annotation (
            Placement(transformation(extent={{-16,-36},{16,-4}}, rotation=0)));

        Buildings.Fluid.Sources.Boundary_ph sin(nPorts=2, redeclare package
            Medium =
              Medium) "Sink"
          annotation (Placement(transformation(extent={{56,-30},{36,-10}})));

        Buildings.Fluid.Sources.MassFlowSource_T sou(
          nPorts=1,
          redeclare package Medium = Medium,
          use_m_flow_in=true,
          T=298.15) "Source"
          annotation (Placement(transformation(extent={{-50,-30},{-30,-10}})));

        Modelica.Blocks.Sources.Pulse pulse(
          amplitude=0.3,
          period=365*86400,
          startTime=365*86400/4)
          annotation (Placement(transformation(extent={{-90,-30},{-70,-10}})));

        Buildings.Fluid.HeatExchangers.Boreholes.UTube borHol_MBL(
          redeclare each package Medium = Medium,
          hBor=100,
          dp_nominal=10000,
          dT_dz=0,
          samplePeriod=604800,
          m_flow_nominal=0.3,
          redeclare each Buildings.HeatTransfer.Data.Soil.Sandstone matFil,
          redeclare Buildings.HeatTransfer.Data.Soil.Sandstone matSoi,
          TExt0_start=273.15,
          TFil0_start=273.15) "Borehole heat exchanger" annotation (
            Placement(transformation(extent={{-20,24},{12,56}}, rotation=0)));

        Buildings.Fluid.Sources.MassFlowSource_T sou1(
          nPorts=1,
          redeclare package Medium = Medium,
          use_m_flow_in=true,
          T=298.15) "Source"
          annotation (Placement(transformation(extent={{-54,30},{-34,50}})));
      equation
        connect(sou.ports[1], borHol.port_a) annotation (Line(
            points={{-30,-20},{-16,-20}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(borHol.port_b, sin.ports[1]) annotation (Line(
            points={{16,-20},{26,-20},{26,-18},{36,-18}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(pulse.y, sou.m_flow_in) annotation (Line(
            points={{-69,-20},{-60,-20},{-60,-12},{-50,-12}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(sou1.ports[1], borHol_MBL.port_a) annotation (Line(
            points={{-34,40},{-20,40}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(pulse.y, sou1.m_flow_in) annotation (Line(
            points={{-69,-20},{-64,-20},{-64,48},{-54,48}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(borHol_MBL.port_b, sin.ports[2]) annotation (Line(
            points={{12,40},{26,40},{26,-22},{36,-22}},
            color={0,127,255},
            smooth=Smooth.None));
        annotation (
          __Dymola_Commands(file=
                "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
              "Simulate and plot"),
          Diagram(coordinateSystem(preserveAspectRatio=false,extent={{-100,-100},
                  {100,100}}),
                          graphics),
          experimentSetupOutput,
          Diagram,
          Documentation(info="<html>
<p>
This example models a borehole heat exchanger with two pipes that are
symmetrically spaced. 
The simulation period is 5 years.
From the 4th to the 10th months, the mass flow source switches on the 
flow rate through the borehole. The leaving
water of the mass flow source is <i>25</i>&deg;C,
and the water that returns from the borehole is between 
<i>20.5</i>&deg;C
and 
<i>21.5</i>&deg;C.
</p>
</html>",       revisions="<html>
<ul>
<li>
August 2011, by Pierre Vigouroux:<br>
First implementation.
</li>
</ul>
</html>"),experiment(
            StopTime=3.1536e+07,
            Tolerance=1e-05,
            Algorithm="Radau"));
      end SingleBoreHole_MBL;

      model SingleBoreHoleSerStepLoad "SingleBoreHoleSer with step input load "
        extends Modelica.Icons.Example;

        import DaPModels;
      //  constant Modelica.SIunits.Temperature T_defaultMedium = 283.15;

        package Medium = Buildings.Media.ConstantPropertyLiquidWater;

        redeclare replaceable parameter Borefield.Data.Records.GenericStepParam
                                               genStePar= DaPModels.Borefield.Data.GenericStepParam.test_NickAndThomas_accurate()
          "generic step load parameter"
          annotation (Placement(transformation(extent={{-18,-76},{-8,-66}})));
            //Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30()
        redeclare replaceable parameter Borefield.Data.Records.Advanced adv=Borefield.Data.Advanced.test_NickAndThomas()
          "Advanced parameters"
          annotation (Placement(transformation(extent={{-2,-76},{8,-66}})));
            //Borefield.Data.Advanced.Default()
        redeclare replaceable parameter Borefield.Data.Records.SoilData matSoi= DaPModels.Borefield.Data.SoilData.test_NickAndThomas()
          annotation (Placement(transformation(extent={{14,-76},{24,-66}})));
           //Borefield.Data.SoilData.Sandstone() "Thermal properties of the ground"
        redeclare replaceable parameter
          Borefield.Data.Records.BoreholeFillingData matFil= DaPModels.Borefield.Data.BoreholeFillingData.test_NickAndThomas()
          "Thermal properties of the filling material"
          annotation (Placement(transformation(extent={{30,-76},{40,-66}})));
            //Borefield.Data.BoreholeFillingData.Sandstone()
        redeclare replaceable parameter
          Borefield.Data.Records.BorefieldGeometryData bfGeo= DaPModels.Borefield.Data.BorefieldGeometricData.test_NickAndThomas()
          "Geometric charachteristic of the borehole"
          annotation (Placement(transformation(extent={{46,-76},{56,-66}})));
            //Borefield.Data.BorefieldGeometricData.Line3_rB010_h100()

        Borefield.BaseClasses.BoreHoles.SingleBoreHolesInSerie borHolSer(
          redeclare each package Medium = Medium,
          matSoi=matSoi,
          matFil=matFil,
          bfGeo=bfGeo,
          adv=adv,
          dp_nominal=10000,
          m_flow_nominal=genStePar.m_flow,
          T_start=genStePar.T_ini) "Borehole heat exchanger" annotation (Placement(
              transformation(extent={{-12,-50},{12,-26}}, rotation=0)));

        Buildings.Fluid.Sources.Boundary_ph sin(redeclare package Medium = Medium,
            nPorts=1) "Sink"
          annotation (Placement(transformation(extent={{22,-34},{34,-22}})));

        Modelica.Blocks.Sources.Step step(height=1)
          annotation (Placement(transformation(extent={{48,-18},{36,-6}})));

        Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
          redeclare package Medium = Medium,
          m_flow_nominal=genStePar.m_flow,
          dp_nominal=10000,
          show_T=true,
          energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial,
          m_flow(start=genStePar.m_flow),
          T_start=genStePar.T_ini,
          Q_flow_nominal=genStePar.q_ste*bfGeo.hBor*bfGeo.nbSer,
          p_start=100000)
          annotation (Placement(transformation(extent={{26,10},{6,-10}})));
        Modelica.Blocks.Sources.Constant mFlo(k=genStePar.m_flow)
          annotation (Placement(transformation(extent={{-54,-24},{-42,-12}})));
        Buildings.Fluid.Movers.FlowMachine_m_flow pum(
          redeclare package Medium = Medium,
          m_flow_nominal=genStePar.m_flow,
          m_flow(start=genStePar.m_flow),
          T_start=genStePar.T_ini)
          annotation (Placement(transformation(extent={{-12,10},{-32,-10}})));

      equation
        connect(mFlo.y, pum.m_flow_in) annotation (Line(
            points={{-41.4,-18},{-21.8,-18},{-21.8,-12}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(pum.port_b, borHolSer.port_a) annotation (Line(
            points={{-32,0},{-58,0},{-58,-38},{-12,-38}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(pum.port_a, hea.port_b) annotation (Line(
            points={{-12,0},{6,0}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(hea.port_a, borHolSer.port_b) annotation (Line(
            points={{26,0},{56,0},{56,-38},{12,-38}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(hea.port_a, sin.ports[1]) annotation (Line(
            points={{26,0},{56,0},{56,-28},{34,-28}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(step.y, hea.u) annotation (Line(
            points={{35.4,-12},{34,-12},{34,-6},{28,-6}},
            color={0,0,127},
            smooth=Smooth.None));
        annotation (
          __Dymola_Commands(file=
                "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
              "Simulate and plot"),
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                  -100},{100,100}}),
                         graphics),
          experimentSetupOutput,
          Diagram,
          Documentation(info="<html>
<p>

</p>
</html>",       revisions="<html>
<ul>
</ul>
</html>"),experiment(
            StopTime=3.1536e+007,
            Tolerance=1e-005,
            __Dymola_Algorithm="Dassl"),
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                  {100,100}}),
              graphics));
      end SingleBoreHoleSerStepLoad;

      model SingleBoreHoleSerStepLoad_MSL
        "SingleBoreHoleSer with step input load "
        extends Modelica.Icons.Example;

        import DaPModels;
        inner Modelica.Fluid.System system(T_ambient=adv.T_ambient)
          annotation (Placement(transformation(extent={{-60,-78},{-44,-62}})));
        package Medium = Buildings.Media.ConstantPropertyLiquidWater;

        redeclare replaceable parameter Borefield.Data.Records.GenericStepParam
                                               genStePar=
            Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30()
          "generic step load parameter"
          annotation (Placement(transformation(extent={{-18,-76},{-8,-66}})));
        redeclare replaceable parameter Borefield.Data.Records.Advanced adv=
            Borefield.Data.Advanced.Default() "Advanced parameters"
          annotation (Placement(transformation(extent={{-2,-76},{8,-66}})));
        redeclare replaceable parameter Borefield.Data.Records.SoilData matSoi=
           Borefield.Data.SoilData.Sandstone()
          "Thermal properties of the ground"
          annotation (Placement(transformation(extent={{14,-76},{24,-66}})));
        redeclare replaceable parameter
          Borefield.Data.Records.BoreholeFillingData matFil=
            Borefield.Data.BoreholeFillingData.Sandstone()
          "Thermal properties of the filling material"
          annotation (Placement(transformation(extent={{30,-76},{40,-66}})));
        redeclare replaceable parameter
          Borefield.Data.Records.BorefieldGeometryData bfGeo=
            Borefield.Data.BorefieldGeometricData.Line3_rB010_h100()
          "Geometric charachteristic of the borehole"
          annotation (Placement(transformation(extent={{46,-76},{56,-66}})));

        Borefield.BaseClasses.BoreHoles.SingleBoreHolesInSerie borHolSer(
          redeclare each package Medium = Medium,
          matSoi=matSoi,
          matFil=matFil,
          bfGeo=bfGeo,
          adv=adv,
          dp_nominal=10000,
          m_flow_nominal=genStePar.m_flow,
          T_start=genStePar.T_ini) "Borehole heat exchanger" annotation (Placement(
              transformation(extent={{-12,-50},{12,-26}}, rotation=0)));

        Modelica.Blocks.Sources.Constant mFlo(k=genStePar.m_flow)
          annotation (Placement(transformation(extent={{-54,-24},{-42,-12}})));

        Modelica.Fluid.Sources.FixedBoundary sin(
          redeclare package Medium = Medium,
          use_T=false,
          nPorts=1,
          use_p=true,
          p=100000) annotation (Placement(transformation(extent={{20,-30},{32,-18}})));
        Modelica.Thermal.HeatTransfer.Sources.PrescribedHeatFlow prescribedHeatFlow
          annotation (Placement(transformation(extent={{-2,-24},{10,-12}})));
        Modelica.Blocks.Sources.RealExpression realExpression(y=genStePar.q_ste*bfGeo.hBor
              *bfGeo.nbSer)
          annotation (Placement(transformation(extent={{-16,-24},{-8,-12}})));
        Modelica.Fluid.Pipes.DynamicPipe pipe(
          redeclare package Medium = Medium,
          use_HeatTransfer=true,
          length=1,
          diameter=1,
          T_start=genStePar.T_ini,
          m_flow_start=genStePar.m_flow,
          nNodes=2)
          annotation (Placement(transformation(extent={{26,10},{6,-10}})));
        Modelica.Fluid.Machines.ControlledPump pump(use_m_flow_set=true,
          T_start=genStePar.T_ini,
          redeclare package Medium = Medium,
          m_flow_nominal=genStePar.m_flow,
          V=0.0001,
          p_a_nominal=100000,
          p_b_nominal=200000)
          annotation (Placement(transformation(extent={{-14,-6},{-34,14}})));
      equation
        connect(realExpression.y, prescribedHeatFlow.Q_flow) annotation (Line(
            points={{-7.6,-18},{-2,-18}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(prescribedHeatFlow.port, pipe.heatPorts[1]) annotation (Line(
            points={{10,-18},{17.45,-18},{17.45,-4.4}},
            color={191,0,0},
            smooth=Smooth.None));
        connect(pipe.port_a, borHolSer.port_b) annotation (Line(
            points={{26,0},{42,0},{42,-38},{12,-38}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(sin.ports[1], borHolSer.port_b) annotation (Line(
            points={{32,-24},{42,-24},{42,-38},{12,-38}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(pump.port_a, pipe.port_b) annotation (Line(
            points={{-14,4},{-10,4},{-10,2},{6,2},{6,0}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(pump.port_b, borHolSer.port_a) annotation (Line(
            points={{-34,4},{-58,4},{-58,-38},{-12,-38}},
            color={0,127,255},
            smooth=Smooth.None));
        connect(mFlo.y, pump.m_flow_set) annotation (Line(
            points={{-41.4,-18},{-42,-18},{-42,18},{-19,18},{-19,12.2}},
            color={0,0,127},
            smooth=Smooth.None));
        annotation (
          __Dymola_Commands(file=
                "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
              "Simulate and plot"),
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                  -100},{100,100}}),
                         graphics),
          experimentSetupOutput,
          Diagram,
          Documentation(info="<html>
<p>

</p>
</html>",       revisions="<html>
<ul>
</ul>
</html>"),experiment(
            StopTime=3.1536e+007,
            Tolerance=1e-005,
            __Dymola_Algorithm="Dassl"),
          Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                  {100,100}}),
              graphics));
      end SingleBoreHoleSerStepLoad_MSL;
    end Examples;

    package Verification
    extends Icons.VerificationPackage;

      package Verification_Bauer
      extends Icons.VerificationModel;
        model UTube_CstT_Bauer "Cst T load"
          /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
          extends Icons.VerificationModel;

          import DaPModels;
          package Medium = Buildings.Media.ConstantPropertyLiquidWater;
          inner Modelica.Fluid.System system(T_ambient=bfSteRes.adv.T_ambient)
            annotation (Placement(transformation(extent={{-100,-100},{-80,-80}})));

          parameter Borefield.Data.BorefieldStepResponse.Validation_Bauer
            bfSteRes annotation (Placement(transformation(extent={{-10,78},{10,98}})));
          parameter Modelica.SIunits.Temperature Thigh=273.15 + 80;
          parameter Modelica.SIunits.Temperature Tlow=273.15 + 40;

          Borefield.BaseClasses.BoreHoles.BaseClasses.BoreHoleSegmentFourPort
            borHol[bfSteRes.adv.nVer](
            redeclare each final package Medium = Medium,
            each final matSoi=bfSteRes.soi,
            each final matFil=bfSteRes.bhFil,
            each final bfGeo=bfSteRes.bfGeo,
            each final genStePar=bfSteRes.genStePar,
            each final adv=bfSteRes.adv,
            final dp_nominal={if i == 1 then 0 else 0 for i in 1:bfSteRes.adv.nVer},
            TExt_start=bfSteRes.adv.TExt_start,
            TFil_start=bfSteRes.adv.TExt_start,
            each final homotopyInitialization=true,
            each final show_T=false,
            each final computeFlowResistance=true,
            each final from_dp=false,
            each final linearizeFlowResistance=false,
            each final deltaM=0.1,
            each final energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial,
            each final massDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial)
            "Discretized borehole segments"
            annotation (Placement(transformation(extent={{-18,-32},{18,4}})));

          Modelica.Blocks.Sources.Constant mFlo(k=bfSteRes.genStePar.m_flow)
            annotation (Placement(transformation(extent={{-94,36},{-74,56}})));

          Buildings.Fluid.Sources.Boundary_ph sinLow(redeclare package Medium
              =                                                                 Medium,
              nPorts=1) "Sink"
            annotation (Placement(transformation(extent={{-60,-50},{-40,-30}})));
          Buildings.Fluid.Sources.Boundary_ph sinHigh(redeclare package Medium
              =                                                                  Medium,
              nPorts=1) "Sink"
            annotation (Placement(transformation(extent={{62,10},{42,30}})));
          Buildings.Fluid.Sources.MassFlowSource_T highT(
            nPorts=1,
            redeclare package Medium = Medium,
            use_m_flow_in=true,
            T=Thigh) "Source"
            annotation (Placement(transformation(extent={{-60,0},{-40,20}})));
          Buildings.Fluid.Sources.MassFlowSource_T lowT(
            nPorts=1,
            redeclare package Medium = Medium,
            use_m_flow_in=true,
            T=Tlow) "Source"
            annotation (Placement(transformation(extent={{62,-50},{42,-30}})));

          Modelica.SIunits.HeatFlowRate Q_fg1Fg2;
          Modelica.SIunits.HeatFlowRate Q_fg1;
          Modelica.SIunits.HeatFlowRate Q_fg2;

        equation
          Q_fg1 = borHol[1].intHEX.Rpg1.port_a.Q_flow;
          Q_fg2 = borHol[1].intHEX.Rpg2.port_a.Q_flow;
          Q_fg1Fg2 = Q_fg1 + Q_fg2;

          connect(sinLow.ports[1], borHol[1].port_b2) annotation (Line(
              points={{-40,-40},{-30,-40},{-30,-24.8},{-18,-24.8}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(sinHigh.ports[1], borHol[1].port_b1) annotation (Line(
              points={{42,20},{32,20},{32,-3.2},{18,-3.2}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(highT.ports[1], borHol[1].port_a1) annotation (Line(
              points={{-40,10},{-32,10},{-32,-3.2},{-18,-3.2}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(mFlo.y, highT.m_flow_in) annotation (Line(
              points={{-73,46},{-68,46},{-68,18},{-60,18}},
              color={0,0,127},
              smooth=Smooth.None));
          connect(lowT.ports[1], borHol[1].port_a2) annotation (Line(
              points={{42,-40},{30,-40},{30,-24.8},{18,-24.8}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(mFlo.y, lowT.m_flow_in) annotation (Line(
              points={{-73,46},{80,46},{80,-32},{62,-32}},
              color={0,0,127},
              smooth=Smooth.None));
          annotation (
            __Dymola_Commands(file=
                  "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
                "Simulate and plot"),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                    100,100}}), graphics),
            experimentSetupOutput,
            Diagram,
            Documentation(info="<html>
<p>

</p>
</html>",         revisions="<html>
<ul>
</ul>
</html>"),  experiment(
              StopTime=10800,
              Tolerance=1e-005,
              __Dymola_Algorithm="Dassl"),
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                    100}}), graphics));
        end UTube_CstT_Bauer;
      end Verification_Bauer;

      package Verification_Spitler_WetterDaP
      extends Icons.VerificationPackage;

        model UTubeStepLoadExperiement_Spitler
          "Spitler experiment with constant load"
          extends Icons.VerificationModel;

          Modelica.Blocks.Sources.CombiTimeTable combiTimeTable(
            tableOnFile=true,
            tableName="data",
            fileName=
                "E:/work/modelica/VerificationData/Spitler/SpitlerCstLoad_Time_Tsup_Tret_deltaT.txt",
            offset={0,0,0},
            columns={2,3,4})
            annotation (Placement(transformation(extent={{-60,0},{-40,20}})));

          Modelica.Blocks.Sources.CombiTimeTable combiTimeTable2(
            tableOnFile=true,
            tableName="data",
            fileName=
                "E:/work/modelica/VerificationData/Spitler/SpitlerCstLoad_Time_Tb_Tb24_Tb44_Tb65_Tb85.txt",
            offset={0,0,0,0,0},
            columns={2,3,4,5,6})
            annotation (Placement(transformation(extent={{40,-20},{60,0}})));

          Modelica.SIunits.Temperature T_sup "water supply temperature";
          Modelica.SIunits.Temperature T_ret "water return temperature";
          Modelica.SIunits.Temperature T_f "average fluid temperature";
          Modelica.SIunits.TemperatureDifference deltaT_fb
            "temperature difference between T_f and T_b";

          Modelica.SIunits.Temperature T_b "borehole wall temperature";
          Modelica.SIunits.Temperature T_b24 "sand temperature at 24cm";
          Modelica.SIunits.Temperature T_b44 "sand temperature at 44cm";
          Modelica.SIunits.Temperature T_b65 "sand temperature at 65cm";
          Modelica.SIunits.Temperature T_b85 "sand temperature at 85cm";

          Modelica.SIunits.TemperatureDifference T_delta24;
          Modelica.SIunits.TemperatureDifference T_delta44;
          Modelica.SIunits.TemperatureDifference T_delta65;
          Modelica.SIunits.TemperatureDifference T_delta85;

        equation
          T_sup = combiTimeTable.y[1] + 273.15;
          T_ret = combiTimeTable.y[2] + 273.15;

          T_b = combiTimeTable2.y[1] + 273.15;
          T_b24 = combiTimeTable2.y[2] + 273.15;
          T_b44 = combiTimeTable2.y[3] + 273.15;
          T_b65 = combiTimeTable2.y[4] + 273.15;
          T_b85 = combiTimeTable2.y[5] + 273.15;

          T_delta24 = T_b24 - T_b;
          T_delta44 = T_b44 - T_b24;
          T_delta65 = T_b65 - T_b44;
          T_delta85 = T_b85 - T_b65;

          T_f = (T_sup + T_ret)/2;
          deltaT_fb = T_f - T_b;

          annotation (experiment(StopTime=186350), __Dymola_experimentSetupOutput);
        end UTubeStepLoadExperiement_Spitler;

        model borHol_MBL_Spitler "borHol with step input load "
          /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
          extends Icons.VerificationModel;

          import DaPModels;
          package Medium = Buildings.Media.ConstantPropertyLiquidWater;
          inner Modelica.Fluid.System system(T_ambient=bfSteRes.adv.T_ambient)
            annotation (Placement(transformation(extent={{-100,-100},{-80,-80}})));

          parameter
            Borefield.Data.BorefieldStepResponse.Validation_Spitler_DaPWetter
            bfSteRes annotation (Placement(transformation(extent={{-64,78},{-44,98}})));

          Modelica.Blocks.Sources.Step step(height=1)
            annotation (Placement(transformation(extent={{88,32},{68,52}})));

          Buildings.Fluid.HeatExchangers.Boreholes.UTube borHol(
            redeclare each package Medium = Medium,
            hBor=bfSteRes.bfGeo.hBor,
            dp_nominal=10000,
            dT_dz=bfSteRes.adv.dT_dz,
            samplePeriod=bfSteRes.adv.samplePeriod,
            m_flow_nominal=bfSteRes.adv.m_flow_nominal,
            redeclare each Buildings.HeatTransfer.Data.Soil.Sandstone matFil(k=bfSteRes.bhFil.k, c=bfSteRes.bhFil.c, d=bfSteRes.bhFil.d),
            redeclare Buildings.HeatTransfer.Data.Soil.Sandstone matSoi(k=bfSteRes.soi.k, c=bfSteRes.soi.c, d=bfSteRes.soi.d),
            T_start=bfSteRes.adv.TExt0_start,
            TExt0_start=bfSteRes.adv.TExt0_start,
            TFil0_start=bfSteRes.adv.TFil0_start,
            rTub=bfSteRes.bfGeo.rTub,
            kTub=bfSteRes.bfGeo.kTub,
            eTub=bfSteRes.bfGeo.eTub,
            nVer=bfSteRes.adv.nVer,
            rBor=bfSteRes.bfGeo.rBor,
            rExt=bfSteRes.adv.rExt,
            nHor=bfSteRes.adv.nHor,
            xC=bfSteRes.bfGeo.xC,
            TFil_start=bfSteRes.adv.TExt_start) "Borehole heat exchanger"   annotation (
             Placement(transformation(extent={{-16,-76},{16,-44}}, rotation=0)));

          Buildings.Fluid.Sources.Boundary_ph sin(redeclare package Medium = Medium,
              nPorts=1) "Sink"
            annotation (Placement(transformation(extent={{90,-30},{70,-10}})));

          Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
            redeclare package Medium = Medium,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            dp_nominal=10000,
            Q_flow_nominal=bfSteRes.genStePar.q_ste*bfSteRes.bfGeo.hBor,
            show_T=true,
            energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial,
            m_flow(start=bfSteRes.genStePar.m_flow),
            T_start=bfSteRes.genStePar.T_ini,
            p_start=100000)
            annotation (Placement(transformation(extent={{40,10},{20,30}})));
          Modelica.Blocks.Sources.Constant mFlo(k=bfSteRes.genStePar.m_flow)
            annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
          Buildings.Fluid.Movers.FlowMachine_m_flow pum(
            redeclare package Medium = Medium,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            m_flow(start=bfSteRes.genStePar.m_flow),
            T_start=bfSteRes.genStePar.T_ini)
            annotation (Placement(transformation(extent={{-20,10},{-40,30}})));

          Modelica.SIunits.Temperature T_sup "water supply temperature";
          Modelica.SIunits.Temperature T_ret "water return temperature";

          Modelica.SIunits.Temperature T_f "average fluid temperature";
          Modelica.SIunits.Temperature T_b "borehole wall temperature";
        equation
          T_sup = borHol.sta_a.T;
          T_ret = borHol.sta_b.T;
          T_f = (T_sup + T_ret)/2;
          T_b = borHol.borHol[1].pipFil.port.T;
          connect(mFlo.y, pum.m_flow_in) annotation (Line(
              points={{-69,50},{-29.8,50},{-29.8,32}},
              color={0,0,127},
              smooth=Smooth.None));
          connect(pum.port_b, borHol.port_a) annotation (Line(
              points={{-40,20},{-60,20},{-60,-60},{-16,-60}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(pum.port_a, hea.port_b) annotation (Line(
              points={{-20,20},{20,20}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(hea.port_a, borHol.port_b) annotation (Line(
              points={{40,20},{60,20},{60,-60},{16,-60}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(hea.port_a, sin.ports[1]) annotation (Line(
              points={{40,20},{60,20},{60,-20},{70,-20}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(step.y, hea.u) annotation (Line(
              points={{67,42},{60,42},{60,26},{42,26}},
              color={0,0,127},
              smooth=Smooth.None));
          annotation (
            __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
                "Simulate and plot"),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                    100}}), graphics),
            experimentSetupOutput,
            Diagram,
            Documentation(info="<html>
<p>

</p>
</html>",         revisions="<html>
<ul>
</ul>
</html>"),  experiment(
              StopTime=186350,
              Tolerance=1e-005,
              __Dymola_Algorithm="Dassl"),
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
                graphics));
        end borHol_MBL_Spitler;

        model borHol_Spitler "UTube with step input load "
          /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
          extends Icons.VerificationModel;

          import DaPModels;
          package Medium = Buildings.Media.ConstantPropertyLiquidWater;
          inner Modelica.Fluid.System system(T_ambient=bfSteRes.adv.T_ambient)
            annotation (Placement(transformation(extent={{-100,-100},{-80,-80}})));

          parameter
            Borefield.Data.BorefieldStepResponse.Validation_Spitler_DaPWetter
            bfSteRes annotation (Placement(transformation(extent={{-64,78},{-44,98}})));

          Modelica.Blocks.Sources.Step step(height=1)
            annotation (Placement(transformation(extent={{88,32},{68,52}})));

          Borefield.BaseClasses.BoreHoles.SingleBoreHolesInSerie borHolSer(
            redeclare each package Medium = Medium,
            matSoi=bfSteRes.soi,
            matFil=bfSteRes.bhFil,
            bfGeo=bfSteRes.bfGeo,
            adv=bfSteRes.adv,
            dp_nominal=10000,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            T_start=bfSteRes.genStePar.T_ini) "Borehole heat exchanger" annotation (
              Placement(transformation(extent={{-16,-76},{16,-44}}, rotation=0)));

          Buildings.Fluid.Sources.Boundary_ph sin(redeclare package Medium = Medium,
              nPorts=1) "Sink"
            annotation (Placement(transformation(extent={{90,-30},{70,-10}})));

          Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
            redeclare package Medium = Medium,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            dp_nominal=10000,
            Q_flow_nominal=bfSteRes.genStePar.q_ste*bfSteRes.bfGeo.hBor,
            show_T=true,
            energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial,
            m_flow(start=bfSteRes.genStePar.m_flow),
            T_start=bfSteRes.genStePar.T_ini,
            p_start=100000)
            annotation (Placement(transformation(extent={{40,10},{20,30}})));
          Modelica.Blocks.Sources.Constant mFlo(k=bfSteRes.genStePar.m_flow)
            annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
          Buildings.Fluid.Movers.FlowMachine_m_flow pum(
            redeclare package Medium = Medium,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            m_flow(start=bfSteRes.genStePar.m_flow),
            T_start=bfSteRes.genStePar.T_ini)
            annotation (Placement(transformation(extent={{-20,10},{-40,30}})));

          Modelica.SIunits.Temperature T_sup "water supply temperature";
          Modelica.SIunits.Temperature T_ret "water return temperature";
          Modelica.SIunits.Temperature T_b "borehole wall temperature";
          Modelica.SIunits.Temperature T_f "average fluid temperature";
          Modelica.SIunits.TemperatureDifference deltaT_fb
            "temperature difference between T_f and T_b";

        equation
          T_sup = borHolSer.sta_a.T;
          T_ret = borHolSer.sta_b.T;
          T_b = borHolSer.borHol[1].borHolSeg[1].intHEX.port.T;
          T_f = (T_sup + T_ret)/2;
          deltaT_fb = T_f - T_b;
          connect(mFlo.y, pum.m_flow_in) annotation (Line(
              points={{-69,50},{-29.8,50},{-29.8,32}},
              color={0,0,127},
              smooth=Smooth.None));
          connect(pum.port_b, borHolSer.port_a) annotation (Line(
              points={{-40,20},{-60,20},{-60,-60},{-16,-60}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(pum.port_a, hea.port_b) annotation (Line(
              points={{-20,20},{20,20}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(hea.port_a, borHolSer.port_b) annotation (Line(
              points={{40,20},{60,20},{60,-60},{16,-60}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(hea.port_a, sin.ports[1]) annotation (Line(
              points={{40,20},{60,20},{60,-20},{70,-20}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(step.y, hea.u) annotation (Line(
              points={{67,42},{60,42},{60,26},{42,26}},
              color={0,0,127},
              smooth=Smooth.None));
          annotation (
            __Dymola_Commands(file=
                  "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
                "Simulate and plot"),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                    100,100}}), graphics),
            experimentSetupOutput,
            Diagram,
            Documentation(info="<html>
<p>

</p>
</html>",         revisions="<html>
<ul>
</ul>
</html>"),  experiment(
              StopTime=186350,
              Tolerance=1e-005,
              __Dymola_Algorithm="Dassl"),
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                    100}}), graphics));
        end borHol_Spitler;

        model TRNSYS_Spitler "Spitler experiment with constant load"
          extends Icons.VerificationModel;

          Modelica.Blocks.Sources.CombiTimeTable combiTimeTable(
            tableOnFile=true,
            tableName="data",
            fileName=
                "E:/work/modelica/VerificationData/Spitler/SpitlerTRNSYS_TinTout_modelica.txt",
            offset={0,0},
            columns={2,3})
            annotation (Placement(transformation(extent={{-60,0},{-40,20}})));

          Modelica.SIunits.Temperature T_sup "water supply temperature";
          Modelica.SIunits.Temperature T_ret "water return temperature";
          Modelica.SIunits.Temperature T_f "average fluid temperature";

        equation
          T_sup = combiTimeTable.y[1] + 273.15;
          T_ret = combiTimeTable.y[2] + 273.15;

          T_f = (T_sup + T_ret)/2;

          annotation (
            experiment(StopTime=186350),
            __Dymola_experimentSetupOutput,
            Commands(file="../Scripts/SpitlerValidation_plot.mos" "plot"));
        end TRNSYS_Spitler;

        model TRNSYS_Spitler_EWS "Spitler experiment with constant load"
          extends Icons.VerificationModel;

          Modelica.Blocks.Sources.CombiTimeTable combiTimeTable(
            tableOnFile=true,
            tableName="data",
            fileName=
                "E:/work/modelica/VerificationData/Spitler/Data_EWS_model_timeTinTout_modelica.txt",
            offset={0,0},
            columns={2,3})
            annotation (Placement(transformation(extent={{-60,0},{-40,20}})));

          Modelica.SIunits.Temperature T_sup "water supply temperature";
          Modelica.SIunits.Temperature T_ret "water return temperature";
          Modelica.SIunits.Temperature T_f "average fluid temperature";

          Real timeHour;
        equation
          T_sup = combiTimeTable.y[1] + 273.15;
          T_ret = combiTimeTable.y[2] + 273.15;

          T_f = (T_sup + T_ret)/2;

          timeHour = time/3600;
          annotation (
            experiment(StopTime=186350),
            __Dymola_experimentSetupOutput,
            Commands(file="../Scripts/SpitlerValidation_plot.mos" "plot"));
        end TRNSYS_Spitler_EWS;

        model UTubeDaP_Spitler_accurateLoad "UTube with step input load "
          /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
          extends Icons.VerificationModel;

          import DaPModels;
          package Medium = Buildings.Media.ConstantPropertyLiquidWater;
          inner Modelica.Fluid.System system(T_ambient=bfSteRes.adv.T_ambient)
            annotation (Placement(transformation(extent={{-100,-100},{-80,-80}})));

          parameter
            Borefield.Data.BorefieldStepResponse.Validation_Spitler_DaPWetter
            bfSteRes annotation (Placement(transformation(extent={{-64,78},{-44,98}})));

          Modelica.Blocks.Sources.CombiTimeTable combiTimeTable(
            tableOnFile=true,
            tableName="data",
            fileName=
                "E:/work/modelica/VerificationData/SpitlerCstLoad_Time_Tsup_Tret_deltaT.txt",
            offset={0,0,0},
            columns={2,3,4}) "Load from Spitler, based on M*Cp*(T_sup - T_ret)"
            annotation (Placement(transformation(extent={{88,32},{68,52}})));

          Borefield.BaseClasses.BoreHoles.BaseClasses.SingleBoreHole
            borHol(
            redeclare each package Medium = Medium,
            matSoi=bfSteRes.soi,
            matFil=bfSteRes.bhFil,
            bfGeo=bfSteRes.bfGeo,
            adv=bfSteRes.adv,
            dp_nominal=10000,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            T_start=bfSteRes.genStePar.T_ini) "Borehole heat exchanger"
            annotation (Placement(transformation(extent={{-16,-76},{16,-44}},
                  rotation=0)));

          Buildings.Fluid.Sources.Boundary_ph sin(redeclare package Medium = Medium,
              nPorts=1) "Sink"
            annotation (Placement(transformation(extent={{90,-30},{70,-10}})));

          Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
            redeclare package Medium = Medium,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            dp_nominal=10000,
            Q_flow_nominal=bfSteRes.genStePar.q_ste*bfSteRes.bfGeo.hBor,
            show_T=true,
            energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial,
            m_flow(start=bfSteRes.genStePar.m_flow),
            T_start=bfSteRes.genStePar.T_ini,
            p_start=100000)
            annotation (Placement(transformation(extent={{40,10},{20,30}})));
          Modelica.Blocks.Sources.Constant mFlo(k=bfSteRes.genStePar.m_flow)
            annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
          Buildings.Fluid.Movers.FlowMachine_m_flow pum(
            redeclare package Medium = Medium,
            m_flow_nominal=bfSteRes.genStePar.m_flow,
            m_flow(start=bfSteRes.genStePar.m_flow),
            T_start=bfSteRes.genStePar.T_ini)
            annotation (Placement(transformation(extent={{-20,10},{-40,30}})));
          Modelica.SIunits.Temperature T_sup "water supply temperature";
          Modelica.SIunits.Temperature T_ret "water return temperature";

          Modelica.SIunits.Temperature T_f "average fluid temperature";
          Modelica.SIunits.Temperature T_b "borehole wall temperature";

        equation
          T_sup = borHol.sta_a.T;
          T_ret = borHol.sta_b.T;
          T_f = (T_sup + T_ret)/2;
          T_b = borHol.borHol[1].pipFil.port.T;

          connect(mFlo.y, pum.m_flow_in) annotation (Line(
              points={{-69,50},{-29.8,50},{-29.8,32}},
              color={0,0,127},
              smooth=Smooth.None));
          connect(pum.port_b, borHol.port_a) annotation (Line(
              points={{-40,20},{-60,20},{-60,-60},{-16,-60}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(pum.port_a, hea.port_b) annotation (Line(
              points={{-20,20},{20,20}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(hea.port_a, borHol.port_b) annotation (Line(
              points={{40,20},{60,20},{60,-60},{16,-60}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(hea.port_a, sin.ports[1]) annotation (Line(
              points={{40,20},{60,20},{60,-20},{70,-20}},
              color={0,127,255},
              smooth=Smooth.None));
          connect(combiTimeTable.y[3], hea.u) annotation (Line(
              points={{67,42},{60,42},{60,26},{42,26}},
              color={0,0,127},
              smooth=Smooth.None));
          annotation (
            __Dymola_Commands(file=
                  "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
                "Simulate and plot"),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                    100,100}}), graphics),
            experimentSetupOutput,
            Diagram,
            Documentation(info="<html>
<p>

</p>
</html>",         revisions="<html>
<ul>
</ul>
</html>"),  experiment(
              StopTime=186350,
              Tolerance=1e-005,
              __Dymola_Algorithm="Dassl"),
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                    100}}), graphics));
        end UTubeDaP_Spitler_accurateLoad;
      end Verification_Spitler_WetterDaP;
    end Verification;
  annotation (preferedView="info", Documentation(info="<html>
This package contains models for borehole heat exchangers.
</html>"));
  end BoreHoles;

  package GroundHX
      extends Modelica.Icons.Package;

    function HeatCarrierFluidStepTemperature "Return the corrected average of the (vertical) mean temperature at the center of each borehole of the boreholes field. \\
  The correction is from t=0 till t_d = tBre. \\
  Input TResSho gives the vector with the correct temperatures for this time period"
      extends
        Borefield.BaseClasses.GroundHX.BaseClasses.partialBoreFieldTemperature;
      import SI = Modelica.SIunits;

      input Borefield.Data.Records.ResponseWetter resWet=
          Borefield.Data.ResponseWetter.Sandstone_Line1_rB010_h100_H100qSte30();

    protected
      SI.TemperatureDifference delta_T_fts_corBre;

    algorithm
      assert(genStePar.tBre_d > genStePar.t_min_d,
        "The choosen tBre_d is too small. It should be bigger than t_min_d!");
      if t_d < genStePar.tBre_d then
        T := resWet.TResSho[t_d + 1];
        delta_T_fts_corBre := 0;
      else
        delta_T_fts_corBre := resWet.TResSho[genStePar.tBre_d] -
          Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
                  t_d=genStePar.tBre_d,
                  r=bfGeo.rBor,
                  genStePar=genStePar,
                  bfGeo=bfGeo,
                  soi=soi);
        T := Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
                  t_d=t_d,
                  r=bfGeo.rBor,
                  genStePar=genStePar,
                  bfGeo=bfGeo,
                  soi=soi) + delta_T_fts_corBre;
      end if;

    end HeatCarrierFluidStepTemperature;

    function BoreFieldWallTemperature
      "Return the bh of bf mean temperature @ r from its center and at time = t"
      extends
        Borefield.BaseClasses.GroundHX.BaseClasses.partialBoreFieldTemperature;
      import SI = Modelica.SIunits;

      input SI.Distance r
        "radial distance from line source (for simulation of single bh. Otherwise = r_b";

    protected
      Real lb "lower boundary of integral";
      Real ub "upper boundary of integral";
      Real res "integral value";
      SI.TemperatureDifference deltaT;
    algorithm
      assert(t_d >= genStePar.t_min_d, "The choosen simulation time is " + String(
        t_d*genStePar.tStep) + "s but the minimum resolution time is " + String(
        genStePar.t_min_d*genStePar.tStep) + "s.");
      lb := 1/sqrt(4*soi.alp*t_d*genStePar.tStep);
      ub := 1/sqrt(4*soi.alp*genStePar.t_min_d*genStePar.tStep);

    // Code commented: make it possible to extend model: for single borehole it would be possible to calculate T at any r.
    //   if bfGeo.nbBh == 1 then
    //     res := Modelica.Math.Nonlinear.quadratureLobatto(
    //       function Borefield.GroundHX.BaseClasses.integrandBh_rt(r=r, D=bfGeo.hBor),
    //       lb,
    //       ub);
    //   else
      assert(r == bfGeo.rBor,
        "The simulation of borefield are only possible for r = rBor. Please ensure both parameters have the same value");
      res := Modelica.Math.Nonlinear.quadratureLobatto(
                function
          Borefield.BaseClasses.GroundHX.BaseClasses.integrandBf_bt(
                  D=bfGeo.hBor,
                  rBor=bfGeo.rBor,
                  nbBh=bfGeo.nbBh,
                  cooBh=bfGeo.cooBh),
                lb,
                ub);
    //   end if;

      deltaT := genStePar.q_ste/(4*Modelica.Constants.pi*soi.k)*res;
      T := genStePar.T_ini + deltaT;

      annotation (Documentation(info="<html>
<p>
This function return the mean borehole or borefield temperature. For the <i>borehole</i>, the function returns the mean temperature at any point at distance <i>r</i> from its center and at time <i>t</i>. For The <i>borefield</i>, the function returns the temperature at <i>r=r_b</i> (the wall temperature) and at time <i>t</i>.
</p>

<p> BAD HACK for the borefield:
<ol>
<li> The mean temperature at the wall of the boreholes of the borefield is approximated by the superposition of the temperature at the borehole + temperature of the other boreholes at the center of the first one. </li>
</ol>
</p>
<p> </p>

</html>"));
    end BoreFieldWallTemperature;

    package BaseClasses
        extends Modelica.Icons.Package;

      function erf "Error function, using the external c-function"
        extends Modelica.Math.Nonlinear.Interfaces.partialScalarFunction;

      external"C" y = erf(u);
        annotation (Include="#include <erf.c>");
      end erf;

      function ierf "Integral of erf(u) for u=0 till u"
        extends Modelica.Math.Nonlinear.Interfaces.partialScalarFunction;

      protected
        Real lb=0 "lower boundary for integral";

      algorithm
        y := u*Borefield.BaseClasses.GroundHX.BaseClasses.erf(u) - 1/
          sqrt(Modelica.Constants.pi)*(1 - Modelica.Constants.e^(-u^2));

      end ierf;

      function integrandBh_rt
        "Integrand for the mean temperature of a single borehole @ r from its center. u = integration variable  y = integrand "
        import SI = Modelica.SIunits;

        extends Modelica.Math.Nonlinear.Interfaces.partialScalarFunction;

        input SI.Distance r "distance to centre of bh";
        input SI.Distance D "depth of bh";

      algorithm
        y := exp(-r^2*u^2)*(4*
          Borefield.BaseClasses.GroundHX.BaseClasses.ierf(D*u) -
          Borefield.BaseClasses.GroundHX.BaseClasses.ierf(2*D*u))/(D*
          u^2);

      end integrandBh_rt;

      function integrandBf_bt
        "Integrand for the mean bore hole wall temperature of a borefield fluid. u = integration variable  y = integrand "
        extends Modelica.Math.Nonlinear.Interfaces.partialScalarFunction;
        import SI = Modelica.SIunits;

        input SI.Distance D "depth of bh";
        input SI.Radius rBor "bh radius";
        input Integer nbBh "number of bh";
        input Real cooBh[nbBh,2] "coordinates of center of boreholes";

      protected
        Real r_ij;
        Real supCoe
          "superposition coefficient of integrand for wall temperature";

      algorithm
        supCoe := 0;
        for i in 1:nbBh loop
          for j in 1:nbBh loop
            if i == j then
              r_ij := rBor;
            else
              r_ij := sqrt((cooBh[i, 1] - cooBh[j, 1])^2 + (cooBh[i, 2] - cooBh[j, 2])
                ^2);
            end if;

            supCoe := supCoe + exp(-r_ij^2*u^2);
          end for;
        end for;
        supCoe := supCoe/nbBh;

        y := supCoe*(4*
          Borefield.BaseClasses.GroundHX.BaseClasses.ierf(D*u) -
          Borefield.BaseClasses.GroundHX.BaseClasses.ierf(2*D*u))/(D*
          u^2);

      end integrandBf_bt;

      partial function partialBoreFieldTemperature
        import SI = Modelica.SIunits;

        input Borefield.Data.Records.GenericStepParam genStePar;
        input Borefield.Data.Records.BorefieldGeometryData bfGeo;
        input Borefield.Data.Records.SoilData soi;
        input Integer t_d
          "discrete time at which the temperature is calculated";

        output SI.Temperature T;

      end partialBoreFieldTemperature;

      package Examples
        extends Modelica.Icons.ExamplesPackage;

        model test_ierf
          extends Modelica.Icons.Example;

          parameter Integer lim=5000;
          Real y_ierf;

        algorithm
          y_ierf := Borefield.BaseClasses.GroundHX.BaseClasses.ierf(
            u=time*lim);
          annotation (experiment(
              StopTime=1,
              __Dymola_NumberOfIntervals=1000,
              Tolerance=1e-005,
              __Dymola_Algorithm="Dassl"), __Dymola_experimentSetupOutput);
        end test_ierf;

        model test_integrandBh_rt
          extends Modelica.Icons.Example;

          parameter Real samplePeriod=0.01;
          parameter Integer lim=5;
          Real int;

        algorithm
          if time < 0.007 then
            int := 0;
          else
            int :=
              Borefield.BaseClasses.GroundHX.BaseClasses.integrandBh_rt(
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

        model test_integrandBf_ft
          extends Modelica.Icons.Example;

          parameter Integer lim=5;
          Real int;

        algorithm
          if time < 0.00785 then
            int := 0;
          else
            int :=
              Borefield.BaseClasses.GroundHX.BaseClasses.integrandBf_bt(
               D=100, u=time*lim);
          end if;
          annotation (experiment(
              StopTime=1,
              __Dymola_NumberOfIntervals=1000,
              Tolerance=1e-005,
              __Dymola_Algorithm="Dassl"), __Dymola_experimentSetupOutput);
        end test_integrandBf_ft;
      end Examples;
    end BaseClasses;

    package Interface
    extends Modelica.Icons.InterfacesPackage;

    end Interface;

    package Examples
      extends Modelica.Icons.ExamplesPackage;

      model test_T_rt
        extends Modelica.Icons.Example;

        parameter Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30
          genStePar;
         parameter Borefield.Data.BorefieldGeometricData.Line3_rB010_h100
           bfGeo_mb;
        parameter Borefield.Data.BorefieldGeometricData.Line1_rB010_h100
          bfGeo_sb;

        parameter Borefield.Data.SoilData.Sandstone soi;
        parameter Borefield.Data.Records.ResponseWetter resWet=
            Data.ResponseWetter.Sandstone_Line1_rB010_h100_H100qSte30();

        Modelica.SIunits.Temperature T_rt_sb;
        Modelica.SIunits.Temperature T_rt_mb;

      equation
        if time < genStePar.t_min_d*genStePar.tStep then
          T_rt_sb = 273.15;
          T_rt_mb = 273.15;
        else
          T_rt_sb = Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
            t_d=integer(time/genStePar.tStep),
            r=bfGeo_sb.rBor,
            genStePar=genStePar,
            bfGeo=bfGeo_sb,
            soi=soi);
           T_rt_mb = Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
             t_d=integer(time/genStePar.tStep),
             r=bfGeo_mb.rBor,
             genStePar=genStePar,
             bfGeo=bfGeo_mb,
             soi=soi);
        end if;

        annotation (experiment(StopTime=700000, __Dymola_NumberOfIntervals=100),
            __Dymola_experimentSetupOutput);
      end test_T_rt;

      model test_T_ft_cor
        extends Modelica.Icons.Example;
        import SI = Modelica.SIunits;

        parameter Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30
          genStePar;
        parameter Borefield.Data.BorefieldGeometricData.Line3_rB010_h100
          bfGeo;
        parameter Borefield.Data.SoilData.Sandstone soi;
        parameter
          Borefield.Data.ResponseWetter.Sandstone_Line1_rB010_h100_H100qSte30         resWet;

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
        T_fts_cor =
            Borefield.BaseClasses.GroundHX.HeatCarrierFluidStepTemperature(
                      t_d=max(t_old, integer(integer(time/timeSca)*timeSca/
              genStePar.tStep)),
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi,
                      resWet=resWet);

        annotation (experiment(StopTime=720000, __Dymola_NumberOfIntervals=10),
            __Dymola_experimentSetupOutput);
      end test_T_ft_cor;
    end Examples;

    package Verification
      extends Icons.VerificationPackage;

      model T_rt_bertagnolio_SB_A
        extends Icons.VerificationModel;

        parameter Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A
          genStePar;
        parameter
          Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_SB
          bfGeo;
        parameter Borefield.Data.SoilData.Validation_bertagnolio_SB_A soi;

        parameter Borefield.Data.ResponseWetter.Validation_bertagnolio_SB_A
          resWet;

        Modelica.SIunits.Temperature T_rt;

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
          T_rt =
            Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
                      t_d=max(t_old, integer(integer(time/timeSca)*timeSca/
              genStePar.tStep)),
                      r=bfGeo.rBor,
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi);

        annotation (experiment(StopTime=2.20752e+009, __Dymola_NumberOfIntervals=
                100), __Dymola_experimentSetupOutput);
      end T_rt_bertagnolio_SB_A;

      model T_rt_bertagnolio_MB_A2x1
        extends Icons.VerificationModel;

        parameter Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A
          genStePar;
        parameter
          Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_MB2x1
          bfGeo;
        parameter Borefield.Data.SoilData.Validation_bertagnolio_SB_A soi;

        Modelica.SIunits.Temperature T_rt;

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

          T_rt =
            Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
                      t_d=max(t_old, integer(integer(time/timeSca)*timeSca/
              genStePar.tStep)),
                      r=bfGeo.rBor,
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi);
        annotation (experiment(StopTime=2.20752e+009, __Dymola_NumberOfIntervals=
                100), __Dymola_experimentSetupOutput);
      end T_rt_bertagnolio_MB_A2x1;

      model T_rt_bertagnolio_MB_A8
        extends Icons.VerificationModel;

        parameter Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A
          genStePar;
        parameter
          Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_MB_A8
          bfGeo;
        parameter Borefield.Data.SoilData.Validation_bertagnolio_SB_A soi;

        Modelica.SIunits.Temperature T_rt;

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

          T_rt =
            Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
                      t_d=max(t_old, integer(integer(time/timeSca)*timeSca/
              genStePar.tStep)),
                      r=bfGeo.rBor,
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi);
        annotation (experiment(StopTime=2.20752e+009, __Dymola_NumberOfIntervals=
                100), __Dymola_experimentSetupOutput);
      end T_rt_bertagnolio_MB_A8;

      model T_rt_bertagnolio_MB_A8x8
        extends Icons.VerificationModel;

        parameter Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A
          genStePar;
        parameter
          Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_MB_A8x8
          bfGeo;
        parameter Borefield.Data.SoilData.Validation_bertagnolio_SB_A soi;

        Modelica.SIunits.Temperature T_rt;

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

          T_rt =
            Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
                      t_d=max(t_old, integer(integer(time/timeSca)*timeSca/
              genStePar.tStep)),
                      r=bfGeo.rBor,
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi);
        annotation (experiment(StopTime=2.20752e+009, __Dymola_NumberOfIntervals=
                100), __Dymola_experimentSetupOutput);
      end T_rt_bertagnolio_MB_A8x8;
    end Verification;
  end GroundHX;

  package Aggregation
      extends Modelica.Icons.Package;

    function transientFrac "Calculates the transient resistance for each cell"
      extends Borefield.BaseClasses.Aggregation.Interface.partialAggFunction;
      import SI = Modelica.SIunits;

      input Borefield.Data.Records.GenericStepParam genStePar;
      input Borefield.Data.Records.BorefieldGeometryData bfGeo;
      input Borefield.Data.Records.SoilData soi;
      input Borefield.Data.Records.ResponseWetter resWet;

      input Integer[q_max,p_max] nuMat
        "number of pulse at the end of each cells";

      input Modelica.SIunits.Temperature TSteSta "steady state temperature";

      output Real[q_max,p_max] kappaMat "transient resistance for each cell";

    //   parameter SI.Temperature TSteSta = Borefield.GroundHX.T_rt(
    //     r=bfGeo.rBor,
    //     genStePar=genStePar,
    //     bfGeo=bfGeo,
    //     soi=soi,
    //     t_d=genStePar.TSteSta_d);
    protected
      Integer q_pre;
      Integer p_pre;

    algorithm
      for q in 1:q_max loop
        for p in 1:p_max loop
          if q == 1 and p == 1 then
            kappaMat[q, p] :=(
              Borefield.BaseClasses.GroundHX.HeatCarrierFluidStepTemperature(
                      t_d=nuMat[q, p],
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi,
                      resWet=resWet) - genStePar.T_ini)/TSteSta;
    //           Borefield.GroundHX.T_ft_cor(
    //           t_d=0,
    //           genStePar=genStePar,
    //           bfGeo=bfGeo,
    //           soi=soi,
    //           resWet=resWet)) / TSteSta;
          else
            (q_pre,p_pre) :=
              Borefield.BaseClasses.Aggregation.BaseClasses.previousCellIndex(
                      q_max,
                      p_max,
                      q,
                      p);

            kappaMat[q, p] :=(
              Borefield.BaseClasses.GroundHX.HeatCarrierFluidStepTemperature(
                      t_d=nuMat[q, p],
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi,
                      resWet=resWet) -
              Borefield.BaseClasses.GroundHX.HeatCarrierFluidStepTemperature(
                      t_d=nuMat[q_pre, p_pre],
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi,
                      resWet=resWet))/TSteSta;
          end if;
        end for;
      end for;

    end transientFrac;

    function aggregateLoad
      extends Borefield.BaseClasses.Aggregation.Interface.partialAggFunction;

      input Integer[q_max] rArr;
      input Real QNew "New load element";
      input Real[q_max,p_max] QAggOld
        "Aggregated load matrix form the previous time step";
      input Integer[q_max,p_max] nuMat
        "number of pulse at the end of each cells";

      output Real[q_max,p_max] QAggNew "New aggregated load matrix";

    protected
      Integer q_pre;
      Integer p_pre;
      Real Q_shiPreCell "load from the previous cell";

    algorithm
      for q in 1:q_max loop
        for p in 1:p_max loop
          if p == 1 and q == 1 then
            Q_shiPreCell := QNew;   //New load
          else
            (q_pre,p_pre) :=
              Borefield.BaseClasses.Aggregation.BaseClasses.previousCellIndex(
                      q_max=q_max,
                      p_max=p_max,
                      q=q,
                      p=p);
            Q_shiPreCell := QAggOld[q_pre, p_pre];
          end if;

          //Load from previous cell
          //new load at QAgg[q,p] = initial load in the cell + shifted load from the previous cell, spread over the width of the cell
          //                                                     - one block of the initial load on the cell (which is going to the next cell).
          QAggNew[q, p] := QAggOld[q, p] + (Q_shiPreCell - QAggOld[q, p])/rArr[q];

        end for;
      end for;
    end aggregateLoad;

    package BaseClasses
      extends Modelica.Icons.Package;

      function nbOfLevelAgg
        "Calculate the number of level necessary to aggregate the whole load and set the value of v_max, q_max and rArr"

        input Integer n_max "nb of load step to aggreagate";
        input Integer p_max "number of cells by level";
        output Integer q_max "number of levels";
        output Integer v_max "nb of pulse covered by aggregation";

      protected
        Integer i_lev;
      algorithm
        v_max := 0;
        i_lev := 0;

        while v_max < n_max and i_lev < 100 loop
          v_max := v_max + integer(2^i_lev)*p_max;
          i_lev := i_lev + 1;
        end while;

        assert(i_lev < 100,
          "Too many or zero levels. Increase the nbOfCells by levels");

        q_max := i_lev;
      end nbOfLevelAgg;

      function cellWidth
        " Calculates the width of the cell of each level. The width increase exponential with base 2 "
        extends Borefield.BaseClasses.Aggregation.Interface.partialAggFunction;

        output Integer[q_max] rArr "width of cell at each level";

      algorithm
        for i in 1:q_max loop
          rArr[i] := integer(2^(i - 1));
        end for;

      end cellWidth;

      function nbPulseAtEndEachLevel
        "Calculates the number of pulse at the end of each cells"
        extends Borefield.BaseClasses.Aggregation.Interface.partialAggFunction;

        input Integer[q_max] rArr "width of cell at each level";
        output Integer[q_max,p_max] nuMat
          "number of pulse at the end of each cells";

      protected
        Integer levelTerm;

      algorithm
        levelTerm := 0;
        for q in 1:q_max loop
          for p in 1:p_max loop
            nuMat[q, p] := levelTerm + rArr[q]*p;
          end for;
          levelTerm := levelTerm + rArr[q]*p_max;
        end for;
      end nbPulseAtEndEachLevel;

      function previousCellIndex
        "This function calculates the index [q,p] of the previous cell "
        extends Borefield.BaseClasses.Aggregation.Interface.partialAggFunction;

        input Integer q;
        input Integer p;
        output Integer q_pre;
        output Integer p_pre;

      algorithm
        assert((q > 0 and p > 0) and (q > 1 or p > 1),
          "The choosen index is 1. No previous index is possible");
        assert((q <= q_max and p <= p_max),
          "The choosen index is out of the boundary.");

        if p == 1 then
          q_pre := q - 1;
          p_pre := p_max;
        else
          q_pre := q;
          p_pre := p - 1;
        end if;

      end previousCellIndex;
    end BaseClasses;

    package Interface
    extends Modelica.Icons.InterfacesPackage;

      partial function partialAggFunction
        input Integer q_max "number of levels";
        input Integer p_max "number of cells by level";

      end partialAggFunction;
    end Interface;

    package Examples
      extends Modelica.Icons.ExamplesPackage;

      function test_aggregateLoad
        input Integer n_max=14;
        input Integer p_max=2;
        // for n_max = 14 and p_max=2 --> q_max = 3

        input Integer q_max=BaseClasses.nbOfLevelAgg(
            n_max, p_max);

        input Real QNew = 2 "New load element";
        input Real[q_max,p_max] QAggOld = fill(1,q_max,p_max)
          "Aggregated load matrix form the previous time step";

        output Integer[q_max] rArr=
            BaseClasses.cellWidth(                             q_max, p_max);
        output Integer[q_max,p_max] nuMat=
            BaseClasses.nbPulseAtEndEachLevel(
              q_max,
              p_max,
              rArr);
        output Real[q_max,p_max] QMat;
      algorithm
        QMat := Borefield.BaseClasses.Aggregation.aggregateLoad(
                    q_max=q_max,
                    p_max=p_max,
                    rArr=rArr,
                    nuMat=nuMat,
                    QNew=QNew,
                    QAggOld=QAggOld);
      end test_aggregateLoad;

      function test_TransientFrac "ATTENTION: don't translate this function! otherwise it doesn't work anymore, \\
  because some of the code is not possible to statically translate into c-code!\\
  ATTENTION: first translate transientFrac!
  ---------------------------------------------------------------------
  Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30(),
  Borefield.Data.BorefieldGeometricData.Line1_rB010_h100(),
  Borefield.Data.SoilData.Sandstone(),
  Borefield.Data.ResponseWetter.SandstoneH100qSte30()
  ---------------------------------------------------------------------
  "
        input Integer n_max=201;
        input Integer p_max=5;
        input Real TSteSta = 280;

        output Integer q_max=BaseClasses.nbOfLevelAgg(
            n_max, p_max);
        output Integer v_max;
        output Integer[q_max] rArr;
        output Integer nbLumpedCells;
        output Integer[q_max,p_max] nuMat;
        output Real[q_max,p_max] kappaMat;

      algorithm
        (,v_max) :=
          Borefield.BaseClasses.Aggregation.BaseClasses.nbOfLevelAgg(
           n_max, p_max);
        (rArr,nbLumpedCells) :=
          Borefield.BaseClasses.Aggregation.BaseClasses.cellWidth(
          q_max, p_max);

        nuMat :=
          Borefield.BaseClasses.Aggregation.BaseClasses.nbPulseAtEndEachLevel(
                    q_max,
                    p_max,
                    rArr);

        kappaMat := Borefield.BaseClasses.Aggregation.transientFrac(
                    q_max,
                    p_max,
            Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30(),
            Borefield.Data.BorefieldGeometricData.Line1_rB010_h100(),
                    Borefield.Data.SoilData.Sandstone(),
            Borefield.Data.BorefieldStepResponse.SandstoneLine1H100qSte30tSte3600(),
                    nuMat=nuMat,
                    TSteSta=TSteSta);

      end test_TransientFrac;

      function T_ft_cor_noCor "Return the corrected mean fluid temperature of the borehole/field. The correction is from t=0 till t_d = tBre. 
  Input TResSho give the vector with the correct temperatures for this time period"
        extends
          Borefield.BaseClasses.GroundHX.BaseClasses.partialBoreFieldTemperature;

        input Borefield.Data.Records.ResponseWetter resWet=
            Borefield.Data.ResponseWetter.SandstoneH100qSte30();
        input Integer t_d
          "discrete time at which the temperature is calculated";

      protected
        Real delta_T_fts_corBre=0;

      algorithm
      //  assert( genStePar.tBre_d > genStePar.t_min_d,  "The choosen tBre_d is too small. It should be bigger than t_min_d!");

        if t_d <= genStePar.t_min_d then
          T := 0;
        else
      /*
    delta_T_fts_corBre := resWet.TResSho[genStePar.tBre_d] - 273.15 - Borefield.GroundHX.T_rt(
      t_d= genStePar.tBre_d,
      r=0,
      genStePar=genStePar,
      bfGeo=bfGeo,
      soi=soi);
*/
          T :=
            Borefield.BaseClasses.GroundHX.BoreFieldWallTemperature(
                      t_d=t_d,
                      r=0,
                      genStePar=genStePar,
                      bfGeo=bfGeo,
                      soi=soi);
                       //+ delta_T_fts_corBre;
        end if;

      end T_ft_cor_noCor;

      function transientFrac_noCor
        "Calculates the transient resistance for each cell"
        extends Borefield.BaseClasses.Aggregation.Interface.partialAggFunction;

        input Borefield.Data.Records.GenericStepParam genStePar;
        input Borefield.Data.Records.BorefieldGeometryData bfGeo;
        input Borefield.Data.Records.SoilData soi;
        input Borefield.Data.Records.ResponseWetter resWet;

        input Integer[q_max,p_max] nuMat
          "number of pulse at the end of each cells";
        output Real[q_max,p_max] kappaMat "transient resistance for each cell";

      protected
        Integer q_pre;
        Integer p_pre;

      algorithm
        for q in 1:q_max loop
          for p in 1:p_max loop
            if q == 1 and p == 1 then
              kappaMat[q, p] :=(
                Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
                          t_d=nuMat[q, p],
                          genStePar=genStePar,
                          bfGeo=bfGeo,
                          soi=soi,
                          resWet=resWet) -
                Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
                          t_d=0,
                          genStePar=genStePar,
                          bfGeo=bfGeo,
                          soi=soi,
                          resWet=resWet))/resWet.T_ss;

            else
              (q_pre,p_pre) :=
                Borefield.BaseClasses.Aggregation.BaseClasses.previousCellIndex(
                          q_max,
                          p_max,
                          q,
                          p);

              kappaMat[q, p] :=(
                Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
                          t_d=nuMat[q, p],
                          genStePar=genStePar,
                          bfGeo=bfGeo,
                          soi=soi,
                          resWet=resWet) -
                Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
                          t_d=nuMat[q_pre, p_pre],
                          genStePar=genStePar,
                          bfGeo=bfGeo,
                          soi=soi,
                          resWet=resWet))/resWet.T_ss;
            end if;
          end for;
        end for;

      end transientFrac_noCor;

      function test_TransientFrac_noCor "ATTENTION: don't translate this function! otherwise it doesn't work anymore, because some of the code is not possible to statically translate into c-code! \\
  ---------------------------------------------------------------------
  Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30(),
  Borefield.Data.BorefieldGeometricData.Line1_rB010_h100(),
  Borefield.Data.SoilData.Sandstone(),
  Borefield.Data.ResponseWetter.SandstoneH100qSte30()
  ---------------------------------------------------------------------
  "
        input Integer n_max=201;
        input Integer p_max=5;

        output Integer q_max=BaseClasses.nbOfLevelAgg(
            n_max, p_max);
        output Integer v_max;
        output Integer[q_max] rArr;
        output Integer nbLumpedCells;
        output Integer[q_max,p_max] nuMat;
        output Real[q_max,p_max] kappaMat;

      algorithm
        (,v_max) :=
          Borefield.BaseClasses.Aggregation.BaseClasses.nbOfLevelAgg(
           n_max, p_max);
        (rArr,nbLumpedCells) :=
          Borefield.BaseClasses.Aggregation.BaseClasses.cellWidth(
          q_max, p_max);

        nuMat :=
          Borefield.BaseClasses.Aggregation.BaseClasses.nbPulseAtEndEachLevel(
                    q_max,
                    p_max,
                    rArr);

        kappaMat :=
          Borefield.BaseClasses.Aggregation.TestClasses.transientFrac_noCor(
                    q_max,
                    p_max,
            Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30(),
            Borefield.Data.BorefieldGeometricData.Line1_rB010_h100(),
                    Borefield.Data.SoilData.Sandstone(),
            Borefield.Data.ResponseWetter.SandstoneH100qSte30(),
                    nuMat=nuMat);

      end test_TransientFrac_noCor;
    end Examples;
  end Aggregation;

  package Scripts
      extends Modelica.Icons.Package;

    function ShortTimeResponseHX
        /* Remark: by calling the function, 3 "true" should appear for: \
      1) translation of model \
      2) simulation of model \
      3) writing the data \
      If you get a false, look for the error!
    */

      import SI = Modelica.SIunits;

      input String name=
          "sandstone_sandstone_line1rB010h100_default_tS3600tMinD1qSte30";
      input String modelToSimulate=
          "DaPModels.Borefield.BaseClasses.BoreHoles.Examples.SingleBoreHoleSerStepLoad"
        "model to simulate";

      input Borefield.Data.Records.SoilData matSoi=
          Borefield.Data.SoilData.Sandstone()
        "Thermal properties of the ground";
      input Borefield.Data.Records.BoreholeFillingData matFil=
          Borefield.Data.BoreholeFillingData.Sandstone()
        "Thermal properties of the filling material";
      input Borefield.Data.Records.BorefieldGeometryData bfGeo=
          Borefield.Data.BorefieldGeometricData.Line1_rB010_h100()
        "Geometric charachteristic of the borehole";
      input Borefield.Data.Records.Advanced adv=
          Borefield.Data.Advanced.Default() "Advanced parameters";
      input Data.Records.GenericStepParam genStePar=
          Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30()
        "generic step load parameter";

      input String savePath=
          "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\";

      output Real[3,genStePar.tBre_d + 1] readData;
    protected
      Integer nbOfPoi=1000;
      String filPathAndName=savePath + name "path and name of file";
      String[2] variablesToStore={"borHolSer.sta_a.T","borHolSer.sta_b.T"}
        "variables to store in result file";
      SI.Time[1,genStePar.tBre_d + 1] timVec={0:genStePar.tStep:genStePar.tBre_d*
          genStePar.tStep} "time vector for which the data are saved";
      String[3] saveName;

    algorithm
      //To ensure that the same number of data points is written in all result files
      //equidistant time grid is enabled and store variables at events is disabled.
      experimentSetupOutput(equdistant=true, events=false);

      //delete the result file if it already exists
      Modelica.Utilities.Files.removeFile(filPathAndName + "_sim");
      Modelica.Utilities.Files.removeFile(filPathAndName + "Data");

      translateModel(modelToSimulate +
        "(redeclare DaPModels.Borefield.Data.SoilData." + matSoi.name + " matSoi" +
        ",redeclare DaPModels.Borefield.Data.BoreholeFillingData." + matFil.name +
        " matFil" + ",redeclare DaPModels.Borefield.Data.BorefieldGeometricData." +
        bfGeo.name + " bfGeo" +
        ",redeclare DaPModels.Borefield.Data.GenericStepParam." + genStePar.name +
        " genStePar" + ",redeclare DaPModels.Borefield.Data.Advanced." + adv.name +
        " adv)");

      // simulation for short time
      simulateModel(
          modelToSimulate,
          stopTime=genStePar.tBre_d*genStePar.tStep,
          numberOfIntervals=nbOfPoi,
          method="dassl",
          resultFile=filPathAndName + "_sim"); //

      // First columns are shorttime, last column is steady state
      readData := cat(
          1,
          timVec,
          interpolateTrajectory(
            filPathAndName + "_sim.mat",
            variablesToStore,
            timVec[1, :]));

       saveName := {"Time",variablesToStore[1],variablesToStore[2]};

      writeTrajectory(
          fileName=filPathAndName + "Data.mat",
          signals=saveName,
          values=transpose(readData));

    end ShortTimeResponseHX;

    function readTrajectoryTRSHX
      input String filPathAndName;
      output Integer[:] tVec;
      output Real[:] TMea "mean in-out temperature";
    protected
      Real[3,:] readData;
    algorithm

      readData := readTrajectory(
          filPathAndName + ".mat",
          {"Time","borHolSer.sta_a.T","borHolSer.sta_b.T"},
          readTrajectorySize(filPathAndName + ".mat"));
      tVec := integer(readData[1, 1:end]);
      TMea := (readData[2, 1:end] + readData[3, 1:end])/2;
    end readTrajectoryTRSHX;

    function readTrajectorytVec
      input String filPathAndName;
      output Integer[:] tVec;
    algorithm
      tVec := Borefield.BaseClasses.Scripts.readTrajectoryTRSHX(
        filPathAndName);
    end readTrajectorytVec;

    function readTrajectoryTResSho
      input String filPathAndName;
      output Real[:] TMea "mean in-out temperature";
    algorithm

      (,TMea) := Borefield.BaseClasses.Scripts.readTrajectoryTRSHX(
        filPathAndName);
    end readTrajectoryTResSho;

    function readTrajectoryVecLen
      input String filPathAndName;
      output Integer vecLen "length of vector";
    algorithm
      vecLen := readTrajectorySize(filPathAndName + ".mat");
    end readTrajectoryVecLen;
  end Scripts;

  function deltaT_ft
    "calculate the temperature difference from the inital temperature of the bore hole wall, due to the Load array."
    extends Borefield.BaseClasses.Aggregation.Interface.partialAggFunction;

    input Real[q_max,p_max] QMat;
    input Real[q_max,p_max] kappaMat;
    input Real R_ss;

    output Modelica.SIunits.TemperatureDifference T_fts;

  protected
    Modelica.SIunits.HeatFlowRate q_sum;
  algorithm
    q_sum := 0;
    for q in 1:q_max loop
      for p in 1:p_max loop
        q_sum := q_sum + QMat[q, p]*kappaMat[q, p];
      end for;
    end for;
    T_fts := R_ss*q_sum;
  end deltaT_ft;

  partial model partial_MultipleBoreHoles
    "Calculates the average fluid temperature T_fts of the borefield for a given (time dependent) load Q_flow"

    // FIXME:
    //  1) make it possible to run model without pre-compilation of g-function (short term)
    //  2) make it possible to run model with full pre-compilation of g-fuction (short and long term)

  // General parameters of borefield
    replaceable parameter Data.Records.BorefieldStepResponse bfSteRes
      constrainedby Data.Records.BorefieldStepResponse
      annotation (Placement(transformation(extent={{-134,-134},{-114,-114}})));

  //General parameters of aggregation
    parameter Integer p_max=5
      "maximum number of cells for each aggreagation level";

    parameter Integer lenSim=3600*24*100
      "Simulation length ([s]). By default = 100 days";

    final parameter Integer q_max=
        Borefield.BaseClasses.Aggregation.BaseClasses.nbOfLevelAgg(
        n_max=integer(lenSim/bfSteRes.genStePar.tStep), p_max=p_max)
      "number of aggregation levels";

    final parameter Modelica.SIunits.Temperature TSteSta=
        Borefield.BaseClasses.GroundHX.HeatCarrierFluidStepTemperature(
              genStePar=bfSteRes.genStePar,
              bfGeo=bfSteRes.bfGeo,
              soi=bfSteRes.soi,
              resWet=bfSteRes.resWet,
              t_d=bfSteRes.genStePar.tSteSta_d);

    final parameter Real R_ss=TSteSta/(bfSteRes.genStePar.q_ste*bfSteRes.bfGeo.hBor
        *bfSteRes.bfGeo.nbBh);

  // Load of borefield
    Modelica.SIunits.HeatFlowRate QAve_flow
      "Average heat flux over a time period";
    Modelica.Blocks.Interfaces.RealInput Q_flow(unit="W")
    annotation (Placement(transformation(extent={{-20,-20},{20,20}},
          rotation=-90,
          origin={0,146}),
          iconTransformation(extent={{-20,-20},{20,20}},
          rotation=-90,
          origin={0,86})));
    Modelica.Blocks.Interfaces.RealOutput T_fts(unit="K")
    annotation (Placement(transformation(extent={{-18,-18},{18,18}},
          rotation=-90,
          origin={0,-144}),
          iconTransformation(extent={{-18,-18},{18,18}},
          rotation=-90,
          origin={2,-98})));

  protected
    final parameter Integer[q_max] rArr=
        Borefield.BaseClasses.Aggregation.BaseClasses.cellWidth(
        q_max=q_max, p_max=p_max) "width of aggregation cell for each level";
    final parameter Integer[q_max,p_max] nuMat=
        Borefield.BaseClasses.Aggregation.BaseClasses.nbPulseAtEndEachLevel(
              q_max=q_max,
              p_max=p_max,
              rArr=rArr)
      "nb of aggregated pulse at end of each aggregation cells";
    final parameter Real[q_max,p_max] kappaMat=
        Borefield.BaseClasses.Aggregation.transientFrac(
              q_max=q_max,
              p_max=p_max,
              genStePar=bfSteRes.genStePar,
              bfGeo=bfSteRes.bfGeo,
              soi=bfSteRes.soi,
              resWet=bfSteRes.resWet,
              nuMat=nuMat,
              TSteSta=TSteSta)
      "transient thermal resistance of each aggregation cells";

    final parameter Integer nbOfAggPulse=nuMat[end, end]
      "number of aggregated pulse";

  //Load
    Real[q_max,p_max] QMat
      "aggregation of load vector. Every discrete time step it is updated.";

  //Utilities
    Integer iSam(min=1)
      "Counter for how many time the model was sampled. Defined as iSam=1 when called at t=0";
    Modelica.SIunits.Energy UOld "Internal energy at the previous period";
    Modelica.SIunits.Energy U
      "Current internal energy, defined as U=0 for t=tStart";
    Modelica.SIunits.Time startTime "Start time of the simulation";

  initial algorithm
    // Initialisation of the internal energy (zeros) and the load vector. Load vector have the same lenght as the number of aggregated pulse and cover lenSim
    U := 0;
    UOld := 0;
  //  loaVec := zeros(nbOfAggPulse);
  equation
    assert( time < lenSim, "The chosen value for lenSim is too small. It cannot cover the simulation time!");
    der(U) = Q_flow
      "integration of load to calculate below the average load/(discrete time step)";

  algorithm
    // Set the start time for the sampling
    when initial() then
      startTime := time;
      iSam := 1;
    end when;

    when initial() or sample(startTime, bfSteRes.genStePar.tStep) then
      QAve_flow := (U - UOld)/bfSteRes.genStePar.tStep;
      UOld := U;

      // Update of aggregated load matrix. Careful: need of inversing order of loaVec (so that [end] = most recent load). FIXME: see if you can change that.
      QMat :=Borefield.BaseClasses.Aggregation.aggregateLoad(
              q_max=q_max,
              p_max=p_max,
              rArr=rArr,
              nuMat=nuMat,
              QNew=QAve_flow,
              QAggOld=QMat);

      T_fts :=Borefield.BaseClasses.deltaT_ft(
              q_max=q_max,
              p_max=p_max,
              QMat=QMat,
              kappaMat=kappaMat,
              R_ss=R_ss) + bfSteRes.genStePar.T_ini;

      iSam := iSam + 1; // FIXME: when I remove this, I get a T_fts = 0 for the whole simulation! ??
    end when;

    annotation (
      experiment(StopTime=70000, __Dymola_NumberOfIntervals=50),
      __Dymola_experimentSetupOutput,
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-140,-140},{140,
              140}}), graphics={
          Rectangle(
            extent={{-90,90},{90,-90}},
            lineColor={0,0,0},
            fillColor={234,210,210},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{10,-10},{70,-70}},
            lineColor={0,0,0},
            fillColor={223,188,190},
            fillPattern=FillPattern.Forward),
          Ellipse(
            extent={{10,70},{70,10}},
            lineColor={0,0,0},
            fillColor={223,188,190},
            fillPattern=FillPattern.Forward),
          Ellipse(
            extent={{-70,70},{-10,10}},
            lineColor={0,0,0},
            fillColor={223,188,190},
            fillPattern=FillPattern.Forward),
          Ellipse(
            extent={{-70,-10},{-10,-70}},
            lineColor={0,0,0},
            fillColor={223,188,190},
            fillPattern=FillPattern.Forward),
          Ellipse(
            extent={{-62,62},{-18,18}},
            lineColor={0,0,0},
            fillColor={0,0,255},
            fillPattern=FillPattern.Forward),
          Ellipse(
            extent={{18,62},{62,18}},
            lineColor={0,0,0},
            fillColor={0,0,255},
            fillPattern=FillPattern.Forward),
          Ellipse(
            extent={{-62,-18},{-18,-62}},
            lineColor={0,0,0},
            fillColor={0,0,255},
            fillPattern=FillPattern.Forward),
          Ellipse(
            extent={{18,-18},{62,-62}},
            lineColor={0,0,0},
            fillColor={0,0,255},
            fillPattern=FillPattern.Forward),
          Text(
            extent={{-76,156},{74,98}},
            lineColor={0,0,255},
            textString="%name")}),
      Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-140,-140},{140,
              140}}),     graphics));
  end partial_MultipleBoreHoles;
end BaseClasses;
