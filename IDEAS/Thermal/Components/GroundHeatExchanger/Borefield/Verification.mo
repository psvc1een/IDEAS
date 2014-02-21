within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield;
package Verification
extends Icons.VerificationPackage;

  package Verification_bertagnolio
  extends Icons.VerificationPackage;

    model ver_multipleBoreholes_bertagnolio_MB_C8x8
      extends Icons.VerificationModel;
      parameter
        Borefield.Data.BorefieldStepResponse.Validation_bertagnolio_MB_A8x8
        bfSteRes
        annotation (Placement(transformation(extent={{-92,72},{-72,92}})));
      parameter Integer lenSim=3600*24*365;

      MultipleBoreHoles_Buildings multipleBoreholes(lenSim=lenSim, redeclare
          Borefield.Data.BorefieldStepResponse.Validation_bertagnolio_MB_A8x8
          bfSteRes)
        annotation (Placement(transformation(extent={{-34,-68},{36,2}})));
      Modelica.Blocks.Interfaces.RealOutput T_fts
        annotation (Placement(transformation(extent={{92,-10},{112,10}})));

      Modelica.Blocks.Sources.CombiTimeTable combiTimeTable(
        tableOnFile=true,
        tableName="data",
        fileName=
            "E:/work/modelica/VerificationData/Bertagnolio/DataBertagnolio/q30asym_time.txt",
        offset={0},
        columns={2})
        annotation (Placement(transformation(extent={{-86,-44},{-66,-24}})));

    equation
      multipleBoreholes.Q_flow = combiTimeTable.y[1]/100*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh;
      connect(multipleBoreholes.T_fts, T_fts)  annotation (Line(
          points={{1.5,-57.5},{62,-57.5},{62,0},{102,0}},
          color={0,0,127},
          smooth=Smooth.None));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}}), graphics),
        experiment(StopTime=3.1535e+007, __Dymola_NumberOfIntervals=100),
        __Dymola_experimentSetupOutput);
    end ver_multipleBoreholes_bertagnolio_MB_C8x8;
  end Verification_bertagnolio;

  package ComparisonCPU
    extends Icons.VerificationPackage;
    model singleBoreHoleWetter
      /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
      extends Icons.VerificationModel;

      import DaPModels;
      package Medium = Buildings.Media.ConstantPropertyLiquidWater;

      parameter Borefield.Data.BorefieldStepResponse.Validation_Spitler_CPU
        bfSteRes
        annotation (Placement(transformation(extent={{-64,78},{-44,98}})));

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
        TFil_start=bfSteRes.adv.TExt_start,
        xC=bfSteRes.bfGeo.xC) "Borehole heat exchanger"
        annotation (Placement(transformation(extent={{-16,-76},{16,-44}},
                                                                        rotation=0)));

      Buildings.Fluid.Sources.Boundary_ph sin(redeclare package Medium = Medium,
          nPorts=1) "Sink"
        annotation (Placement(transformation(extent={{90,-30},{70,-10}})));

      Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
        redeclare package Medium = Medium,
        m_flow_nominal=bfSteRes.genStePar.m_flow,
        dp_nominal=10000,
        Q_flow_nominal=430/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh,
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

      Modelica.Blocks.Sources.CombiTimeTable lamarcheProfiel(
        tableOnFile=true,
        tableName="data",
        fileName=
            "E:/work/modelica/VerificationData/Bertagnolio/DataBertagnolio/q30asym_time.txt",
        offset={0},
        columns={2})
        annotation (Placement(transformation(extent={{90,30},{70,50}})));

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

      lamarcheProfiel.y[1]/430 = hea.u annotation (Line(
          points={{69,40},{48,40},{48,26},{42,26}},
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
</html>",   revisions="<html>
<ul>
</ul>
</html>"),
        experiment(
          StopTime=6.307e+008,
          __Dymola_NumberOfIntervals=1000,
          Tolerance=1e-005,
          __Dymola_Algorithm="Dassl"),
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics));
    end singleBoreHoleWetter;

    model singleBoreHoleDaP
      /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
      extends Icons.VerificationModel;

      import DaPModels;
      package Medium = Buildings.Media.ConstantPropertyLiquidWater;

      parameter Borefield.Data.BorefieldStepResponse.Validation_Spitler_CPU
        bfSteRes
        annotation (Placement(transformation(extent={{-64,78},{-44,98}})));
      parameter Integer lenSim=3600*24*365*20;

      Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
        redeclare package Medium = Medium,
        m_flow_nominal=bfSteRes.genStePar.m_flow,
        dp_nominal=10000,
        Q_flow_nominal=430/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh,
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

      Modelica.Blocks.Sources.CombiTimeTable lamarcheProfiel(
        tableOnFile=true,
        tableName="data",
        fileName=
            "E:/work/modelica/VerificationData/Bertagnolio/DataBertagnolio/q30asym_time.txt",
        offset={0},
        columns={2})
        annotation (Placement(transformation(extent={{-60,-60},{-40,-40}})));

      Borefield.MultipleBoreHoles_Buildings multipleBoreholes(lenSim=
            lenSim, bfSteRes=bfSteRes) annotation (Placement(transformation(
            extent={{-22,22},{22,-22}},
            rotation=180,
            origin={0,-68})));
    equation
      T_sup = multipleBoreholes.T_hcf_in;
      T_ret = multipleBoreholes.T_hcf_out;
      T_f = multipleBoreholes.T_fts;
      connect(mFlo.y, pum.m_flow_in) annotation (Line(
          points={{-69,50},{-29.8,50},{-29.8,32}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(pum.port_a, hea.port_b) annotation (Line(
          points={{-20,20},{20,20}},
          color={0,127,255},
          smooth=Smooth.None));

      lamarcheProfiel.y[1]/430 = hea.u;
      lamarcheProfiel.y[1]/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh = -
        multipleBoreholes.Q_flow;

      connect(multipleBoreholes.flowPort_b, hea.port_a) annotation (Line(
          points={{22,-68},{80,-68},{80,20},{40,20}},
          color={0,127,255},
          smooth=Smooth.None));
      connect(pum.port_b, multipleBoreholes.flowPort_a) annotation (Line(
          points={{-40,20},{-80,20},{-80,-68},{-22,-68}},
          color={0,127,255},
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
</html>",   revisions="<html>
<ul>
</ul>
</html>"),
        experiment(
          StopTime=6.307e+008,
          __Dymola_NumberOfIntervals=1000,
          Tolerance=1e-005,
          __Dymola_Algorithm="Dassl"),
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics));
    end singleBoreHoleDaP;

    model singleBoreHoleSer3Wetter
      extends Icons.VerificationModel;

      import DaPModels;
      package Medium = Buildings.Media.ConstantPropertyLiquidWater;

      parameter
        Borefield.Data.BorefieldStepResponse.Validation_Spitler_CPU_3bhSer
        bfSteRes
        annotation (Placement(transformation(extent={{68,-66},{88,-46}})));

      Buildings.Fluid.Sources.Boundary_ph sin(redeclare package Medium = Medium,
          nPorts=1) "Sink"
        annotation (Placement(transformation(extent={{-10,-20},{10,0}})));

      Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
        redeclare package Medium = Medium,
        m_flow_nominal=bfSteRes.genStePar.m_flow,
        dp_nominal=10000,
        Q_flow_nominal=430/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh,
        show_T=true,
        energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial,
        m_flow(start=bfSteRes.genStePar.m_flow),
        T_start=bfSteRes.genStePar.T_ini,
        p_start=100000)
        annotation (Placement(transformation(extent={{40,10},{20,30}})));
      Modelica.Blocks.Sources.Constant mFlo(k=bfSteRes.genStePar.m_flow)
        annotation (Placement(transformation(extent={{-72,-12},{-52,8}})));
      Buildings.Fluid.Movers.FlowMachine_m_flow pum(
        redeclare package Medium = Medium,
        m_flow_nominal=bfSteRes.genStePar.m_flow,
        m_flow(start=bfSteRes.genStePar.m_flow),
        T_start=bfSteRes.genStePar.T_ini)
        annotation (Placement(transformation(extent={{-20,30},{-40,10}})));

      Modelica.SIunits.Temperature T_sup "water supply temperature";
      Modelica.SIunits.Temperature T_ret "water return temperature";

      Modelica.SIunits.Temperature T_f "average fluid temperature";
      Modelica.SIunits.Temperature T_b "borehole wall temperature";

      Modelica.Blocks.Sources.CombiTimeTable lamarcheProfiel(
        tableOnFile=true,
        tableName="data",
        fileName=
            "E:/work/modelica/VerificationData/Bertagnolio/DataBertagnolio/q30asym_time.txt",
        offset={0},
        columns={2})
        annotation (Placement(transformation(extent={{68,-6},{48,14}})));

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
        TFil_start=bfSteRes.adv.TExt_start,
        xC=bfSteRes.bfGeo.xC) "Borehole heat exchanger"
        annotation (Placement(transformation(extent={{-56,-66},{-24,-34}},
                                                                        rotation=0)));
      Buildings.Fluid.HeatExchangers.Boreholes.UTube borHol1(
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
        TFil_start=bfSteRes.adv.TExt_start,
        xC=bfSteRes.bfGeo.xC) "Borehole heat exchanger"
        annotation (Placement(transformation(extent={{-16,-66},{16,-34}},
                                                                        rotation=0)));
      Buildings.Fluid.HeatExchangers.Boreholes.UTube borHol2(
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
        TFil_start=bfSteRes.adv.TExt_start,
        xC=bfSteRes.bfGeo.xC) "Borehole heat exchanger"
        annotation (Placement(transformation(extent={{24,-66},{56,-34}},rotation=0)));
    equation
      T_sup = borHol.sta_a.T;
      T_ret = borHol2.sta_b.T;
      T_f = (T_sup + T_ret)/2;
      T_b = borHol.borHol[1].pipFil.port.T;
      connect(mFlo.y, pum.m_flow_in) annotation (Line(
          points={{-51,-2},{-29.8,-2},{-29.8,8}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(pum.port_a, hea.port_b) annotation (Line(
          points={{-20,20},{20,20}},
          color={0,127,255},
          smooth=Smooth.None));
      connect(hea.port_a, sin.ports[1]) annotation (Line(
          points={{40,20},{74,20},{74,-10},{10,-10}},
          color={0,127,255},
          smooth=Smooth.None));

      lamarcheProfiel.y[1]/430 = hea.u annotation (Line(
          points={{69,40},{48,40},{48,26},{42,26}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(borHol.port_b, borHol1.port_a) annotation (Line(
          points={{-24,-50},{-20,-50},{-20,-50},{-16,-50}},
          color={0,127,255},
          smooth=Smooth.None));
      connect(borHol1.port_b, borHol2.port_a) annotation (Line(
          points={{16,-50},{24,-50}},
          color={0,127,255},
          smooth=Smooth.None));
      connect(borHol2.port_b, hea.port_a) annotation (Line(
          points={{56,-50},{66,-50},{66,-20},{74,-20},{74,20},{40,20}},
          color={0,127,255},
          smooth=Smooth.None));
      connect(pum.port_b, borHol.port_a) annotation (Line(
          points={{-40,20},{-80,20},{-80,-40},{-60,-40},{-60,-50},{-56,-50}},
          color={0,127,255},
          smooth=Smooth.None));
      annotation (
        __Dymola_Commands(file=
              "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
            "Simulate and plot"),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-80},{
                100,40}}), graphics),
        experimentSetupOutput,
        Diagram,
        Documentation(info="<html>
<p>

</p>
</html>",   revisions="<html>
<ul>
</ul>
</html>"),
        experiment(
          StopTime=6.307e+008,
          __Dymola_NumberOfIntervals=1000,
          Tolerance=1e-005,
          __Dymola_Algorithm="Dassl"),
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-80},{100,
                40}}), graphics));
    end singleBoreHoleSer3Wetter;

    model singleBoreHoleSer3DaP_Buildings
      /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
      extends Icons.VerificationModel;

      import DaPModels;
      package Medium = Buildings.Media.ConstantPropertyLiquidWater;

      parameter
        Borefield.Data.BorefieldStepResponse.Validation_Spitler_CPU_3bhSer
        bfSteRes
        annotation (Placement(transformation(extent={{38,-98},{58,-78}})));
      parameter Integer lenSim=3600*24*50;

      Buildings.Fluid.HeatExchangers.HeaterCoolerPrescribed hea(
        redeclare package Medium = Medium,
        m_flow_nominal=bfSteRes.genStePar.m_flow,
        dp_nominal=10000,
        Q_flow_nominal=430/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh,
        show_T=true,
        energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial,
        m_flow(start=bfSteRes.genStePar.m_flow),
        T_start=bfSteRes.genStePar.T_ini,
        p_start=100000)
        annotation (Placement(transformation(extent={{30,-32},{10,-12}})));
      Modelica.Blocks.Sources.Constant mFlo(k=bfSteRes.genStePar.m_flow)
        annotation (Placement(transformation(extent={{-52,-58},{-32,-38}})));
      Buildings.Fluid.Movers.FlowMachine_m_flow pum(
        redeclare package Medium = Medium,
        m_flow_nominal=bfSteRes.genStePar.m_flow,
        m_flow(start=bfSteRes.genStePar.m_flow),
        T_start=bfSteRes.genStePar.T_ini)
        annotation (Placement(transformation(extent={{-10,-12},{-30,-32}})));

      Modelica.SIunits.Temperature T_sup "water supply temperature";
      Modelica.SIunits.Temperature T_ret "water return temperature";

      Modelica.SIunits.Temperature T_f "average fluid temperature";

      Modelica.Blocks.Sources.CombiTimeTable lamarcheProfiel(
        tableOnFile=true,
        tableName="data",
        fileName=
            "E:/work/modelica/VerificationData/Bertagnolio/DataBertagnolio/q30asym_time.txt",
        offset={0},
        columns={2})
        annotation (Placement(transformation(extent={{50,-60},{30,-40}})));

      Borefield.MultipleBoreHoles_Buildings mulBor(lenSim=lenSim, bfSteRes=
            bfSteRes, redeclare final package Medium = Medium)  annotation (Placement(transformation(
            extent={{-19,18},{19,-18}},
            rotation=180,
            origin={-1,-76})));
    equation
      T_sup = mulBor.T_hcf_in;
      T_ret = mulBor.T_hcf_out;
      T_f = mulBor.T_fts;
      connect(mFlo.y, pum.m_flow_in) annotation (Line(
          points={{-31,-48},{-19.8,-48},{-19.8,-34}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(pum.port_a, hea.port_b) annotation (Line(
          points={{-10,-22},{10,-22}},
          color={0,127,255},
          smooth=Smooth.None));

      lamarcheProfiel.y[1]/430 = hea.u "nominal heater power";
      lamarcheProfiel.y[1]/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh = -mulBor.Q_flow;

      connect(pum.port_b, mulBor.flowPort_a) annotation (Line(
          points={{-30,-22},{-58,-22},{-58,-76},{-20,-76}},
          color={0,127,255},
          smooth=Smooth.None));
      connect(hea.port_a, mulBor.flowPort_b) annotation (Line(
          points={{30,-22},{58,-22},{58,-76},{18,-76}},
          color={0,127,255},
          smooth=Smooth.None));
      annotation (
        __Dymola_Commands(file=
              "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
            "Simulate and plot", file="../Scripts/PlotCPUTest_WetterDaP.mos"
            "PlotCPUTest_WetterDaP"),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
                         graphics),
        experimentSetupOutput,
        Diagram,
        Documentation(info="<html>
<p>

</p>
</html>",   revisions="<html>
<ul>
</ul>
</html>"),
        experiment(
          StopTime=6.307e+008,
          __Dymola_NumberOfIntervals=1000,
          Tolerance=1e-005,
          __Dymola_Algorithm="Dassl"),
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
            graphics));
    end singleBoreHoleSer3DaP_Buildings;

    model singleBoreHoleSer3DaP_IDEAS
      /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
      extends Icons.VerificationModel;

      import DaPModels;
      package MediumBuildings = Buildings.Media.ConstantPropertyLiquidWater;
      parameter IDEAS.Thermal.Data.Media.WaterBuildingsLib mediumIDEAS;

      parameter
        Borefield.Data.BorefieldStepResponse.Validation_Spitler_CPU_3bhSer
        bfSteRes
        annotation (Placement(transformation(extent={{38,-98},{58,-78}})));
      parameter Integer lenSim=3600*24*50;

      Modelica.Blocks.Sources.Constant mFlo(k=1)
        annotation (Placement(transformation(extent={{-52,-58},{-32,-38}})));

      Modelica.SIunits.Temperature T_sup "water supply temperature";
      Modelica.SIunits.Temperature T_ret "water return temperature";

      Modelica.SIunits.Temperature T_f "average fluid temperature";

      Modelica.Blocks.Sources.CombiTimeTable lamarcheProfiel(
        tableOnFile=true,
        tableName="data",
        fileName=
            "E:/work/modelica/VerificationData/Bertagnolio/DataBertagnolio/q30asym_time.txt",
        offset={0},
        columns={2})
        annotation (Placement(transformation(extent={{50,-60},{30,-40}})));

      Borefield.MultipleBoreHoles_IDEAS mulBor(lenSim=lenSim, bfSteRes=
            bfSteRes,final medium = mediumIDEAS) annotation (Placement(transformation(
            extent={{-19,18},{19,-18}},
            rotation=180,
            origin={-1,-76})));
      IDEAS.Thermal.Components.BaseClasses.Pump pump(
        useInput=true,
        medium=mediumIDEAS,
        m=0.1,
        TInitial=bfSteRes.genStePar.T_ini,
        m_flowNom=bfSteRes.genStePar.m_flow,
        dpFix=0,
        etaTot=1,
        m_flowSet(start=bfSteRes.genStePar.m_flow))
        annotation (Placement(transformation(extent={{-18,-10},{-38,-30}})));
      IDEAS.Thermal.Components.Production.IdealHeater idealHeater(medium=mediumIDEAS)
        annotation (Placement(transformation(
            extent={{-10,11},{10,-11}},
            rotation=-90,
            origin={0,-11})));
      Buildings.HeatTransfer.Sources.PrescribedHeatFlow prescribedHeatFlow
        annotation (Placement(transformation(extent={{36,-18},{16,2}})));
      Modelica.Blocks.Sources.RealExpression heatLossHea(y = 0)
        annotation (Placement(transformation(extent={{60,-16},{46,-2}})));
      Modelica.Blocks.Sources.RealExpression QFlowHP( y = heatLoadHPVal)
        annotation (Placement(transformation(extent={{18,-36},{4,-22}})));

    protected
        Modelica.SIunits.HeatFlowRate heatLoadHPVal "Heat load of the HP";
    public
      Modelica.Blocks.Math.Product product annotation (Placement(transformation(
            extent={{-6,-6},{6,6}},
            rotation=-90,
            origin={-2,-48})));
      Modelica.Blocks.Sources.RealExpression inv(y=-1)
        annotation (Placement(transformation(extent={{18,-44},{4,-30}})));
    equation
      T_sup = mulBor.T_hcf_in;
      T_ret = mulBor.T_hcf_out;
      T_f = mulBor.T_fts;

      lamarcheProfiel.y[1]/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh = heatLoadHPVal
        "nominal heater power";

      connect(pump.flowPort_b, mulBor.flowPort_a) annotation (Line(
          points={{-38,-20},{-60,-20},{-60,-76},{-20,-76}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(idealHeater.flowPort_b, pump.flowPort_a) annotation (Line(
          points={{-2,-20},{-18,-20}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(idealHeater.flowPort_a, mulBor.flowPort_b) annotation (Line(
          points={{3.8,-20},{56,-20},{56,-76},{18,-76}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(idealHeater.heatPort, prescribedHeatFlow.port) annotation (Line(
          points={{10,-7},{14,-7},{14,-8},{16,-8}},
          color={191,0,0},
          smooth=Smooth.None));
      connect(heatLossHea.y, prescribedHeatFlow.Q_flow) annotation (Line(
          points={{45.3,-9},{40.65,-9},{40.65,-8},{36,-8}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(QFlowHP.y, idealHeater.TSet) annotation (Line(
          points={{3.3,-29},{-14,-29},{-14,-9},{-12,-9}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(mFlo.y, pump.m_flowSet) annotation (Line(
          points={{-31,-48},{-28,-48},{-28,-30}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(product.y, mulBor.Q_flow) annotation (Line(
          points={{-2,-54.6},{-2,-60},{-2,-64.9429},{-1,-64.9429}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(QFlowHP.y, product.u2) annotation (Line(
          points={{3.3,-29},{-5.6,-29},{-5.6,-40.8}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(inv.y, product.u1) annotation (Line(
          points={{3.3,-37},{1.6,-37},{1.6,-40.8}},
          color={0,0,127},
          smooth=Smooth.None));
      annotation (
        __Dymola_Commands(file=
              "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
            "Simulate and plot", file="../Scripts/PlotCPUTest_WetterDaP.mos"
            "PlotCPUTest_WetterDaP"),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
                         graphics),
        experimentSetupOutput,
        Diagram,
        Documentation(info="<html>
<p>

</p>
</html>",   revisions="<html>
<ul>
</ul>
</html>"),
        experiment(
          StopTime=6.307e+008,
          __Dymola_NumberOfIntervals=1000,
          Tolerance=1e-005,
          __Dymola_Algorithm="Dassl"),
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
            graphics));
    end singleBoreHoleSer3DaP_IDEAS;
  end ComparisonCPU;

  model singleBoreHoleSer3DaPWithHP
    /* BAD HACK: careful: the power input = q_ste * hBor. The number of borehole is not taken into account.*/
    extends Icons.VerificationModel;

    import DaPModels;
    parameter IDEAS.Thermal.Data.Media.WaterBuildingsLib medium;

    parameter
      Borefield.Data.BorefieldStepResponse.Validation_bertagnolio_MB_A2x1
      bfSteRes
      annotation (Placement(transformation(extent={{40,-50},{60,-30}})));
    parameter Integer lenSim=3600*24*50;

    Modelica.Blocks.Sources.Constant mFlo(k=1)
      annotation (Placement(transformation(extent={{-50,-10},{-30,10}})));

    Modelica.SIunits.Temperature T_sup "water supply temperature";
    Modelica.SIunits.Temperature T_ret "water return temperature";

    Modelica.SIunits.Temperature T_f "average fluid temperature";

    Modelica.Blocks.Sources.CombiTimeTable lamarcheProfiel(
      tableOnFile=true,
      tableName="data",
      fileName=
          "E:/work/modelica/VerificationData/Bertagnolio/DataBertagnolio/q30asym_time.txt",
      offset={0},
      columns={2})
      annotation (Placement(transformation(extent={{52,-12},{32,8}})));

    Borefield.MultipleBoreHoles_IDEAS mulBor(lenSim=lenSim, bfSteRes=
          bfSteRes,final medium = medium) annotation (Placement(transformation(
          extent={{-19,18},{19,-18}},
          rotation=180,
          origin={1,-28})));
    IDEAS.Thermal.Components.BaseClasses.Pump pump(
      useInput=true,
      medium=medium,
      TInitial=bfSteRes.genStePar.T_ini,
      m_flowNom=bfSteRes.genStePar.m_flow,
      dpFix=0,
      etaTot=1,
      m_flowSet(start=bfSteRes.genStePar.m_flow),
      m=0.1)
      annotation (Placement(transformation(extent={{-16,38},{-36,18}})));

  protected
      Modelica.SIunits.HeatFlowRate heatLoadHPVal "Heat load of the HP";
  public
    IDEAS.Thermal.Components.BaseClasses.Pump pump1(
      medium=medium,
      TInitial=bfSteRes.genStePar.T_ini,
      dpFix=0,
      etaTot=1,
      m_flowSet(start=bfSteRes.genStePar.m_flow),
      useInput=false,
      m=0.001,
      m_flowNom=0.01)
      annotation (Placement(transformation(extent={{-22,86},{-42,66}})));
    Modelica.Blocks.Sources.RealExpression T_HP_set(y=303.15)
      annotation (Placement(transformation(extent={{74,48},{60,62}})));
    IDEAS.Thermal.Components.BaseClasses.Ambient ambient(medium=medium,
        constantAmbientPressure=100000)
      annotation (Placement(transformation(extent={{-66,38},{-86,58}})));
    Extended_IDEAS.HeatPump_BrineWater heatPump_BrineWater(
      give_Q_flow=true,
      mediumCond=medium,
      mediumEvap=medium,
      mMedEva=10,
      mMedCond=10)
      annotation (Placement(transformation(extent={{16,28},{-4,48}})));
    IDEAS.Thermal.Components.BaseClasses.Pipe_HeatPort pipe_HeatPort(m=2, medium=
          medium)
      annotation (Placement(transformation(extent={{10,86},{-10,66}})));
    Modelica.Thermal.HeatTransfer.Sources.PrescribedTemperature
      prescribedTemperature
      annotation (Placement(transformation(extent={{54,82},{40,96}})));
    Modelica.Blocks.Sources.RealExpression QFlowHP1(y=283.15)
      annotation (Placement(transformation(extent={{74,82},{60,96}})));
    IDEAS.Thermal.Components.BaseClasses.MixingVolume stor_emi(
      medium=medium,
      m=100,
      TInitial=293.15) "storage tank at emission side"
      annotation (Placement(transformation(extent={{-64,76},{-44,96}})));
    IDEAS.Thermal.Components.BaseClasses.MixingVolume sto_pro(
      medium=medium,
      m=1000,
      TInitial=283.15) "storage tank at production side" annotation (Placement(
          transformation(
          extent={{-10,-10},{10,10}},
          rotation=90,
          origin={-68,0})));
    Modelica.Thermal.HeatTransfer.Components.ThermalResistor thermalResistor(R=
          100) annotation (Placement(transformation(extent={{30,82},{16,96}})));
  equation
    T_sup = mulBor.T_hcf_in;
    T_ret = mulBor.T_hcf_out;
    T_f = mulBor.T_fts;

    lamarcheProfiel.y[1]/10*bfSteRes.bfGeo.hBor*bfSteRes.bfGeo.nbBh = heatLoadHPVal
      "nominal heater power";

    connect(mFlo.y, pump.m_flowSet) annotation (Line(
        points={{-29,0},{-26,0},{-26,18}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(heatPump_BrineWater.flowPort_b1, pump.flowPort_a) annotation (Line(
        points={{0,28.2},{-8,28.2},{-8,28},{-16,28}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(heatPump_BrineWater.flowPort_a1, mulBor.flowPort_b) annotation (Line(
        points={{12,28},{60,28},{60,-28},{20,-28}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(ambient.flowPort, heatPump_BrineWater.flowPort_b) annotation (Line(
        points={{-66,48},{0,48}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(heatPump_BrineWater.Q_flow_borefield, mulBor.Q_flow) annotation (Line(
        points={{6,27.6},{6,6},{1,6},{1,-16.9429}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(pipe_HeatPort.flowPort_b, pump1.flowPort_a) annotation (Line(
        points={{-10,76},{-22,76}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(pipe_HeatPort.flowPort_a, heatPump_BrineWater.flowPort_a) annotation (
       Line(
        points={{10,76},{26,76},{26,48},{12,48}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(QFlowHP1.y, prescribedTemperature.T) annotation (Line(
        points={{59.3,89},{55.4,89}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(T_HP_set.y, heatPump_BrineWater.T_eva_out_set) annotation (Line(
        points={{59.3,55},{6,55},{6,48}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(pump1.flowPort_b, stor_emi.flowPorts[1]) annotation (Line(
        points={{-42,76},{-48,76},{-48,75.5},{-54,75.5}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(stor_emi.flowPorts[2], heatPump_BrineWater.flowPort_b) annotation (
        Line(
        points={{-54,76.5},{-54,48},{0,48}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(pump.flowPort_b, sto_pro.flowPorts[1]) annotation (Line(
        points={{-36,28},{-57.5,28},{-57.5,0}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(sto_pro.flowPorts[2], mulBor.flowPort_a) annotation (Line(
        points={{-58.5,0},{-58.5,-30},{-18,-30},{-18,-28}},
        color={0,0,255},
        smooth=Smooth.None));
    connect(thermalResistor.port_a, prescribedTemperature.port) annotation (Line(
        points={{30,89},{40,89}},
        color={191,0,0},
        smooth=Smooth.None));
    connect(thermalResistor.port_b, pipe_HeatPort.heatPort) annotation (Line(
        points={{16,89},{8,89},{8,90},{0,90},{0,86}},
        color={191,0,0},
        smooth=Smooth.None));
    annotation (
      __Dymola_Commands(file=
            "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatExchangers/Boreholes/Examples/UTube.mos"
          "Simulate and plot", file="../Scripts/PlotCPUTest_WetterDaP.mos"
          "PlotCPUTest_WetterDaP"),
      Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
              100,100}}),
                       graphics),
      experimentSetupOutput,
      Diagram,
      Documentation(info="<html>
<p>

</p>
</html>", revisions="<html>
<ul>
</ul>
</html>"),
      experiment(
        StopTime=4.32e+006,
        __Dymola_NumberOfIntervals=1000,
        Tolerance=1e-005,
        __Dymola_Algorithm="Dassl"),
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
          graphics));
  end singleBoreHoleSer3DaPWithHP;
end Verification;
