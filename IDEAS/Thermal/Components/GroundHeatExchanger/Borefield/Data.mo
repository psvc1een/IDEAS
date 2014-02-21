within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield;
package Data
  extends Modelica.Icons.Package;

  package Records
    extends Modelica.Icons.Package;

    record SoilData
      import Buildings;

      extends Buildings.HeatTransfer.Data.Soil.Generic;
      parameter String name="SoilData";
      final parameter Modelica.SIunits.DiffusionCoefficient alp = k/d/c;
    end SoilData;

    record BoreholeFillingData
      import Buildings;

      extends Buildings.HeatTransfer.Data.BoreholeFillings.Generic;

      parameter String name="BoreholeFillingData";
      final parameter Modelica.SIunits.DiffusionCoefficient alp = k/d/c;

    end BoreholeFillingData;

    record BorefieldGeometryData
      extends Modelica.Icons.Record;
      import SI = Modelica.SIunits;

      parameter String name="BorefieldGeometryData";

      /*Borehole*/
      parameter SI.Height hBor=100 "Total height of the borehole"
          annotation(Dialog(group="Borehole"));
      parameter SI.Radius rBor=0.1 "Radius of the borehole"
          annotation(Dialog(group="Borehole"));
      parameter Integer nbBh=1 "number of boreholes"
          annotation(Dialog(group="Borehole"));

      parameter Integer nbSer=2 "number of boreholes"
          annotation(Dialog(group="Borehole"));

      parameter Real[nbBh,2] cooBh={{0,0}} "coordinate of the boreholes"
          annotation(Dialog(group="Borehole"));

      /*Pipe*/
      parameter SI.Radius rTub=0.02 "Radius of the tubes"
          annotation(Dialog(group="Tubes"));
      parameter SI.ThermalConductivity kTub=0.5
        "Thermal conductivity of the tube"
      annotation(Dialog(group="Tubes"));

      parameter SI.Length eTub=0.002 "Thickness of a tube"
      annotation(Dialog(group="Tubes"));

      parameter SI.Length xC=0.05
        "Shank spacing, defined as the distance between the center of a pipe and the center of the borehole"
          annotation(Dialog(group="Tubes"));

    end BorefieldGeometryData;

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

    record Advanced
      extends Modelica.Icons.Record;
      import SI = Modelica.SIunits;

      parameter String name="Advanced";

      parameter SI.Height hBor "Total height of the borehole";

      /* System */
      parameter SI.Temperature T_ambient=TExt0_start
        "Ambient temperature for fluid.system";

      /*--------Discretization: */
      parameter Integer nVer=10
        "Number of segments used for discretization in the vertical direction"
          annotation(Dialog(tab="Discretization"));
      parameter Integer nHor(min=1) = 10
        "Number of state variables in each horizontal layer of the soil"
          annotation(Dialog(tab="Discretization"));
      final parameter SI.Height hSeg=hBor/nVer "Height of horizontal element"
      annotation(Dialog(tab="Discretization"));

      /*--------Flow: */
      parameter SI.MassFlowRate m_flow_nominal=0.1 "Nominal mass flow rate"
        annotation(Dialog(tab = "Nominal condition"));
      parameter SI.MassFlowRate m_flow_small(min=0) = 1E-4*abs(m_flow_nominal)
        "Small mass flow rate for regularization of zero flow"
        annotation(Dialog(tab = "Nominal condition"));

      /*--------Boundary condition: */
      /*----------------T_start: */
      /*------------------------Ground: */
      parameter SI.Height z0=10
        "Depth below which the temperature gradient starts"
        annotation(Dialog(tab="Boundary conditions", group="T_start: ground"));
      parameter SI.Height z[nVer]={hBor/nVer*(i - 0.5) for i in 1:nVer}
        "Distance from the surface to the considered segment"
        annotation(Dialog(tab="Boundary conditions", group="T_start: ground"));
      parameter Real dT_dz(unit="K/m") = 0.0
        "Vertical temperature gradient of the undisturbed soil for h below z0"
        annotation(Dialog(tab="Boundary conditions", group="T_start: ground"));

      parameter SI.Radius rExt=3
        "Radius of the soil used for the external boundary condition"
        annotation(Dialog(tab="Boundary conditions", group="T_start: ground"));

      parameter SI.Temperature TExt0_start=273.15
        "Initial far field temperature"
        annotation(Dialog(tab="Boundary conditions", group="T_start: ground"));

      parameter SI.Temperature TExt_start[nVer]={if z[i] >= z0 then TExt0_start + (
          z[i] - z0)*dT_dz else TExt0_start for i in 1:nVer}
        "Temperature of the undisturbed ground"
        annotation(Dialog(tab="Boundary conditions", group="T_start: ground"));

     /*------------------------Filling:*/
      parameter SI.Temperature TFil0_start=TExt0_start
        "Initial temperature of the filling material for h = 0...z0"
        annotation(Dialog(tab="Boundary conditions", group="T_start: filling"));

     /*----------------T_ground_evolution*/
      parameter SI.Time samplePeriod=3600*24*7
        "Sample period for the external boundary condition"
        annotation(Dialog(tab="Boundary conditions", group="T_evolution"));

      /*--------Assumption: */
      parameter Boolean allowFlowReversal=true
        "= true to allow flow reversal, false restricts to design direction (port_a -> port_b)"
        annotation(Dialog(tab="Assumption"), Evaluate=true);
      parameter Boolean homotopyInitialization=true
        "= true, use homotopy method"
        annotation(Dialog(tab="Assumption"), Evaluate=true);
      parameter Boolean computeFlowResistance=false;

      parameter SI.Pressure p_constant=101300;
    end Advanced;

    record ResponseWetter
      extends Modelica.Icons.Record;
      import SI = Modelica.SIunits;

      parameter String name="ResponseWetter";
      parameter Integer vecLen "vector lenght (=tBre_d)";
      parameter SI.Time[vecLen] tVec "[s] time vector";
      parameter SI.Temperature[vecLen] TResSho
        "Short time temperature step response from Wetter's model";
    end ResponseWetter;

    record BorefieldStepResponse
      extends Modelica.Icons.Record;

    //  parameter String name = "nameBfSteRes" "name of the record";

      replaceable record Soi = Borefield.Data.Records.SoilData
        constrainedby Borefield.Data.Records.SoilData
        annotation (__Dymola_choicesAllMatching=true);
      Soi soi;

      replaceable record BhFill =
          Borefield.Data.Records.BoreholeFillingData
        constrainedby Borefield.Data.Records.BoreholeFillingData
        annotation (__Dymola_choicesAllMatching=true);
      BhFill bhFil;

      replaceable record BfGeo = BorefieldGeometryData constrainedby
        Borefield.Data.Records.BorefieldGeometryData
      annotation (__Dymola_choicesAllMatching=true);
      BfGeo bfGeo;

      replaceable record GenStePar =
          Borefield.Data.Records.GenericStepParam
        constrainedby Borefield.Data.Records.GenericStepParam
        annotation (
          __Dymola_choicesAllMatching=true);
      GenStePar genStePar;

      replaceable record Adv = Borefield.Data.Records.Advanced (hBor=bfGeo.hBor)
        constrainedby Borefield.Data.Records.Advanced
        annotation (
          __Dymola_choicesAllMatching=true);
      Adv adv;

      replaceable record ResWet =
          Borefield.Data.Records.ResponseWetter
        constrainedby Borefield.Data.Records.ResponseWetter
        annotation (
          __Dymola_choicesAllMatching=true);
      ResWet resWet;
    end BorefieldStepResponse;
  end Records;

  package SoilData
      extends Modelica.Icons.Package;

    record Sandstone
      extends Borefield.Data.Records.SoilData(
        name="Sandstone",
        k=2.8,
        c=1210,
        d=540);
    end Sandstone;

    record Validation_Spitler
      extends Borefield.Data.Records.SoilData(
        name="Validation_Spitler",
        k=2.82,
        c=1600,
        d=2000); // Beier estimate the volumetric heat capacity = 3.2*10^6 J/m3K. Diffusion coefficient: 0.88 * 10^-6 m2/s
    end Validation_Spitler;

    record Validation_Bauer
      extends Borefield.Data.Records.SoilData(
        name="Validation_Bauer",
        k=2.3,
        c=908,
        d=2500); // Limestone. No value given
    end Validation_Bauer;

    record Validation_bertagnolio_SB_A
      "Validation data set SB-A of article 'Comparing vertical gournd heat exchnager models' from Bertagnolio, Bernier and Kummert, 2012"
      extends Borefield.Data.Records.SoilData(
        name="Validation_bertagnolio_SB_A",
        k=3.5,
        c=1210,
        d=1785);
      // c is randomly chosen and alpha = k/(rho*cp) = 1.6 * 10^-6 m2/s, or 0.014 m2/day
    end Validation_bertagnolio_SB_A;

    record Validation_bertagnolio_SB_B
      "Validation data set SB-B of article 'Comparing vertical gournd heat exchnager models' from Bertagnolio, Bernier and Kummert, 2012"
      extends Borefield.Data.Records.SoilData(
        name="Validation_bertagnolio_SB_B",
        k=1.3,
        c=1210,
        d=1487.6);
      // c is randomly chosen and alpha = k/(rho*cp) = 7.222 * 10^7 m2/s, or 0.0624 m2/day
    end Validation_bertagnolio_SB_B;

    record test_NickAndThomas
      extends Borefield.Data.Records.SoilData(
        name="Validation_bertagnolio_SB_A",
        k=3.5,
        c=1210,
        d=1785);
      // c is randomly chosen and alpha = k/(rho*cp) = 1.6 * 10^-6 m2/s, or 0.014 m2/day
    end test_NickAndThomas;
  end SoilData;

  package BoreholeFillingData
      extends Modelica.Icons.Package;
    record Sandstone
      extends Borefield.Data.Records.BoreholeFillingData(
        name="Sandstone",
        k=2.8,
        c=1210,
        d=540);
    end Sandstone;

    record Betonite
      extends Borefield.Data.Records.BoreholeFillingData(
        name="Betonite",
        k=1.5,
        c=800,
        d=1600);
    end Betonite;

    record Validation_Spitler
      extends Borefield.Data.Records.BoreholeFillingData(
        name="Validation_Spitler",
        k=0.73,
        c=2000,
        d=2000);
        //
        // carefull: c en d of filling not known!
    end Validation_Spitler;

    record Validation_Bauer
      extends Borefield.Data.Records.BoreholeFillingData(
        name="Validation_Bauer",
        k=0.8,
        c=1000,
        d=2000);
        // Betonite / cement / sand / water
    end Validation_Bauer;

    record Validation_bertagnolio_SB_A
      extends Borefield.Data.Records.BoreholeFillingData(
        name="Validation_bertagnolio_SB_A",
        k=3.5,
        c=1210,
        d=1785);
        // same as SoilData.Validation_bertagnolio
    end Validation_bertagnolio_SB_A;

    record Validation_bertagnolio_SB_B_high_kFil
      extends Borefield.Data.Records.BoreholeFillingData(
        name="Validation_bertagnolio_SB_B_high_kFil",
        k=20,
        c=10,
        d=1487.6);
        // same as SoilData.Validation_bertagnolio
    end Validation_bertagnolio_SB_B_high_kFil;

    record test_NickAndThomas
      extends Borefield.Data.Records.BoreholeFillingData(
        name="Validation_bertagnolio_SB_A",
        k=3.5,
        c=1210,
        d=1785);
        // same as SoilData.Validation_bertagnolio
    end test_NickAndThomas;
  end BoreholeFillingData;

  package BorefieldGeometricData
    extends Modelica.Icons.Package;

    record Line3_rB010_h100
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Line3_rB010_h100",
        rBor=0.1,
        hBor=100,
        nbBh=3,
        nbSer=3,
        cooBh={{0,0},{0,1},{0,2}},
        rTub=0.02,
        kTub=0.5,
        eTub=0.002,
        xC=0.05);
    end Line3_rB010_h100;

    record Line1_rB010_h100
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Line1_rB010_h100",
        rBor=0.1,
        hBor=100,
        nbBh=1,
        nbSer=1,
        cooBh={{0,0}},
        rTub=0.02,
        kTub=0.5,
        eTub=0.002,
        xC=0.05);
    end Line1_rB010_h100;

    record Validation_Spitler
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_Spitler",
        rBor=0.063,
        hBor=18.3,
        nbBh=1,
        nbSer=1,
        cooBh={{0,0}},
        rTub=0.02733/2,
        kTub=0.39,
        eTub=0.003,
        xC=0.053/2);
    end Validation_Spitler;

    record Validation_Bauer
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_Bauer",
        rBor=0.075,
        hBor=1,
        nbBh=1,
        nbSer=1,
        cooBh={{0,0}},
        rTub=0.016,
        kTub=0.39,
        eTub=0.0029,
        xC=0.0425);
    end Validation_Bauer;

    record Validation_bertagnolio_SB
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_bertagnolio_SB",
        rBor=0.055,
        hBor=110,
        nbBh=1,
        nbSer=1,
        cooBh={{0,0}},
        rTub=0.02,
        kTub=0.5,
        eTub=0.002,
        xC=0.05);
        // only rBor and hBor is given
    end Validation_bertagnolio_SB;

    record Validation_bertagnolio_MB2x1
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_bertagnolio_MB2x1",
        rBor=0.055,
        hBor=110,
        nbBh=2,
        nbSer=1,
        cooBh={{0,0},{5.5,0}},
        rTub=0.02,
        kTub=0.5,
        eTub=0.002,
        xC=0.05);
        // only rBor and hBor is given
    end Validation_bertagnolio_MB2x1;

    record Validation_bertagnolio_MB_A8
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_bertagnolio_MB_A8",
        rBor=0.055,
        hBor=110,
        nbBh=8,
        nbSer=1,
        cooBh={{0,0},{5.5,0},{11,0},{16.5,0},{22,0},{27.5,0},{33,0},{38.5,0}},
        rTub=0.02,
        kTub=0.5,
        eTub=0.002,
        xC=0.05);
        // only rBor and hBor is given
    end Validation_bertagnolio_MB_A8;

    record Validation_bertagnolio_MB_A8x8
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_bertagnolio_MB_A8x8",
        rBor=0.055,
        hBor=110,
        nbBh=64,
        nbSer=1,
        cooBh={{0,0},{5.5,0},{11,0},{16.5,0},{22,0},{27.5,0},{33,0},{38.5,0},{0,5.5},
            {5.5,5.5},{11,5.5},{16.5,5.5},{22,5.5},{27.5,5.5},{33,5.5},{38.5,5.5},{
            0,11},{5.5,11},{11,11},{16.5,11},{22,11},{27.5,11},{33,11},{38.5,11},{0,
            16.5},{5.5,16.5},{11,16.5},{16.5,16.5},{22,16.5},{27.5,16.5},{33,16.5},
            {38.5,16.5},{0,22},{5.5,22},{11,22},{16.5,22},{22,22},{27.5,22},{33,22},
            {38.5,22},{0,27.5},{5.5,27.5},{11,27.5},{16.5,27.5},{22,27.5},{27.5,
            27.5},{33,27.5},{38.5,27.5},{0,33},{5.5,33},{11,33},{16.5,33},{22,33},{
            27.5,33},{33,33},{38.5,33},{0,38.5},{5.5,38.5},{11,38.5},{16.5,38.5},{
            22,38.5},{27.5,38.5},{33,38.5},{38.5,38.5}},
        rTub=0.02,
        kTub=0.5,
        eTub=0.002,
        xC=0.05);
        // only rBor and hBor is given
    end Validation_bertagnolio_MB_A8x8;

    record Validation_Spitler_3bhSer
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_Spitler",
        rBor=0.063,
        hBor=18.3,
        nbBh=3,
        nbSer=3,
        cooBh={{0,0},{0,5},{2.5,4.3}},
        rTub=0.02733/2,
        kTub=0.39,
        eTub=0.003,
        xC=0.053/2);
    end Validation_Spitler_3bhSer;

    record test_NickAndThomas
      extends Borefield.Data.Records.BorefieldGeometryData(
        name="Validation_bertagnolio_MB_A8",
        rBor=0.055,
        hBor=110,
        nbBh=8,
        nbSer=1,
        cooBh={{0,0},{5.5,0},{11,0},{16.5,0},{22,0},{27.5,0},{33,0},{38.5,0}},
        rTub=0.02,
        kTub=0.5,
        eTub=0.002,
        xC=0.05);
        // only rBor and hBor is given
    end test_NickAndThomas;
  end BorefieldGeometricData;

  package GenericStepParam
      extends Modelica.Icons.Package;

    record tS3600_tmind1_qSte30
      extends Borefield.Data.Records.GenericStepParam(
        name="tS3600_tmind1_qSte30",
        tStep=60,
        t_min_d=1,
        tBre_d=6000,
        q_ste=30,
        m_flow=0.3,
        T_ini=273.15);
           // redeclare package Medium = Buildings.Media.ConstantPropertyLiquidWater
    end tS3600_tmind1_qSte30;

    record Validation_Spitler
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_Spitler",
        tStep=60,
        t_min_d=1,
        tBre_d=6000,
        q_ste=1056/18.3,
        m_flow=0.197,
        T_ini=273.15 + 22);
    end Validation_Spitler;

    record Validation_Bauer
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_Bauer",
        tStep=3600,
        t_min_d=1,
        tBre_d=100,
        q_ste=100,
        m_flow=0.2,
        T_ini=273.15 + 10);
                          // mass flow rate and q_ste is not given (constant T's are send to HexIntermnalElementDaP
    end Validation_Bauer;

    record Validation_bertagnolio_SB_A
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_bertagnolio_SB_A",
        tStep=60,
        t_min_d=1,
        tBre_d=6000,
        q_ste=21.99,
        m_flow=0.3,
        T_ini=283.15); //q_ste = 2*pi*k_s with k_s = 3.5 W/m-K
    end Validation_bertagnolio_SB_A;

    record Validation_bertagnolio_SB_B
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_bertagnolio_SB_B",
        tStep=60,
        t_min_d=1,
        tBre_d=6000,
        q_ste=8.168,
        m_flow=0.3,
        T_ini=283.15); //q_ste = 2*pi*k_s with k_s = 3.5 W/m-K
    end Validation_bertagnolio_SB_B;

    record Validation_bertagnolio_MB_C
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_bertagnolio_MB_C",
        tStep=2400,
        t_min_d=1,
        tBre_d=200,
        q_ste=21.99,
        m_flow=0.3,
        T_ini=283.15); //q_ste = 2*pi*k_s with k_s = 3.5 W/m-K
    end Validation_bertagnolio_MB_C;

    record Validation_Spitler_CPU
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_Spitler",
        tStep=2400,
        t_min_d=1,
        tBre_d=150,
        q_ste=1056/18.3,
        m_flow=0.197,
        T_ini=273.15 + 22);
    end Validation_Spitler_CPU;

    record test_NickAndThomas
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_bertagnolio_SB_A",
        tStep=86400,
        t_min_d=1,
        tBre_d=10,
        q_ste=21.99,
        m_flow=0.3,
        T_ini=283.15); //q_ste = 2*pi*k_s with k_s = 3.5 W/m-K
    end test_NickAndThomas;

    record test_NickAndThomas_accurate
      extends Borefield.Data.Records.GenericStepParam(
        name="Validation_bertagnolio_SB_A",
        tStep=8640,
        t_min_d=1,
        tBre_d=100,
        q_ste=21.99,
        m_flow=0.3,
        T_ini=283.15); //q_ste = 2*pi*k_s with k_s = 3.5 W/m-K
    end test_NickAndThomas_accurate;
  end GenericStepParam;

  package Advanced
      extends Modelica.Icons.Package;
    record Default
      extends Borefield.Data.Records.Advanced(
        hBor=100,
        nVer=10,
        nHor=10,
        m_flow_nominal=0.1,
        z0=10,
        dT_dz=0,
        rExt=3,
        TExt0_start=273.15,
        name="Default");
    end Default;

    record Validation_Spitler
      extends Borefield.Data.Records.Advanced(
        hBor=18.3,
        nVer=10,
        nHor=10,
        m_flow_nominal=0.197,
        z0=10,
        dT_dz=0,
        rExt=3,
        TExt0_start=273.15 + 22,
        samplePeriod=1.5e+6,
        name="Validation_Spitler");
    end Validation_Spitler;

    record Validation_Bauer
      extends Borefield.Data.Records.Advanced(
        hBor=1,
        nVer=1,
        nHor=10,
        m_flow_nominal=0.2,
        z0=10,
        dT_dz=0,
        rExt=3,
        TExt0_start=273.15 + 10,
        samplePeriod=1.5e+6,
        name="Validation_Bauer");
    end Validation_Bauer;

    record Validation_bertagnolio
      extends Borefield.Data.Records.Advanced(
        hBor=110,
        nVer=10,
        nHor=10,
        m_flow_nominal=0.1,
        z0=10,
        dT_dz=0,
        rExt=3,
        TExt0_start=273.15,
        samplePeriod=820000,
        name="Validation_bertagnolio");
    end Validation_bertagnolio;

    record test_NickAndThomas
      extends Borefield.Data.Records.Advanced(
        hBor=110,
        nVer=10,
        nHor=10,
        m_flow_nominal=0.3,
        z0=10,
        dT_dz=0,
        rExt=3,
        TExt0_start=283.15,
        samplePeriod=820000,
        name="test_NickAndThomas");
    end test_NickAndThomas;
  end Advanced;

  package ResponseWetter
      extends Modelica.Icons.Package;

    record Sandstone_Line1_rB010_h100_H100qSte30
      extends Borefield.Data.Records.ResponseWetter(
        name="Sandstone_Line1_rB010_h100_H100qSte30",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line1rB010h100_default_tS3600tmind1qSte30Data"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line1rB010h100_default_tS3600tmind1qSte30Data"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line1rB010h100_default_tS3600tmind1qSte30Data"));

    end Sandstone_Line1_rB010_h100_H100qSte30;

    record Sandstone_Line3_rB010_h100_H100qSte30
      extends Borefield.Data.Records.ResponseWetter(
        name="Sandstone_Line3_rB010_h100_H100qSte30",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line3rB010h100_default_tS3600tmind1qSte30Data"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line3rB010h100_default_tS3600tmind1qSte30Data"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line3rB010h100_default_tS3600tmind1qSte30Data"));

    end Sandstone_Line3_rB010_h100_H100qSte30;

    record Validation_Spitler
      extends Borefield.Data.Records.ResponseWetter(
        name="Validation_Spitler",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_SpitlerData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_SpitlerData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_SpitlerData"));

        //,
        //T_ss = Borefield.Scripts.readTrajectoryT_ss("E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line1rB010h100_default_tS3600tmind1qSte30Data")
    end Validation_Spitler;

    record Validation_Bauer
      extends Borefield.Data.Records.ResponseWetter(
        name="Validation_Bauer",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_BauerData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_BauerData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_BauerData"));

        //,
        //T_ss = Borefield.Scripts.readTrajectoryT_ss("E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_Bauer")
    end Validation_Bauer;

    record Validation_bertagnolio_SB_A
      extends Borefield.Data.Records.ResponseWetter(
        name="Validation_bertagnolio_SB_A",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_bertagnolio_SB_AData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_bertagnolio_SB_AData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_bertagnolio_SB_AData"));

        //,
        //T_ss = Borefield.Scripts.readTrajectoryT_ss("E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line1rB010h100_default_tS3600tmind1qSte30Data")
    end Validation_bertagnolio_SB_A;

    record Validation_bertagnolio_SB_B_high_kFil
      extends Borefield.Data.Records.ResponseWetter(
        name="Validation_bertagnolio_SB_B",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_bertagnolio_SB_B_high_kFilData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_bertagnolio_SB_B_high_kFilData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_bertagnolio_SB_B_high_kFilData"));

    end Validation_bertagnolio_SB_B_high_kFil;

    record Validation_Spitler_CPU
      extends Borefield.Data.Records.ResponseWetter(
        name="Validation_Spitler_CPU",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_Spitler_CPUData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_Spitler_CPUData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_Spitler_CPUData"));

        //,
        //T_ss = Borefield.Scripts.readTrajectoryT_ss("E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line1rB010h100_default_tS3600tmind1qSte30Data")
    end Validation_Spitler_CPU;

    record Validation_Spitler_CPU_3bhSer
      extends Borefield.Data.Records.ResponseWetter(
        name="Validation_Spitler_CPU",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_Spitler_CPU_3bhSerData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_Spitler_CPU_3bhSerData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\Validation_Spitler_CPU_3bhSerData"));

        //,
        //T_ss = Borefield.Scripts.readTrajectoryT_ss("E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\sandstone_sandstone_line1rB010h100_default_tS3600tmind1qSte30Data")
    end Validation_Spitler_CPU_3bhSer;

    record test_NickAndThomas
      extends Borefield.Data.Records.ResponseWetter(
        name="test_NickAndThomas",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\test_NickAndThomasData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\test_NickAndThomasData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\test_NickAndThomasData"));
    end test_NickAndThomas;

    record test_NickAndThomas_accurate
      extends Borefield.Data.Records.ResponseWetter(
        name="test_NickAndThomas_accurate",
        vecLen=Borefield.BaseClasses.Scripts.readTrajectoryVecLen(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\test_NickAndThomas_accurateData"),
        tVec=Borefield.BaseClasses.Scripts.readTrajectorytVec(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\test_NickAndThomas_accurateData"),
        TResSho=Borefield.BaseClasses.Scripts.readTrajectoryTResSho(
            "E:\\work\\modelica\\DaPModels\\Borefield\\Data\\ResponseWetter\\test_NickAndThomas_accurateData"));
    end test_NickAndThomas_accurate;
  end ResponseWetter;

  package BorefieldStepResponse
    extends Modelica.Icons.Package;
    record SandstoneLine1H100qSte30tSte3600 =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Sandstone,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Sandstone,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Line1_rB010_h100,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Default,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Sandstone_Line1_rB010_h100_H100qSte30);

    record SandstoneLine3H100qSte30tSte3600 =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Sandstone,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Sandstone,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Line3_rB010_h100,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Default,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.SandstoneH100qSte30);

    record Validation_Spitler =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_Spitler,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_Spitler,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_Spitler,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_Spitler,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_Spitler,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_Spitler);
    record Validation_Bauer =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_Bauer,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_Bauer,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_Bauer,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_Bauer,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_Bauer,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_Bauer);
    record Validation_bertagnolio_SB_A =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_SB,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_bertagnolio,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_bertagnolio_SB_A);

    record Validation_bertagnolio_SB_B =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_bertagnolio_SB_B,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_bertagnolio_SB_B,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_SB,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_B,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_bertagnolio,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_bertagnolio_SB_B_high_kFil);

    record Validation_bertagnolio_MB_A2x1 =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_MB2x1,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_bertagnolio,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_bertagnolio_SB_A);

    record Validation_bertagnolio_MB_A8 =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_MB_A8,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_bertagnolio,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_bertagnolio_SB_A);

    record Validation_bertagnolio_MB_A8x8 =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_bertagnolio_SB_A,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_MB_A8x8,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_A,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_bertagnolio,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_bertagnolio_SB_A);

    record Validation_bertagnolio_SB_B_high_kFil =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_bertagnolio_SB_B,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_bertagnolio_SB_B_high_kFil,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_bertagnolio_SB,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_bertagnolio_SB_B,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_bertagnolio,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_bertagnolio_SB_B_high_kFil);

    record Validation_Spitler_CPU =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_Spitler,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_Spitler,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_Spitler,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_Spitler_CPU,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_Spitler,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_Spitler_CPU);
    record Validation_Spitler_CPU_3bhSer =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_Spitler,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_Spitler,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_Spitler_3bhSer,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_Spitler_CPU,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_Spitler,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_Spitler_CPU_3bhSer);

    record Validation_Spitler_DaPWetter =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.Validation_Spitler,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.Validation_Spitler,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.Validation_Spitler,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.Validation_Spitler,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Validation_Spitler(nHor=10),
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.Validation_Spitler);
    record test_NickAndThomas =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.test_NickAndThomas,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.test_NickAndThomas,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.test_NickAndThomas,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.test_NickAndThomas,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Default,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.test_NickAndThomas);
    record test_NickAndThomas_accurate =
        Borefield.Data.Records.BorefieldStepResponse (
        redeclare replaceable record Soi =
            Borefield.Data.SoilData.test_NickAndThomas,
        redeclare replaceable record BhFill =
            Borefield.Data.BoreholeFillingData.test_NickAndThomas,
        redeclare replaceable record BfGeo =
            Borefield.Data.BorefieldGeometricData.test_NickAndThomas,
        redeclare replaceable record GenStePar =
            Borefield.Data.GenericStepParam.test_NickAndThomas_accurate,
        redeclare replaceable record Adv =
            Borefield.Data.Advanced.Default,
        redeclare replaceable record ResWet =
            Borefield.Data.ResponseWetter.test_NickAndThomas_accurate);
  end BorefieldStepResponse;
end Data;
