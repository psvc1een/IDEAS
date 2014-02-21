within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.Examples;
function test_T_ft
  input Integer q_max=11;
  input Integer p_max=6;

  input Real[q_max,p_max] QMat=BaseClasses.Aggregation.aggregateLoad(
        q_max=q_max,
        p_max=p_max,
        rArr=BaseClasses.Aggregation.BaseClasses.cellWidth(q_max, p_max),
        nuMat=BaseClasses.Aggregation.BaseClasses.nbPulseAtEndEachLevel(
          q_max=q_max,
          p_max=p_max,
          rArr=BaseClasses.Aggregation.BaseClasses.cellWidth(q_max, p_max)),
        lenLoa=12282,
        loaVec=ones(12282));
  input Real[q_max,p_max] kappaMat=BaseClasses.Aggregation.transientFrac(
        q_max=q_max,
        p_max=p_max,
        genStePar=Data.GenericStepParam.tS3600_tmind1_qSte30(),
        bfGeo=Data.BorefieldGeometricData.Line1_rB010_h100(),
        soi=Data.SoilData.Sandstone(),
        resWet=Data.ResponseWetter.SandstoneH100qSte30(),
        nuMat=BaseClasses.Aggregation.BaseClasses.nbPulseAtEndEachLevel(
          q_max=q_max,
          p_max=p_max,
          rArr=BaseClasses.Aggregation.BaseClasses.cellWidth(q_max, p_max)));
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
