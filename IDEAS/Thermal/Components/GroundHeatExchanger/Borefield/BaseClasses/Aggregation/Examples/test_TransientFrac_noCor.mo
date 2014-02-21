within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation.Examples;
function test_TransientFrac_noCor "ATTENTION: don't translate this function! otherwise it doesn't work anymore, because some of the code is not possible to statically translate into c-code! \\
  ---------------------------------------------------------------------
  Borefield.Data.GenericStepParam.tS3600_tmind1_qSte30(),
  Borefield.Data.BorefieldGeometricData.Line1_rB010_h100(),
  Borefield.Data.SoilData.Sandstone(),
  Borefield.Data.ResponseWetter.SandstoneH100qSte30()
  ---------------------------------------------------------------------
  "
  import IDEAS;
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
  (,v_max) := BaseClasses.nbOfLevelAgg(n_max, p_max);
  (rArr,nbLumpedCells) := BaseClasses.cellWidth(q_max, p_max);

  nuMat := BaseClasses.nbPulseAtEndEachLevel(
    q_max,
    p_max,
    rArr);

  kappaMat :=
    IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation.TestClasses.transientFrac_noCor(
    q_max,
    p_max,
    Data.GenericStepParam.tS3600_tmind1_qSte30(),
    Data.BorefieldGeometricData.Line1_rB010_h100(),
    Data.SoilData.Sandstone(),
    Data.ResponseWetter.SandstoneH100qSte30(),
    nuMat=nuMat);

end test_TransientFrac_noCor;
