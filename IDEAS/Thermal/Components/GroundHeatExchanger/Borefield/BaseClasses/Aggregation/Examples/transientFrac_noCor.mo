within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation.Examples;
function transientFrac_noCor
  "Calculates the transient resistance for each cell"
  import IDEAS;
  extends Interface.partialAggFunction;

  input Data.Records.GenericStepParam genStePar;
  input Data.Records.BorefieldGeometryData bfGeo;
  input Data.Records.SoilData soi;
  input Data.Records.ResponseWetter resWet;

  input Integer[q_max,p_max] nuMat "number of pulse at the end of each cells";
  output Real[q_max,p_max] kappaMat "transient resistance for each cell";

protected
  Integer q_pre;
  Integer p_pre;

algorithm
  for q in 1:q_max loop
    for p in 1:p_max loop
      if q == 1 and p == 1 then
        kappaMat[q, p] :=(
          IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
          t_d=nuMat[q, p],
          genStePar=genStePar,
          bfGeo=bfGeo,
          soi=soi,
          resWet=resWet) -
          IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
          t_d=0,
          genStePar=genStePar,
          bfGeo=bfGeo,
          soi=soi,
          resWet=resWet))/resWet.T_ss;

      else
        (q_pre,p_pre) := BaseClasses.previousCellIndex(
          q_max,
          p_max,
          q,
          p);

        kappaMat[q, p] :=(
          IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
          t_d=nuMat[q, p],
          genStePar=genStePar,
          bfGeo=bfGeo,
          soi=soi,
          resWet=resWet) -
          IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation.TestClasses.T_ft_cor_noCor(
          t_d=nuMat[q_pre, p_pre],
          genStePar=genStePar,
          bfGeo=bfGeo,
          soi=soi,
          resWet=resWet))/resWet.T_ss;
      end if;
    end for;
  end for;

end transientFrac_noCor;
