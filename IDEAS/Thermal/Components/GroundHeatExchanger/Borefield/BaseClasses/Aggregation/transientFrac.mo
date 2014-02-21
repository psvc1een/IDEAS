within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.Aggregation;
function transientFrac "Calculates the transient resistance for each cell"
  extends Interface.partialAggFunction;
  import SI = Modelica.SIunits;

  input Data.Records.GenericStepParam genStePar;
  input Data.Records.BorefieldGeometryData bfGeo;
  input Data.Records.SoilData soi;
  input Data.Records.ResponseWetter resWet;

  input Integer[q_max,p_max] nuMat "number of pulse at the end of each cells";

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
        kappaMat[q, p] := (GroundHX.HeatCarrierFluidStepTemperature(
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
        (q_pre,p_pre) := BaseClasses.previousCellIndex(
          q_max,
          p_max,
          q,
          p);

        kappaMat[q, p] := (GroundHX.HeatCarrierFluidStepTemperature(
          t_d=nuMat[q, p],
          genStePar=genStePar,
          bfGeo=bfGeo,
          soi=soi,
          resWet=resWet) - GroundHX.HeatCarrierFluidStepTemperature(
          t_d=nuMat[q_pre, p_pre],
          genStePar=genStePar,
          bfGeo=bfGeo,
          soi=soi,
          resWet=resWet))/TSteSta;
      end if;
    end for;
  end for;

end transientFrac;
