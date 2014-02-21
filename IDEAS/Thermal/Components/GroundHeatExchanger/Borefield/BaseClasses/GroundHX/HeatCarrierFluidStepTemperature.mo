within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.BaseClasses.GroundHX;
function HeatCarrierFluidStepTemperature "Return the corrected average of the (vertical) mean temperature at the center of each borehole of the boreholes field. \\
  The correction is from t=0 till t_d = tBre. \\
  Input TResSho gives the vector with the correct temperatures for this time period"
  extends BaseClasses.partialBoreFieldTemperature;
  import SI = Modelica.SIunits;

  input Data.Records.ResponseWetter resWet=
      Data.ResponseWetter.example();

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
      BoreFieldWallTemperature(
      t_d=genStePar.tBre_d,
      r=bfGeo.rBor,
      genStePar=genStePar,
      bfGeo=bfGeo,
      soi=soi);
    T := BoreFieldWallTemperature(
      t_d=t_d,
      r=bfGeo.rBor,
      genStePar=genStePar,
      bfGeo=bfGeo,
      soi=soi) + delta_T_fts_corBre;
  end if;

end HeatCarrierFluidStepTemperature;
