within IDEAS.Thermal.Components.GroundHeatExchanger.Borefield.Data.Records;
record BorefieldStepResponse
  extends Modelica.Icons.Record;

  //  parameter String name = "nameBfSteRes" "name of the record";

  replaceable record Soi = SoilData constrainedby SoilData annotation (
      __Dymola_choicesAllMatching=true);
  Soi soi;

  replaceable record BhFill = BoreholeFillingData constrainedby
    BoreholeFillingData annotation (__Dymola_choicesAllMatching=true);
  BhFill bhFil;

  replaceable record BfGeo = BorefieldGeometryData constrainedby
    BorefieldGeometryData annotation (__Dymola_choicesAllMatching=true);
  BfGeo bfGeo;

  replaceable record GenStePar = GenericStepParam constrainedby
    GenericStepParam annotation (__Dymola_choicesAllMatching=true);
  GenStePar genStePar;

  replaceable record Adv = Advanced (hBor=bfGeo.hBor) constrainedby Advanced
    annotation (__Dymola_choicesAllMatching=true);
  Adv adv;

  replaceable record ResWet = ResponseWetter constrainedby ResponseWetter
    annotation (__Dymola_choicesAllMatching=true);
  ResWet resWet;
end BorefieldStepResponse;
