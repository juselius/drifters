// DO NOT MODIFY
module UTM

open System
open ProjNet

let private utm = CoordinateSystems.ProjectedCoordinateSystem.WGS84_UTM(33,true)
let private wgs = CoordinateSystems.GeographicCoordinateSystem.WGS84
let private ctf = CoordinateSystems.Transformations.CoordinateTransformationFactory()
let private transformUTM = ctf.CreateFromCoordinateSystems(wgs, utm)
let private transformWGS = ctf.CreateFromCoordinateSystems(utm, wgs)

let fromLatLon (lat: float, lon: float) =
    transformUTM.MathTransform.Transform(lon, lat).ToTuple()

let toLatLon (x: float, y: float) =
    transformWGS.MathTransform.Transform(x, y).ToTuple() |> fun (lon, lat) -> lat, lon
