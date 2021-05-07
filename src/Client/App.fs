// DO NOT MODIFY
module App

open Fable.Core.JsInterop
open Browser.Dom
open Feliz

importAll "./public/style.scss"
importAll "../../node_modules/leaflet/dist/leaflet.css"

ReactDOM.render(Index.app (), document.getElementById "feliz-app")