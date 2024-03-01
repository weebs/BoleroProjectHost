module BoleroProjectHost.Server.Index

open Bolero
open Bolero.Html
open Bolero.Server.Html
open BoleroProjectHost

let page = doctypeHtml {
    head {
        meta { attr.charset "UTF-8" }
        meta { attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0" }
        title { "Bolero Application" }
        ``base`` { attr.href "/" }
        link { attr.rel "stylesheet"; attr.href "BoleroProjectHost.Client.styles.css" }
    }
    body {
        div { attr.id "main"; comp<ProjectView.Project> }
        boleroScript
    }
}
