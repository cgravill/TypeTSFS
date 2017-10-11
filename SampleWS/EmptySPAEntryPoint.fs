[<WebSharper.JavaScript>]
module EmptySPAEntryPoint

// We must have an entry point (to force JavaScript out of WebSharper),
// but we don't need it (because we're not an actual WebSharper SPA),
// there can be only one:
[<WebSharper.SPAEntryPoint>]
let Main () = ()