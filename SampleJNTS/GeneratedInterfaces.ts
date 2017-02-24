//These interfaces are code generated from F#, any changes to this file will be lost.
export namespace ExperimentalGeneratedInterfaces {

    export namespace JSAPI {
        export interface CustomGeneric<t> { }
    }

    export namespace System {

    }

    export namespace Shapes {
        export interface Rectangle {
            Width: number;
            Height: number;
        }
    }

    export namespace Microsoft.FSharp.Core {
        export interface Unit { }
    }

    export namespace Opaque {
        export interface FSharpMap<K, V> { }
        export interface Dictionary<K, V> { }
        export interface FSharpTuple { }
    }

    export interface something {
        (): JSAPI.CustomGeneric<number>
    }

    export interface makeRectangle {
        (p0: void): Shapes.Rectangle
    }

    export interface makeRectangleShape {
        (p0: void): string
    }

    export interface makeRectangles {
        (p0: void): Array<Shapes.Rectangle>
    }

    export interface makeLookUp {
        (p0: void): Opaque.FSharpMap<number, Shapes.Rectangle>
    }

    export interface shapeLookUp {
        (look: Opaque.FSharpMap<number, Shapes.Rectangle>): Shapes.Rectangle
    }

    export interface shapeLookUpDict {
        (look: Opaque.Dictionary<number, Shapes.Rectangle>): Shapes.Rectangle
    }

}