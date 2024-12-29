namespace Gomu.Vectors

[<Struct>]
type Vector2i =
    { X: int
      Y: int }

    static member (~-)(v: Vector2i) = { X = -v.X; Y = -v.Y }

    static member (+)(v1: Vector2i, v2: Vector2i) =
        { X = v1.X + v2.X; Y = v1.Y + v2.Y }

    static member (-)(v1: Vector2i, v2: Vector2i) =
        { X = v1.X - v2.X; Y = v1.Y - v2.Y }

    static member (*)(s: int, v: Vector2i) = { X = s * v.X; Y = s * v.Y }

    static member (*)(v1: Vector2i, v2: Vector2i) = v1.X * v2.X + v1.Y * v2.Y

module Vector2i =

    let i = { X = 1; Y = 0 }
    let j = { X = 0; Y = 1 }

    let magnitude (v: Vector2i) =
        (v.X <<< 1) + (v.Y <<< 1) |> (>>>) 1

    let dot (v1: Vector2i) (v2: Vector2i) = v1 * v2

    let inline map fx fy (v: Vector2i) = { X = fx v.X; Y = fy v.Y }

    let toTuple (v: Vector2i) = v.X, v.Y