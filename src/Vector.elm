module Vector exposing(..)


type alias Vector =
    { x : Float
    , y : Float
    }

distance: Vector -> Vector -> Float
distance v1 v2 =
    let
        distTmp = (v2.x - v1.x) *  (v2.x - v1.x) + (v2.y - v1.y) * (v2.y - v1.y) 
    in
        (sqrt distTmp)

limit: Vector -> Float -> Vector
limit vector scalar =
   let
        mSq = vector.x*vector.x + vector.y* vector.y
        vec = 
            if mSq > (scalar*scalar) then
                (normalize >>  multiVectorByScalar)  vector scalar
            else
                vector
   in
       vec

radianToDegrees: Float -> Float
radianToDegrees radians= 
    (radians * 180) / pi

normalize : Vector -> Vector
normalize v =
    let
        dist =
            sqrt (v.x * v.x + v.y * v.y)

        x =
            v.x / dist

        y =
            v.y / dist
    in
    Vector x y


addTwoVectors v1 v2 =
    Vector (v1.x + v2.x) (v1.y + v2.y)

multiVectorByScalar v scalar = 
    Vector (v.x * scalar) (v.y * scalar)


map n  start1 stop1 start2 stop2 = 
    ((n-start1)/(stop1-start1))*(stop2-start2)+start2


calculateFitness v target (crashed, completed) width =
    let
        d = distance v target
        fitness  =  map d  0  width  width  0
        --gg = Debug.log "fitness: " fitness
    in
        if completed then
            ( fitness * 10)
        else if crashed then
            ( fitness / 10)
        else 
             fitness

