module Test exposing (..)


type alias User =
    { name : String, age : Int }


user =
    User


aUser =
    User "A"


a42User =
    User "A" 42


indirectUserCall0 =
    identity User "A" 42


indirectUserCall1 =
    User |> (\usr -> usr "A" 42)


pipeLeftUserCall =
    (User <| "A") <| 42


pipeRightUserCall =
    42 |> ("A" |> User)
