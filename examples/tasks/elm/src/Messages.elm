module Messages exposing (..)


type alias Task =
    { id : Int
    , title : String
    , completed : Bool
    }


type ToBackend
    = AddTask String
    | ToggleTask Int
    | DeleteTask Int
    | RequestTasks


type ToFrontend
    = TaskList (List Task)
    | TaskAdded Task
    | TaskToggled Int Bool
    | TaskDeleted Int
