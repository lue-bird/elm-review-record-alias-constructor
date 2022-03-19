module NoRecordAliasConstructor.Common exposing (errorInfo)


errorInfo : { message : String, details : List String }
errorInfo =
    { message = "Record alias constructor used"
    , details =
        [ "Construct this record by specifying the field and values instead."
        , "This makes your code easier to understand and read."
        ]
    }
