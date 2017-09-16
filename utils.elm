module Utils exposing (..)


mapIf : (a -> Bool) -> (a -> a) -> List a -> List a
mapIf pred map =
    List.map
        (\e ->
            if pred e then
                map e
            else
                e
        )
