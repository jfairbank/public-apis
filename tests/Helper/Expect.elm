module Helper.Expect exposing (expectErr)

import Expect exposing (Expectation)


expectErr : String -> Result x a -> Expectation
expectErr errorMessage result =
    case result of
        Ok _ ->
            Expect.fail errorMessage

        Err _ ->
            Expect.pass
