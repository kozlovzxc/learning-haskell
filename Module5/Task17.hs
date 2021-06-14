-- https://stepik.org/lesson/%D0%9C%D0%BE%D0%BD%D0%B0%D0%B4%D0%B0-Writer-8442/step/3

import Control.Monad.Writer

evalWriter :: Writer w a -> a
evalWriter m = fst (runWriter m)