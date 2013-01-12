import Layout
import Logic
import Structure

main :: IO ()
main = initTetrisLayout >>== initFieldData >>== registerSingals >> runTetris
     