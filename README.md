MachineLearning in Haskell
==========================

This is a library implementing some basic machine learning algorithms.


## Hopfield Networks ##

To run the demonstration, run
`MachineLearning/HopfieldDemonstration.hs`.  It demonstrates the
training of a network on an `O` and an `X`, and shows the network
reconstructing the trained patterns from a perturbed version.

    ~/Code/haskell/machinelearning $ runhaskell MachineLearning/HopfieldDemonstration.hs
    Training patterns
    --------
    |X    X|
    | X  X |
    |  XX  |
    |  XX  |
    |  XX  |
    | X  X |
    |X    X|
    --------
    --------
    |XXXXXX|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |XXXXXX|
    --------
    Validation
    ("Corruption error",6.3245554)
    ("Reproduction error",0.0)
    "Original"
    --------
    |X    X|
    | X  X |
    |  XX  |
    |  XX  |
    |  XX  |
    | X  X |
    |X    X|
    --------
    "Corrupted"
    --------
    |X  X X|
    |    X |
    |X XXXX|
    |  XX  |
    |  XXX |
    | XX X |
    |X XXXX|
    --------
    "Reproduction"
    --------
    |X    X|
    | X  X |
    |  XX  |
    |  XX  |
    |  XX  |
    | X  X |
    |X    X|
    --------
    ("Corruption error",6.6332498)
    ("Reproduction error",0.0)
    "Original"
    --------
    |XXXXXX|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |XXXXXX|
    --------
    "Corrupted"
    --------
    |XXXXXX|
    |     X|
    |     X|
    | X X X|
    |X  X  |
    |  XX X|
    |X XXXX|
    --------
    "Reproduction"
    --------
    |XXXXXX|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |XXXXXX|
    --------
