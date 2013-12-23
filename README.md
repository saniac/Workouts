Workouts
========

Some Haskell code to calculate weights for a series of workouts, tailored to the actual free weights available.

Goals are:

- a command line app for calculating Wendler 5 3 1 workouts
- ditto for any arbitrary linear progression
- a module of functions for a Yesod application

Compile with

    ghc -main-is Workouts workout.hs

Link the resulting "workout" executable to the name "wendler"

    ln -s workout wendler
