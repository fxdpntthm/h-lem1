# h-lem1
LEM1 algorithm in haskell


## Installation Directions

#### untar the file archive  
- `tar xvf a553i967_EECS837_project.tar.gz`  


#### Execute the following commands

- `cd h-lem1`  
- `cabal sandbox init`  
- `cabal install cabal-install`  
- `cabal configure`  
- `cabal install --dependencies-only`  
- `cabal install`  
- `cabal run`  

#### Sample flow below
```
h-lem1 (master*) $ cabal run
Resolving dependencies...
Configuring h-lem1-0.1.0.0...
Preprocessing executable 'h-lem1' for h-lem1-0.1.0.0..
Building executable 'h-lem1' for h-lem1-0.1.0.0..
[1 of 7] Compiling LEM1.RoughSet    ( src/LEM1/RoughSet.hs, dist/build/h-lem1/h-lem1-tmp/LEM1/RoughSet.o )
[2 of 7] Compiling Model.Util       ( src/Model/Util.hs, dist/build/h-lem1/h-lem1-tmp/Model/Util.o )
[3 of 7] Compiling Model.DataSet    ( src/Model/DataSet.hs, dist/build/h-lem1/h-lem1-tmp/Model/DataSet.o )
[4 of 7] Compiling Model.Rules      ( src/Model/Rules.hs, dist/build/h-lem1/h-lem1-tmp/Model/Rules.o )
[5 of 7] Compiling LERS.Parser      ( src/LERS/Parser.hs, dist/build/h-lem1/h-lem1-tmp/LERS/Parser.o )
[6 of 7] Compiling LEM1.Algorithm   ( src/LEM1/Algorithm.hs, dist/build/h-lem1/h-lem1-tmp/LEM1/Algorithm.o )
[7 of 7] Compiling Main             ( src/Main.hs, dist/build/h-lem1/h-lem1-tmp/Main.o )
Linking dist/build/h-lem1/h-lem1 ...
Running h-lem1...
Please enter file path of dataset:
test/data/hw.txt
Data set is consistent
writing certain rules to file: test/data/hw.txt.rules.certain
```


#### To enable and execute some sanity tests
- `cabal configure --enable-tests`  
- `cabal install --enable-tests`   
- `cabal test`
