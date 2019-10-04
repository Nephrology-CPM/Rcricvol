# Rcricvol
CRIC Data Processing Functions

# building, checking, and testing the R package

To building the package from the command line first, navigate to the directory
containing this package.
```
R CMD build Rcricvol
```
This will return a tar balled package named something like `mypackage.tar.gz`.
You can unpack this package anywhere you want on you computer and perform the
check to see how what someone else will see when they check the package
```
mv Rcricvol_0.1.0.tar.gz ~/some/where/else
cd ~/some/where/else
tar xvzf Rcricvol_0.1.0.tar.gz
R CMD check Rcricvol
```
You can, of course, check your development package with the
`R CMD check Rcricvol` command too.
