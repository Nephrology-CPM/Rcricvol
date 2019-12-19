# Rcricvol
CRIC Data Processing Package

# building, checking, and testing the R package

To building the package from the command line first, navigate to the directory
containing the `crpmcric` folder.
```
R CMD build crpmcric
```
This will return a tar balled package named something like `crpmcirc.tar.gz`.
You can unpack this package anywhere you want on your computer and perform the
check to see how what someone else will see when they check the package
```
mv crpmcric_0.1.0.tar.gz ~/some/where/else
cd ~/some/where/else
tar xvzf crpmcric_0.1.0.tar.gz
R CMD check crpmcric
```
You can, of course, check your development package with the
`R CMD check crpmcric` command too.
