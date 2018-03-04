[![Build Status](https://travis-ci.org/schmidtchristoph/bibDelete.svg?branch=master)](https://travis-ci.org/schmidtchristoph/bibDelete) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/schmidtchristoph/bibDelete?branch=master&svg=true)](https://ci.appveyor.com/project/schmidtchristoph/bibDelete)

# bibDelete:
## Delete selected BibTeX field types
##### version 0.1.0

The bibDelete package allows to easily delete a specified BibTeX field type from all bib entries in a BibTeX file.

This can be useful, for example, in scientific research, where a reference management software like [Papers](https://www.readcube.com/papers/) might add ```annote``` fields containing personal notes on read research literature, or notes about ideas for future research to exported BibTeX libraries. Such .bib files can be cleaned before being shared with coworkers using the ```deleteField()``` function that automatically removes all occurrences of a given nonessential field type.

- - -

##### How to install this package from GitHub

There are several ways of installing the package, e.g.:

- install the "devtools" package first, then use
```devtools::install_github("schmidtchristoph/bibDelete")```

- install the "devtools" package first, clone the repository, then use
```devtools::install("path/to/repository/bibDelete")```

- - - 

This package passes ```devtools::check()``` with zero problems, notes or warnings on my machine running R 3.4.3 on macOS 10.13.3.

- - - 
The [MIT License (MIT)](http://opensource.org/licenses/MIT)
Copyright (c) 2018 Christoph Schmidt