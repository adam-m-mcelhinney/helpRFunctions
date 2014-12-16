#helpRFunctions

## Objective: 
To provide a set of standardized helper functions to enable faster, cleaner and more modular R programming.

## Installation
This package may be installed using [devtools](http://cran.r-project.org/web/packages/devtools/index.html). 

```
install.packages('devtools') # Only needed if you dont have this installed.
library(devtools)
install_github('adam-m-mcelhinney/helpRFunctions')
library(helpRFunctions)
```
## Guiding Principles
These principles are an evolving document designed to provide guiding principles for this package:

1. Clean code is more important than optimized code. This package is not intended to be a bleeding-edge analysis tool for giant data sets. 
2. Provide the user with common sense defaults, but let them configure as much as possible if they chose. 
3. Roxygen is a great tool. Let's use it.
4. Unit test as much as possible.
5. Ensure all assumptions are explicitly checked. If these assumptions fail, then provide a very clear message to the user. 
6. Longer but more readable names are preferrable (within reason). Most of us are using an IDE or text editor with autocomplete, so longer variable names shouldn't require much more typing.
7. Jibberish names are explicitly forbidden.
8. Get rid of [global variables](http://c2.com/cgi/wiki?GlobalVariablesAreBad) whenever possible.
9. In non-obvious or potentially trickey situations, provide comments that describe why you are doing something. 
10. If in doubt whether to comment, comment.
11. More small functions are preferred to fewer but larger functions.

