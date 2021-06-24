#  Version 3.0.1

- Added author J. R. Méndez-Reboredo.

#  Version 3.0.0

## New features

- Added not-re-execution handler (N-ER) of pipelines to guarantee that only new both input data and pipelines are executed.
- Fuly compatible with both serial and parallel paradigm.
- The log system has been completely restructured to support both file and console logging. 
- Supported the visualization of the state of the instances between pipes and a final summary of the execution.
- A sample has been provided to show the compatibility with different types like image data.
- Updated the documentation version roxygen2 from 6.1.1 to 7.1.1.
- Removed dependencies with several packages: readr, pipeR, textutils, magrittr and purrr.

## Bug fixes
- Fixed the path of the example files.
- Fixed the quote of special characters when launching the Python script.
- Fixed the bug related to not use cat function to save the tweet info.

#  Version 2.0.0

- Object-based operator has been fully redesigned in order to reduce verbosity and increase functionality.
- Allowed to use a set of folders or files as input for pre-processing.
- Configuration options handler have been remodeled.
- Improved the extractor registration process.
- Added new functionality to dynamically create pipelines.
- Some functions and classes have been renamed to increase readability.
  
#  Version 1.0.1

- Improvements on the structure of the tests. Now taken into account the suggested packages
- Fixed issues caused by archiving the rtweet package

#  Version 1.0.0

- Implement the functionality of the package

