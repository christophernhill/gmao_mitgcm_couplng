Simple directory of miscellaneous code fragements.

* '''mkplot.m'''
  - simple Matlab for plotting inline binary I/O written stuff 
  
* '''genmake2'''
  - version of genmake2 that adds "SPECIAL_FILES" target and $(ROOT_DIR) as
    a variable that can be easily changed
    
* '''prep_header_4f90.sh'''
  - small script that pipes F77 .h with 'C' prefix comments to F90 compatible form
    with '!' comment prefix. Needed for a few .h files that are used in both experiment
    configuration and in glue code with MAPL driver.
