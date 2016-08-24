Contains code to interface between standard MITgcm and GEOS-5/MAPL world.

* `mitgcm_special_bits.tar` This contains a custom `build/Makefile` that is a
   mix of MITgcm and GEOS-5 pieces. It also contains a custom inc/ directory
   that is a copy of many MITgcm header files, with some comment "C" markers
   changes to "!" instead - to allow for fixed form and free form compile use
   (we think).

* `code/` This is a directory of standard MITgcm code/ customization for a specific
    experiment (global_ocean_c32 in this case).

* `code_split_driver/` This is core F90 code that connects MITgcm data structutures and
    MAPL data structures. The code is in a tree of files but Makefile dependencies are 
    currently for a flat set of files in the `code_split_driver/` directory. There
    is an example of copying things into this form in 
    `https://github.com/christophernhill/gmao_mitgcm_couplng/blob/master/chris_notes/dimitris_notes.md`
    e.g.

```
$ find driver/ utils/ state/ -type f | awk '{print "cp "$1" ."}'
```
