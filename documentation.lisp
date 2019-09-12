#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.uax-9)

(docs:define-docs
  (variable *bidi-class-database-file*
    "Variable containing the absolute path of the bidi class database file.

See LOAD-DATABASES
See COMPILE-DATABASES")
  
  (variable *bidi-brackets-table-file*
    "Variable containing the absolute path of the brackets table file.

See LOAD-DATABASES
See COMPILE-DATABASES")
  
  (variable *bidi-mirroring-table-file*
    "Variable containing the absolute path of the mirroring table file.

See LOAD-DATABASES
See COMPILE-DATABASES")
  
  (type no-database-files
    "Warning signalled when LOAD-DATABASES is called and the files are not present.

Two restarts must be active when this condition is signalled:

  COMPILE --- Call COMPILE-DATABASES
  ABORT   --- Abort loading the databases, leaving them at their
              previous state.

See LOAD-DATABASES")
  
  (function load-databases
    "Loads the databases from their files into memory.

If one of the files is missing, a warning of type NO-DATABASE-FILES is
signalled. If the loading succeeds, T is returned.

See *LINE-BREAK-DATABASE-FILE*
See *BIDI-BRACKETS-TABLE-FILE*
See *BIDI-CLASS-DATABASE-FILE*
See NO-DATABASE-FILES")
  
  (function compile-databases
    "Compiles the database files from their sources.

This will load an optional part of the system and compile the database
files to an efficient byte representation. If the compilation is
successful, LOAD-DATABASES is called automatically.

See *LINE-BREAK-DATABASE-FILE*
See *BIDI-BRACKETS-TABLE-FILE*
See *BIDI-CLASS-DATABASE-FILE*
See LOAD-DATABASES")
  
  (function levels
    "Computes the directional level for every code point in the string.

Returns two values:

  LEVELS         --- A vector of levels for each code point in the
                     input string. Has the length of the input
                     string.
  BASE-DIRECTION --- Returns the base direction of the string. If
                     BASE-DIRECTION was not :AUTO, this is the
                     determined direction.

BASE-DIRECTION must be one of three values:

  :LEFT-TO-RIGHT
  :RIGHT-TO-LEFT
  :AUTO (default)

This designates how the text is interpreted at its base level. When
this level is :AUTO, the base level is determined automatically by
scanning for the first directional code point in the string.

LINE-BREAKS should be a list of indexes into the string. Each index
designates a code point after which a line break is inserted. This is
used to normalise the levels across breaks. If you pass this argument,
you must pass the same argument to REORDER. If you do not pass this,
the line end is assumed to be at the end of the string.

See REORDER")
  
  (function reorder
    "Computes a reordering of indexes into the string to process the code points in the correct order.

Returns one value, the reordered index vector, the same length as the
input LEVELS vector. The vector should be filled with indices into the
original string. Iterating through this index vector in order should
provide the correct ordering for the resulting code points when
rendering along the base direction.

LINE-BREAKS should be a list of indexes into the string. Each index
designates a code point after which a line break is inserted. This
argument must be the same as what you passed to LEVELS to get the
levels vector.

INDEXES is the index vector that's permuted and returned. You can pass
this to save on allocation. If not passed, a vector the length of the
levels vector is created. If passed, you should make sure that the
indices in the vector make sense -- meaning they should typically be
in ascending order starting with 0.

See LEVELS"))
