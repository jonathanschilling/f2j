JLAPACK timing routines README
May 31, 2007

----

NOTE: the timing routines weren't included in LAPACK 3.1.  These are the timing
  routines from 3.0.  I'm not sure if they work with the 3.1 library, but I'm
  keeping the files here as placeholders to drop in the timing code if it becomes
  available.

----

This directory should contain:
  README           - this file
  
  Timing Scripts for Double Precision:
    time_eig_small.sh   - Timer for LAPACK linear equation routines (small matrix sizes)
    time_eig_small.bat  - Windows version of the previous script
    time_eig_large.sh   - Timer for LAPACK linear equation routines (large matrix sizes)
    time_eig_large.bat  - Windows version of the previous script
    time_lin_small.sh   - Timer for LAPACK eigenvalue routines (small matrix sizes)
    time_lin_small.bat  - Windows version of the previous script
    time_lin_large.sh   - Timer for LAPACK eigenvalue routines (large matrix sizes)
    time_lin_large.bat  - Windows version of the previous script

  Timing Scripts for Single Precision:
    time_seig_small.sh  - Timer for LAPACK linear equation routines (small matrix sizes)
    time_seig_small.bat - Windows version of the previous script
    time_seig_large.sh  - Timer for LAPACK linear equation routines (large matrix sizes)
    time_seig_large.bat - Windows version of the previous script
    time_slin_small.sh  - Timer for LAPACK eigenvalue routines (small matrix sizes)
    time_slin_small.bat - Windows version of the previous script
    time_slin_large.sh  - Timer for LAPACK eigenvalue routines (large matrix sizes)
    time_slin_large.bat - Windows version of the previous script

  Jar Files for Double Precision:
    eigtime.jar      - LAPACK eigenvalue timing code
    lintime.jar      - LAPACK linear equation timing code
    matgen.jar       - support routines for the timers

  Jar Files for Single Precision:
    seigtime.jar     - LAPACK eigenvalue timing code
    slintime.jar     - LAPACK linear equation timing code
    smatgen.jar      - support routines for the timers

  Double Precision Linear Equation Timer Input Files (small sizes):
    dband.in
    dblasa.in
    dblasb.in
    dblasc.in
    dtime.in
    dtime2.in

  Single Precision Linear Equation Timer Input Files (small sizes):
    sband.in
    sblasa.in
    sblasb.in
    sblasc.in
    stime.in
    stime2.in

  Double Precision Eigenvalue Timer Input Files (small sizes):
    dgeptim.in
    dseptim.in
    dneptim.in
    dsvdtim.in

  Single Precision Eigenvalue Timer Input Files (small sizes):
    sgeptim.in
    sseptim.in
    sneptim.in
    ssvdtim.in

  The following input files for large matrix sizes are located in the
  subdirectory named "input_files_large":

  Double Precision Linear Equation Timer Input Files (large sizes):
    DBAND.in
    DBLASB.in
    DTIME.in
    DBLASA.in
    DBLASC.in
    DTIME2.in

  Single Precision Linear Equation Timer Input Files (large sizes):
    SBAND.in
    SBLASA.in
    SBLASB.in
    SBLASC.in
    STIME.in
    STIME2.in

  Double Precision Eigenvalue Timer Input Files (large sizes):
    DGEPTIM.in
    DSEPTIM.in
    DNEPTIM.in
    DSVDTIM.in

  Single Precision Eigenvalue Timer Input Files (large sizes):
    SGEPTIM.in
    SSEPTIM.in
    SNEPTIM.in
    SSVDTIM.in


To run the timers, simply execute the appropriate script, which
depends on the operating system you are running.

Some of the output is not exactly as the Fortran versions
would be (e.g. some arrays are printed as "NULL", but that is
only a limitation of the f2j I/O handling), however this
does not affect the running of the timers.
