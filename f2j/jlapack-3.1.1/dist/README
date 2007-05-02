JLAPACK 0.8 testing routines README
May 31, 2007

This directory should contain:
  README           - this file
  
  Test Scripts for Double Precision:
    test_blas1.bat   - windows batch file to test BLAS level 1
    test_blas2.bat   - windows batch file to test BLAS level 2
    test_blas3.bat   - windows batch file to test BLAS level 3
    test_eig.bat     - windows batch file to test LAPACK linear equation routines
    test_lin.bat     - windows batch file to test LAPACK eigenvalue routines
    test_blas1.sh    - unix shell script to test BLAS level 1
    test_blas2.sh    - unix shell script to test BLAS level 2
    test_blas3.sh    - unix shell script to test BLAS level 3
    test_eig.sh      - unix shell script to test LAPACK linear equation routines
    test_lin.sh      - unix shell script to test LAPACK eigenvalue routines

  Test Scripts for Single Precision:
    test_sblas1.bat  - windows batch file to test BLAS level 1
    test_sblas2.bat  - windows batch file to test BLAS level 2
    test_sblas3.bat  - windows batch file to test BLAS level 3
    test_seig.bat    - windows batch file to test LAPACK linear equation routines
    test_slin.bat    - windows batch file to test LAPACK eigenvalue routines
    test_sblas1.sh   - unix shell script to test BLAS level 1
    test_sblas2.sh   - unix shell script to test BLAS level 2
    test_sblas3.sh   - unix shell script to test BLAS level 3
    test_seig.sh     - unix shell script to test LAPACK linear equation routines
    test_slin.sh     - unix shell script to test LAPACK eigenvalue routines

  Jar Files for Double Precision:
    dblat1.jar       - BLAS level 1 testing code
    dblat2.jar       - BLAS level 2 testing code
    dblat3.jar       - BLAS level 3 testing code
    eigtest.jar      - LAPACK eigenvalue testing code
    lintest.jar      - LAPACK linear equation testing code
    matgen.jar       - support routines for the testers

  Jar Files for Single Precision:
    sblat1.jar       - BLAS level 1 testing code
    sblat2.jar       - BLAS level 2 testing code
    sblat3.jar       - BLAS level 3 testing code
    seigtest.jar     - LAPACK eigenvalue testing code
    slintest.jar     - LAPACK linear equation testing code
    smatgen.jar      - support routines for the testers

  Test Input Files:
    dblat2.in        - Double precision BLAS level 2 input file
    dblat3.in        - Double precision BLAS level 3 input file
    sblat2.in        - Single precision BLAS level 2 input file
    sblat3.in        - Single precision BLAS level 3 input file
    dtest.in         - Double precision linear equation input file
    stest.in         - Single precision linear equation input file
    dbak.in          - eigenvalue input file
    dbal.in          - eigenvalue input file
    dec.in           - eigenvalue input file
    ded.in           - eigenvalue input file
    dgbak.in         - eigenvalue input file
    dgbal.in         - eigenvalue input file
    dgg.in           - eigenvalue input file
    dsb.in           - eigenvalue input file
    dsg.in           - eigenvalue input file
    glm.in           - eigenvalue input file
    gqr.in           - eigenvalue input file
    gsv.in           - eigenvalue input file
    lse.in           - eigenvalue input file
    nep.in           - eigenvalue input file
    sbak.in          - eigenvalue input file
    sbal.in          - eigenvalue input file
    sbb.in           - eigenvalue input file
    sec.in           - eigenvalue input file
    sed.in           - eigenvalue input file
    sep.in           - eigenvalue input file
    sgbak.in         - eigenvalue input file
    sgbal.in         - eigenvalue input file
    sgd.in           - eigenvalue input file
    sgg.in           - eigenvalue input file
    ssb.in           - eigenvalue input file
    ssg.in           - eigenvalue input file
    svd.in           - eigenvalue input file

To run the tests, simply execute the appropriate script, which
depends on the operating system you are running.

Some of the output is not exactly as the Fortran versions
would be (e.g. some arrays are printed as "NULL", but that is
only a limitation of the f2j I/O handling), however this
does not affect the running of the tests.  As long as the
test results say "All tests passed the threshold", then things
are fine (see the note below, however).

Test results
------------

We tested JLAPACK on the following platforms:

-Solaris 9 (sparc), Java version:

   java version "1.5.0_02"
   Java(TM) 2 Runtime Environment, Standard Edition (build 1.5.0_02-b09)
   Java HotSpot(TM) Client VM (build 1.5.0_02-b09, mixed mode, sharing)

-Solaris 9 (x86), Java version:

   java version "1.4.2_05"
   Java(TM) 2 Runtime Environment, Standard Edition (build 1.4.2_05-b04)
   Java HotSpot(TM) Client VM (build 1.4.2_05-b04, mixed mode)

-Linux x86 (Debian 3.1), Java version:

   java version "1.6.0"
   Java(TM) SE Runtime Environment (build 1.6.0-b105)
   Java HotSpot(TM) Client VM (build 1.6.0-b105, mixed mode, sharing)

-Linux x86 (Fedora Core 4), Java version:

   java version "1.4.2"
   gij (GNU libgcj) version 4.0.0 20050519 (Red Hat 4.0.0-8)

-Mac OS X 10.4.7 (ppc), Java version:

   java version "1.5.0_06"
   Java(TM) 2 Runtime Environment, Standard Edition (build 1.5.0_06-112)
   Java HotSpot(TM) Client VM (build 1.5.0_06-64, mixed mode, sharing)

-Win2000 x86 / Sun JDK 1.6

Note: in single precision, the eigenvalue testers will report
some failures, but they match the failures observed during execution 
of the native Fortran code.  For more details, see:

  http://www.netlib.org/lapack/faq.html#1.23
