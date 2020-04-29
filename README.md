# Fortran-to-Java Source Code

Version 0.8.1
June 30, 2008

The old [Fortran to Java CVS repository](https://sourceforge.net/projects/f2j/) was migrated to this Git repository:

```bash
rsync -ai a.cvs.sourceforge.net::cvsroot/f2j/ cvs2git-f2j
cd cvs2git-f2j
cvs2git --blobfile=blob.dat --dumpfile=dump.dat \
    --username=keithseymour --default-eol=native \
    --encoding=utf8 --encoding=latin1 --fallback-encoding=ascii \
    .
cd ..
mkdir f2j
cd f2j
git init
cat ../cvs2git-f2j/blob.dat ../cvs2git-f2j/dump.dat | git fast-import
git remote add origin git@github.com:jonathanschilling/f2j.git
git checkout
git push origin --mirror
mv f2j/* .
rm -r CVSROOT
git add .
git commit -m "one folder less"
git branch --set-upstream-to=origin/master master
git pull
git push
```

---

Before using the f2j source code, realize that f2j was originally geared
to a very specific problem - that is, translating the LAPACK and BLAS numerical
libraries.  f2j does not and probably never will handle all Fortran code.

# Building the code

```bash
./configure
make
```

and optionally:

```bash
make install
```

For more details, see the [f2j manual](https://github.com/jonathanschilling/f2j/blob/master/doc/f2j_ug.pdf).
