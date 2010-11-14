      program test16
      integer j
*
      WRITE( NOUNIT, FMT = 9993 )'orthogonal', '''',
     $  'transpose', ( '''', J = 1, 10 )
*
 9993 FORMAT( / ' Tests performed:   (H is Hessenberg, S is Schur, B, ',
     $      'T, P are triangular,', / 20X, 'U, V, Q, and Z are ', A,
     $      ', l and r are the', / 20X,
     $      'appropriate left and right eigenvectors, resp., a is',
     $      / 20X, 'alpha, b is beta, and ', A, ' means ', A, '.)',
     $      / ' 1 = | A - U H V', A,
     $      ' | / ( |A| n ulp )      2 = | B - U T V', A,
     $      ' | / ( |B| n ulp )', / ' 3 = | I - UU', A,
     $      ' | / ( n ulp )             4 = | I - VV', A,
     $      ' | / ( n ulp )', / ' 5 = | H - Q S Z', A,
     $      ' | / ( |H| n ulp )', 6X, '6 = | T - Q P Z', A,
     $      ' | / ( |T| n ulp )', / ' 7 = | I - QQ', A,
     $      ' | / ( n ulp )             8 = | I - ZZ', A,
     $      ' | / ( n ulp )', / ' 9 = max | ( b S - a P )', A,
     $      ' l | / const.  10 = max | ( b H - a T )', A,
     $      ' l | / const.', /
     $      ' 11= max | ( b S - a P ) r | / const.   12 = max | ( b H',
     $      ' - a T ) r | / const.', / 1X )
      end
