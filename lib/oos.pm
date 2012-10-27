package oos;

sub import {
    moos(), exit if $0 eq '-';
    require Moos;
    splice @_, 0, 1, 'Moos';
    goto &Moos::import;
}

sub moos {
  print <<'EOMOO';
 ______                      ______
< Moo! >                    < Moo! >
 ------                      ------
        \   ^__^                    \   ^__^
         \  (oo)\_______             \  (oo)\_______
            (__)\       )\/\            (__)\       )\/\
                ||----w |                   ||----w |
                ||     ||                   ||     ||
EOMOO
  exit 0;
}

1;

