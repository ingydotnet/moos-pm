package oos;

sub import {
    moos() if $0 eq '-';
    require Moos;
    splice @_, 0, 1, 'Moos';
    goto &Moos::import;
}

sub moos {
  print <<'EOEOMOO';
                              ______   ______
                             < Moo! > < !ooM >
                              ------   ------
                     ^__^   /                 \   ^__^
             _______/(öö)  /                   \  (oo)\_______
         /\/( ingy  /(__)                         (__)\  tsm  )\/\
            | w----||                              /  ||----w |
            ||     ||                                 ||     ||
EOEOMOO
  exit 0;
}

1;

=head1 Name

oos - It's a Cow I<Party>!

=head1 Synopsis

    perl -Moos

=head1 Description

Moo! Moo!

=head1 Authors

Ingy döt Net <ingy@cpan.org>

Toby Inkster <tobyink@cpan.org>

=head1 Copyright and License

Copyright (c) 2012. Ingy döt Net.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
