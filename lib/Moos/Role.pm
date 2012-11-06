package Moos::Role;

use Moos ();
use Carp;
sub import {
	carp("Please 'use Moos-Role' instead");
	shift and unshift @_, qw( Moos -Role );
	goto \&Moos::import;
}

1;

=encoding utf8

=head1 Name

Moos::Role - Simple roles for Moos.

=head1 Synopsis

    package Milkable;
    use Moos-Role;        # note not "Moos::Role"
    has 'udders';
    sub milk { ... }
    
    package Cow;
    use Moos;
    with 'Milkable';

=head1 Description

C<Moos-Role> is a small wrapper around L<Role::Tiny> providing a few
additional L<Moos> features.

=head1 Features

=head2 strict / warnings

Turns on C<strict> and C<warnings> for you.

=head2 Helpful exports

The ever useful C<blessed> (from L<Scalar::Util>) and C<confess> (from
L<Carp>) are exported to your namespace.

=head2 has

Accessor generator. See L<Moos>.

=head2 Development Options

=over

=item PERL_MOOS_XXX

By setting the environment variable, Moos will export the L<XXX> debugging
keywords.

=back

=head1 See Also

=over

=item * L<Moos>

=item * L<Role::Tiny>

=back

=head1 Authors

Ingy döt Net <ingy@cpan.org>

Toby Inkster <tobyink@cpan.org>

=head1 Copyright and License

Copyright (c) 2012. Ingy döt Net.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
