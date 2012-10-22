##
# name:      Moos
# abstract:  Moo s{imple,peedy,ingle}
# author:    Ingy döt Net <ingy@cpan.org>
# license:   perl
# copyright: 2012
# see:
# - M
# - Mo
# - Moo
# - Moose
# - Mouse
# - Mousse

use strict;
use warnings;

package Moos;

our $VERSION = '0.03';

sub import {
    my $caller = caller;
    no strict 'refs';
    no warnings 'once';
    strict->import;
    warnings->import;
    ${"$caller\::__meta"} = { has => [] };
    *{"$caller\::has"} = sub {
        my ($name, %args) = @_;
        my $meta = ${"$caller\::__meta"};
        push @{$meta->{has}}, [$name, \%args];
        my ($builder, $default) = @args{qw(builder default)};

        my $method =
            $builder ? sub {
                $#_ ? $_[0]{$name} = $_[1] :
                exists($_[0]{$name}) ? $_[0]{$name} :
                ($_[0]{$name} = $_[0]->$builder);
            } :
            $default ? sub {
                $#_ ? $_[0]{$name} = $_[1] :
                exists($_[0]{$name}) ?  $_[0]{$name} :
                ($_[0]{$name} = $default->(@_));
            } :
            sub {
                $#_ ? $_[0]{$name} = $_[1] :
                $_[0]{$name};
            };

        $method = trace_accessor_calls($name, $method)
            if $ENV{PERL_MOOS_ACCESSOR_CALLS};

        *{"$caller\::$name"} = $method;
    };
    *{"$caller\::extends"} = sub {
        eval "require $_[0]";
        @{"$caller\::ISA"} = $_[0];
    },
    @{"$caller\::ISA"} = ('Moos::Object');

    export_xxx($caller) if $ENV{PERL_MOOS_XXX};
}

sub export_xxx {
    my ($caller) = @_;
    eval "use XXX -with => 'YAML::XS'; 1" or die $@;
    no strict 'refs';
    *{"$caller\::WWW"} = \&{__PACKAGE__ . '::WWW'};
    *{"$caller\::XXX"} = \&{__PACKAGE__ . '::XXX'};
    *{"$caller\::YYY"} = \&{__PACKAGE__ . '::YYY'};
    *{"$caller\::ZZZ"} = \&{__PACKAGE__ . '::ZZZ'};
}

my $trace_exclude = +{
    map {($_, 1)} (
#         'Some::Module some_accessor',
#         'Some::Module some_other_accessor',
    )
};
sub trace_accessor_calls {
    require Time::HiRes;
    my ($name, $accessor) = @_;
    sub {
        my ($pkg, $file, $line, $sub) = caller(0);
        unless ($trace_exclude->{"$pkg $name"}) {
            warn "$pkg $name $line\n";
#             Time::HiRes::usleep(100000);
        }
        goto &$accessor;
    };
}

package Moos::Object;

sub new {
    my $self = bless {@_[1..$#_]}, $_[0];
    no strict 'refs';
    for my $class (@{[@{"$_[0]::ISA"}, $_[0]]}) {
        if (defined &{"$class\::BUILD"}) {
            &{"$class\::BUILD"}($self);
        }
    }
    return $self;
}

sub BUILD {
    no strict 'refs';
    my $meta = ${ref($_[0]) . "::__meta"};
    for my $has (@{$meta->{has}}) {
        my ($name, $args) = @$has;
        next if exists $_[0]->{$name} || $args->{lazy};
        if (my $default = $args->{default}) {
            $_[0]->{$name} = $default->($_[0]);
        }
        elsif (my $builder = $args->{builder}) {
            $_[0]->{$name} = $_[0]->$builder();
        }
    }
    return;
}

1;

=head1 SYNOPSIS

    package Foos;
    use Moos;

    extends 'Boos';

    has this => ();
    has that => (default => sub { 42 });
    has other => (
        builder => 'build_other',
        lazy => 1,
    );

    sub BUILD {
        my $self = shift;
        # build, build, build
    }

=head1 DESCRIPTION

Moos completes the M to Moose sequence of Perl OO modules.

This one is pure Perl, no dependencies, single file and Moose compatible (for
what it does). It is fairly minimal; it supports the features shown in the
L<SYNOPSIS>.

=head1 FEATURES

Here's a quick list of the L<Moose> compatible features that are supported by
L<Moos>:

=over

=item extends

For inheritance. C<Moos::Object> is the default base class.

    package MyClass;
    extends 'MyBaseClass';

=item new

A constructor class method.

    my $object = MyClass->new(this => 'nice', that => 2);

=item BUILD

Custom object construction. If you define BUILD, it is passed the value of the
new object during construction. You can modify the object. Any value you
return is ignored.

    sub BUILD { my $self = shift; ... }

=item has

Accessor generator. Supports the C<default>, C<build> and C<lazy> options,
described below.

    has this => ();

=item default

Specify the sub to generate a default value.

    has this => ( default => sub { 42 } );

=item builder

Specify the method name to generate a default value.

    has this => ( builder => 'build_this' );

=item lazy

Don't generate defaults during object construction.

    has this => ( builder => 'build_this', lazy => 1 );

=back

Note that currently all accessors are read-write, and the C<is> keyword is
silently ignored (as are all other unknown keywords).

=head1 DEV OPTIONS

Moos has a couple builtin dev options. They are controlled by environment
variables.

=over

=item PERL_MOOS_ACCESSOR_CALLS

By setting this environment variable, Moos will warn everytime an accessor
method is called.

=item PERL_MOOS_XXX

By setting the environment variable, Moos will export the L<XXX> debugging
keywords.

=back

=head1 WHENCE

I(ngy) created Moos during L<Pegex> development. Pegex uses a clone of Moos
called L<Pegex::Base>.

Pegex is a parser framework and needs to be fast. While looking into speed
issues I noted that accessor calling was the biggest hit. I tried all the
various Mo* solutions and L<Mouse> was the fastest.

I was happy until I remembered that Mouse uses XS, and for various reasons
this broke my toolchain (TestML, Module::Install, etc).

I tried to inline L<Moo> into one file but failed, and ended up with this.
I've shared Pegex::Base as L<Moos> in case any other projects want it.

=head1 ON SPEED

In the end, I got Pegex to run even faster with Moos than it originally did
with Mouse. I'll tell you my secret...

B<<Accessors I<(usually)> do not need to be method calls.>>

Replace these:

    my $foo = $self->foo;
    $self->foo($foo);

with:

    my $foo = $self->{foo};
    $self->{foo} = $foo;

And your code will be faster (and a bit uglier).

The only time that you need to call an accessor method is when you are reading
a property and it might invoke a C<lazy> C<builder> or C<default> method.
Otherwise you are just wasting time. At least with the minimal feature set
offered by Moos.

The PERL_MOOS_ACCESSOR_CALLS feature described above is for finding these
method calls.

Note that users of your module's accessor methods can still use the method
calls like they would expect to.

I'm sure I've missed some subtlties, and would be glad to hear opinions, but
in the meantime I'm happy that my code is faster and pure Perl.
