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
# - Moos
# - Moose
# - Mouse
# - Mousse

# The entire implementation of Moos (and all its related classes) are defined
# inside this one file.
use strict;
use warnings;

package Moos;
use v5.10.0;
use mro;

our $VERSION = '0.04';

sub import {
    # Get name of the "class" from whence "use Moos;"
    my $package = caller;

    # Turn on strict/warnings for caller
    strict->import;
    warnings->import;

    # Create/register a metaclass object for the package
    my $meta = Moos::Meta::Class->initialize($package);

    # Make calling class inherit from Moos::Object by default
    extends($meta, 'Moos::Object');

    # Export the 'has' and 'extends' helper functions
    _export($package, has => \&has, $meta);
    _export($package, extends => \&extends, $meta);

    # Possibly export some handy debugging stuff
    _export_xxx($package) if $ENV{PERL_MOOS_XXX};
}

# Attribute generator
sub has {
    my ($meta, $name, %args) = @_;

    # Add attribute to meta class object
    $meta->add_attribute($name => %args);

    # Make a Setter/Getter accessor
    my ($builder, $default) = @args{qw(builder default)};
    my $accessor =
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

    # Dev debug thing to trace calls to accessor subs.
    $accessor = _trace_accessor_calls($name, $accessor)
        if $ENV{PERL_MOOS_ACCESSOR_CALLS};

    # Export the accessor.
    _export($meta->{package}, $name, $accessor);
}

# Inheritance maker
sub extends {
    my ($meta, $parent) = @_;
    eval "require $parent";
    no strict 'refs';
    @{"$meta->{package}\::ISA"} = ($parent);
}

# Use this for exports and meta-exports
sub _export {
    my ($package, $name, $code, $meta) = @_;
    if (defined $meta) {
        my $orig = $code;
        $code = sub {
            unshift @_, $meta;
            goto &$orig;
        };
    }
    no strict 'refs';
    *{"$package\::$name"} = $code;
}

# Export the 4 debugging subs from XXX.pm
sub _export_xxx {
    my ($package) = @_;
    eval "use XXX -with => 'YAML::XS'; 1" or die $@;
    no strict 'refs';
    _export($package, WWW => \&{__PACKAGE__ . '::WWW'});
    _export($package, XXX => \&{__PACKAGE__ . '::XXX'});
    _export($package, YYY => \&{__PACKAGE__ . '::YYY'});
    _export($package, ZZZ => \&{__PACKAGE__ . '::ZZZ'});
}

# A tracing wrapper for debugging accessors
my $trace_exclude = +{
    map {($_, 1)} (
        'Some::Module some_accessor',
        'Some::Module some_other_accessor',
    )
};
sub _trace_accessor_calls {
    require Time::HiRes;
    my ($name, $accessor) = @_;
    sub {
        my ($pkg, $file, $line, $sub) = caller(0);
        unless ($trace_exclude->{"$pkg $name"}) {
            warn "$pkg $name $line\n";
            Time::HiRes::usleep(100000);
        }
        goto &$accessor;
    };
}

# The remainder of this module was heavily inspired by Moose, and tried to do
# what Moose does, only much less.
package Moos::Meta::Class;

my $meta_class_objects = {};

sub name { $_[0]->{package} }

sub initialize {
    my ($class, $package) = @_;

    return $meta_class_objects->{$package} //= do {
        bless {
            package => $package,
            attributes => {},
            _attributes => [],
        }, $class;
    };
}

# Make a new attrribute object and add it to both a hash and an array, so that
# we can preserve the order defined.
sub add_attribute {
    my $self = shift;
    my ($name, %args) = @_;
    push @{$self->{_attributes}}, (
        $self->{attributes}{$name} =
            bless {
                name => $name,
                %args,
            }, 'Moos::Meta::Attribute'
    );
}

sub new_object {
    my ($self, $params) = @_;
    my $object = $self->_construct_instance($params);
    $object->BUILDALL($params) if $object->can('BUILDALL');
    return $object;
}

sub _construct_instance {
    my ($self, $params) = @_;
    my $instance = bless {}, $self->name;
    foreach my $attr ($self->get_all_attributes()) {
        my $name = $attr->{name};
        next if exists $instance->{$name};
        if (exists $params->{$name}) {
            $instance->{$name} = $params->{$name};
            next;
        }
        if (not $attr->{lazy}) {
            if (my $builder = $attr->{builder}) {
                $instance->{$name} = $instance->$builder();
                next;
            }
            elsif (my $default = $attr->{default}) {
                $instance->{$name} = $default->($instance);
            }
        }
    }
    return $instance;
}

sub get_all_attributes {
    my $self = shift;
    my (@attrs, %attrs);
    for my $package (@{mro::get_linear_isa($self->name)}) {
        my $meta = Moos::Meta::Class->initialize($package);
        for my $attr (@{$meta->{_attributes}}) {
            my $name = $attr->{name};
            next if $attrs{$name};
            push @attrs, ($attrs{$name} = $attr);
        }
    }
    return @attrs;
}

package Moos::Object;

sub new {
    my $class = shift;
    my $real_class = ref($class) || $class;
    my $params = $real_class->BUILDARGS(@_);
    return Moos::Meta::Class->initialize($real_class)->new_object($params);
}

sub BUILDARGS {
    return {@_[1..$#_]};
}

sub BUILDALL {
    return unless $_[0]->can('BUILD');
    my ($self, $params) = @_;
    for my $package (reverse @{mro::get_linear_isa(ref($self))}) {
        no strict 'refs';
        if (defined &{"$package\::BUILD"}) {
            &{"$package\::BUILD"}($self, $params);
        }
    }
}

sub dump {
    no warnings 'once';
    my $self = shift;
    require Data::Dumper;
    local $Data::Dumper::Maxdepth = shift if @_;
    Data::Dumper::Dumper $self;
}

sub meta {
    Moos::MOP::Class->initialize(ref($_[0]) || $_[0]);
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
