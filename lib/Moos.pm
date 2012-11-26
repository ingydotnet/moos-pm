# The entire implementation of Moos (and all its related classes)
# are defined inside this one file.
use strict;
use warnings;
use 5.008;

my $VALID_NAME = qr{ ^ [^\W0-9] \w* $ }ix;

package Moos;

our $VERSION = '0.14';

use Scalar::Util;
use Carp qw(confess);

if ($] >= 5.010) {
    require mro;
}
else {
    require MRO::Compat;
}

our $CAN_HAZ_XS =
    !$ENV{PERL_MOOS_XS_DISABLE} &&
    eval{ require Class::XSAccessor; Class::XSAccessor->VERSION("1.07"); 1 };

use constant default_metaclass => 'Moos::Meta::Class';
use constant default_metarole  => 'Moos::Meta::Role';
use constant default_base_class => 'Moos::Object';

sub import {
    # Turn on strict/warnings for caller
    strict->import;
    warnings->import;

    ($_[1]||'') eq -Role and goto \&role_import;

    my ($class, %args) = @_;
    my $package = caller;

    # Create/register a metaclass object for the package
    my $metaclass =
        delete $args{metaclass}
        || $class->default_metaclass;
    my $meta = $metaclass->initialize($package, %args);

    # Make calling class inherit from Moos::Object by default
    my $baseclass = exists $args{base_class}
        ? delete $args{base_class}
        : $class->default_base_class;
    extends($meta, $baseclass) if defined $baseclass;

    # Export the 'has', 'extends', and 'with' helper functions
    _export($package, has => \&has, $meta);
    _export($package, extends => \&extends, $meta);
    _export($package, with => \&with, $meta);

    # Export the 'blessed' and 'confess' functions
    _export($package, blessed => \&Scalar::Util::blessed);
    _export($package, confess => \&Carp::confess);

    # Possibly export some handy debugging stuff
    _export_xxx($package) if $ENV{PERL_MOOS_XXX};
}

sub role_import {
    my ($class, undef, %args) = @_;
    my $package = caller;

    # Create/register a metaclass object for the package
    my $metarole =
        delete $args{metarole}
        || $class->default_metarole;
    my $meta = $metarole->initialize($package, %args);

    # Using 'eval' rather that exporting ensures that this method
    # will not be cleaned up by namespace::autoclean-type things.
    eval q{
        package }.$package.q{;
        sub meta {
            Moos::Meta::Role->initialize(
                Scalar::Util::blessed($_[0]) || $_[0]
            );
        }
    };

    # Export the 'has' helper function
    _export($package, has => \&has, $meta);

    # Export the 'blessed' and 'confess' functions
    _export($package, blessed => \&Scalar::Util::blessed);
    _export($package, confess => \&Carp::confess);

    # Possibly export some handy debugging stuff
    _export_xxx($package) if $ENV{PERL_MOOS_XXX};

    # Now do Role::Tiny's import stuff.
    require Role::Tiny;
    @_ = qw(Role::Tiny);
    goto \&Role::Tiny::import; # preserve caller
}

# Attribute generator
sub has {
    my ($meta, $name) = splice(@_, 0, 2);
    $name = [$name] unless ref $name;
    my %args;

    # Support 2-arg shorthand:
    #     has foo => 42;
    if (@_ % 2) {
        my $default = shift;
        my $sub =
            ref($default) eq 'HASH' ? sub {+{%$default}} :
            ref($default) eq 'ARRAY' ? sub {[@$default]} :
            sub {$default};
        %args = (default => $sub);
    }
    %args = (%args, @_);

    # Add attributes to meta class object
    $meta->add_attribute($_ => \%args) for @$name;
}

# Inheritance maker
sub extends {
    my ($meta, @parent) = @_;
    eval "require $_" for @parent;
    $meta->superclasses(@parent);
}

sub with {
    my ($meta, @roles) = @_;
    $meta->apply_roles(@roles);
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

# The remainder of this module was heavily inspired by Moose, and tried to do
# what Moose does, only much less.
package Moos::Meta::Class;
use Carp qw(confess);
our @ISA = 'Moos::Object';

# Store all the Moos meta-class-objects in a private hash, keyed on
# package/class name:
my $meta_class_objects = {};

# Helper method to get class name:
sub name { $_[0]->{package} }

sub default_attribute_metaclass { 'Moos::Meta::Attribute' }

# read-only accessor
sub attribute_metaclass {
    $_[0]{attribute_metaclass};
}
__PACKAGE__->meta->add_attribute(
    attribute_metaclass => {
        is => 'ro',
        default => \&default_attribute_metaclass,
        _skip_setup => 1,
    },
);

# Either looking the existing meta-class-object or register a new one:
sub initialize {
    my ($class, $package, %args) = @_;

    # Class to use to generate attribute accessors, etc
    $args{attribute_metaclass} ||= $class->default_attribute_metaclass;

    # This is a tiny version of a Moose meta-class-object.
    # We really just need a place to keep the attributes.
    return $meta_class_objects->{$package} ||= do {
        bless {
            package => $package,
            # This isn't currently used but matches Moose and is cheap.
            attributes => {},
            # We construct with attribute in order defined. (Unlike Moose)
            _attributes => [],
            %args,
        }, $class;
    };
}

# Make a new attribute object and add it to both a hash and an array, so that
# we can preserve the order defined.
sub add_attribute {
    my $self = shift;
    my $name = shift;
    my %args = @_==1 ? %{$_[0]} : @_;

    push @{$self->{_attributes}}, (
        $self->{attributes}{$name} =
        $self->attribute_metaclass->new(
            name => $name,
            associated_class => $self,
            %args,
        )
    );
}

{
    my $has_subname = eval { require Sub::Name; 1 };
    sub add_method {
        my ($self, $name, $code) = @_;
        my $pkg = $self->name;
        if (ref $code) {
            if ($has_subname) {
                $code = Sub::Name::subname("$pkg\::$name", $code);
                Moos::_export($pkg, $name, $code);
            }
            else {
                # close over $code
                eval "package $pkg; sub $name { goto \$code }";
            }
        }
        else {
            eval "package $pkg; sub $name { $code }";
        }
    }
}

# A tracing wrapper for debugging accessors
our $TRACE_EXCLUDE = +{
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
        unless ($TRACE_EXCLUDE->{"$pkg $name"}) {
            warn "$pkg $name $line\n";
            Time::HiRes::usleep(100000);
        }
        goto &$accessor;
    };
}

sub superclasses {
    no strict 'refs';
    my ($self, @supers) = @_;
    if (@supers) {
        @{"$self->{package}\::ISA"} = @supers;
    }
    return @{"$self->{package}\::ISA"};
}

sub linearized_isa {
    my $self = shift;
    my %seen;
    return grep { not $seen{$_}++ } @{ mro::get_linear_isa($self->name) };
}

sub apply_roles
{
    my ($self, @roles) = @_;
    my $package = $self->name;

    require Role::Tiny;

    # Load the role modules. (Role::Tiny would do this for us anyway.)
    Role::Tiny::_load_module($_) for @roles;

    # If any of them were Moose roles, then Class::MOP will now be
    # available to us. Use it to detect which roles have antlers.
    if (my $class_of = 'Class::MOP'->can('class_of')) {
        # Divide list of roles into Moose and non-Moose.
        my (@moose, @nonmoose);
        while (@roles) {
            my $role = shift @roles;
            my $list = $class_of->($role) ? \@moose : \@nonmoose;
            push @$list, $role;
            if (ref $roles[0] eq 'HASH') {
                push @$list, shift @roles;
            }
        }
        # Apply Moose roles
        if (@moose and my $apply = 'Moose::Util'->can('apply_all_roles')) {
            $apply->($package, @moose);

            foreach my $role (@moose) {
                my $rolemeta = $class_of->($role);
                my @attributes =
                    sort { $a->insertion_order <=> $b->insertion_order }
                    map  { $rolemeta->get_attribute($_) }
                    $rolemeta->get_attribute_list;
                foreach my $attr ( @attributes ) {
                    my $name = $attr->name;
                    my %args = (
                        lazy        => $attr->is_lazy,
                        required    => $attr->is_required,
                        is          => $attr->{is},
                        _skip_setup => 1,
                    );
                    for my $arg (qw/ clearer predicate builder default documentation handles trigger /)
                    {
                        my $has = "has_$arg";
                        $args{$arg} = $attr->$arg if $attr->$has;
                    }
                    $self->add_attribute($name, \%args);
                }
            }
        }
        # Allow non-Moose roles to fall through
        @roles = @nonmoose;
    }

    if (@roles) {
        'Role::Tiny'->apply_roles_to_package($package, @roles);

        my @more_roles = map {
            keys %{ $Role::Tiny::APPLIED_TO{$_} }
        } @roles;

        foreach my $role (@more_roles) {
            # Moo::Role stashes its attributes here...
            my @attributes = @{ $Role::Tiny::INFO{$role}{attributes} || [] };
            while (@attributes) {
                my $name = shift @attributes;
                my %args = %{ shift @attributes };
                $args{_skip_setup} = 1;  # Moo::Role already made accessors
                $self->add_attribute($name, \%args);
            }
        }
    }
}

# This is where new objects are constructed. (Moose style)
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
        }
        elsif (not $attr->{lazy}) {
            if (my $builder = $attr->{builder}) {
                $builder = "_build_$name"
                    if defined $builder && $builder eq "1";
                $instance->{$name} = $instance->$builder();
                next;
            }
            elsif (my $default = $attr->{default}) {
                $instance->{$name} = $default->($instance);
            }
            if ($attr->{required} and not exists $instance->{$name}) {
                confess "missing required attribute '$name'";
            }
        }
        # Triggers only fire for explicit assignment; not defaults.
        if (exists $attr->{trigger} and exists $params->{$name}) {
            $attr->{trigger}->($instance, $params->{$name});
        }
    }
    return $instance;
}

# Return all the unique attributes in the order defined from the outer class
# inwards:
sub get_all_attributes {
    my $self = shift;
    my (@attrs, %attrs);
    for my $package ($self->linearized_isa) {
        my $meta = Moos::Meta::Class->initialize($package);
        for my $attr (@{$meta->{_attributes}}) {
            my $name = $attr->{name};
            next if $attrs{$name};
            push @attrs, ($attrs{$name} = $attr);
        }
    }
    return @attrs;
}

# Cheap introspection stuff
sub get_attribute {
    my ($self, $name) = @_;
    return $self->{attributes}{$name};
}

sub find_attribute_by_name {
    my ($self, $name) = @_;
    for ($self->get_all_attributes) {
        return $_ if $_->name eq $name;
    }
    return;
}

# Package for roles
package Moos::Meta::Role;
use Carp qw(confess);
our @ISA = 'Moos::Meta::Class';

sub add_attribute {
    my $self = shift;
    my $name = shift;
    my %args = @_==1 ? %{$_[0]} : @_;

    push @{$Role::Tiny::INFO{ $self->name }{attributes}},
        $name => \%args;

    $self->SUPER::add_attribute($name, \%args);
}

# Package for blessed attributes
package Moos::Meta::Attribute;
use Carp qw(confess);
BEGIN { our @ISA = 'Moos::Object' };

__PACKAGE__->meta->add_attribute($_, { is=>'ro' })
    for qw(
        name associated_class is isa coerce does required
        weak_ref lazy trigger handles builder default clearer
        predicate documentation _skip_setup
    );

sub _is_simple {
    not (  $_[0]{builder}
        || $_[0]{default}
        || $_[0]{trigger}
        || $ENV{PERL_MOOS_ACCESSOR_CALLS}
    );
}

# Not sure why it is necessary to override &new here...
sub new {
    my $class = shift;
    my $self = bless $class->BUILDARGS(@_) => $class;
    $self->Moos::Object::BUILDALL;
    return $self;
}

sub BUILDARGS {
    shift;
    my $args = @_==1 ? $_[0] : +{@_};

    # Massage %args
    my $name = $args->{name};
    $args->{builder} = "_build_$name"
        if defined $args->{builder} && $args->{builder} eq "1";
    $args->{clearer} = $name =~ /^_/ ? "_clear$name" : "clear_$name"
        if defined $args->{clearer} && $args->{clearer} eq "1";
    $args->{predicate} = $name =~ /^_/ ? "_has$name" : "has_$name"
        if defined $args->{predicate} && $args->{predicate} eq "1";
    $args->{trigger} = do {
            my ($trigger, $method) = "_trigger_$name";
            sub {
                $method ||= $_[0]->can($trigger)
                    or confess "method $trigger does not exist for class ".ref($_[0]);
                goto $method;
            };
        } if defined $args->{trigger} && $args->{trigger} eq "1";
    $args->{is} = 'rw'
        unless defined $args->{is};

    return $args;
}

sub BUILD {
    my $self = shift;
    my $metaclass = $self->{associated_class} or return;

    foreach (qw( name builder predicate clearer ))
    {
        next if !exists $self->{$_};
        next if $self->{$_} =~ $VALID_NAME;
        confess sprintf(
            "invalid method name '%s' for %s",
            $self->{$_},
            $_ eq 'name' ? 'attribute' : $_,
        );
    }

    unless ( $self->{_skip_setup} ) {
        $self->_setup_accessor($metaclass);
        $self->_setup_clearer($metaclass)    if $self->{clearer};
        $self->_setup_predicate($metaclass)  if $self->{predicate};
        $self->_setup_delegation($metaclass) if $self->{handles};
    }
}

# Make a Setter/Getter accessor
sub _setup_accessor
{
    my ($self, $metaclass) = @_;
    my $name = $self->{name};

    if ($self->_is_simple) {
        if ($Moos::CAN_HAZ_XS) {
            my $type = $self->{is} eq 'ro' ? 'getters' : 'accessors';
            return Class::XSAccessor->import(
                class => $metaclass->{package},
                $type => [$name],
            );
        }
        else {
            my $accessor = $self->{is} eq 'ro'
                ? qq{ Carp::confess("cannot set value for read-only accessor '$name'") if \@_ > 1; \$_[0]{'$name'} }
                : qq{ \$#_ ? \$_[0]{'$name'} = \$_[1] : \$_[0]{'$name'} };
            return $metaclass->add_method($name, $accessor);
        }
    }

    my ($builder, $default) = map $self->{$_}, qw(builder default);
    my $accessor =
        $builder ? sub {
            $#_ ? $_[0]{$name} = $_[1] :
            exists($_[0]{$name}) ? $_[0]{$name} :
            ($_[0]{$name} = $_[0]->$builder);
        } :
        $default ? sub {
            $#_ ? $_[0]{$name} = $_[1] :
            exists($_[0]{$name}) ? $_[0]{$name} :
            ($_[0]{$name} = $default->($_[0]));
        } :
        sub {
            $#_ ? $_[0]{$name} = $_[1] : $_[0]{$name};
        };

    if ($self->{is} eq 'ro') {
        my $orig = $accessor;
        $accessor = sub {
            confess "cannot set value for read-only accessor '$name'" if @_ > 1;
            goto $orig;
        };
    }

    elsif (exists $self->{trigger}) {
        ref $self->{trigger} or confess "trigger for $name is not a reference";
        my $orig = $accessor;
        $accessor = sub {
            if (@_ > 1) {
                $self->{trigger}->(
                    @_[0, 1],
                    exists($_[0]{$name}) ? $_[0]{$name} : (),
                );
            }
            goto $orig;
        };
    }

    # Dev debug thing to trace calls to accessor subs.
    $accessor = _trace_accessor_calls($name, $accessor)
        if $ENV{PERL_MOOS_ACCESSOR_CALLS};

    # Export the accessor.
    $metaclass->add_method($name, $accessor);

    return;
}

sub _setup_clearer {
    my ($self, $metaclass) = @_;
    my $name = $self->{name};

    my $clearer = $self->{clearer} or return;
    $metaclass->add_method($clearer, qq{ delete \$_[0]{'$name'} });
    return;
}

sub _setup_predicate {
    my ($self, $metaclass) = @_;
    my $name = $self->{name};

    my $predicate = $self->{predicate} or return;

    if ($Moos::CAN_HAZ_XS) {
        return Class::XSAccessor->import(
            class => $metaclass->{package},
            predicates => { $predicate => $name },
        );
    }

    $metaclass->add_method($predicate, qq{ exists \$_[0]{'$name'} });
    return;
}

sub _setup_delegation {
    my ($self, $metaclass) = @_;
    my $name = $self->{name};

    return unless exists $self->{handles};

    my %map;
    %map = %{$self->{handles}}
        if Scalar::Util::reftype($self->{handles}) eq 'HASH';
    %map = map { ;$_=>$_ } @{$self->{handles}}
        if Scalar::Util::reftype($self->{handles}) eq 'ARRAY';

    while (my ($local, $remote) = each %map) {
        for my $method ($local, $remote) {
            next if $method =~ $VALID_NAME;
            confess "invalid delegated method name '$method'";
        }
        if ($self->_is_simple) {
            $metaclass->add_method($local, qq{ shift->{$name}->$remote(\@_) });
        }
        else {
            $metaclass->add_method($local, qq{ shift->$name\->$remote(\@_) });
        }
    }
    return;
}

# This is the default base class for all Moos classes:
package Moos::Object;

# Moos constructor
sub new {
    my $class = shift;
    my $real_class = Scalar::Util::blessed($class) || $class;
    my $params = $real_class->BUILDARGS(@_);
    return Moos::Meta::Class->initialize($real_class)->new_object($params);
}

# A default BUILDARGS
sub BUILDARGS {
    shift;
    return +{(@_ and ref($_[0]) eq 'HASH') ? %{$_[0]} : @_};
}

# A default BUILDALL
sub BUILDALL {
    return unless $_[0]->can('BUILD');
    my ($self, $params) = @_;
    for my $package (reverse $self->meta->linearized_isa) {
        no strict 'refs';
        if (defined &{"$package\::BUILD"}) {
            &{"$package\::BUILD"}($self, $params);
        }
    }
}

# A Data::Dumper method. (Moose has it. No cost.)
sub dump {
    no warnings 'once';
    my $self = shift;
    require Data::Dumper;
    local $Data::Dumper::Maxdepth = shift if @_;
    Data::Dumper::Dumper $self;
}

# Retrieve the Moos meta-class-object.
sub meta {
    Moos::Meta::Class->initialize(Scalar::Util::blessed($_[0]) || $_[0]);
}

sub does {
    my ($self, $role) = @_;
    return 1
        if Role::Tiny::does_role($self, $role);
    return 1
        if UNIVERSAL::can('Moose::Util', 'can')
        && Moose::Util->can('does_role')
        && Moose::Util::does_role($self, $role);
    return 0;
}

sub DOES {
    my ($self, $role) = @_;
    my $universal_does = UNIVERSAL->can('DOES') || UNIVERSAL->can('isa');
    $self->does($role) or $self->$universal_does($role);
}

1;

=encoding utf8

=head1 Name

Moos - Moo s{imple,peedy,ingle}

=head1 Synopsis

    package Foos;
    use Moos;

    extends 'Boos';
    with 'Cloos';

    has this => ();
    has that => 42;
    has other => (
        builder => 'build_other',
        lazy => 1,
    );

    sub BUILD {
        my $self = shift;
        # build, build, build
    }

    sub BUILDARGS {
        my ($self, @args) = @_;
        # munge, munge, munge
        return {%munged_args};
    }

=head1 Description

Moos completes the M to Moose sequence of Perl OO modules.

This one is pure Perl, single file and mostly Moose compatible (for what it
does). Moos has no non-core dependencies, but certain features (roles,
debugging functions, legacy Perl support) do require additional modules. If
you steer away from those features, you don't need those additional modules.

=head1 Features

Here's a quick list of the L<Moose> compatible features that are supported by
L<Moos>.

=head2 strict / warnings

Turns on C<strict> and C<warnings> for you.

=head2 Helpful exports

The ever useful C<blessed> (from L<Scalar::Util>) and C<confess> (from
L<Carp>) are exported to your namespace.

=head2 extends

For inheritance. C<Moos::Object> is the default base class.

    package MyClass;
    extends 'MyBaseClass';

Supports multiple inheritance, by allowing multiple classes on a single
invocation.

=head2 with

Moos can consume roles using the C<with> keyword. Using this feature requires
Role::Tiny to be installed.

    with 'ThisClass', 'ThatClass';

=head2 has

Accessor generator. Supports the C<is>, C<default>, C<build>, C<lazy>,
C<clearer>, C<predicate>, C<required>, C<handles> and C<trigger> options,
described below. The supported options are about the same as Moose. Other
arguments (e.g. C<isa> and C<coerce>) are currently ignored.

    has this => ();

NOTE: Class::XSAccessor will be used for simple accessors if it is installed.
This can be disabled by setting $Moos::CAN_HAZ_XS to false or by setting the
PERL_MOOS_XS_DISABLE to true.

=over

=item is

Specify which type of attribute accessor to provide. The default is "rw",
a read-write accessor. Read-only "ro" accessors are also supported.

    has this => ( is => "ro" );
    has 'this';                   # read-write
    has that => ();               # read-write

Unlike Moose, Moos cannot generate differently named getters and setters.
If you want your setter named something different (e.g. a private method),
then you could do something like:

    has this => ( is => 'ro' );
    sub _set_this { $_[0]{this} = $_[1] }

=item required

Require that a value for the attribute be provided to the constructor or
generated during object construction.

    has this => ( required => 1 );

=item lazy

Don't generate defaults during object construction.

    has this => ( builder => '_build_this', lazy => 1 );

=item trigger

A coderef which will be called when the attribute is assigned to via a method
call or the constructor. (But not when an attribute is implicitly given a
value via a default or builder.) The coderef is called with the instance as
the first parameter, the new value as the second parameter, and the old value
(if any) as the third parameter.

    has age => ( trigger => sub {
        croak "non-numeric age" unless looks_like_number($_[1]);
    } );

Triggers can be used to emulate Moose's type constraints, coercion and
weakened reference features, but if you find yourself doing this frequently
then you should consider upgrading to Moo or Moose.

=item handles

Delegated method calls.

    has wheels => (handles => [qw/ roll /]);

This accepts a hashref or arrayref, but not the other possibilities
offered by L<Moose>.

=item builder

Specify the method name to generate a default value.

    has this => ( builder => '_build_this' );
    has that => ( builder => 1 );  # accept default name for method

=item default

Specify the sub to generate a default value.

    has this => ( default => sub { 42 } );

Moos provides a shortcut for specifying the default. If the number of
arguments (after the name) is an odd number, then the first argument is
the default. The following forms are valid:

    has a => 42;
    has b => 'string' => (lazy => 1);
    has c => {};
    has d => [1, 2, 3, 4];

These all result in creating a Moos C<default> argument. If the default is an
array or hash reference, a shallow copy is made.

=item clearer

Creates a clearer method.

    has this => ( clearer => "clear_this" );
    has that => ( clearer => 1 );  # accept default name for method

=item predicate

Creates a predicate method, which can be used to check if the attribute is
set or unset.

    has this => ( predicate => "has_this" );
    has that => ( predicate => 1 );  # accept default name for method

=back

=head2 Class and Object Methods

=over

=item new

A constructor class method.

    my $object = MyClass->new(this => 'nice', that => 2);

=item BUILD

Custom object construction. If you define BUILD, it is passed the value of the
new object during construction. You can modify the object. Any value you
return is ignored.

    sub BUILD { my $self = shift; ... }

=item BUILDARGS

Custom constructor argument processing. If you define BUILDARGS, you can
control how the constructor's arguments are built into the object hashref.

    sub BUILDARGS { my ($class, @args) = @_; ... }

=item dump

Returns a textual dump of the object.

=item meta

Returns a Moos::Meta::Class object for the class. This has a very limited
subset of L<Moose::Meta::Class>' functionality, including implementations
of the following methods:
C<name>,
C<attribute_metaclass>,
C<add_attribute>,
C<add_method>,
C<superclasses>,
C<linearized_isa>,
C<new_object>,
C<get_all_attributes>,
C<get_attribute> and
C<find_attribute_by_name>.

The attribute introspection methods return L<Moos::Meta::Attribute> objects
which provide a very limited subset of L<Moose::Meta::Attribute>'s
functionality, including implementations of the following methods:
C<name>,
C<associated_class>,
C<predicate>,
C<clearer>,
C<default>,
C<builder>,
C<trigger>,
C<required>,
C<lazy> and
C<documentation>.

=item does / DOES

Methods to check whether the class/object performs a particular
role. The methods differ in that C<does> checks roles only in
the Moose/Moo/Role::Tiny sense; C<DOES> also takes into account
L<UNIVERSAL>::DOES.

=back

=head2 Roles

If you need roles, then Moos classes have B<experimental> support for
L<Role::Tiny>, L<Moo::Role> and L<Moose::Role> roles. (Moos provides a
C<with> command that uses L<Role::Tiny> to do the work.)

    {
        package Local::Class;
        use Moos;
        with "Local::Role";
        ...;
    }

B<Limitations:> Note that Moo and Moose each allow type constraints for
attributes; Moos does not. This means that if you compose, say, a Moose::Role
into a Moos class, you end up with a strange situation where the accessor
methods will enforce type constraints (because they were generated by Moose)
but the constructor will not (because it is inherited from Moos::Object).

See also L<Moos::Role>.

=head2 Method Modifiers

If you need method modifiers, then try L<Class::Method::Modifiers>.

=head2 Development Options

Moos has a couple of builtin dev options. They are controlled by environment
variables.

=over

=item PERL_MOOS_ACCESSOR_CALLS

By setting this environment variable, Moos will warn everytime an accessor
method is called.

=item PERL_MOOS_XXX

By setting the environment variable, Moos will export the L<XXX> debugging
keywords.

=back

=head1 Whence Moos

I(ngy) created Moos during L<Pegex> development. Pegex uses a clone of Moos
called L<Pegex::Base>. (L<Moos> ships with a commandline utility called
C<remoos> that does this cloning.)

Pegex is a parser framework and needs to be fast. While looking into speed
issues I noted that accessor calling was the biggest hit. I tried all the
various Mo* solutions and L<Mouse> was the fastest.

I was happy until I remembered that Mouse uses XS, and for various reasons
this broke my toolchain (TestML, Module::Install, etc).

So I made a single module/file Moose clone and it worked out well. I've shared
Pegex::Base as L<Moos> in case any other projects want it.

Later on, Toby Inkster added a bunch of low-cost but very handy features from
Moose.

The name L<Moos> was chosen because it was the only name left between M and
Moose. (Thus adding to the epic confusion that we embrace as Perl Mongers! :)

=head1 On Speed

In the end, I got Pegex to run even faster with Moos than it originally did
with Mouse. I'll tell you my secret...

B<< Accessors I<(usually)> do not need to be method calls. >>

Replace these:

    my $foo = $self->foo;
    $self->foo($foo);

with:

    my $foo = $self->{foo};
    $self->{foo} = $foo;

And your code will be faster (and a bit uglier).

The only time that you need to call an accessor method is when you are
accessing a property and it might invoke a C<lazy> C<builder>, C<default> or
C<trigger> method. Otherwise you are just wasting time. At least with the
minimal feature set offered by Moos.

The PERL_MOOS_ACCESSOR_CALLS feature described above is for finding these
method calls.

Note that third parties can still use your module's accessor methods like
they would expect to.

I'm sure I've missed some subtleties, and would be glad to hear opinions, but
in the meantime I'm happy that my code is faster and pure Perl.

=head1 See Also

=over

=item * L<M>

=item * L<Mo>

=item * L<Moo>

=item * L<Moos>

=item * L<Moose>

=item * L<Mouse>

=item * L<Mousse>

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
