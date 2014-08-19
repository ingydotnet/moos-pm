# The entire implementation of Moos (and all its related classes) are defined
# inside this one file.
use strict; use warnings;

my $VALID_NAME = qr{ ^ [^\W0-9] \w* $ }ix;

package Moos;

our $VERSION = '0.30';

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
    my ($meta, @parents) = @_;
    for my $parent (@parents) {
        eval "require $parent";
    }
    $meta->superclasses(@parents);
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
        if $INC{'Role/Tiny.pm'}
        && Role::Tiny::does_role($self, $role);
    return 1
        if $INC{'Moose/Util.pm'}
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
