use Test::More;

package Foo;
use Moos;

has 'foo' => ( default => sub {42} );
has 'bar' => ( default => sub {42} );

package Bar;
use Moos;
extends 'Foo';

has 'bar' => ( default => sub {43} );
has 'baz' => ( default => sub {43}, lazy => 1 );


package main;

my $foo = Foo->new;
my $bar = Bar->new;

ok $foo->isa('Moos::Object');
ok $foo->isa('Foo');
is $foo->{foo}, 42;
is $foo->{bar}, 42;

ok $bar->isa('Bar');
ok $bar->isa('Foo');
ok $bar->isa('Moos::Object');
is $bar->{foo}, 42;
is $bar->{bar}, 43;
is $bar->foo, 42;
is $bar->{baz}, undef;
is $bar->baz, 43;

done_testing;
