use Test::More;

package Foo;
use Moos;

has 'foo' => ( default => sub {42} );

package Bar;
use Moos;

extends 'Foo';

package main;

my $foo = Foo->new;
my $bar = Bar->new;

ok $foo->isa('Moos::Object');
ok $foo->isa('Foo');
is $foo->{foo}, 42;

ok $bar->isa('Bar');
ok $bar->isa('Foo');
ok $bar->isa('Moos::Object');
is $bar->foo, 42;

done_testing;
