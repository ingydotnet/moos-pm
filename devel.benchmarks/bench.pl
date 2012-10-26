use v5.14;
use Benchmark qw(:all);
use Method::Generate::Accessor ();

{
	package UseMoose::Slow;
	use Moose;
	has foo => (is => 'ro');
}

{
	package UseMoose::Fast;
	use Moose;
	has foo => (is => 'ro');
	__PACKAGE__->meta->make_immutable;
}

{
	package UseMouse;
	use Mouse;
	has foo => (is => 'ro');
}

{
	package UseMoo::XS;
	use Moo;
	has foo => (is => 'ro');
}

{
	package UseMoos::XS;
	use Moos;
	has foo => (is => 'ro');
}

{
	package UseMoo::PP;
	use Moo;
	local $Method::Generate::Accessor::CAN_HAZ_XS = 0;
	has foo => (is => 'ro');
}

{
	package UseMoos::PP;
	use Moos;
	local $Moos::CAN_HAZ_XS = 0;
	has foo => (is => 'ro');
}

cmpthese(-3, +{
	map {
		my $class = "Use$_";
		$_ => sub {
			my $o = $class->new(foo => 42);
			$o->foo for 1..100;
		},
	}
	qw( Moose::Slow Moose::Fast Moo::XS Moo::PP Moos::XS Moos::PP Mouse )
});