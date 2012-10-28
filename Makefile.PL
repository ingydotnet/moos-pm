use inc::Module::Package 'Ingy:modern 0.20';

recommends 'Class::XSAccessor' => '1.07';
requires 'MRO::Compat' => 0 if $] < 5.010;
