use inc::Module::Package 'Ingy:modern 0.20';

recommends 'Class::XSAccessor' => '1.07';
recommends 'Role::Tiny' => '1.002000';
requires 'MRO::Compat' => 0 if $] < 5.010;
