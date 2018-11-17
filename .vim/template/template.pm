#!/usr/bin/env python
## -*- encoding: utf-8 -*- vim:shiftwidth=4

package PackageName;

use strict;
use warnings;

sub new
{
    my ($proto, %arg) = @_;
    my $class = ref($proto) || $proto;

    my $self = {};
    bless($self, $class);

    return $self;
}

return 1;
