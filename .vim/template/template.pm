#!/usr/bin/env python
## -*- encoding: utf-8 -*- vim:shiftwidth=4
##
## SPDX-FileCopyrightText: 20XX SATOH Fumiyasu @ OSSTech Corp., Japan
## SPDX-License-Identifier: GPL-3.0-or-later
##

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
