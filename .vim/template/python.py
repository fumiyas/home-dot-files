#!/usr/bin/env python
## -*- coding: utf-8 -*- vim:shiftwidth=4:expandtab:

from __future__ import print_function

import logging
import sys

logger = logging.getLogger(__name__)


def main(argv):
    logger.warning("warning log test")
    print(argv)

    return 0


if __name__ == '__main__':
    logging.basicConfig()
    logformatter = logging.Formatter('%(filename)s: %(levelname)s: %(message)s')
    loghandler = logging.StreamHandler()
    loghandler.setlevel(logging.INFO)
    loghandler.setFormatter(logformatter)
    logger.addHandler(loghandler)

    if len(sys.argv) < 2:
        print("Usage: %s STRING [...]" % (sys.argv[0]), file=sys.stderr)
        sys.exit(1)

    sys.exit(main(sys.argv[1:]))
