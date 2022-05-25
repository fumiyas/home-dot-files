#!/usr/bin/env python3
## -*- coding: utf-8 -*- vim:shiftwidth=4:expandtab:

from __future__ import print_function

import logging
import sys


logger = logging.getLogger(__name__)


def main(argv):
    logger.warning("warning log test")
    logger.info("info log test")
    try:
        undefined_func()
    except:
        logger.error("Boo!", exc_info=True)

    print(argv)

    return 0


if __name__ == '__main__':
    logging.basicConfig(
        level=logging.WARN,
        format=(sys.argv[0] + ': %(levelname)s: %(message)s'),
    )

    ## Set a specific log level to a module
    logging.getLogger("urllib3").setLevel(logging.DEBUG)

    if len(sys.argv) < 2:
        print("Usage: %s STRING [...]" % (sys.argv[0]), file=sys.stderr)
        sys.exit(1)

    sys.exit(main(sys.argv[1:]))
