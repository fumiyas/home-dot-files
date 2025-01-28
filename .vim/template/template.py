#!/usr/bin/env python3
# -*- coding: utf-8 -*- vim:shiftwidth=4:expandtab:
#
# SPDX-FileCopyrightText: 20XX SATOH Fumiyasu @ OSSTech Corp., Japan
# SPDX-License-Identifier: GPL-3.0-or-later
#

from __future__ import print_function

import logging
import os
import sys

logger = logging.getLogger(__name__)


def main(argv):
    print(argv)
    logger.warning("warning log test")
    logger.info("info log test")
    try:
        raise
    except Exception as e:
        logger.error("error log test with trace: %s", e, exc_info=True)

    return 0


if __name__ == '__main__':
    logging_handlers = []

    # stderr
    logging_handlers.append(logging.StreamHandler())

    # file
    import datetime
    log_basename = f'{os.path.splitext(os.path.basename(sys.argv[0]))[0]}'
    log_filename = f'{log_basename}.{datetime.datetime.now().strftime("%Y%m%d%H%M%S")}.log'
    logging_handlers.append(logging.FileHandler(log_filename))

    # syslog
    import logging.handlers
    syslog_handler = logging.handlers.SysLogHandler(
        address='/dev/log',
        facility='user',
    )
    syslog_handler.ident = f'{os.path.basename(sys.argv[0])}[{os.getpid()}]: '
    logging_handlers.append(
        syslog_handler
    )

    logging.basicConfig(
        level=logging.WARN,
        format=(f'{sys.argv[0]}: %(levelname)s: %(message)s'),
        handlers=logging_handlers,
    )

    # Set a specific log level to a module
    logging.getLogger("urllib3").setLevel(logging.DEBUG)

    if len(sys.argv) < 2:
        print("Usage: %s STRING [...]" % (sys.argv[0]), file=sys.stderr)
        sys.exit(1)

    sys.exit(main(sys.argv[1:]))
