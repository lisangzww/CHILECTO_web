# -*- coding: utf-8 -*-
"""Utilities developed for the Chilecto project.

"""


__author__ = "Weiwei"
__date__ = "12/29/2018"
__version__ = '1.0.0'


# import
from .gen import *
from .processor import *


# package logger

import logging

logger = logging.getLogger('cu')

logger.setLevel(logging.INFO)

# create file handler which logs even debug messages
fh = logging.FileHandler('CU.log')
fh.setLevel(logging.INFO)

# create console handler
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)

# create formatter and add it to the handlers
formatter = logging.Formatter(
    '%(asctime)s - %(name)s - %(levelname)s - %(message)s')

fh.setFormatter(formatter)
ch.setFormatter(formatter)

# add the handlers to the logger
logger.addHandler(fh)
logger.addHandler(ch)
