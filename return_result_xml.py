#!/usr/bin/env python

# coding=utf-8
"""Author: Rafał Widziszewski
Copyright (C) 2022 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

"""


import string
from unittest import TestCase
from lxml import etree
from lxml.builder import ElementMaker
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("name")
parser.add_argument("result")
args = parser.parse_args()

maker = ElementMaker()
TESTSUITE = maker.testsuite
TESTCASE = maker.testcase
SYSTEMOUT = maker.systemout

if(args.result=='failed'):    
    xml_doc = TESTSUITE(
        TESTCASE(
            SYSTEMOUT(" "), 
            time="0.02", name="api"),
            tests="1",failures="1",errors="1",time="0.0",name=args.name)
else:
    xml_doc = TESTSUITE(
        TESTCASE(
            SYSTEMOUT(" "), 
            time="0.03", name="api"),
            tests="1",failures="0",errors="0",time="0.0",name=args.name)                
        

string_file = etree.ElementTree(xml_doc)
string_file.write('TEST-dialyzer.xml',pretty_print=True)       