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

log_file = open('time.log', 'r')
log_file_content = log_file.readlines()
times_usbstracted=int(log_file_content[1])-int(log_file_content[0])
execution_time=str(times_usbstracted)

maker = ElementMaker()
TESTSUITE = maker.testsuite
TESTCASE = maker.testcase
SYSTEMOUT = maker.systemout


if(args.result=='failed'):    
    xml_doc = TESTSUITE(
        TESTCASE(
            SYSTEMOUT(" "), 
            time=execution_time, name=args.name),
            tests="1",failures="1",errors="1",time=execution_time,name=args.name)
else:
    xml_doc = TESTSUITE(
        TESTCASE(
            SYSTEMOUT(" "), 
            time=execution_time, name=args.name),
            tests="1",failures="0",errors="0",time=execution_time,name=args.name)                
        

string_file = etree.ElementTree(xml_doc)
string_file.write('test/dialyzer_results/TEST-dialyzer.xml',pretty_print=True)       