#!/usr/bin/env python

# coding=utf-8
"""Author: Rafał Widziszewski
Copyright (C) 2022 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

"""

import argparse
from distutils.log import error
import subprocess
import time
import os

parser = argparse.ArgumentParser()
parser.add_argument("--test-name")
parser.add_argument("--report-path")
parser.add_argument("rest", nargs=argparse.REMAINDER)
args = parser.parse_args()

execution_time_start = time.time()
result = subprocess.run(args.rest, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True )
execution_time_end = time.time()

print(result.stdout)

execution_time=execution_time_end-execution_time_start


if result.stderr == None: 
    xml_content = '''<?xml version="1.0" encoding="UTF-8" ?>
    <testsuite tests="1" failures="{failures}" errors="0" skipped="0" time="{time}" name="{name}">
    <testcase time="{time}" name="{name}">
        <system-out>
        </system-out>
    </testcase>
    </testsuite>
    '''.format(
        failures=0,
        time=execution_time,
        name=args.test_name
    )
else:
    xml_content = '''<?xml version="1.0" encoding="UTF-8" ?>
        <testsuite tests="1" failures="{failures}" errors="0" skipped="0" time="{time}" name="{name}">
        <testcase time="{time}" name="{name}">
        <failure type="assertEqual_failed">
    {failure}
        </failure>
            <system-out>
            </system-out>
        </testcase>
        </testsuite>
        '''.format(
            failures=1,
            time=execution_time,
            name=args.test_name,
            failure=result.stderr
        )        


directory=args.report_path

os.makedirs(os.path.dirname(directory), exist_ok=True)
with open(directory, "w") as f:
    f.write(xml_content)       