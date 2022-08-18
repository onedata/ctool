#!/usr/bin/env python3

# coding=utf-8
"""Author: Rafał Widziszewski
Copyright (C) 2022 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

"""

import argparse
import subprocess
import time
import os

parser = argparse.ArgumentParser()
parser.add_argument("--test-name")
parser.add_argument("--report-path")
parser.add_argument("rest", nargs=argparse.REMAINDER)
args = parser.parse_args()

execution_time_start = time.time()
result = subprocess.run(args.rest, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
execution_time_end = time.time()

print(result.stdout, end = '')

execution_time=execution_time_end-execution_time_start


if result.returncode == 0:
    failures=0
    failure_format_xml=''
else:
    failures=1
    failure_format_xml='''<failure>
{error}
    </failure>'''.format(
     error=result.stdout    
)    

xml_content = '''<?xml version="1.0" encoding="UTF-8" ?>
    <testsuite tests="1" failures="{failures}" errors="0" skipped="0" time="{time}" name="{name}">
    <testcase time="{time}" name="{name}">
    {failure_text}
        <system-out>
        </system-out>
    </testcase>
    </testsuite>
    '''.format(
        failures=failures,
        time=execution_time,
        name=args.test_name,
        failure=result.stdout,
        failure_text=failure_format_xml
    )        


os.makedirs(os.path.dirname(args.report_path), exist_ok=True)
with open(args.report_path, "w") as f:
    f.write(xml_content)
