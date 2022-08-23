#!/usr/bin/env python3

# coding=utf-8
"""Author: Rafał Widziszewski
Copyright (C) 2022 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

"""

import argparse
import subprocess
import time
import unicodedata
import os


def should_include_char_in_output(c):
    # Returns true for printable characters and newlines/tabs. Used to filter out unwanted
    # characters from a stdout string that is placed inside the surefire XML.
    if c == '\n' or c == '\r' or c == '\t':
        return True
    else:
        return not unicodedata.category(c).startswith('C')



parser = argparse.ArgumentParser()
parser.add_argument("--test-name", help="Name of test displayed in xml file")
parser.add_argument("--report-path", help="Path where the xml file is generated")
parser.add_argument("rest", nargs=argparse.REMAINDER, help="Command run as subprocess")
args = parser.parse_args()

execution_time_start = time.time()
result = subprocess.run(args.rest, stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT, universal_newlines=True)
execution_time = execution_time_start - time.time()

print(result.stdout, end='')

if result.returncode == 0:
    failures = 0
    failure_element = ''
else:
    failures = 1

    result.stdout = result.stdout.replace(">", "&gt;")
    result.stdout = result.stdout.replace("<", "&lt;")
    filtered_stdout = ''.join(
        c for c in result.stdout if should_include_char_in_output(c))

    failure_element = '<failure>{stdout}</failure>'.format(stdout=filtered_stdout)

xml_content = '''<?xml version="1.0" encoding="UTF-8" ?>
<testsuite tests="1" failures="{failures}" errors="0" skipped="0" time="{time}" name="{name}">
    <testcase time="{time}" name="{name}">
        {failure_element}
        <system-out>
        </system-out>
    </testcase>
</testsuite>
    '''.format(
    failures=failures,
    time=execution_time,
    name=args.test_name,
    failure_element=failure_element
)

os.makedirs(os.path.dirname(args.report_path), exist_ok=True)
with open(args.report_path, "w") as f:
    f.write(xml_content)
