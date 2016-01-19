#!/usr/bin/env python
import string
import argparse
import sys
import os

class MyParser(argparse.ArgumentParser):
    def error(self, message):
        sys.stderr.write('error: %s\n' % message)
        self.print_help()
        sys.exit(2)

template = string.Template("""
mkdir -p /tmp/webjar-tmp/META-INF/resources
cp $recursive $files /tmp/webjar-tmp/META-INF/resources
jar cf $out -C /tmp/webjar-tmp META-INF
rm -rf /tmp/webjar-tmp
""")

parser = MyParser()
parser.add_argument("-r", "--recursive", help="add nested files and folders", action="store_true")
parser.add_argument("-p", "--pretend", help="print shell command instead of execute it", action="store_true")
parser.add_argument("out", help="output jar filename")
parser.add_argument("resources", help="list of files to add", nargs="+")
args = parser.parse_args()

cmd = template.substitute({
    "recursive": "-r" if args.recursive else "",
    "files": " ".join(args.resources),
    "out": args.out
})

if args.pretend:
    print(cmd)
else:
    os.system(cmd)
