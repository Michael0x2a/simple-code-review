#!/usr/bin/env python

from __future__ import print_function

import sys
import subprocess
import json
import traceback

TEMPLATE = '''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Test Huskylint Output</title>
<link href="https://cdnjs.cloudflare.com/ajax/libs/prism/0.0.1/prism.min.css" rel="stylesheet" />
<style type="text/css">
body {{padding:1em;font-family: Arial Narrow, Arial, sans-serif; }}
.wrapper {{display: flex; width:100%;line-height:1.25em;}}
.wrapper > div {{margin: 0.5em;}}
pre,code {{max-height:100% !important; overflow: hidden !important}}
.code {{z-index:1; width: 70%;}}
.commentary {{z-index:50; width:30%; padding-top: 1.5em;}}
p {{z-index:100; margin: 0; height: 1.25em;}}
p > span {{font-size: 12px;}}
p:hover:after {{position:absolute; left:0; content:"";background-color:rgba(180, 255, 255, 0.4); width:100vw; height:1.25em;}}
</style>
</head>
<body>
<h1>Example output</h1>
<div class="wrapper">
  <div class="commentary">{commentary}</div>
  <div class="code"><pre><code class="language-java">{code}</code></pre></div>
</div>
<h1>Raw json output</h1>
<div class="raw"><pre><code class="language-javascript">{raw}</code></pre></div>
<script src="https://cdnjs.cloudflare.com/ajax/libs/prism/0.0.1/prism.min.js"></script>
</body>
</html>'''

def blacklisted(data):
    key = data[1]['key']
    return key not in {'indentation.error', 'indentation.child.error'}

def parse(lines):
    lines = filter(lambda line: not line.startswith("{kill=["), lines)
    lines = filter(lambda line: not line.startswith("hloa"), lines)
    lines = map(lambda line: (line, json.loads(line)), lines)
    lines = filter(blacklisted, lines)
    return list(lines)
    
def gen_errors(lines):
    errors = {}
    for raw, data in lines:
        lineNo = data['lineNo']
        key = data['key']
        if lineNo not in errors:
            errors[lineNo] = []
        errors[lineNo].append([key, raw])
    return errors
    
def gen_commentary(errors):
    largest = max(errors.keys())
    for lineno in range(1, largest + 1):
        if lineno in errors:
            yield ', '.join(err[0] for err in errors[lineno])
        else:
            yield ''
            
def create_html(filename, commentary,lines):
    with open(filename, "r") as stream:
        return TEMPLATE.format(
            code=stream.read().replace("<", "&lt;").replace(">", "&gt;").replace("\r", ""), 
            commentary='\n'.join(['<p><span>{0}</span></p>'.format(key) for key in commentary]),
            raw=',\n'.join([json.dumps(line[1], indent=4) for line in lines]))
    
def main():
    try:
        with open(sys.argv[1], "r") as stream:
            lines = stream.readlines()
        lines = parse(lines)
        errors = gen_errors(lines)
        commentary = gen_commentary(errors)
        print(create_html(sys.argv[2], commentary, lines))
    except:
        print(sys.argv[2])
        with open("log.txt", "a") as stream:
            stream.write(sys.argv[2] + "\n")
            stream.write(traceback.format_exc())
            stream.write("\n\n\n")
        raise
    
if __name__ == '__main__':
    main()
