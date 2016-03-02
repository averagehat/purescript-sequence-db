from __future__ import print_function
import subprocess, os, sh
import itertools
import yaml
yml = yaml.load(open('.travis.yml'))

def run(version, cmd):
     return subprocess.check_output(["bash", "-c"] + [cmd],
            env=(dict(os.environ, TRAVIS_PYTHON_VERSION=version)))

for version in filter(lambda x: '6' not in x, yml['python']):
    bash = sh.bash.bake("-c")
    try:
        #sh.source.activate(version)
        bash("source activate %s" % version)
    except Exception:
        sh.conda.create("python=%s" % version[0], name=version)
        bash("source activate %s" % version)
    for cmd in itertools.chain(yml['install'],
                               yml['before_script'], yml['script']):
        print(run(version, cmd))
    bash("source deactivate")

#def Try(func, backupfunc, expected=Exception):
#    try:
#        return func()
#    except Exception:
#        return backupfunc()
#source = lambda: sh.source.activate(version),

