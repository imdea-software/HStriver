#!/usr/bin/python3
from pathlib import Path
import shutil
import sys
import subprocess
import os
import stat

outpath = "/tmp/toolc/"
shutil.rmtree(outpath,ignore_errors=True)
outpathlibs = outpath+"Lib/"
os.makedirs(outpathlibs)
outpathinnerspecs = outpath+"INNERSPECSDIR/"
os.makedirs(outpathinnerspecs)
outpathdomains = outpath+"domains/"
os.makedirs(outpathdomains)
outpathdomain = outpath+"TimeDomain/"
os.makedirs(outpathdomain)
outbin = "./a.out"
forcebuild = False

try:
    oindex = sys.argv.index("-o")
    outbin = sys.argv.pop(oindex+1)
    del sys.argv[oindex]
except ValueError:{} # Element not in the list

try:
    fbix = sys.argv.index("--force-build")
    forcebuild = True
    del sys.argv[fbix]
except ValueError:{} # Element not in the list

if len(sys.argv) > 1:
    hstriverfiles = map(Path,sys.argv[1:])
else:
    hstriverfiles = list(Path(".").rglob("*.[hH][sS][tT][rR][iI][vV][eE][rR]"))

hstriverbasedir = str(Path(os.path.dirname(os.path.realpath(__file__))).parent.parent) + "/src/"
preprocbin = str(Path(os.path.dirname(os.path.realpath(__file__))).parent.parent) + "/frontend/hstriverc/preproc/preproc"

shutil.copy(hstriverbasedir + "../domains/TimeDouble.bak", outpathdomains +'/TimeDouble')
shutil.copy(hstriverbasedir + "../domains/TimeUTC.bak", outpathdomains +'/TimeUTC')

for file in hstriverfiles:
    args = [preprocbin, outpath, file]
    cp = subprocess.run(args)
    if cp.returncode != 0:
        print("Error in file: "+str(file))
        sys.exit()

libfiles = list(Path(outpathlibs).rglob("*.hs"))
for file in libfiles:
    shutil.copy(str(file), hstriverbasedir+'/Lib/')

innerspecfiles = list(Path(outpathinnerspecs).rglob("*.hs"))
for file in innerspecfiles:
    shutil.copy(str(file), hstriverbasedir+'/INNERSPECSDIR/')

timetfile = outpathdomain + "/TimeT.hs" if os.path.isfile(outpathdomain + "/TimeT.hs") else outpathdomains + "/TimeUTC"
shutil.copy(timetfile, hstriverbasedir + "TimeDomain/TimeT.hs")

mainfile = outpath+"Main.hs"
if os.path.isfile(mainfile):
    shutil.copy(mainfile, hstriverbasedir)

print("Running Stack")
sys.stdout.flush()
if forcebuild:
    args = ["stack", "clean"]
    subprocess.run(args, cwd=hstriverbasedir)
args = ["stack", "install", "--local-bin-path", outpath]
cp = subprocess.run(args, cwd=hstriverbasedir)
if cp.returncode != 0:
    sys.exit("Error in Stack")

if os.path.isfile(mainfile):
    shutil.copy(outpath+"/HStriver",outbin)
