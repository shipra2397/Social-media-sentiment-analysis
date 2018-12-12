import os
import csv
import sys
import subprocess
command = 'C:/Program Files/R/R-3.4.3/bin/Rscript'
path = 'C:/Users/user/Desktop/minor2/pos-neg/posneg.r'
cmd = [command, path]


try:
   process = subprocess.Popen(cmd,shell = False)
   process.wait()
   print("\n")
   print("I hope you are happy with the results!!!")
except subprocess.CalledProcessError as e:
    raise RuntimeError("command '{}' return with error (code {}): {}".format(e.cmd, e.returncode, e.output))
os.system("pause")
    
