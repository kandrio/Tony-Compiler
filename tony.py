import sys
import argparse
import subprocess
import shutil
import os

def cleanup():
    if(os.path.isfile("out.ll")):
        os.remove("out.ll")
    if os.path.isfile("_tmp_.tony"):
        os.remove("_tmp_.tony")
    if os.path.isfile("out.s"):
        os.remove("out.s")
    if os.path.isfile("exec.o"):
        os.remove("exec.o")



parser = argparse.ArgumentParser(description='Compile tony file')
parser.add_argument('-O', dest='O_option', action='store_true', default=False, help="Enable optimizations.")

parser.add_argument('-f', dest='f_option', action='store_true', default=False, help="Use stdin to input program. Final code printed on stdout.")
parser.add_argument('-i', dest='i_option', action='store_true', default=False, help="Use stdin to input program. Final code printed on stdout.")
parser.add_argument('file',type=str, action='store', default=None, nargs="?", help="Input .tony file")
args = parser.parse_args()

if(not args.f_option and not args.i_option and args.file == None):
        print("No input file provided and option -i or -f not set. Use option -h for help.")
        exit(1)

if((args.f_option or args.i_option) and args.file != None):
        print("Option -i or -f set and input file provided, expected input from stdin. Use option -h for help.")
        exit(1)

if args.file == None:
    #either i or f option provided
    filestr = ""
    for line in sys.stdin:
        filestr+=line
    
    f = open("_tmp_.tony", "w")
    f.write(filestr)
    f.close()

    fileIn = "_tmp_.tony"

else:
    if not args.file.endswith(".tony"):
        print("Unsupported file type. Programs must end with extension \".tony\"")
        exit(1)
    fileIn = args.file
    filepath = args.file.rsplit('.tony', 1)[0]
    imm_path = filepath+".imm"
    final_path = filepath+".asm"
    executable_path = filepath+".o"

o_option = "-O" if args.O_option else ""

cmd = "./tonyc "+fileIn+" "+o_option
p = subprocess.Popen(cmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE, shell=True)
output, errors = p.communicate()

if p.wait()!=0:
    print(errors.decode("utf-8"))
    cleanup()
    exit(1)



p = subprocess.Popen("llc-10 out.ll -o out.s", stdout=subprocess.PIPE,stderr=subprocess.PIPE, shell=True)
output, errors = p.communicate()
if p.wait()!=0:
    print(errors.decode("utf-8"))
    cleanup()
    exit(1)

p = subprocess.Popen("clang -o exec.o out.s lib.a -lgc", stdout=subprocess.PIPE,stderr=subprocess.PIPE, shell=True)
output, errors = p.communicate()

if p.wait()!=0:
    print(errors.decode("utf-8"))
    cleanup()
    exit(1)

if args.i_option:
    p = subprocess.Popen("cat out.ll", shell=True)
    if p.wait()!=0:
        cleanup()
        exit(1) 
    
if args.f_option:
    p = subprocess.Popen("cat out.s", shell=True)
    if p.wait()!=0:
        cleanup()
        exit(1) 

if not args.f_option and not args.i_option:
    shutil.move("out.ll", imm_path)
    shutil.move("out.s", final_path)
    shutil.move("exec.o", executable_path)
    
cleanup()




