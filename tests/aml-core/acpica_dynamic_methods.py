"""Compare temporary method declarations and lifetime with pinned ACPICA."""
import argparse
from pathlib import Path
import re
import subprocess
ROOT = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--tools', type=Path, required=True)
TOOLS = parser.parse_args().tools
out = ROOT/'build/acpica-dynamic-methods'
out.mkdir(exist_ok=True)
cases = [
('duplicate', 'Method(MAIN,0,Serialized) { Store(2,Local0) While(Local0) { Method(TEMP,0) { Return(One) } Subtract(Local0,One,Local0) } Return(Zero) }','MAIN',[],'AE_ALREADY_EXISTS'),
('arguments', 'Method(MAIN,0,Serialized) { Method(TEMP,2) { Return(Add(Arg0,Arg1)) } Return(TEMP(11,13)) }','MAIN',[],24),
('empty', 'Method(MAIN,0,Serialized) { Method(TEMP,0) {} TEMP() Return(17) }','MAIN',[],17),
('order', 'Method(MAIN,0,Serialized,3) { Method(TEMP,0,Serialized,2) { Return(One) } Return(TEMP()) }','MAIN',[],'AE_AML_MUTEX_ORDER'),

('local', 'Method (MAIN,0,Serialized) { Method(TEMP,0) { Return(42) } Return(TEMP()) }', 'MAIN', [],42),
('recursive', 'Method(RECU,1,Serialized) { If(Arg0) { RECU(Zero) Return(\\TEMP()) } Method(\\TEMP,0) { Return(7) } Return(Zero) }', 'RECU', [1],7),
('expired', 'Method(MAKE,0,Serialized) { Method(\\TEMP,0) { Return(7) } Return(Zero) } Method(MAIN,0,Serialized) { MAKE() Return(\\TEMP()) }', 'MAIN', [],'AE_NOT_FOUND'),
('error', 'Method(MAIN,0,Serialized) { Method(TEMP,0) { Return(42) } Return(Divide(One,Zero)) }','MAIN',[],'AE_AML_DIVIDE_BY_ZERO'),
('repeat', 'Method(MAKE,0,Serialized) { Method(TEMP,0) { Return(42) } Return(TEMP()) } Method(MAIN,0,Serialized) { MAKE() Return(MAKE()) }','MAIN',[],42),
('siblings', 'Method(MAIN,0,Serialized) { Method(AAA0,0) { Return(BBB0()) } Method(BBB0,0) { Return(9) } Return(AAA0()) }','MAIN',[],9),
('parent', 'Method(MAIN,0,Serialized) { Method(^TEMP,0) { Return(7) } Return(\\TEMP()) }','MAIN',[],7),
('subtree', 'Method(SUB0,0,Serialized) { OUTR(Zero) Return(Zero) } Method(OUTR,1,Serialized) { If(Arg0) { SUB0() Return(\\SUB0.TEMP()) } Method(\\SUB0.TEMP,0) { Return(7) } Return(Zero) }','OUTR',[1],'AE_NOT_FOUND'),
('retained', 'Method(MIDL,1,Serialized) { If(Arg0) { HELP() Return(\\LIVE()) } Method(\\LIVE,0) { Return(9) } Return(Zero) } Method(HELP,0,Serialized) { Method(\\DEAD,0) { Return(Zero) } MIDL(Zero) Return(Zero) }','MIDL',[1],9),
]
status = {'AE_NOT_FOUND':'UNKNOWN_NAME','AE_AML_DIVIDE_BY_ZERO':'DIVISION_BY_ZERO','AE_ALREADY_EXISTS':'DUPLICATE_NAME','AE_AML_MUTEX_ORDER':'MUTEX_ORDER'}
checks=0
for rev in (1,2):
 for name,body,method,args,expected in cases:
    src=out/f'{name}-{rev}.asl'
    src.write_text(f'DefinitionBlock("","DSDT",{rev},"CUBIT","DYNAMIC",1) {{ {body} }}')
    def run(cmd,log):
        r=subprocess.run([str(x) for x in cmd],text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=60)
        log.write_text(r.stdout)
        return r
    compiled=run([TOOLS/'iasl','-oa',src],src.with_suffix('.compile.log'))
    if compiled.returncode: raise RuntimeError((name,compiled.stdout))
    ref=run([TOOLS/'acpiexec','-b','execute '+method+' '+ ' '.join(map(str,args)),src.with_suffix('.aml')],src.with_suffix('.reference.log'))
    actual=run([ROOT/'build/table_runner',src.with_suffix('.aml'),method,*map(str,args)],src.with_suffix('.actual.log'))
    if ref.returncode: raise RuntimeError(ref.stdout)
    if isinstance(expected,int):
        values=re.findall(r'\[Integer\] = ([0-9A-Fa-f]+)',ref.stdout)
        if len(values)!=1 or int(values[0],16)!=expected or actual.returncode or actual.stdout.strip()!=f'RESULT {expected}': raise RuntimeError((name,ref.stdout,actual.stdout))
    elif ('Evaluation of '+chr(92)+method+' failed with status '+expected) not in ref.stdout or actual.returncode==0 or 'execute: '+status[expected] not in actual.stdout:
        raise RuntimeError((name,ref.stdout,actual.stdout))
    checks+=1
print('AML-ACPICA-DYNAMIC-METHODS: PASS',checks)
