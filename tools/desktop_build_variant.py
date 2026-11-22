"""Resolve the validated Desktop GPR scenario to its artifact directory."""
import argparse
import os

def directory(env):
    options={
      'CUBIT_COMPOSITOR':(('legacy','mesa','vulkan'),'vulkan'),
      'CUBIT_INPUT_OVERLAY':(('off','on'),'off'),
      'CUBIT_COMPOSITOR_TIMING':(('off','on'),'off'),
      'CUBIT_COMPOSITOR_STORAGE':(('production','limited'),'production'),
      'CUBIT_DISPLAY_TEST_MODE':(('production','delayed'),'production'),
      'CUBIT_DESKTOP_FAULT':(('off','exit-once'),'off')}
    chosen={}
    for key,(values,default) in options.items():
        value=env.get(key,default)
        if value not in values:raise ValueError(f'{key}: expected one of {values}, got {value!r}')
        chosen[key]=value
    result='build-delayed' if chosen['CUBIT_DISPLAY_TEST_MODE']=='delayed' else 'build'
    if chosen['CUBIT_COMPOSITOR']!='legacy':result+='-'+chosen['CUBIT_COMPOSITOR']
    if chosen['CUBIT_INPUT_OVERLAY']=='on':result+='-input-overlay'
    if chosen['CUBIT_COMPOSITOR_TIMING']=='on':result+='-timing'
    if chosen['CUBIT_DESKTOP_FAULT']=='exit-once':result+='-fault-exit-once'
    if chosen['CUBIT_COMPOSITOR_STORAGE']=='limited':result+='-limited-storage'
    return result

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.parse_args()
    try:print(directory(os.environ))
    except ValueError as e:raise SystemExit(str(e))
