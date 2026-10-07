"""Resolve the validated Desktop GPR scenario to its artifact directory."""
import argparse
import os

def directory(metrics,env):
    options={
      'CUBIT_COMPOSITOR':(('legacy','mesa','vulkan'),'legacy'),
      'CUBIT_COMPOSITOR_TIMING':(('off','on'),'off'),
      'CUBIT_COMPOSITOR_STORAGE':(('production','limited'),'production'),
      'CUBIT_DISPLAY_TEST_MODE':(('production','delayed'),'production')}
    chosen={}
    for key,(values,default) in options.items():
        value=env.get(key,default)
        if value not in values:raise ValueError(f'{key}: expected one of {values}, got {value!r}')
        chosen[key]=value
    if metrics not in ('off','on'):raise ValueError('metrics must be off or on')
    result='build-delayed' if chosen['CUBIT_DISPLAY_TEST_MODE']=='delayed' else 'build'
    if chosen['CUBIT_COMPOSITOR']!='legacy':result+='-'+chosen['CUBIT_COMPOSITOR']
    if chosen['CUBIT_COMPOSITOR_TIMING']=='on':result+='-timing'
    if metrics=='on':result+='-metrics'
    if chosen['CUBIT_COMPOSITOR_STORAGE']=='limited':result+='-limited-storage'
    return result

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--metrics',choices=('off','on'),required=True)
    args=p.parse_args()
    try:print(directory(args.metrics,os.environ))
    except ValueError as e:raise SystemExit(str(e))
