import os,resource,sys
resource.setrlimit(resource.RLIMIT_AS,(134217728,134217728))
resource.setrlimit(resource.RLIMIT_STACK,(8388608,8388608))
resource.setrlimit(resource.RLIMIT_CORE,(0,0))
os.nice(19)
os.execv(sys.argv[1],sys.argv[1:])
