import os, resource, sys
resource.setrlimit(resource.RLIMIT_STACK, (67108864, 67108864))
resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
os.nice(19)
os.execv(sys.argv[1], sys.argv[1:])
