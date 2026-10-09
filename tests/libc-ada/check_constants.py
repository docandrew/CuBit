#!/usr/bin/env python3
"""CuBit.Kernel_ABI and the libc's C-ABI constants: each system-call number
against the kernel's enumeration (kernel/src/syscall.ads), each C-named
constant against musl's headers, and musl's struct pthread layout."""
import re, sys
from pathlib import Path
repo = Path(__file__).resolve().parents[2]
abi = (repo / "userspace/runtime/gnat/cubit-kernel_abi.ads").read_text()
kernel = (repo / "kernel/src/syscall.ads").read_text()
labels_text = (repo / "kernel/src/ipc_labels.ads").read_text()
def value(text):
    """An Ada literal: decimal, or based (16#FF#, 8#777#), underscores allowed."""
    text = text.replace("_", "")
    m = re.fullmatch(r"(\d+)#([0-9A-Fa-f]+)#", text)
    return int(m.group(2), int(m.group(1))) if m else int(text, 0)
known = {n: int(v) for n, v in re.findall(r"(SYSCALL_\w+)\s*=>\s*(\d+)\s*,", kernel)}
pairs = {
    "Exit_Process": "SYSCALL_EXIT", "Get_Process_Id": "SYSCALL_GETPID",
    "Grow_Heap": "SYSCALL_SBRK", "Write": "SYSCALL_WRITE", "Info": "SYSCALL_INFO",
    "Receive": "SYSCALL_RECEIVE", "Reply": "SYSCALL_REPLY",
    "Poll_Any_IPC": "SYSCALL_POLL_ANY_IPC",
    "Create_Shared_Memory_Grant_For_Process_Id": "SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID",
    "Wait_Completion": "SYSCALL_WAIT_COMPLETION",
    "Inspect_Capability": "SYSCALL_INSPECT_CAPABILITY",
    "Receive_Event_Nonblocking": "SYSCALL_RECEIVE_EVENT_NB",
    "Get_Time": "SYSCALL_GETTIME",
    "Call_Via_Endpoint_Capability": "SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY",
    "Submit_Via_Endpoint_Capability": "SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY",
    "Revoke_Shared_Memory_Grant": "SYSCALL_REVOKE_SHARED_MEMORY_GRANT",
    "Revoke_Shared_Memory_Grant_Reference": "SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE",
    "Reserve_Owned_Memory": "SYSCALL_RESERVE_OWNED_MEMORY",
    "Commit_Owned_Memory_Prefix": "SYSCALL_COMMIT_OWNED_MEMORY_PREFIX",
    "Release_Owned_Reservation": "SYSCALL_RELEASE_OWNED_RESERVATION",
    "Send_Control": "SYSCALL_SEND_CONTROL",
    "Thread_Exit": "SYSCALL_THREAD_EXIT", "Futex_Wait": "SYSCALL_FUTEX_WAIT",
    "Futex_Wake": "SYSCALL_FUTEX_WAKE",
    "Create_Shared_Memory_Grant_Via_Capability": "SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY",
    "Get_Owned_Shared_Memory_Grant_Generation": "SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION",
    "Acquire_Shared_Memory_Grant": "SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT",
    "Acquire_Shared_Memory_Grant_Via_Capability": "SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY",
    "Wait_For_IPC_Or_Completion_Until_Monotonic_Millisecond": "SYSCALL_WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND",
    "Read_Monotonic_Microseconds": "SYSCALL_READ_MONOTONIC_MICROSECONDS",
    "Allocate_Owned_Memory": "SYSCALL_ALLOCATE_OWNED_MEMORY",
    "Release_Owned_Memory": "SYSCALL_RELEASE_OWNED_MEMORY",
    "Protect_Owned_Memory": "SYSCALL_PROTECT_OWNED_MEMORY",
    "Yield": "SYSCALL_YIELD",
    "Sleep_Until_Monotonic_Microsecond": "SYSCALL_SLEEP_UNTIL_MONOTONIC_MICROSECOND",
}
ours = {n: value(v) for n, v in re.findall(
    r"(\w+)\s*:\s*constant\s+System_Call\s*:=\s*([\w#]+)\s*;", abi, re.S)}
bad = [f"{n}: Kernel_ABI {ours.get(n)}, kernel {known.get(m)}"
       for n, m in pairs.items() if ours.get(n) is None or ours.get(n) != known.get(m)]
bad += [f"{n}: not checked" for n in ours if n not in pairs]
# The grant and control events (CuBit.Control_Events) and control kinds,
# against kernel/src/ipc_labels.ads.
events = (repo / "userspace/runtime/gnat/cubit-control_events.ads").read_text()
for ours_name, theirs in (("Grant_Revoked_Label", "EVENT_GRANT_REVOKED"),
                          ("Grant_Returned_Label", "EVENT_GRANT_RETURNED"),
                          ("Control_Label", "EVENT_CONTROL")):
    a = re.search(r"\b" + ours_name + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", events)
    b = re.search(r"\b" + theirs + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", labels_text)
    if not a or not b or value(a.group(1)) != value(b.group(1)):
        bad.append(f"{ours_name} differs from the kernel's {theirs}")
a = re.search(r"for Control_Kind use \(([^)]*)\)", events)
b = re.search(r"for Control_Kind use \(([^)]*)\)", labels_text)
norm = lambda t: re.sub(r"Control_|\s", "", t)
if not a or not b or norm(a.group(1)) != norm(b.group(1)):
    bad.append("Control_Kind differs from the kernel's")
# A channel region fits one grant (CuBit.Channel_Contracts), as the kernel
# sizes them (Memory_Grants.Maximum_Page_Count).
contracts = (repo / "userspace/runtime/gnat/cubit-channel_contracts.ads").read_text()
grants = (repo / "kernel/src/memory_grants.ads").read_text()
a = re.search(r"Maximum_Region_Pages\s*:\s*constant\s*:=\s*([\w#]+)\s*;", contracts)
b = re.search(r"Maximum_Page_Count\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", grants)
if not a or not b or value(a.group(1)) != value(b.group(1)):
    bad.append("Channel_Contracts.Maximum_Region_Pages differs from Memory_Grants.Maximum_Page_Count")
# The futex results and owned-memory limit, against the kernel's.
futex = (repo / "kernel/src/process-futex.ads").read_text()
for ours_name, theirs in (("Futex_Woken", "FUTEX_WOKEN"), ("Futex_Retry", "FUTEX_RETRY"),
                          ("Futex_Timed_Out", "FUTEX_TIMED_OUT")):
    a = re.search(ours_name + r"\s*:\s*constant[^:]*:=\s*(\d+)", abi)
    b = re.search(theirs + r"\s*:\s*constant[^:]*:=\s*(\d+)", futex)
    if not a or not b or a.group(1) != b.group(1):
        bad.append(f"{ours_name} differs from the kernel's {theirs}")
messages = (repo / "userspace/runtime/gnat/cubit-messages.ads").read_text()
for ours_name, theirs in (("Wall_Clock_Offset", "SYSINFO_WALL_CLOCK_OFFSET"),
                          ("Registered_Driver", "SYSINFO_REGISTERED_DRIVER"),
                          ("Driver_Netstack", "DRIVER_NETSTACK")):
    a = re.search(ours_name + r"\s*:\s*constant[^:]*:=\s*(\d+)", abi)
    b = re.search(theirs + r"\s*:\s*constant[^:]*:=\s*(\d+)", messages)
    if not a or not b or a.group(1) != b.group(1):
        bad.append(f"{ours_name} differs from CuBit.Messages {theirs}")
labels = (repo / "kernel/src/ipc_labels.ads").read_text()
net_text = (repo / "userspace/libc/ada/cubit-libc_net.adb").read_text()
for name in ("OP_NET_SHUT",):
    a = re.search(name + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", net_text)
    b = re.search(name + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", labels)
    if not a or not b or value(a.group(1)) != value(b.group(1)):
        bad.append(f"{name} differs from kernel/src/ipc_labels.ads")
streams_ads = (repo / "userspace/runtime/gnat/cubit-streams.ads").read_text()
streams_adb = (repo / "userspace/runtime/gnat/cubit-streams.adb").read_text()
ours_streams = (repo / "userspace/libc/ada/cubit-libc_streams.adb").read_text()
#  The ring (CuBit.Stream_Regions) and the channel protocol
#  (CuBit.Channel_Protocol) are shared units; the limits are each side's.
for ours_name, theirs in (("Maximum_Streams", "MAX_STREAMS"),
                          ("Maximum_Subscribers", "MAX_SUBSCRIBERS")):
    a = re.search(r"\b" + ours_name + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", ours_streams)
    b = (re.search(r"\b" + theirs + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", streams_adb)
         or re.search(r"\b" + theirs + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", streams_ads))
    if not a or not b or value(a.group(1)) != value(b.group(1)):
        bad.append(f"{ours_name} differs from CuBit.Streams {theirs}")
for line in bad:
    print("FAIL:", line)
print(f"kernel-abi: {'PASS' if not bad else 'FAIL'} ({len(pairs)} system calls)")
failed = bool(bad)

# CuBit.Libc_ABI and CuBit.Linux_System_Calls: every constant named as a C
# macro has musl's value (musl's headers, as the libc build unpacks them).
musl = Path(__file__).resolve().parents[2] / "userspace/libc/build/musl-src"
if not musl.is_dir():
    print("FAIL: build the libc first (userspace/libc/build.sh): musl's headers")
    sys.exit(1)
headers = ""
for pattern in ("include/**/*.h", "arch/x86_64/bits/*.h*", "arch/generic/bits/*.h",
                "src/internal/futex.h", "src/network/lookup.h"):
    for f in musl.glob(pattern):
        headers += f.read_text(errors="ignore") + "\n"
defines = {}
for m in re.finditer(r"^[ \t]*#[ \t]*define[ \t]+(\w+)[ \t]+(\S.*?)[ \t]*(?:/\*.*)?$", headers, re.M):
    defines.setdefault(m.group(1), m.group(2))
def macro(name, depth=0):
    text = defines.get(name)
    if text is None or depth > 10:
        return None
    text = text.strip()
    if re.fullmatch(r"0[0-7]+", text):
        return int(text, 8)
    try:
        return int(text, 0)
    except ValueError:
        pass
    expr = re.sub(r"\b([A-Za-z_]\w*)\b",
                  lambda m: str(macro(m.group(1), depth + 1)), text)
    expr = re.sub(r"\b0([0-7]+)\b", lambda m: str(int(m.group(1), 8)), expr)
    try:
        return eval(expr.replace("U", "").replace("L", ""))
    except Exception:
        return None
libc_dir = Path(__file__).resolve().parents[2] / "userspace/libc/ada"
checked = 0
for unit, prefix in (("cubit-libc_abi.ads", ""), ("cubit-linux_system_calls.ads", "__NR_")):
    for name, text in re.findall(r"^\s+(SYS_\w+|[A-Z][A-Z0-9_]+)\s*:\s*constant[^:]*:=\s*(-?[\w#']+)\s*;",
                                 (libc_dir / unit).read_text(), re.M):
        if name.startswith("INSPECTED_"):
            continue                    # the filesystem service's, below
        want = macro(prefix + (name[4:] if prefix else name))
        if name == "RLIM_INFINITY":
            want, got = macro(name) % 2**64, 2**64 - 1
        else:
            got = value(text.lstrip("-")) * (-1 if text.startswith("-") else 1)
        if want is None and name == "AF_UNSPEC":
            want = macro("PF_UNSPEC")
        checked += 1
        if want != got:
            print(f"FAIL: {unit} {name} = {got}, musl {want}")
            failed = True

# musl's struct pthread: the fields before tid, as Pthread_Tid_Offset counts.
impl = (musl / "src/internal/pthread_impl.h").read_text()
body = impl[impl.index("struct pthread {"):]
before_tid = body[:body.index("int tid;")]
fields = re.findall(r"^\s*(?:struct pthread \*|uintptr_t \*?)\s*([\w, *]+);", before_tid, re.M)
names = [n.strip(" *") for group in fields for n in group.split(",")]
expected = ["self", "dtv", "prev", "next", "sysinfo", "canary_pad", "canary"]
if [n for n in names if n != "canary_pad"] != [n for n in expected if n != "canary_pad"]:
    print(f"FAIL: musl struct pthread fields before tid changed: {names}")
    failed = True
# The filesystem service's open options and Directory.Page.V1 layout, as
# the libc's Ada repeats them, against CuBit.Filesystems.
fs = (repo / "userspace/runtime/gnat/cubit-filesystems.ads").read_text()
def fs_constant(name):
    m = re.search(r"\b" + name + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", fs)
    return value(m.group(1)) if m else None
rules = (libc_dir / "cubit-libc_descriptor_rules.ads").read_text()
entries = (libc_dir / "cubit-libc_directory_entries.ads").read_text()
def ours_constant(text, name):
    m = re.search(r"\b" + name + r"\s*:\s*constant[^:]*:=\s*([\w#]+)\s*;", text)
    return value(m.group(1)) if m else None
abi_text = (libc_dir / "cubit-libc_abi.ads").read_text()
for name in ("INSPECTED_SIZE", "INSPECTED_TIMES", "INSPECTED_MODE", "INSPECTED_LINKS",
             "INSPECTED_OWNER", "INSPECTED_OBJECT"):
    checked += 1
    if ours_constant(abi_text, name) != fs_constant(name):
        print(f"FAIL: {name} differs from CuBit.Filesystems")
        failed = True
for name in ("OPEN_READ_ONLY", "OPEN_WRITE_ONLY", "OPEN_READ_WRITE", "OPEN_CREATE",
             "OPEN_TRUNCATE", "OPEN_EXCLUSIVE"):
    checked += 1
    if ours_constant(rules, name) != fs_constant(name):
        print(f"FAIL: {name} differs from CuBit.Filesystems")
        failed = True
for ours_name, theirs in (("Page_Bytes", "DIRECTORY_PAGE_BYTES"),
                          ("Page_Header_Bytes", "DIRECTORY_PAGE_HEADER_BYTES"),
                          ("Entry_Bytes", "DIRECTORY_ENTRY_BYTES"),
                          ("Maximum_Entries", "MAXIMUM_DIRECTORY_PAGE_ENTRIES"),
                          ("Page_End", "DIRECTORY_PAGE_END"),
                          ("Kind_File", "DIRECTORY_KIND_FILE"),
                          ("Kind_Directory", "DIRECTORY_KIND_DIRECTORY"),
                          ("Kind_Symlink", "DIRECTORY_KIND_SYMLINK")):
    checked += 1
    if ours_constant(entries, ours_name) != fs_constant(theirs):
        print(f"FAIL: {ours_name} differs from CuBit.Filesystems {theirs}")
        failed = True
for field, offset in (("objectHint", 0), ("nameLength", 16), ("kind", 18), ("name", 24)):
    checked += 1
    if not re.search(field + r"\s+at\s+" + str(offset) + r"\b", fs):
        print(f"FAIL: Directory_Entry {field} is not at {offset}")
        failed = True
# musl's struct stat field order (x86-64), as the libc's Ada lays it out.
stat_h = (musl / "arch/x86_64/bits/stat.h").read_text()
fields = re.findall(r"^\s*[\w ]+?\s+\**(\w+)(?:\[\d+\])?;", stat_h, re.M)
want = ["st_dev", "st_ino", "st_nlink", "st_mode", "st_uid", "st_gid", "__pad0",
        "st_rdev", "st_size", "st_blksize", "st_blocks", "st_atim", "st_mtim",
        "st_ctim", "__unused"]
checked += 1
if fields != want:
    print(f"FAIL: musl struct stat fields changed: {fields}")
    failed = True
# The rest of struct pthread up to guard_size (CuBit.Libc_Threads' offsets
# 56, 88, 96, 104), the detach states and pthread_attr_t's macros.
part2 = body[body.index("int tid;"):body.index("void *result;")]
fields2 = [re.sub(r"[\s*]+", " ", f).strip() for f in part2.split(";") if f.strip()]
want2 = ["int tid", "int errno_val", "volatile int detach_state", "volatile int cancel",
         "volatile unsigned char canceldisable, cancelasync", "unsigned char tsd_used:1",
         "unsigned char dlerror_flag:1", "unsigned char map_base", "size_t map_size",
         "void stack", "size_t stack_size", "size_t guard_size"]
checked += 1
if fields2 != want2:
    print(f"FAIL: musl struct pthread fields changed: {fields2}")
    failed = True
checked += 1
if not re.search(r"DT_EXITED = 0,\s*DT_EXITING,\s*DT_JOINABLE,\s*DT_DETACHED,", impl):
    print("FAIL: musl detach states changed")
    failed = True
for macro_line in ("#define _a_stacksize __u.__s[0]", "#define _a_guardsize __u.__s[1]",
                   "#define _a_stackaddr __u.__s[2]", "#define _a_detach __u.__i[3*__SU+0]"):
    checked += 1
    if macro_line not in impl:
        print(f"FAIL: musl pthread_attr_t macro changed: {macro_line}")
        failed = True
print(f"libc-abi: {'PASS' if not failed else 'FAIL'} ({checked} constants against musl)")
sys.exit(1 if failed else 0)
