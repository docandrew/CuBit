#!/usr/bin/env python3
"""Rewrite a v1 keyword manifest (executable-manifest v1 ...) as a typed CCL
Executable_Manifest expression (docs/ccl-typed-manifests.md).

    convert-manifest.py MANIFEST.ccl            prints the typed form
    convert-manifest.py --in-place MANIFEST...  rewrites the files

Request order is kept (capability slots follow it), and each comment stays
with the declaration it precedes. The result must compile to the same ELF
sections; compare-tree.py --convert checks that for the whole tree.
"""
import sys


class Atom(str):
    pass


def tokens(text):
    i = 0
    while i < len(text):
        c = text[i]
        if c in " \t\r":
            i += 1
        elif c == "\n":
            yield ("newline", None)
            i += 1
        elif c == "#":
            j = text.find("\n", i)
            j = len(text) if j < 0 else j
            yield ("comment", text[i:j])
            i = j
        elif c in "()":
            yield (c, None)
            i += 1
        elif c == '"':
            j = i + 1
            while text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            yield ("atom", text[i:j + 1])
            i = j + 1
        else:
            j = i
            while j < len(text) and text[j] not in " \t\r\n()#":
                j += 1
            yield ("atom", text[i:j])
            i = j


def parse(text):
    """The top-level form as nested lists. Each form keeps the comment lines
    before it (comments) and a comment on the line where it ends (eol)."""
    stack = [Form([])]
    pending = []
    last_closed = None
    same_line = False
    for kind, value in tokens(text):
        if kind == "newline":
            same_line = False
        elif kind == "comment":
            if same_line and last_closed is not None:
                last_closed.eol = value
            else:
                pending.append(value)
        elif kind == "(":
            stack.append(Form(pending))
            pending = []
            same_line = False
        elif kind == ")":
            done = stack.pop()
            done.trailing = pending
            pending = []
            stack[-1].append(done)
            last_closed = done
            same_line = True
        else:
            stack[-1].append(Atom(value))
            same_line = False
    top = stack[0]
    if top:
        top[-1].after = pending
    return top


class Form(list):
    def __init__(self, comments):
        super().__init__()
        self.comments = list(comments)
        self.trailing = []
        self.after = []
        self.eol = None


def quoted(atom):
    return atom if atom.startswith('"') else '"%s"' % atom


ACCESS = {"read": "Access.Read", "write": "Access.Write", "read-write": "Access.Read_Write"}
NOTIFY = {"publish": "Notify_Access.Publish", "manage": "Notify_Access.Manage",
          "publish-and-manage": "Notify_Access.Publish_And_Manage"}
ACTION = {"tcp-connect": "Network_Action.TCP_Connect", "tcp-listen": "Network_Action.TCP_Listen",
          "udp-connect": "Network_Action.UDP_Connect"}
RIGHT = {"read": "File_Right.Read", "write": "File_Right.Write", "execute": "File_Right.Execute",
         "create": "File_Right.Create"}
PLATFORM = {"ps2-controller": "Platform_Device.PS2_Controller", "ata-primary": "Platform_Device.ATA_Primary",
            "cmos-rtc": "Platform_Device.CMOS_RTC"}


def keyed(form, key):
    """(key value): the value of a keyed sub-form."""
    if not isinstance(form, list) or len(form) != 2 or form[0] != key:
        raise ValueError("expected (%s ...)" % key)
    return form[1]


def look(table, atom):
    if atom not in table:
        raise ValueError("unknown word " + atom)
    return table[atom]


def scope(form, function):
    rights = form[1]
    if not isinstance(rights, list) or rights[0] != "rights":
        raise ValueError("expected (rights ...)")
    listed = "[" + " ".join(look(RIGHT, r) for r in rights[1:]) + "]"
    if form[2] == "all":
        return "(%s-all %s)" % (function, listed)
    return "(%s %s %s)" % (function, listed, render(form[2]))


def render(node):
    if isinstance(node, list):
        return "(" + " ".join(render(n) for n in node) + ")"
    return str(node)


def convert(text):
    top = parse(text)
    if len(top) != 1 or top[0][:2] != ["executable-manifest", "v1"]:
        raise ValueError("not a v1 executable manifest")
    manifest = top[0]
    fields = {}
    requests, scopes = [], []
    device = None
    none = False
    for form in manifest[2:]:
        head = form[0]
        notes = form.comments + ([form.eol] if form.eol else [])
        if head in ("identity", "version"):
            fields[head] = render(form[1])
            fields[head + "-notes"] = notes
        elif head == "request-service":
            _, name, rights, binding = form
            item = ("(service %s %s)" % (quoted(name), quoted(binding)) if rights == "read-write" else
                    "(Request.Service (Service_Request %s %s %s))" % (quoted(name), quoted(binding), look(ACCESS, rights)))
            requests.append((notes, item))
        elif head == "request-notification":
            _, name, rights, binding = form
            requests.append((notes, "(notification %s %s %s)" % (quoted(name), look(NOTIFY, rights), quoted(binding))))
        elif head == "request-framebuffer":
            _, rights, binding = form
            requests.append((notes, "(framebuffer %s)" % quoted(binding) if rights == "read-write" else
                             "(Request.Framebuffer (Framebuffer_Request %s %s))" % (quoted(binding), look(ACCESS, rights))))
        elif head == "request-render":
            _, rights, binding = form
            if rights != "read-write":
                raise ValueError("render requests are read-write")
            requests.append((notes, "(render %s)" % quoted(binding)))
        elif head == "request-network":
            _, action, ipv4, ports, dns, connections, binding = form
            if ipv4[0] != "ipv4" or ports[0] != "ports":
                raise ValueError("malformed request-network")
            resolve = {"allow": "true", "deny": "false"}[keyed(dns, "dns")]
            requests.append((notes, "(network %s %s %s %s %s %s %s %s)" % (
                look(ACTION, action), render(ipv4[1]), render(ipv4[2]), render(ports[1]), render(ports[2]),
                resolve, render(keyed(connections, "connections")), quoted(binding))))
        elif head == "filesystem-scope":
            scopes.append((notes, scope(form, "filesystem")))
        elif head == "config-scope":
            scopes.append((notes, scope(form, "config")))
        elif head == "tls-scope":
            scopes.append((notes, "(tls %s)" % render(form[1])))
        elif head == "match-pci-class":
            device = (notes, "(Device_Match.PCI_Class (PCI_Class %s))" % " ".join(render(v) for v in form[1:]))
        elif head == "match-pci-id":
            device = (notes, "(Device_Match.PCI_ID (PCI_ID %s))" % " ".join(render(v) for v in form[1:]))
        elif head == "platform-device":
            device = (notes, "(Device_Match.Platform %s)" % look(PLATFORM, form[1]))
        elif head == "device-memory":
            _, name, index, size, rights = form
            requests.append((notes, "(device-memory %s %s %s %s)" % (
                quoted(name), render(index[1]), render(keyed(size, "max-bytes")), look(ACCESS, rights))))
        elif head == "io-ports":
            _, name, index, count = form
            requests.append((notes, "(io-ports %s %s %s)" % (quoted(name), render(index[1]), render(keyed(count, "count")))))
        elif head == "interrupt":
            name = form[1]
            if isinstance(form[2], list):
                requests.append((notes, "(platform-interrupt %s %s)" % (quoted(name), render(keyed(form[2], "resource")))))
            elif form[2] == "line":
                requests.append((notes, "(interrupt %s Interrupt_Mode.Line 1)" % quoted(name)))
            else:
                mode = {"msix": "Interrupt_Mode.MSI_X", "msi": "Interrupt_Mode.MSI"}[form[2]]
                requests.append((notes, "(interrupt %s %s %s)" % (quoted(name), mode, render(keyed(form[3], "vectors")))))
        elif head == "dma":
            requests.append((notes, "(dma %s %s)" % (quoted(form[1]), render(keyed(form[2], "bytes")))))
        elif head == "request-scheduling":
            _, name, kind, budget, period = form
            if kind != "realtime":
                raise ValueError("scheduling requests are realtime")
            requests.append((notes, "(scheduling %s %s %s)" % (
                quoted(name), render(keyed(budget, "budget-us")), render(keyed(period, "period-us")))))
        elif head == "requests-none":
            none = True
        else:
            raise ValueError("unknown declaration " + head)

    out = []
    for line in manifest.comments:
        out.append(line)
    out.append("(Executable_Manifest")
    for name in ("identity", "version"):
        for line in fields.get(name + "-notes", []):
            out.append("  " + line)
        out.append("  %s => %s" % (name, fields[name]))

    def items(name, listed):
        if not listed:
            return
        out.append("  %s => [" % name)
        for notes, item in listed:
            for line in notes:
                out.append("    " + line)
            out.append("    " + item)
        out[-1] += "]"

    if device:
        for line in device[0]:
            out.append("  " + line)
        out.append("  device => " + device[1])
    items("requests", requests)
    items("scopes", scopes)
    if none:
        out.append("  requests_none => true")
    out[-1] += ")"
    for line in manifest.trailing:
        out.insert(len(out) - 1, "  " + line)
    if manifest.eol:
        out[-1] += "  " + manifest.eol
    for line in manifest.after:
        out.append(line)
    return "\n".join(out) + "\n"


def main():
    args = sys.argv[1:]
    if args and args[0] == "--in-place":
        for path in args[1:]:
            with open(path) as f:
                typed = convert(f.read())
            with open(path, "w") as f:
                f.write(typed)
        return 0
    if len(args) != 1:
        print(__doc__, file=sys.stderr)
        return 2
    try:
        with open(args[0]) as f:
            sys.stdout.write(convert(f.read()))
    except (ValueError, IndexError, KeyError) as error:
        print("convert-manifest: %s: %s" % (args[0], error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
