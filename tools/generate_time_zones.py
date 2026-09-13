#!/usr/bin/env python3
"""Compile bundled IANA rules into a typed, bounded native lookup (2000..2099).

Build-time only. ZoneInfo is supplied by Nix's pinned tzdata via PYTHONTZPATH.
The source timezone database remains unchanged; outputs live in build/.
"""
import argparse
from datetime import datetime, timezone
from pathlib import Path
import os
import zoneinfo
from zoneinfo import _common, _tzpath
from bisect import bisect_right

parser = argparse.ArgumentParser()
parser.add_argument('output', type=Path)
args = parser.parse_args()
if not os.environ.get('PYTHONTZPATH'):
    parser.error('use nix develop: pinned PYTHONTZPATH is required')
names = sorted(zoneinfo.available_timezones())
if 'UTC' not in names:
    parser.error('missing UTC in bundled database')
def identifier(name):
    return 'Zone_' + name.replace('_', '_Under_').replace('/', '_').replace(
        '-', '_Minus_').replace('+', '_Plus_')
ids = [identifier(name) for name in names]
assert len(set(i.lower() for i in ids)) == len(ids)
start = int(datetime(2000, 1, 1, tzinfo=timezone.utc).timestamp())
end = int(datetime(2100, 1, 1, tzinfo=timezone.utc).timestamp())
transitions = []
ranges = []
cache = {}
for name in names:
    z = zoneinfo.ZoneInfo(name)
    def offset(t):
        return int(datetime.fromtimestamp(t, timezone.utc).astimezone(z).utcoffset().total_seconds())
    # Sample at six-hour intervals and locate offset changes to the second.
    # This build supports the bundled 2000..2099 rules, not arbitrary TZif input.
    first = len(transitions) + 1
    old = offset(start)
    entries = [(start, old)]
    previous = start
    for t in range(start + 21600, end, 21600):
        new = offset(t)
        if new != old:
            lo, hi = previous, t
            while hi - lo > 1:
                middle = (lo + hi) // 2
                if offset(middle) == old:
                    lo = middle
                else:
                    hi = middle
            entries.append((hi, new))
            old = new
        previous = t
    # Check every explicit TZif transition, including both sides of each
    # boundary. Fail the build if the sampler missed a short-lived offset.
    # These private build-host helpers are tied to our pinned Nix Python;
    # neither Python nor a TZif parser ships in the clock service.
    with open(_tzpath.find_tzfile(name), 'rb') as source:
        _, explicit, _, _, _, _ = _common.load_data(source)
    times = [at for at, _ in entries]
    for at in explicit:
        for t in (at - 1, at):
            if start <= t < end:
                assert entries[bisect_right(times, t) - 1][1] == offset(t), (name, t)
    assert entries[-1][1] == offset(end - 1), name
    # Aliases share their complete transition table, but retain enum identities.
    key = tuple(entries)
    if key in cache:
        ranges.append(cache[key])
    else:
        transitions.extend(entries)
        ranges.append((first, len(transitions)))
        cache[key] = ranges[-1]
args.output.mkdir(parents=True, exist_ok=True)
spec = '''with Interfaces; use Interfaces;
package Time_Zones with SPARK_Mode, Pure is
   type Time_Zone is (\n'''
spec += ',\n'.join('      ' + i for i in ids) + ''');
   procedure Find (Name : String; Zone : out Time_Zone; Found : out Boolean);
   function Name (Zone : Time_Zone) return String;
   function Supported (UTC : Integer_64) return Boolean is
     (UTC >= 946_684_800 and then UTC < 4_102_444_800);
   function Offset (Zone : Time_Zone; UTC : Integer_64) return Integer_32
     with Pre => Supported (UTC);
end Time_Zones;
'''
body = '''package body Time_Zones with SPARK_Mode is
   type Transition is record
      At_UTC : Integer_64;
      Offset_Seconds : Integer_32;
   end record;
   Table : constant array (Positive range <>) of Transition := [\n'''
body += ',\n'.join(f'      ({t}, {o})' for t, o in transitions) + '];\n'
body += '   type Span is record First, Last : Positive; end record;\n'
body += '   Spans : constant array (Time_Zone) of Span := [\n'
body += ',\n'.join(f'      {i} => ({a}, {b})' for i, (a,b) in zip(ids, ranges)) + '];\n'
body += '   function Name (Zone : Time_Zone) return String is\n   begin\n      case Zone is\n'
body += ''.join(f'         when {i} => return "{n}";\n' for i,n in zip(ids,names))
body += '''      end case;
   end Name;
   procedure Find (Name : String; Zone : out Time_Zone; Found : out Boolean) is
   begin
      Zone := Zone_UTC; Found := False;
      for Candidate in Time_Zone loop
         if Name = Time_Zones.Name (Candidate) then
            Zone := Candidate; Found := True; return;
         end if;
      end loop;
   end Find;
   function Offset (Zone : Time_Zone; UTC : Integer_64) return Integer_32 is
      Low : Positive := Spans (Zone).First;
      High : Positive := Spans (Zone).Last;
      Middle : Positive;
   begin
      while Low < High loop
         Middle := Low + (High - Low + 1) / 2;
         if Table (Middle).At_UTC <= UTC then Low := Middle;
         else High := Middle - 1; end if;
      end loop;
      return Table (Low).Offset_Seconds;
   end Offset;
end Time_Zones;
'''
(args.output / 'time_zones.ads').write_text(spec)
(args.output / 'time_zones.adb').write_text(body)
print(f'Timezones: {len(names)} enum values, {len(transitions)} transitions, 2000..2099', flush=True)
