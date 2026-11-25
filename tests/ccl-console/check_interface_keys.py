#!/usr/bin/env python3
"""CCL interfaces whose types are CCL source (TYPE_SOURCE) carry schema keys
precomputed from it: SHA-256 of the source, and of the source, "#" and each
bound type's name. A change to the source without new keys fails here."""
import hashlib
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SRC = ROOT / 'userspace/ccl/src'
CHECKS = [
    (SRC / 'ccl-interfaces-processes.ads',
     {'DIGEST': None, 'PROCESS_KEY': 'Process', 'PROCESSES_KEY': 'List-Process'}),
    (SRC / 'ccl-interfaces-wifi.ads',
     {'DIGEST': None, 'NETWORK_KEY': 'Wifi_Network', 'NETWORKS_KEY': 'List-Wifi_Network',
      'STATUS_KEY': 'Wifi_Status'}),
    (SRC / 'ccl-interfaces-files.ads',
     {'DIGEST': None, 'KIND_KEY': 'File_Kind', 'PLACE_KEY': 'Place', 'CHILD_KEY': 'Child',
      'METADATA_KEY': 'File_Metadata', 'LISTING_KEY': 'List-File_Metadata', 'EXCERPT_KEY': 'Excerpt',
      'TEXT_KEY': 'String'}),
    (SRC / 'ccl-interfaces-console.ads',
     {'DIGEST': None, 'NOTATION_KEY': 'Notation', 'THEME_KEY': 'Theme', 'STATS_KEY': 'Console_Stats'}),
    (SRC / 'ccl-interfaces-images.ads',
     {'DIGEST': None, 'IMAGE_KEY': 'Image', 'SIZE_KEY': 'Size', 'SERIES_KEY': 'List-Integer',
      'GRID_KEY': 'Grid', 'IMAGES_KEY': 'List-Image', 'SCALED_KEY': 'Scaled'}),
    (SRC / 'ccl-interfaces-display.ads', {'DIGEST': None, 'AREA_KEY': 'Capture_Area'}),
    (SRC / 'ccl-interfaces-logs.ads',
     {'DIGEST': None, 'SCHEMA_KEY': 'List-LogItem', 'SEVERITY_KEY': 'Severity', 'WINDOW_KEY': 'LogWindow'}),
    (SRC / 'ccl-interfaces-lexicon.ads',
     {'DIGEST': None, 'DEFINITION_KEY': 'Definition', 'THESAURUS_KEY': 'Thesaurus_Result',
      'SYNONYMS_KEY': 'Synonyms', 'SPELLINGS_KEY': 'Near_Spellings',
      'MISSPELLINGS_KEY': 'List-Misspelling', 'PAGE_KEY': 'Page_Query'}),
    ((SRC / 'ccl-interfaces-calc.ads', SRC / 'ccl-interfaces-calc_types.ads'),
     {'DIGEST': None, 'FORM_KEY': 'Calc_Form', 'NUMBER_KEY': 'Calc_Number', 'OPTIONS_KEY': 'Calc_Options',
      'REQUEST_KEY': 'Calc_Request'}),
    (SRC / 'ccl-interfaces-install.ads',
     {'DIGEST': None, 'DISK_KEY': 'Disk', 'DISKS_KEY': 'List-Disk', 'REQUEST_KEY': 'Install_Request',
      'REPORT_KEY': 'Install_Report', 'PROGRESS_KEY': 'Install_Progress'}),
    (SRC / 'ccl-interfaces-llm.ads',
     {'DIGEST': None, 'REQUEST_KEY': 'Llm_Request', 'EXCHANGE_KEY': 'Llm_Exchange', 'REPLY_KEY': 'Llm_Reply'}),
    (SRC / 'ccl-interfaces-secrets.ads',
     {'DIGEST': None, 'SECRET_KEY': 'Secret', 'STATE_KEY': 'Secret_State'}),
    (SRC / 'ccl-interfaces-bible.ads',
     {'DIGEST': None, 'PARSE_KEY': 'Parse_Result', 'VERSE_KEY': 'Verse_Result',
      'PASSAGE_RESULT_KEY': 'Passage_Result', 'SEARCH_KEY': 'Search_Result', 'REFERENCE_KEY': 'Reference',
      'PASSAGE_KEY': 'Passage', 'VERSE_QUERY_KEY': 'Verse_Query', 'PASSAGE_QUERY_KEY': 'Passage_Query',
      'SEARCH_QUERY_KEY': 'Search_Query'}),
    (SRC / 'ccl-interfaces-machine.ads', {'DIGEST': None, 'ANSWER_KEY': 'Power_Answer'}),
    (SRC / 'ccl-interfaces-power.ads',
     {'DIGEST': None, 'SOURCE_KEY': 'Power_Source', 'SOURCES_KEY': 'List-Power_Source'}),
    (SRC / 'ccl-interfaces-graphics.ads',
     {'DIGEST': None, 'ENTRY_KEY': 'Gfx_Entry', 'ENTRIES_KEY': 'List-Gfx_Entry'}),
    (SRC / 'ccl-interfaces-metrics.ads',
     {'DIGEST': None, 'SERIES_KEY': 'List-Metric_Series', 'ITEMS_KEY': 'List-Metric_Item',
      'SELECTOR_KEY': 'Metric_Selector', 'RANGE_KEY': 'Metric_Range', 'WINDOW_KEY': 'Metric_Window',
      'STATS_KEY': 'Metric_Stats'}),
    (SRC / 'ccl-interfaces-debug.ads',
     {'DIGEST': None, 'SESSION_KEY': 'Debug_Session', 'ATTACH_QUERY_KEY': 'Attach_Query',
      'THREADS_KEY': 'List-Debug_Thread', 'FRAME_QUERY_KEY': 'Frame_Query', 'FRAMES_KEY': 'List-Debug_Frame',
      'LOCALS_QUERY_KEY': 'Locals_Query', 'BINDING_KEY': 'Debug_Binding', 'BINDINGS_KEY': 'List-Debug_Binding',
      'VALUE_QUERY_KEY': 'Value_Query', 'CHILDREN_QUERY_KEY': 'Children_Query', 'GRAPH_QUERY_KEY': 'Graph_Query',
      'GRAPH_KEY': 'Debug_Graph', 'MEMORY_QUERY_KEY': 'Memory_Query', 'MEMORY_KEY': 'Debug_Memory'}),
    (SRC / 'ccl-interfaces-agent.ads',
     {'DIGEST': None, 'REQUEST_KEY': 'Agent_Request', 'SESSION_KEY': 'Agent_Session',
      'REPLY_KEY': 'Agent_Reply'}),
    (SRC / 'ccl-interfaces-net.ads',
     {'DIGEST': None, 'REQUEST_KEY': 'Fetch_Request', 'FETCH_KEY': 'Fetch', 'REPLY_KEY': 'Fetch_Reply'}),
    (SRC / 'ccl-interfaces-archive.ads',
     {'DIGEST': None, 'REQUEST_KEY': 'Unpack_Request', 'REPLY_KEY': 'Unpack_Reply'}),
    (SRC / 'ccl-interfaces-git.ads',
     {'DIGEST': None, 'REQUEST_KEY': 'Clone_Request', 'CLONE_KEY': 'Git_Clone', 'REPLY_KEY': 'Git_Reply'})]


def constant(text, name):
    body = re.search(name + r' : constant [\w.]+ :=\s*\[([^\]]*)\]', text).group(1)
    return ''.join(re.findall(r'16#([0-9A-F_]+)#', body)).replace('_', '').lower()


def main():
    failures = 0
    for paths, keys in CHECKS:
        # The keys' unit, and the unit that spells TYPE_SOURCE out (the same
        # one unless a spec-only unit holds the types).
        path, source_path = paths if isinstance(paths, tuple) else (paths, paths)
        text = path.read_text()
        source = re.search(r'TYPE_SOURCE : constant String :=\s*(.*?);\n', source_path.read_text(), re.S).group(1)
        source = ''.join(part.replace('""', '"') for part in re.findall(r'"((?:[^"]|"")*)"', source))
        for name, type_name in keys.items():
            expected = hashlib.sha256(source.encode() + (b'#' + type_name.encode() if type_name else b'')).hexdigest()
            if constant(text, name) != expected:
                print('FAIL: %s %s is not SHA-256 of its TYPE_SOURCE' % (path.name, name))
                failures += 1
    if not failures:
        print('PASS: interface schema keys match their CCL type source')
    return 1 if failures else 0


if __name__ == '__main__':
    sys.exit(main())
