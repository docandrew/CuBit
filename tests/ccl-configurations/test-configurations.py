#!/usr/bin/env python3
"""Checked CCL configuration output and equivalence to the old boot profiles."""
import pathlib
import random
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
import os
# CCL_CONFIG_TOOL: a ccl-config built elsewhere (a private build tree).
TOOL = pathlib.Path(os.environ.get('CCL_CONFIG_TOOL', ROOT / 'userspace/ccl/build/config/ccl-config'))
FIXTURES = pathlib.Path(__file__).with_name('fixtures')


class Configurations(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='ccl-config-test.')
        self.addCleanup(self.temp.cleanup)
        self.source = pathlib.Path(self.temp.name) / 'profile.ccl'

    def compile(self, text):
        self.source.write_text(text)
        result = subprocess.run([TOOL, self.source, "--dump-plan"], text=True, capture_output=True, timeout=5)
        self.assertNotIn('raised ', result.stderr)
        return result

    def reject(self, text, diagnostic=None):
        result = self.compile(text)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(result.stdout, '', 'failed compile emitted partial config')
        if diagnostic:
            self.assertIn(diagnostic, result.stderr)

    def test_render_requires_explicit_approval(self):
        base = '(startup v1 (start "render.app" %s))'
        for clause in ('', '(render deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'render.app\n')
        result = self.compile(base % '(render approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'render.app render=declared\n')
        # HW-002: the approval delegated to a GPU_Rendering catalog setting.
        result = self.compile(base % '(render setting "desktop.render.gpu")')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'render.app render=setting:desktop.render.gpu\n')
        for clause in ('(render all)', '(render true)', '(render 1)',
                       '(render)', '(render approve-declared extra)',
                       '(render setting)', '(render setting "gpu.power.waitboost")',
                       '(render setting "desktop.render.gpu.x")', '(render setting desktop)'):
            self.reject(base % clause)
        for first in ('deny', 'approve-declared'):
            for second in ('deny', 'approve-declared'):
                self.reject(base % f'(render {first}) (render {second})',
                            'DUPLICATE_FIELD')

    def test_firmware_snapshot_requires_explicit_approval(self):
        base = '(startup v1 (start "acpi.svc" %s))'
        for clause in ('', '(firmware-snapshot deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'acpi.svc\n')
        result = self.compile(base % '(firmware-snapshot approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'acpi.svc firmware-snapshot=approved\n')
        for clause in ('(firmware-snapshot)', '(firmware-snapshot all)',
                       '(firmware-snapshot approve-declared extra)', '(firmware-snapshot "tables")'):
            self.reject(base % clause)
        self.reject(base % '(firmware-snapshot deny) (firmware-snapshot approve-declared)',
                    'DUPLICATE_FIELD')

    def test_acpi_approvals_require_explicit_entries(self):
        """(acpi-hardware approve-declared): explicit, once, and combinable
        with the firmware snapshot reader's approval on one entry. The one
        acpi.svc (ACPI-ONE-001) holds the hardware itself; the former
        role-35 (acpi-registers ...) approval no longer exists."""
        base = '(startup v1 (start "acpi.svc" %s))'
        for clause in ('', '(acpi-hardware deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'acpi.svc\n')
        result = self.compile(base % '(firmware-snapshot approve-declared) (acpi-hardware approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'acpi.svc firmware-snapshot=approved acpi-hardware=approved\n')
        for clause in ('(acpi-hardware)', '(acpi-hardware all)', '(acpi-hardware approve-declared extra)',
                       '(acpi-hardware "hardware")'):
            self.reject(base % clause)
        self.reject(base % '(acpi-hardware deny) (acpi-hardware approve-declared)', 'DUPLICATE_FIELD')
        for clause in ('(acpi-registers approve-declared)', '(acpi-registers deny)'):
            self.reject(base % clause)

    def test_trace_approval_is_explicit(self):
        """(trace approve-declared) (Trace_ABI): off unless written, once;
        the manifest's Request.Trace alone never installs the object."""
        base = '(startup v1 (start "bench-ipc-client.app" %s))'
        for clause in ('', '(trace deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'bench-ipc-client.app\n')
        result = self.compile(base % '(platform-power approve-declared) (trace approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'bench-ipc-client.app platform-power=approved trace=approved\n')
        for clause in ('(trace)', '(trace all)', '(trace approve-declared extra)', '(trace "trace")'):
            self.reject(base % clause)
        self.reject(base % '(trace deny) (trace approve-declared)', 'DUPLICATE_FIELD')

    def test_test_console_approval_is_explicit(self):
        """(test-console approve-declared) (Test_Console_ABI, ABI-002): off
        unless written, once; the manifest's request alone never installs
        the object (test plans approve it for logstore's test variant)."""
        base = '(startup v1 (start "logstore.svc" %s))'
        for clause in ('', '(test-console deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'logstore.svc\n')
        result = self.compile(base % '(test-console approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'logstore.svc test-console=approved\n')
        for clause in ('(test-console)', '(test-console all)', '(test-console approve-declared extra)',
                       '(test-console "test-console")'):
            self.reject(base % clause)
        self.reject(base % '(test-console deny) (test-console approve-declared)', 'DUPLICATE_FIELD')

    def test_vm_create_approval_is_explicit(self):
        """(vm-create approve-declared) (VMM-001, docs/hypervisor.md): off
        unless written, once, beside the entry's other approvals; the
        manifest's Kernel_Object.VM_Create request alone never installs the
        authority, and the approval alone installs nothing undeclared."""
        base = '(startup v1 (start "vmm.app" %s))'
        for clause in ('', '(vm-create deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'vmm.app\n')
        result = self.compile(base % '(vm-create approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'vmm.app vm-create=approved\n')
        result = self.compile(base % '(test-console approve-declared) (vm-create approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'vmm.app test-console=approved vm-create=approved\n')
        for clause in ('(vm-create)', '(vm-create all)', '(vm-create approve-declared extra)',
                       '(vm-create "vm-create")', '(vm approve-declared)'):
            self.reject(base % clause)
        self.reject(base % '(vm-create deny) (vm-create approve-declared)', 'DUPLICATE_FIELD')

    def test_platform_power_approval_is_explicit(self):
        """(platform-power approve-declared) (KERN-004): off unless written,
        once, beside the entry's other approvals; the manifest's
        Request.Platform_Power alone never installs the object."""
        base = '(startup v1 (start "acpi.svc" %s))'
        for clause in ('', '(platform-power deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'acpi.svc\n')
        result = self.compile(base % '(acpi-hardware approve-declared) (platform-power approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'acpi.svc acpi-hardware=approved platform-power=approved\n')
        for clause in ('(platform-power)', '(platform-power all)',
                       '(platform-power approve-declared extra)', '(platform-power "power")'):
            self.reject(base % clause)
        self.reject(base % '(platform-power deny) (platform-power approve-declared)', 'DUPLICATE_FIELD')

    def test_display_capture_approval_is_explicit(self):
        """(display-capture approve-declared) (REMOTE-001 screenshots): off
        unless written, once, beside the entry's other approvals; the
        manifest's request alone never grants Desktop's capture badge."""
        base = ('(startup v1 (start "ccl-remote.app" (network approve-declared) %s))')
        for clause in ('', '(display-capture deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertNotIn('display-capture', result.stdout)
        result = self.compile(base % '(display-capture approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(' display-capture=approved', result.stdout)
        for clause in ('(display-capture)', '(display-capture all)',
                       '(display-capture approve-declared extra)', '(display-capture "screen")'):
            self.reject(base % clause)
        self.reject(base % '(display-capture deny) (display-capture approve-declared)', 'DUPLICATE_FIELD')

    def test_acpi_pci_config_approval_is_opt_in(self):
        """(acpi-pci-config approve-declared) (ACPI-PCI-001): explicit, once,
        and only beside (acpi-hardware approve-declared): alone it would
        approve a window of a grant the entry does not have."""
        base = '(startup v1 (start "acpi.svc" %s))'
        hardware = '(firmware-snapshot approve-declared) (acpi-hardware approve-declared)'
        result = self.compile(base % hardware)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('acpi-pci-config', result.stdout)
        result = self.compile(base % (hardware + ' (acpi-pci-config deny)'))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'acpi.svc firmware-snapshot=approved acpi-hardware=approved\n')
        result = self.compile(base % (hardware + ' (acpi-pci-config approve-declared)'))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'acpi.svc firmware-snapshot=approved acpi-hardware=approved'
                         ' acpi-pci-config=approved\n')
        self.reject(base % '(acpi-pci-config approve-declared)', 'INVALID_APPROVAL')
        self.reject(base % '(acpi-hardware deny) (acpi-pci-config approve-declared)',
                    'INVALID_APPROVAL')
        for clause in ('(acpi-pci-config)', '(acpi-pci-config all)',
                       '(acpi-pci-config approve-declared extra)', '(acpi-pci-config "hardware")'):
            self.reject(base % (hardware + ' ' + clause))
        self.reject(base % (hardware + ' (acpi-pci-config deny) (acpi-pci-config approve-declared)'),
                    'DUPLICATE_FIELD')

    def test_acpi_firmware_data_approval_is_opt_in(self):
        """(acpi-firmware-data approve-declared) (table-named firmware data,
        CuBit.Firmware_Data_Exchange): explicit, once, and only beside
        (acpi-hardware approve-declared), like acpi-pci-config."""
        base = '(startup v1 (start "acpi.svc" %s))'
        hardware = '(firmware-snapshot approve-declared) (acpi-hardware approve-declared)'
        result = self.compile(base % hardware)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('acpi-firmware-data', result.stdout)
        result = self.compile(base % (hardware + ' (acpi-firmware-data deny)'))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'acpi.svc firmware-snapshot=approved acpi-hardware=approved\n')
        result = self.compile(base % (hardware + ' (acpi-firmware-data approve-declared)'))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'acpi.svc firmware-snapshot=approved acpi-hardware=approved'
                         ' acpi-firmware-data=approved\n')
        result = self.compile(base % (hardware + ' (acpi-pci-config approve-declared)'
                                      ' (acpi-firmware-data approve-declared)'))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'acpi.svc firmware-snapshot=approved acpi-hardware=approved'
                         ' acpi-pci-config=approved acpi-firmware-data=approved\n')
        self.reject(base % '(acpi-firmware-data approve-declared)', 'INVALID_APPROVAL')
        self.reject(base % '(acpi-hardware deny) (acpi-firmware-data approve-declared)',
                    'INVALID_APPROVAL')
        for clause in ('(acpi-firmware-data)', '(acpi-firmware-data all)',
                       '(acpi-firmware-data approve-declared extra)',
                       '(acpi-firmware-data "hardware")'):
            self.reject(base % (hardware + ' ' + clause))
        self.reject(base % (hardware + ' (acpi-firmware-data deny)'
                            ' (acpi-firmware-data approve-declared)'), 'DUPLICATE_FIELD')

    def test_no_shipped_plan_approves_firmware_data(self):
        """Only the headless firmware-data test approves it: every image and
        init plan leaves it off."""
        for plan in sorted(ROOT.glob('**/*.ccl')):
            relative = plan.relative_to(ROOT).as_posix()
            if relative.startswith(('.', 'tests/headless/init-acpi-firmware-data.ccl')) or '/build/' in relative:
                continue
            text = plan.read_text(errors='replace')
            self.assertNotIn('(acpi-firmware-data approve-declared)', text, relative)

    def test_every_shipped_startup_plan_compiles(self):
        """Every startup plan an image or test boots compiles. The laptop
        build plans outgrew the 16-program table when metrics.svc and the
        Metrics app joined (image 24), and only the ISO build noticed."""
        plans = []
        for plan in sorted(ROOT.glob('**/*.ccl')):
            relative = plan.relative_to(ROOT).as_posix()
            if relative.startswith('.') or '/build/' in relative or '/fixtures/' in relative:
                continue
            text = plan.read_text(errors='replace')
            if '(startup v1' not in text:
                continue
            plans.append(relative)
            with self.subTest(plan=relative):
                result = self.compile(text)
                self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('tests/hardware/init-usb-live-build.ccl', plans)

    def test_all_existing_profiles(self):
        fixtures = sorted(FIXTURES.rglob('*.conf'))
        self.assertEqual(len(fixtures), 28)
        for fixture in fixtures:
            relative = fixture.relative_to(FIXTURES)
            source = ROOT / relative.with_suffix('.ccl')
            with self.subTest(profile=relative):
                expected = ''.join(line + '\n' for line in fixture.read_text().splitlines()
                                   if line and not line.startswith('#'))
                if relative.as_posix() in ('system.conf', 'tests/hardware/system-live.conf'):
                    # Intentional post-migration default added with RTC time.
                    # Preserve the legacy oracle and account for the exact key.
                    self.assertNotIn('clock.time-zone=', expected)
                    # The live disc carries the NUC owner's zone.
                    zone = 'UTC' if relative.as_posix() == 'system.conf' else 'America/Denver'
                    expected = f'clock.time-zone={zone}\n' + expected
                    expected = ('desktop.appearance.theme.light=(theme v1 (base alloy-light))\n'
                                'desktop.appearance.theme.dark=(theme v1 (base alloy-dark))\n' + expected)
                    if relative.as_posix() == 'system.conf':
                        expected = expected.replace('clock.time-zone=UTC\n',
                            'clock.time-zone=UTC\ntime.servers=time.cloudflare.com 0.pool.ntp.org 1.pool.ntp.org 2.pool.ntp.org\n')
                elif relative.as_posix() in ('init.conf', 'tests/headless/init-desktop-session.conf'):
                    # Explicit, reviewed post-migration service additions.
                    added = ('timesync.svc network=declared\n'
                             if relative.as_posix() == 'init.conf' else '')
                    expected = expected.replace('clock.svc\n',
                        'clock.svc\n' + added + 'tls.svc network=declared\n')
                elif relative.as_posix() == 'tests/headless/init-capability-security.conf':
                    # The committed construction-failure tests intentionally
                    # extended this profile after the original migration.
                    expected = ('bad-phdr.app\nbad-segment.app\n'
                                'bad-stack.app\n' + 'overlap.app\n' * 8 + expected)
                result = self.compile(source.read_text())
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, expected)
                self.assertFalse((ROOT / relative).exists(), 'old source .conf still present')

    def test_live_graphics_diagnostics_menu(self):
        result = self.compile((ROOT / 'tests/hardware/system-live.ccl').read_text())
        self.assertEqual(result.returncode, 0, result.stderr)
        settings = dict(line.split('=', 1) for line in result.stdout.splitlines())
        self.assertEqual(settings['logs.minimum-level'], 'Severity.Information')
        # Typed Launch_Entry values, stored in their canonical spelling.
        self.assertEqual(settings['desktop.launch.96-logs'],
                         '(Launch_Entry label => "Logs" action => (Launch_Action.Program "logs.app") '
                         'icon => (Launch_Icon name => "Logs") category => App_Category.System single_instance => false '
                         'color => (App_Color.RGB (RGB_Color red => 133 green => 147 blue => 170)))')
        self.assertEqual(settings['desktop.launch.12-console'],
                         '(Launch_Entry label => "CCL Console" action => (Launch_Action.Program "ccl-console.app") '
                         'icon => (Launch_Icon name => "Console") category => App_Category.Development single_instance => false '
                         'color => (App_Color.RGB (RGB_Color red => 43 green => 122 blue => 70)))')
        self.assertIn('desktop.launch.95-boot-logs', settings)

    def test_typed_launch_entries(self):
        self.reject('(system-config v1 (setting "desktop.launch.10-x" "(launch v1 (label \\"X\\") (internal settings))"))',
                    'INVALID_TYPED_VALUE')
        self.reject('(system-config v1 (setting "desktop.launch.10-x" '
                    '(Launch_Entry label => "X" action => Launch_Action.Settings category => App_Category.Toys)))',
                    'INVALID_TYPED_VALUE')
        self.reject('(system-config v1 (setting "desktop.launch.10-x" '
                    '(Launch_Entry "X" Launch_Action.Settings (Launch_Icon "Toaster"))))',
                    'INVALID_TYPED_VALUE')
        result = self.compile('(system-config v1 (setting "desktop.launch.10-x" '
                              '(Launch_Entry "X" Launch_Action.Settings (Launch_Icon "Files"))))')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('desktop.launch.10-x=(Launch_Entry label => "X" action => Launch_Action.Settings '
                      'icon => (Launch_Icon name => "Files") category => App_Category.Tools single_instance => false '
                      'color => App_Color.Theme_Accent)',
                      result.stdout)

    def test_real_ccl_field_expressions(self):
        result = self.compile('''# Ordinary CCL, no host callbacks
          (system-config v1
            (setting (concat "net." "name") (concat "Cu" "Bit"))
            (setting "count" (* 6 7))
            (setting "negative" -4)
            (setting "enabled" (if (= 2 2) true false)))''')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'net.name=CuBit\ncount=42\nnegative=-4\nenabled=true\n')

    def test_explicit_approval_and_order(self):
        result = self.compile('''(startup v1
          (start "a.app")
          (start "a.app" (network approve-declared))
          (start "b.svc" (network deny)))''')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'a.app\na.app network=declared\nb.svc\n')

    def test_rejected_config(self):
        for text, diagnostic in [
            ('(system-config v1 (set "x" 1))', 'UNKNOWN_DECLARATION'),
            ('(system-config v2 (setting "x" 1))', 'UNSUPPORTED_VERSION'),
            ('(system-config v1)', 'MISSING_FIELD'),
            ('(startup v1)', 'MISSING_FIELD'),
            ('(system-config v1 (setting "x" ""))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "" 1))', 'INVALID_KEY'),
            ('(system-config v1 (setting "a=b" 1))', 'INVALID_KEY'),
            ('(system-config v1 (setting "a b" 1))', 'INVALID_KEY'),
            ('(system-config v1 (setting "x" "a\\nb=2"))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "x" "a\\rb=2"))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "x" "é"))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "x" 1) (setting "x" 2))', 'DUPLICATE_KEY'),
            ('(system-config v1 (unknown "x" 1))', 'UNKNOWN_DECLARATION'),
            ('(system-config v1 (setting "x" (clock.monotonic-ms)))', 'INVALID_SYNTAX'),
            ('(system-config v1 (setting "x" 1)) trailing', 'TRAILING_INPUT')]:
            with self.subTest(text=text):
                self.reject(text, diagnostic)

    def test_storage_role_is_explicit_and_unique(self):
        start = '(start "store.svc" (role config-storage))'
        result = self.compile(f'(startup v1 {start})')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'store.svc role=config-storage\n')
        self.reject(f'(startup v1 {start} {start})', 'INVALID_ROLE')
        self.reject(f'(startup v1 {start.replace("config-storage", "root")})', 'INVALID_ROLE')
        self.reject('(startup v1 (start "store.svc" (role application) (role config-storage)))', 'DUPLICATE_FIELD')

    def test_rejected_launches(self):
        base = '(startup v1 (start "a.app" (network deny)))'
        for name in ('../evil.app', '/a.app', 'a.app network=declared', '@mem:0/a.app', '.app', 'a.'):
            self.reject(base.replace('a.app', name), 'INVALID_EXECUTABLE')
        # Fixed priorities are retired (docs/scheduler.md): scheduling is a
        # granted class, never a number a plan picks.
        self.reject(base.replace('(network deny)', '(priority 5)'), 'UNKNOWN_DECLARATION')
        self.reject(base.replace('(network deny)', '(network yes)'), 'INVALID_APPROVAL')
        self.reject(base.replace('(network deny)', '(network deny) (network approve-declared)'),
                    'DUPLICATE_FIELD')
        self.reject(base.replace('(network deny)', '(network deny) (grant everything)'), 'UNKNOWN_DECLARATION')

    def test_symbolic_format_versions(self):
        for kind, fields in (("system-config", '(setting "x" 1)'),
                             ("startup", '(start "a.app")')):
            for token in ("1", "2", "v2", "V1", '"v1"', "(+ 0 1)"):
                with self.subTest(kind=kind, token=token):
                    self.reject(f"({kind} {token} {fields})")

    def test_bounds(self):
        self.reject('#' * (16 * 1024 + 1))
        for length, good in ((128, True), (129, False)):
            source = f'(system-config v1 (setting "{"k" * length}" 1))'
            if good:
                self.assertEqual(self.compile(source).returncode, 0)
            else:
                self.reject(source, 'INVALID_KEY')
        self.reject('(system-config v1 ' + ' '.join(f'(setting "k{i}" 1)' for i in range(129)) + ')',
                    'TOO_MANY_ENTRIES')
        self.assertEqual(self.compile('(startup v1 ' + '(start "a.app")' * 32 + ')').returncode, 0)
        self.reject('(startup v1 ' + '(start "a.app")' * 33 + ')', 'TOO_MANY_ENTRIES')
        value = 'x5'
        for i in reversed(range(1, 6)):
            value = f'(let ((x{i} (concat x{i-1} x{i-1}))) {value})'
        value = f'(let ((x0 "{"x" * 32}")) {value})'
        source = '(system-config v1 ' + ' '.join(f'(setting "k{i}" {value})' for i in range(33)) + ')'
        self.assertLessEqual(len(source), 16 * 1024)
        self.assertEqual(self.compile(source).returncode, 0)

    def test_preflight_is_silent(self):
        self.source.write_text('(startup v1 (start "a.app"))')
        result = subprocess.run([TOOL, self.source], text=True, capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, '')

    def test_malformed_inputs_do_not_raise(self):
        source = '(startup v1 (start "a.app" (stop-grace-ms (let ((n 4)) (* n 1000))) (network deny)))'
        for end in range(len(source)):
            self.reject(source[:end])
        rng = random.Random(1729)
        for _ in range(100):
            text = ''.join(rng.choice('()#"\\abc 123\n') for _ in range(rng.randrange(80)))
            result = self.compile(text)
            self.assertIn(result.returncode, (0, 1), result.stderr)

    def test_startup_policy_fields(self):
        policy = '''(startup v1
          (start "logstore.svc")
          (start "ps2.drv" (launch per-device) (approve-device)
            (after "logstore.svc") (ready-deadline-ms 2000) (stop-grace-ms 750))
          (start "hda.drv" (launch per-device) (approve-device))
          (start "mixer.svc" (stop-stage power-tail) (after "logstore.svc" "hda.drv")
            (approve-scheduling realtime (budget-us 1500) (period-us 5000))))'''
        result = self.compile(policy)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'logstore.svc\n'
                         'ps2.drv launch=per-device device=approved after=1 ready-ms=2000'
                         ' stop-grace-ms=750\n'
                         'hda.drv launch=per-device device=approved\n'
                         'mixer.svc after=1,3 scheduling=1500/5000 stop-stage=power-tail\n')
        for old, bad, diagnostic in [
                # Dependencies name an earlier, unique entry: acyclic by construction.
                ('(after "logstore.svc" "hda.drv")', '(after "mixer.svc")', 'INVALID_DEPENDENCY'),
                ('(after "logstore.svc" "hda.drv")', '(after "nope.svc")', 'INVALID_DEPENDENCY'),
                ('(after "logstore.svc" "hda.drv")', '(after "hda.drv" "hda.drv")', 'INVALID_DEPENDENCY'),
                ('(after "logstore.svc" "hda.drv")', '(after "logstore.svc") (after "hda.drv")',
                 'DUPLICATE_FIELD'),
                ('(after "logstore.svc" "hda.drv")', '(after 5)', 'INVALID_EXECUTABLE'),
                # Device resources go only to per-device drivers, which need them.
                ('(launch per-device) (approve-device)\n            (after',
                 '(approve-device)\n            (after', 'INVALID_LAUNCH_MODE'),
                ('(launch per-device) (approve-device))', '(launch per-device))',
                 'INVALID_LAUNCH_MODE'),
                ('(launch per-device) (approve-device))', '(launch sometimes) (approve-device))',
                 'INVALID_LAUNCH_MODE'),
                ('(launch per-device) (approve-device))',
                 '(launch per-device) (approve-device) (role config-storage))', 'INVALID_ROLE'),
                ('(launch per-device) (approve-device))',
                 '(launch per-device) (approve-device) (approve-device))', 'DUPLICATE_FIELD'),
                # The kernel admits budget <= period and at most 70% of a CPU.
                ('(budget-us 1500)', '(budget-us 3501)', 'INVALID_SCHEDULING'),
                ('(budget-us 1500)', '(budget-us 0)', 'INVALID_SCHEDULING'),
                ('(period-us 5000)', '(period-us 1073741825)', 'INVALID_SCHEDULING'),
                ('(budget-us 1500)', '(budget "1500")', 'INVALID_SCHEDULING'),
                ('realtime (budget-us', 'batch (budget-us', 'INVALID_SCHEDULING'),
                ('(ready-deadline-ms 2000)', '(ready-deadline-ms 0)', 'INVALID_DEADLINE'),
                ('(ready-deadline-ms 2000)', '(ready-deadline-ms 60001)', 'INVALID_DEADLINE'),
                ('(ready-deadline-ms 2000)', '(ready-deadline-ms 2000) (ready-deadline-ms 1)',
                 'DUPLICATE_FIELD'),
                # (stop-grace-ms N): the entry's stop grace (PWR-003), 1 .. 600000, once.
                ('(stop-grace-ms 750)', '(stop-grace-ms 0)', 'INVALID_DEADLINE'),
                ('(stop-grace-ms 750)', '(stop-grace-ms 600001)', 'INVALID_DEADLINE'),
                ('(stop-grace-ms 750)', '(stop-grace-ms 750) (stop-grace-ms 1)', 'DUPLICATE_FIELD'),
                ('(stop-grace-ms 750)', '(stop-grace-ms "750")', 'INVALID_DEADLINE'),
                # (stop-stage apps | services | power-tail), once (PWR-003).
                ('(stop-stage power-tail)', '(stop-stage later)', 'INVALID_LAUNCH_MODE'),
                ('(stop-stage power-tail)', '(stop-stage apps) (stop-stage services)', 'DUPLICATE_FIELD')]:
            with self.subTest(bad=bad):
                self.assertIn(old, policy)
                self.reject(policy.replace(old, bad), diagnostic)
        self.assertEqual(self.compile(policy.replace('(budget-us 1500)', '(budget-us 3500)')).returncode, 0)
        # A name started twice is an ambiguous dependency.
        # (restart on-failure): procmgr's own entries only; once.
        restart = policy.replace('(start "logstore.svc")',
                                 '(start "logstore.svc" (restart on-failure))')
        result = self.compile(restart)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(result.stdout.startswith('logstore.svc restart=on-failure\n'), result.stdout)
        self.assertEqual(self.compile(policy.replace(
            '(start "logstore.svc")',
            '(start "logstore.svc" (restart never))')).stdout, result.stdout.replace(
                ' restart=on-failure', ''))
        for bad, diagnostic in [
                ('(restart sometimes)', 'INVALID_LAUNCH_MODE'),
                ('(restart on-failure) (restart never)', 'DUPLICATE_FIELD')]:
            with self.subTest(bad=bad):
                self.reject(policy.replace('(start "logstore.svc")',
                                           '(start "logstore.svc" ' + bad + ')'), diagnostic)
        self.reject(policy.replace('(launch per-device) (approve-device))',
                                   '(launch per-device) (approve-device) (restart on-failure))'),
                    'INVALID_LAUNCH_MODE')
        # (replace CONTRACT): the restart contract, a per-service exception
        # in the trusted plan; procmgr's own entries only; once
        # (CuBit.Service_Replacement).
        for contract in ("transparent", "graceful"):
            replace = policy.replace('(start "logstore.svc")',
                                     '(start "logstore.svc" (replace %s))' % contract)
            result = self.compile(replace)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue(result.stdout.startswith('logstore.svc replace=%s\n' % contract), result.stdout)
            self.assertEqual(self.compile(policy.replace(
                '(start "logstore.svc")',
                '(start "logstore.svc" (replace reboot))')).stdout, result.stdout.replace(
                    ' replace=%s' % contract, ''))
        for bad, diagnostic in [
                ('(replace staged)', 'INVALID_LAUNCH_MODE'),
                ('(replace graceful) (replace reboot)', 'DUPLICATE_FIELD')]:
            with self.subTest(bad=bad):
                self.reject(policy.replace('(start "logstore.svc")',
                                           '(start "logstore.svc" ' + bad + ')'), diagnostic)
        self.reject(policy.replace('(launch per-device) (approve-device))',
                                   '(launch per-device) (approve-device) (replace transparent))'),
                    'INVALID_LAUNCH_MODE')
        twice = policy.replace('(start "logstore.svc")',
                               '(start "logstore.svc") (start "logstore.svc")')
        self.reject(twice, 'INVALID_DEPENDENCY')
        for end in range(len(policy)):
            self.reject(policy[:end])


if __name__ == '__main__':
    unittest.main()
