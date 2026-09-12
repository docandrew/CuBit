"""Host-side validation; real KVM runs additionally exercise QMP and cleanup."""
import io
import unittest
from unittest.mock import patch

from qemu_affinity import QMP, cpu_list, pin_vcpus, run


class AffinityTests(unittest.TestCase):
    def test_mapping_order(self):
        self.assertEqual(cpu_list("5,2,4,3", 4, set(range(16))), [5, 2, 4, 3])

    def test_invalid_mapping(self):
        for value in ("", "2-5", "2,2,4,5", "2,3", "2,3,4,16", "-1,2,3,4", "2,3,4,5,"):
            with self.subTest(value=value), self.assertRaises(ValueError):
                cpu_list(value, 4, set(range(16)))

    def test_pin_by_index_not_reply_order(self):
        entries = [{"cpu-index": 1, "thread-id": 102}, {"cpu-index": 0, "thread-id": 101}]
        with patch("qemu_affinity.Path.is_dir", return_value=True), \
             patch("qemu_affinity.Path.read_text", return_value="2,10\n"), \
             patch("qemu_affinity.os.sched_setaffinity") as setter, \
             patch("qemu_affinity.os.sched_getaffinity", side_effect=[{2}, {3}]), \
             patch("sys.stdout", new_callable=io.StringIO):
            pin_vcpus(entries, [2, 3], 100)
            self.assertEqual([call.args for call in setter.call_args_list], [(101, {2}), (102, {3})])

    def test_reject_bad_threads(self):
        for entries in ([], [{"cpu-index": 1, "thread-id": 101}],
                        [{"cpu-index": 0, "thread-id": 101}, {"cpu-index": 0, "thread-id": 102}],
                        [{"cpu-index": 0, "thread-id": 101}, {"cpu-index": 1, "thread-id": 101}]):
            with self.subTest(entries=entries), self.assertRaises(ValueError):
                pin_vcpus(entries, [2, 3], 100)
        with patch("qemu_affinity.Path.is_dir", return_value=False), self.assertRaises(ValueError):
            pin_vcpus([{"cpu-index": 0, "thread-id": 999}], [2], 100)

    def test_verify_affinity(self):
        with patch("qemu_affinity.Path.is_dir", return_value=True), \
             patch("qemu_affinity.os.sched_setaffinity"), \
             patch("qemu_affinity.os.sched_getaffinity", return_value={2, 3}), \
             self.assertRaises(ValueError):
            pin_vcpus([{"cpu-index": 0, "thread-id": 101}], [2], 100)

    def test_setup_failure_stops_child_without_resuming(self):
        with patch("qemu_affinity.subprocess.Popen") as spawn, \
             patch("qemu_affinity.socket.socket") as connection, \
             patch("qemu_affinity.QMP") as protocol, \
             patch("qemu_affinity.pin_vcpus", side_effect=ValueError("cannot pin")):
            spawn.return_value.poll.return_value = None
            with self.assertRaises(ValueError):
                run([2], ["qemu"])
            spawn.return_value.terminate.assert_called_once()
            spawn.return_value.wait.assert_called_once_with(timeout=5)
            self.assertEqual([call.args[0] for call in protocol.return_value.execute.call_args_list],
                             ["qmp_capabilities", "query-cpus-fast"])
            self.assertIn("-S", spawn.call_args.args[0])

    def test_qmp_events_and_errors(self):
        class Stream:
            def __init__(self, reply):
                self.input = io.StringIO('{"QMP": {}}\n{"event": "STOP"}\n' + reply + '\n')
                self.output = io.StringIO()

            def readline(self):
                return self.input.readline()

            def write(self, text):
                self.output.write(text)

            def flush(self):
                pass

        stream = Stream('{"id": 1, "return": []}')
        self.assertEqual(QMP(stream).execute("query-cpus-fast"), [])
        self.assertIn('"id": 1', stream.output.getvalue())
        for reply in ('{"id": 1, "error": {}}', '{"id": 2, "return": []}', ''):
            with self.subTest(reply=reply), self.assertRaises(ValueError):
                QMP(Stream(reply)).execute("cont")


if __name__ == "__main__":
    unittest.main()
