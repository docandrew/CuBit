"""Hosted controls for the visible-input oracle; not native USB evidence."""
import unittest
from PIL import Image, ImageDraw
from input_pixels import check_usb_pointer


class InputPixels(unittest.TestCase):
    def exercise(self, fault=None):
        closed = Image.new('RGB', (1024, 768), (32, 64, 96))
        opened = closed.copy()
        ImageDraw.Draw(opened).rectangle((2, 608, 90, 728), fill='white')
        cursor = closed.copy()
        ImageDraw.Draw(cursor).rectangle((10, 610, 25, 635), fill='white')
        state = {'open': False, 'x': 512, 'y': 384, 'time': 0, 'pending': 0,
                 'clicks': 0}

        def command(text):
            parts = text.split()
            if parts[0] == 'mouse_move' and fault != 'no-motion':
                state['x'] = max(0, min(1023, state['x'] + int(parts[1])))
                state['y'] = max(0, min(767, state['y'] + int(parts[2])))
            if text == 'mouse_button 0':
                state['clicks'] += 1
                if (fault != 'no-buttons' and state['x'] == 24 and
                        state['y'] == 750):
                    if fault != 'stuck-open' or not state['open']:
                        state['open'] = not state['open']
                        state['pending'] = 3

        def snapshot(name):
            if state['pending']:
                state['pending'] -= 1
                return closed if state['open'] else opened
            if fault == 'cursor-only' and state['open']:
                return cursor
            return opened if state['open'] else closed

        def pause(duration):
            state['time'] += duration

        check_usb_pointer(command, snapshot, lambda: None,
                          timeout=1, pause=pause, clock=lambda: state['time'])
        self.assertEqual(state['clicks'], 4)

    def test_delayed_open_and_restore(self):
        self.exercise()

    def test_missing_motion(self):
        with self.assertRaisesRegex(RuntimeError, 'usb-apps-open'):
            self.exercise('no-motion')

    def test_missing_buttons(self):
        with self.assertRaisesRegex(RuntimeError, 'usb-apps-open'):
            self.exercise('no-buttons')

    def test_cursor_is_not_menu(self):
        with self.assertRaisesRegex(RuntimeError, 'usb-apps-open'):
            self.exercise('cursor-only')

    def test_missing_restoration(self):
        with self.assertRaisesRegex(RuntimeError, 'usb-apps-closed'):
            self.exercise('stuck-open')


if __name__ == '__main__':
    unittest.main()
