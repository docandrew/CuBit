"""Visible USB pointer acceptance, independent of periodic desktop logs."""
import time


def check_usb_pointer(command, snapshot, healthy, timeout=8,
                      pause=time.sleep, clock=time.monotonic):
    """Click Apps twice and require visible open/close responses each time.

    The live profile starts with Apps closed. Its bottom-left strip is outside
    the initial diagnostic window (x >= 98), taskbar, clock and input overlay.
    This checks pointer delivery and button handling, not GPU acceleration.
    """
    initial = snapshot('usb-input-initial')
    width, height = initial.size
    if width < 640 or height < 480:
        raise RuntimeError('USB input pixel fixture requires at least 640x480')
    region = (2, height - 160, 90, height - 40)

    def pixels(frame):
        if frame.size != (width, height):
            raise RuntimeError('display resized during USB input check')
        return frame.convert('RGB').crop(region).tobytes()

    def move(dx, dy):
        while dx or dy:
            healthy()
            x, y = max(-80, min(80, dx)), max(-80, min(80, dy))
            command(f'mouse_move {x} {y}')
            dx -= x
            dy -= y
            pause(0.04)

    def wait_pixels(name, accept):
        until = clock() + timeout
        while True:
            healthy()
            value = pixels(snapshot(name))
            if accept(value):
                return value
            if clock() >= until:
                raise RuntimeError('USB input visible response missing: ' + name)
            pause(0.15)

    baseline = pixels(initial)
    # Clamp to origin, then target Apps. The cursor is below the compared strip.
    move(-width * 2, -height * 2)
    move(24, height - 18)
    wait_pixels('usb-input-positioned', lambda value: value == baseline)

    def click():
        command('mouse_button 1')
        pause(0.15)
        command('mouse_button 0')

    def opened(value):
        # Count changed RGB pixels, not channels; cursor-only damage cannot pass.
        changed = sum(value[i:i + 3] != baseline[i:i + 3]
                      for i in range(0, len(value), 3))
        return changed > 2000

    for cycle in range(2):
        click()
        wait_pixels(f'usb-apps-open-{cycle}', opened)
        click()
        wait_pixels(f'usb-apps-closed-{cycle}', lambda value: value == baseline)
