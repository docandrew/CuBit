package Services.Keyboard.Scancodes is

    -- Control key bitmasks
    -- type ControlMask is (
    --     RIGHTSHIFT,
    --     LEFTSHIFT,
    --     CTRL,
    --     ALT,  
    --     SCROLLLOCK_ON,
    --     NUMLOCK_ON,
    --     CAPSLOCK_ON,
    --     INSERT_ON,
    --     LEFTCTRL,
    --     LEFTALT,
    --     RIGHTCTRL,
    --     RIGHTALT,
    --     SCROLLLOCK_DOWN,
    --     NUMLOCK_DOWN,
    --     CAPSLOCK_DOWN,
    --     SYSREQ_DOWN
    -- ) with Size => 16;

    -- for ControlMask use (
    --     RIGHTSHIFT      => 16#0001#,
    --     LEFTSHIFT       => 16#0002#,
    --     CTRL            => 16#0004#,
    --     ALT             => 16#0008#,
    --     SCROLLLOCK_ON   => 16#0010#,
    --     NUMLOCK_ON      => 16#0020#,
    --     CAPSLOCK_ON     => 16#0040#,
    --     INSERT_ON       => 16#0080#,
    --     LEFTCTRL        => 16#0100#,
    --     LEFTALT         => 16#0200#,
    --     RIGHTCTRL       => 16#0400#,
    --     RIGHTALT        => 16#0800#,
    --     SCROLLLOCK_DOWN => 16#1000#,
    --     NUMLOCK_DOWN    => 16#2000#,
    --     CAPSLOCK_DOWN   => 16#4000#,
    --     SYSREQ_DOWN     => 16#8000#
    -- );

    type ScanCode is (
        SCAN_ESC,
        SCAN_1,
        SCAN_2,
        SCAN_3,
        SCAN_4,
        SCAN_5,
        SCAN_6,
        SCAN_7,
        SCAN_8,
        SCAN_9,
        SCAN_0,
        SCAN_MINUS,
        SCAN_EQUALS,
        SCAN_BACKSPACE,
        SCAN_TAB,
        SCAN_Q,
        SCAN_W,
        SCAN_E,
        SCAN_R,
        SCAN_T,
        SCAN_Y,
        SCAN_U,
        SCAN_I,
        SCAN_O,
        SCAN_P,
        SCAN_LEFT_BRACKET,
        SCAN_RIGHT_BRACKET,
        SCAN_ENTER,
        SCAN_CTRL,
        SCAN_A,
        SCAN_S,
        SCAN_D,
        SCAN_F,
        SCAN_G,
        SCAN_H,
        SCAN_J,
        SCAN_K,
        SCAN_L,
        SCAN_SEMICOLON,
        SCAN_APOSTROPHE,
        SCAN_TILDE,
        SCAN_LEFT_SHIFT,
        SCAN_BACKSLASH,
        SCAN_Z,
        SCAN_X,
        SCAN_C,
        SCAN_V,
        SCAN_B,
        SCAN_N,
        SCAN_M,
        SCAN_COMMA,
        SCAN_PERIOD,
        SCAN_FORWARD_SLASH,
        SCAN_RIGHT_SHIFT,
        SCAN_PRINT_SCREEN,
        SCAN_ALT,
        SCAN_SPACE,
        SCAN_CAPS_LOCK,
        SCAN_F1,
        SCAN_F2,
        SCAN_F3,
        SCAN_F4,
        SCAN_F5,
        SCAN_F6,
        SCAN_F7,
        SCAN_F8,
        SCAN_F9,
        SCAN_F10,
        SCAN_NUM_LOCK,
        SCAN_SCROLL_LOCK,
        SCAN_HOME,
        SCAN_UP,
        SCAN_PAGE_UP,
        SCAN_NUMPAD_MINUS,
        SCAN_LEFT_ARROW,
        SCAN_CENTER,
        SCAN_RIGHT_ARROW,
        SCAN_NUMPAD_PLUS,
        SCAN_END,
        SCAN_DOWN_ARROW,
        SCAN_PAGE_DOWN,
        SCAN_INSERT,
        SCAN_DELETE,
        SCAN_F11,
        SCAN_F12
    ) with Size => 8;

    for ScanCode use (
        SCAN_ESC           => 1,
        SCAN_1             => 2,
        SCAN_2             => 3,
        SCAN_3             => 4,
        SCAN_4             => 5,
        SCAN_5             => 6,
        SCAN_6             => 7,
        SCAN_7             => 8,
        SCAN_8             => 9,
        SCAN_9             => 10,
        SCAN_0             => 11,
        SCAN_MINUS         => 12,
        SCAN_EQUALS        => 13,
        SCAN_BACKSPACE     => 14,
        SCAN_TAB           => 15,
        SCAN_Q             => 16,
        SCAN_W             => 17,
        SCAN_E             => 18,
        SCAN_R             => 19,
        SCAN_T             => 20,
        SCAN_Y             => 21,
        SCAN_U             => 22,
        SCAN_I             => 23,
        SCAN_O             => 24,
        SCAN_P             => 25,
        SCAN_LEFT_BRACKET  => 26,
        SCAN_RIGHT_BRACKET => 27,
        SCAN_ENTER         => 28,
        SCAN_CTRL          => 29,
        SCAN_A             => 30,
        SCAN_S             => 31,
        SCAN_D             => 32,
        SCAN_F             => 33,
        SCAN_G             => 34,
        SCAN_H             => 35,
        SCAN_J             => 36,
        SCAN_K             => 37,
        SCAN_L             => 38,
        SCAN_SEMICOLON     => 39,
        SCAN_APOSTROPHE    => 40,
        SCAN_TILDE         => 41,
        SCAN_LEFT_SHIFT    => 42,
        SCAN_BACKSLASH     => 43,
        SCAN_Z             => 44,
        SCAN_X             => 45,
        SCAN_C             => 46,
        SCAN_V             => 47,
        SCAN_B             => 48,
        SCAN_N             => 49,
        SCAN_M             => 50,
        SCAN_COMMA         => 51,
        SCAN_PERIOD        => 52,
        SCAN_FORWARD_SLASH => 53,
        SCAN_RIGHT_SHIFT   => 54,
        SCAN_PRINT_SCREEN  => 55,
        SCAN_ALT           => 56,
        SCAN_SPACE         => 57,
        SCAN_CAPS_LOCK     => 58,
        SCAN_F1            => 59,
        SCAN_F2            => 60,
        SCAN_F3            => 61,
        SCAN_F4            => 62,
        SCAN_F5            => 63,
        SCAN_F6            => 64,
        SCAN_F7            => 65,
        SCAN_F8            => 66,
        SCAN_F9            => 67,
        SCAN_F10           => 68,
        SCAN_NUM_LOCK      => 69,
        SCAN_SCROLL_LOCK   => 70,
        SCAN_HOME          => 71,
        SCAN_UP            => 72,
        SCAN_PAGE_UP       => 73,
        SCAN_NUMPAD_MINUS  => 74,
        SCAN_LEFT_ARROW    => 75,
        SCAN_CENTER        => 76,
        SCAN_RIGHT_ARROW   => 77,
        SCAN_NUMPAD_PLUS   => 78,
        SCAN_END           => 79,
        SCAN_DOWN_ARROW    => 80,
        SCAN_PAGE_DOWN     => 81,
        SCAN_INSERT        => 82,
        SCAN_DELETE        => 83,
        SCAN_F11           => 133,
        SCAN_F12           => 134
    );

end Services.Keyboard.Scancodes;
