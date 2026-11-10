------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The pairing token of one enablement: 16 bytes from the CPU's random
--  source (CuBit.Hardware_Random, RDRAND), made when the endpoint starts,
--  shown to the person at the device and nowhere else: never logged, never
--  sent back over the wire. A new start of the endpoint is a new token, so
--  stopping it (or restarting it) revokes every host paired before.
--
--  @description
--  Where the code is shown is a launch parameter (Pairing_Display):
--    Window          a small window on the desktop, until the host pairs;
--    Kernel_Console  the kernel console (serial), for headless machines and
--                    the QEMU test, where the console is the screen.
--  With Window and no desktop, the code cannot be shown, and the endpoint
--  does not listen.
--
--  The window stays while the endpoint runs: once a host pairs, "Paired
--  with <address>" takes the code's place (the token is never shown again)
--  and the window lists what the host does (Remote_Activity), newest at
--  the bottom, in a read-only text field (CuBit.UI.Text_Views): it follows
--  new entries unless scrolled back (wheel, Up/Down/Page Up/Page Down/
--  Home/End, its scroll bar), selects by dragging or Shift+keys, and Ctrl+C
--  copies the selection. The window is laid out by CuBit.UI.Boxes and
--  resizes down to its layout's minimum (UI-033). On Kernel_Console the
--  same lines go to the console and logstore.
------------------------------------------------------------------------------
pragma Ada_2022;
with Remote_Wire;

package Remote_Pairing is
   type Display is (Window, Kernel_Console);

   procedure Generate (Success : out Boolean);
   function Matches (Credential : Remote_Wire.Token) return Boolean;

   --  Show the code with what a person needs to pair: the addresses the
   --  endpoint listens on and what the host will be allowed to do.
   --  Shown False: the code could not be shown.
   procedure Show
     (Where : Display; Addresses, Grants : String; Shown : out Boolean);
   --  The listening addresses changed: show them again (Window only).
   procedure Update (Addresses : String);
   --  A host paired from Peer ("a.b.c.d"): the window shows that in place
   --  of the code (which stays valid until the endpoint stops).
   procedure Paired (Peer : String);
   --  Remote_Activity has a new entry: repaint the list (Window only).
   procedure Activity_Changed;
   --  Window input and repaint, without blocking: call between requests.
   procedure Service;
   function Window_Open return Boolean;
end Remote_Pairing;
