------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The input source of each device class (DEV-002): CuBit.Input_Source
--  instantiated with the class's retention. A driver declares a Source of
--  its class per device and publishes its decoded reports into it:
--    Keyboards: set-1 scan code bytes (Keyboard_Pending frames them);
--    Relative_Pointers: Pointer_Pending.Report displacements;
--    Absolute_Pointers: Absolute_Pointer.Report positions.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Input;
with CuBit.Input_Source;
with Absolute_Pending;
with Absolute_Pointer;
with Input_Pending;
with Keyboard_Pending;
with Pointer_Pending;

package Input_Sources is

   procedure Keyboard_Append
     (S : in out Keyboard_Pending.State; R : Keyboard_Pending.Byte;
      Observed_Us : Unsigned_64; Lost : out Boolean);
   procedure Keyboard_Reset (S : in out Keyboard_Pending.State)
     renames Keyboard_Pending.Reset_Consumer;
   function No_Buttons (Ignore_Item : Input_Pending.Item) return Unsigned_64 is (0);

   procedure Pointer_Append
     (S : in out Pointer_Pending.State; R : Pointer_Pending.Report;
      Observed_Us : Unsigned_64; Lost : out Boolean);
   --  A relative report's payload carries its buttons in the low byte.
   function Pointer_Buttons (Item : Input_Pending.Item) return Unsigned_64 is (Item.Payload);

   procedure Absolute_Append
     (S : in out Absolute_Pending.State; R : Absolute_Pointer.Report;
      Observed_Us : Unsigned_64; Lost : out Boolean);
   function Absolute_Buttons (Item : Input_Pending.Item) return Unsigned_64 is
     (Item.Payload and Absolute_Pointer.Byte_Mask);

   package Keyboards is new CuBit.Input_Source
     (Pending => Keyboard_Pending.State, Report => Keyboard_Pending.Byte,
      Device => CuBit.Input.KEYBOARD, Append => Keyboard_Append,
      Count => Keyboard_Pending.Count, Element => Keyboard_Pending.Element,
      Acknowledge => Keyboard_Pending.Acknowledge, Reset => Keyboard_Reset,
      Buttons => No_Buttons);

   package Relative_Pointers is new CuBit.Input_Source
     (Pending => Pointer_Pending.State, Report => Pointer_Pending.Report,
      Device => CuBit.Input.RELATIVE_POINTER, Append => Pointer_Append,
      Count => Pointer_Pending.Count, Element => Pointer_Pending.Element,
      Acknowledge => Pointer_Pending.Acknowledge, Reset => Pointer_Pending.Reset,
      Buttons => Pointer_Buttons);

   package Absolute_Pointers is new CuBit.Input_Source
     (Pending => Absolute_Pending.State, Report => Absolute_Pointer.Report,
      Device => CuBit.Input.ABSOLUTE_POINTER, Append => Absolute_Append,
      Count => Absolute_Pending.Count, Element => Absolute_Pending.Element,
      Acknowledge => Absolute_Pending.Acknowledge, Reset => Absolute_Pending.Reset,
      Buttons => Absolute_Buttons, Mark_Lost => Absolute_Pending.Mark_Lost);

end Input_Sources;
