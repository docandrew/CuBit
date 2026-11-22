------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  virtio-input event stream (virtio 1.2 5.8, Linux evdev codes) assembled
--  into absolute-pointer reports. Pure: no device access, no IPC.
--
--  A device sends one event per changed axis, button or wheel step and then
--  EV_SYN/SYN_REPORT; the frame is the report. Positions persist across
--  frames (an axis absent from a frame did not move); wheel steps belong to
--  the frame that carried them. A frame that changed nothing is not a report.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Absolute_Pointer;

package Virtio_Input_Events with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16, Interfaces.Unsigned_32;

   --  struct virtio_input_event: le16 type, le16 code, le32 value.
   Event_Bytes : constant := 8;
   type Event is record
      Kind  : Interfaces.Unsigned_16 := 0;
      Code  : Interfaces.Unsigned_16 := 0;
      Value : Interfaces.Unsigned_32 := 0;
   end record;

   --  Event types and codes (linux/input-event-codes.h).
   EV_SYN : constant Interfaces.Unsigned_16 := 16#00#;
   EV_KEY : constant Interfaces.Unsigned_16 := 16#01#;
   EV_REL : constant Interfaces.Unsigned_16 := 16#02#;
   EV_ABS : constant Interfaces.Unsigned_16 := 16#03#;
   SYN_REPORT  : constant Interfaces.Unsigned_16 := 0;
   SYN_DROPPED : constant Interfaces.Unsigned_16 := 3;
   ABS_X : constant Interfaces.Unsigned_16 := 16#00#;
   ABS_Y : constant Interfaces.Unsigned_16 := 16#01#;
   REL_WHEEL : constant Interfaces.Unsigned_16 := 16#08#;
   BTN_LEFT   : constant Interfaces.Unsigned_16 := 16#110#;
   BTN_RIGHT  : constant Interfaces.Unsigned_16 := 16#111#;
   BTN_MIDDLE : constant Interfaces.Unsigned_16 := 16#112#;
   BTN_SIDE   : constant Interfaces.Unsigned_16 := 16#113#;
   BTN_EXTRA  : constant Interfaces.Unsigned_16 := 16#114#;
   KEY_RELEASED : constant Interfaces.Unsigned_32 := 0;

   --  CuBit pointer button bits (PS/2 and USB HID boot order).
   subtype Button_Bit is Natural range 0 .. 4;
   function Button_Of (Code : Interfaces.Unsigned_16) return Button_Bit
     with Pre => Code in BTN_LEFT .. BTN_EXTRA;
   Button_Masks : constant array (Button_Bit) of Absolute_Pointer.Button_State :=
     [1, 2, 4, 8, 16];

   --  le32 value as the signed evdev value.
   function Signed (Value : Interfaces.Unsigned_32)
     return Absolute_Pointer.Device_Value
   is (if Value >= 2 ** 31 then Long_Long_Integer (Value) - 2 ** 32
       else Long_Long_Integer (Value));

   type Assembler is private;

   --  X and Y: the device's ABS_INFO ranges for ABS_X and ABS_Y.
   function Start (X, Y : Absolute_Pointer.Device_Range) return Assembler
     with Pre => Absolute_Pointer.Valid_Range (X) and then
                 Absolute_Pointer.Valid_Range (Y);
   function Valid (A : Assembler) return Boolean;
   --  The pointer state as of the last completed frame.
   function Current (A : Assembler) return Absolute_Pointer.Report
     with Pre => Valid (A);

   type Outcome is
     (Absorbed,       --  part of a frame; nothing to publish yet
      Frame_Report,   --  a frame changed the pointer: publish Report
      Frame_Unchanged, --  a frame ended without changing anything
      Frame_Dropped,  --  the device lost events (SYN_DROPPED): resynchronize
      Ignored);       --  an event this pointer does not use

   procedure Feed
     (A : in out Assembler; E : Event; Result : out Outcome;
      Report : out Absolute_Pointer.Report)
     with Pre => Valid (A),
       Post => Valid (A) and then
         (if Result = Frame_Report then
            Report.X = Current (A).X and then Report.Y = Current (A).Y and then
            Report.Buttons = Current (A).Buttons and then
            Report.Flags = 0 and then Current (A).Wheel = 0);
private
   type Assembler is record
      X_Range, Y_Range : Absolute_Pointer.Device_Range :=
        (Minimum => 0, Maximum => Absolute_Pointer.Axis_Last);
      --  Committed state (Wheel always zero) and the frame being built.
      Committed : Absolute_Pointer.Report;
      Building  : Absolute_Pointer.Report;
      Changed   : Boolean := False;
      --  After SYN_DROPPED: discard events until the next SYN_REPORT.
      Discarding : Boolean := False;
   end record;

   function Valid (A : Assembler) return Boolean is
     (Absolute_Pointer.Valid_Range (A.X_Range) and then
      Absolute_Pointer.Valid_Range (A.Y_Range) and then
      A.Committed.Wheel = 0 and then A.Committed.Flags = 0 and then
      A.Building.Flags = 0);

   function Current (A : Assembler) return Absolute_Pointer.Report is
     (A.Committed);
end Virtio_Input_Events;
