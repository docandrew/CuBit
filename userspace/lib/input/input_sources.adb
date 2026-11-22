------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Input_Sources is

   Microseconds_Per_Millisecond : constant := CuBit.Input.Microseconds_Per_Millisecond;

   --  The millisecond stamp a pending item also carries.
   function Milliseconds (Observed_Us : Unsigned_64) return Unsigned_64 is
     (if Observed_Us = Unsigned_64'Last then Unsigned_64'Last
      else Observed_Us / Microseconds_Per_Millisecond);

   procedure Keyboard_Append
     (S : in out Keyboard_Pending.State; R : Keyboard_Pending.Byte;
      Observed_Us : Unsigned_64; Lost : out Boolean)
   is
      Ignore_Added : Keyboard_Pending.Frame_Length;
   begin
      Keyboard_Pending.Append_Byte (S, R, Ignore_Added, Lost, Observed_Us => Observed_Us);
   end Keyboard_Append;

   procedure Pointer_Append
     (S : in out Pointer_Pending.State; R : Pointer_Pending.Report;
      Observed_Us : Unsigned_64; Lost : out Boolean)
   is
      use type Pointer_Pending.Append_Outcome;
      Outcome : Pointer_Pending.Append_Outcome;
   begin
      Pointer_Pending.Append (S, R, Milliseconds (Observed_Us), Outcome, Observed_Us => Observed_Us);
      Lost := Outcome = Pointer_Pending.Overflowed;
   end Pointer_Append;

   procedure Absolute_Append
     (S : in out Absolute_Pending.State; R : Absolute_Pointer.Report;
      Observed_Us : Unsigned_64; Lost : out Boolean)
   is
      use type Absolute_Pending.Append_Outcome;
      Outcome : Absolute_Pending.Append_Outcome;
   begin
      Absolute_Pending.Append (S, R, Milliseconds (Observed_Us), Outcome, Observed_Us => Observed_Us);
      Lost := Outcome = Absolute_Pending.Overflowed;
   end Absolute_Append;

end Input_Sources;
