pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Bindings;
with HID_Input_Values;
with HID_Touchpad_Contact;
with HID_Report_Metadata;
with HID_Input_Source;
procedure Calibration_Tests is
   package Schema is new HID_Report_Schema (4, 8);
   package Names is new HID_Field_Usages (Schema);
   package Bindings is new HID_Input_Bindings (Schema, Names);
   package Values is new HID_Input_Values (Schema);
   package Contacts is new HID_Touchpad_Contact (Schema, Names, Bindings, Values);
   package Metadata is new HID_Report_Metadata (Schema, Names, Bindings, Values);
   package Source is new HID_Input_Source (5, 2, Schema, Names, Bindings, Values, Contacts, Metadata);
   package Reports renames Source.Reports;
   use type Interfaces.Unsigned_32, Schema.Status, Schema.Numeric_Value;
   use type Source.Configure_Status, Reports.Coordinate_Status, Reports.Compile_Status, Bindings.Work_Count;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   Data : Schema.Bytes (1 .. 1024);
   Used : Natural := 0;
   Fields : Schema.Field_Table (1 .. 64);
   Usages : Schema.Usage_Table (1 .. 128);
   Collections : Schema.Collection_Table (1 .. 16);
   Layout : Schema.Result;
   Signal : Source.State;
   Info : Source.Configure_Report;
   Charged : Bindings.Work_Count;
   type Shape is (Ordinary, Wide, Empty_Axis, Reversed_Axis);
   procedure Byte (Value : Natural) is
   begin Used := Used + 1; Data (Used) := Schema.Byte (Value); end Byte;
   procedure Item (Prefix : Natural; Value : Interfaces.Unsigned_32; Width : Positive := 1) is
   begin
      Byte (Prefix);
      for Index in 0 .. Width - 1 loop
         Byte (Natural (Interfaces.Shift_Right (Value, 8 * Index) mod 256));
      end loop;
   end Item;
   procedure Build (Fingers : Positive; Different : Boolean; Form : Shape) is
   begin
      Used := 0;
      Item (16#05#, 13); Item (16#09#, 5); Item (16#A1#, 1); Item (16#85#, 7);
      for Finger in 1 .. Fingers loop
         Item (16#05#, 13); Item (16#09#, 16#22#); Item (16#A1#, 2);
         Item (16#15#, 0); Item (16#25#, 1); Item (16#09#, 16#42#);
         Item (16#75#, 1); Item (16#95#, 1); Item (16#81#, 2);
         Item (16#75#, 7); Item (16#81#, 3);
         Item (16#09#, 16#51#); Item (16#75#, 8); Item (16#25#, 255); Item (16#81#, 2);
         Item (16#05#, 1); Item (16#15#, 0); Item (16#09#, 16#30#);
         if Form = Wide then
            Item (16#75#, 32); Item (16#27#, 16#FFFF_FFFF#, 4);
         else
            Item (16#75#, 16);
            if Form = Reversed_Axis then Item (16#15#, 1); end if;
            Item (16#26#, (if Form in Empty_Axis | Reversed_Axis then 0
               elsif Different and then Finger = 2 then 1023 else 4095), 2);
         end if;
         Item (16#81#, 2); Item (16#09#, 16#31#);
         if Form = Wide then
            Item (16#17#, 16#8000_0000#, 4); Item (16#27#, 16#7FFF_FFFF#, 4);
         else Item (16#15#, 0); Item (16#26#, 2047, 2); end if;
         Item (16#81#, 2); Byte (16#C0#);
      end loop;
      Item (16#05#, 13); Item (16#15#, 0); Item (16#09#, 16#54#); Item (16#25#, 5);
      Item (16#75#, 8); Item (16#81#, 2);
      Item (16#09#, 16#56#); Item (16#26#, 65_535, 2); Item (16#75#, 16);
      Item (16#55#, 12); Item (16#66#, 16#1001#, 2); Item (16#81#, 2); Byte (16#C0#);
      Schema.Parse (Data (1 .. Used), Fields, Usages, Collections, Layout);
      Check (Layout.Outcome = (if Form = Reversed_Axis then Schema.Invalid_Value_Range else Schema.Decoded));
   end Build;
   procedure Configure (Mode : Source.Coordinate_Policy; Wide_Gain : Boolean := False;
      Budget : Bindings.Work_Count := 100_000) is
      Gain : constant Source.Pointer.Gain := (1, (if Wide_Gain then Source.Pointer.Gain_Denominator'Last else 4));
   begin
      Source.Configure (Signal, Layout, Fields, Usages, Collections, 1, 5, Budget,
         (0, 4095), (0, 2047), Gain, Gain, Source.Pointer.As_Reported,
         Source.Pointer.As_Reported, Source.Pointer.Accept_Unknown, [others => Source.Ignore],
         1, Charged, Info, Mode);
      Check (Charged <= Budget);
   end Configure;
begin
   Build (1, False, Ordinary); Configure (Source.Report_Bounds);
   Check (Source.Ready (Signal) and then Info.Outcome = Source.Source_Configured);
   Check (Info.Coordinates.Outcome = Reports.Uniform_Coordinates);
   Check (Info.Coordinates.Bounds.X_Minimum = 0 and then Info.Coordinates.Bounds.X_Maximum = 4095);
   Check (Info.Coordinates.Bounds.Y_Minimum = 0 and then Info.Coordinates.Bounds.Y_Maximum = 2047);
   Build (2, False, Ordinary); Configure (Source.Report_Bounds);
   Check (Source.Ready (Signal) and then Info.Coordinates.Outcome = Reports.Uniform_Coordinates);
   Build (2, True, Ordinary); Configure (Source.Report_Bounds);
   Check (not Source.Ready (Signal) and then Info.Outcome = Source.Coordinates_Refused);
   Check (Info.Coordinates.Outcome = Reports.Inconsistent_Coordinates);
   Configure (Source.Caller_Bounds); Check (Source.Ready (Signal));
   Build (1, False, Empty_Axis); Configure (Source.Report_Bounds);
   Check (not Source.Ready (Signal) and then Info.Outcome = Source.Coordinates_Refused);
   Build (1, False, Reversed_Axis); Configure (Source.Report_Bounds);
   Check (not Source.Ready (Signal) and then Info.Outcome = Source.Reports_Refused);
   Build (1, False, Wide); Configure (Source.Report_Bounds, Wide_Gain => True);
   Check (Source.Ready (Signal) and then Info.Outcome = Source.Source_Configured);
   Check (Info.Coordinates.Bounds.X_Maximum = Schema.Numeric_Value'Last);
   Check (Info.Coordinates.Bounds.Y_Minimum = Schema.Numeric_Value'First and then Info.Coordinates.Bounds.Y_Maximum = 2 ** 31 - 1);
   Configure (Source.Report_Bounds, Budget => 0);
   Check (not Source.Ready (Signal) and then Info.Outcome = Source.Reports_Refused);
   Check (Info.Coordinates.Outcome = Reports.Coordinates_Unavailable);
   declare Raw : Reports.State; Report : Reports.Compile_Report; begin
      Reports.Configure (Raw, Layout, Fields, Usages, Collections, 1, 5, 100_000, Charged, Report);
      Check (Report.Outcome = Reports.Configured and then Reports.Coordinates (Raw).Outcome = Reports.Uniform_Coordinates);
      Reports.Invalidate (Raw);
      Check (not Reports.Ready (Raw) and then Reports.Coordinates (Raw).Outcome = Reports.Coordinates_Unavailable);
   end;
   Ada.Text_IO.Put_Line ("HID calibration checks" & Checks'Image);
end Calibration_Tests;
