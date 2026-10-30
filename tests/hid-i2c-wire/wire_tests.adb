pragma Ada_2022;
with Ada.Text_IO;
with HID_I2C_Wire;
with HID_I2C_Descriptors;
procedure Wire_Tests is
   package W renames HID_I2C_Wire;
   package D renames HID_I2C_Descriptors;
   use type D.Byte;
   use type D.Byte_Count;
   use type W.Frame_Status;
   use type W.Control;
   use type D.Bytes;
   Checks : Natural := 0;
   Result : W.Frame_Result;
   Full : D.Bytes (0 .. Natural (D.Byte_Count'Last) - 1) := [others => 16#AB#];
   Small : D.Bytes (100 .. 102) := [others => 16#CD#];
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Prefix (Buffer : in out D.Bytes; Length : D.Byte_Count) is
   begin
      Buffer (Buffer'First) := D.Byte (Length mod 256);
      Buffer (Buffer'First + 1) := D.Byte (Length / 256);
   end Prefix;
   procedure Refusal (Status : W.Frame_Status) is
   begin
      Check (Result.Status = Status, "typed frame result");
      Check (Result.Payload_Length = 0, "no payload on refusal/control/empty");
   end Refusal;
begin
   --  Independent fixed vectors document byte order and reserved/power bits.
   Check (W.Select_Register (16#ABCD#) = [16#CD#, 16#AB#], "register fixed vector");
   Check (W.Encode_Control (16#ABCD#, W.Reset) = [16#CD#, 16#AB#, 0, 1], "reset fixed vector");
   Check (W.Encode_Control (16#ABCD#, W.Power_On) = [16#CD#, 16#AB#, 0, 8], "power fixed vector");
   Check (W.Encode_Control (16#ABCD#, W.Sleep) = [16#CD#, 16#AB#, 1, 8], "sleep fixed vector");
   for Register_Value in Natural range 0 .. 65_535 loop
      declare
         Number : constant D.Register_Number := D.Register_Number (Register_Value);
         Selector : constant W.Register_Selector := W.Select_Register (Number);
      begin
         Check (Natural (Selector (0)) + 256 * Natural (Selector (1)) = Register_Value, "selector exact round trip");
         for Kind in W.Control loop
            declare Message : constant W.Control_Message := W.Encode_Control (Number, Kind); begin
               Check (Message (0 .. 1) = Selector, "control register exact");
               Check (Message (2) = (if Kind = W.Sleep then 1 else 0), "reserved/power bits exact");
               Check (Message (3) = (if Kind = W.Reset then 1 else 8), "typed opcode exact");
            end;
         end loop;
      end;
   end loop;
   --  Every possible length, with a full transfer, a three-byte transfer and
   --  a smaller advertised maximum. Never forward trailing transfer padding.
   for Length in D.Byte_Count loop
      Prefix (Full, Length); Prefix (Small, Length);
      Result := W.Inspect_Input (Full, D.Byte_Count'Last, True);
      Check (Result.Declared_Length = Length, "prefix exact full width");
      case Length is
         when 0 => Refusal (W.Reset_Completed);
         when 1 => Refusal (W.Invalid_Length);
         when 2 => Refusal (W.Empty_Frame);
         when others =>
            Check (Result.Status = W.Input_Report and Result.Payload_Length = Length - 2, "full payload span");
      end case;
      Result := W.Inspect_Input (Small, D.Byte_Count'Last, False);
      case Length is
         when 0 => Refusal (W.Unexpected_Reset);
         when 1 => Refusal (W.Invalid_Length);
         when 2 => Refusal (W.Empty_Frame);
         when 3 => Check (Result.Status = W.Input_Report and Result.Payload_Length = 1, "minimum report");
         when others => Refusal (W.Truncated);
      end case;
      Result := W.Inspect_Input (Full, 3000, False);
      case Length is
         when 0 => Refusal (W.Unexpected_Reset);
         when 1 => Refusal (W.Invalid_Length);
         when 2 => Refusal (W.Empty_Frame);
         when 3 .. 3000 =>
            Check (Result.Status = W.Input_Report and Result.Payload_Length = Length - 2, "bounded report span");
         when others => Refusal (W.Exceeds_Maximum);
      end case;
   end loop;
   for Maximum in D.Byte_Count range 0 .. 1 loop
      Result := W.Inspect_Input ([0, 0], Maximum, True); Refusal (W.Invalid_Maximum);
   end loop;
   declare
      Empty : constant D.Bytes (1 .. 0) := [others => 0];
      One : constant D.Bytes := [100 => 0];
      High : constant D.Bytes (Natural'Last - 2 .. Natural'Last) := [3, 0, 16#42#];
      Over_Word : constant D.Bytes (0 .. 65_535) := [others => 0];
   begin
      Result := W.Inspect_Input (Empty, 3, False); Refusal (W.Short_Prefix);
      Result := W.Inspect_Input (One, 3, True); Refusal (W.Short_Prefix);
      Result := W.Inspect_Input (High, 3, False);
      Check (Result.Status = W.Input_Report and Result.Payload_Length = 1, "high index span");
      Check (W.Buffer_Capacity (Empty) = 0, "null buffer capacity");
      Check (W.Buffer_Capacity (One) = 1, "single buffer capacity");
      Check (W.Buffer_Capacity (High) = 3, "high buffer capacity");
      Check (W.Buffer_Capacity (Full) = D.Byte_Count'Last, "maximum protocol capacity");
      Check (W.Buffer_Capacity (Over_Word) = D.Byte_Count'Last, "larger buffer clamps capacity");
   end;
   --  Report bytes and optional Report ID are opaque. Do not invent a boot
   --  mouse/touchpad format, strip an ID byte or include nonzero bus padding.
   Result := W.Inspect_Input ([4, 0, 16#AA#, 16#BB#, 16#CC#, 16#DD#], 6, False);
   Check (Result.Status = W.Input_Report and Result.Payload_Length = 2, "padding excluded");
   Result := W.Inspect_Input ([0, 0, 16#FF#, 16#FF#], 4, True); Refusal (W.Reset_Completed);
   Ada.Text_IO.Put_Line ("HID I2C wire checks" & Checks'Image);
end Wire_Tests;
