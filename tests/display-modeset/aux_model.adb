package body AUX_Model is
   Clock : Unsigned_64 := 0;
   Store : Byte_Array (0 .. 32_767) := [others => 0];
   Regs : array (0 .. 5) of Unsigned_32 := [others => 0];  --  CTL, DATA1..5 of channel A
   Segment, Pointer : Natural := 0;
   DPCD_Store : Byte_Array (0 .. 16#FFF#) := [others => 0];

   procedure Reset (EDID : Byte_Array) is
   begin
      Store := [others => 0];
      Store (0 .. EDID'Length - 1) := EDID;
      Regs := [others => 0]; Segment := 0; Pointer := 0;
      Defers := 0; Nak_All := False; Receive_Error_Once := False; Never_Done := False;
      for I in DPCD_Store'Range loop DPCD_Store (I) := Unsigned_8 (I mod 256); end loop;
      DPCD_Store (0) := 16#14#;  --  DPCD 1.4
      DPCD_Store (1) := 16#1E#;  --  HBR3
      DPCD_Store (2) := 16#84#;  --  4 lanes, enhanced framing
   end Reset;

   function DPCD (Address : Natural) return Unsigned_8 is (DPCD_Store (Address));

   function Byte_Of (Index : Positive) return Unsigned_8 is
     (Unsigned_8 (Shift_Right (Regs ((Index - 1) / 4 + 1), 24 - 8 * ((Index - 1) mod 4)) and 16#FF#));

   procedure Reply (Bytes : Byte_Array) is
   begin
      for W in 1 .. 5 loop Regs (W) := 0; end loop;
      for I in Bytes'Range loop
         declare
            K : constant Natural := I - Bytes'First;
         begin
            Regs (K / 4 + 1) := Regs (K / 4 + 1) or Shift_Left (Unsigned_32 (Bytes (I)), 24 - 8 * (K mod 4));
         end;
      end loop;
      Regs (0) := (Regs (0) and not (16#8000_0000# or 16#01F0_0000#)) or 16#4000_0000# or
                  Shift_Left (Unsigned_32 (Bytes'Length), 20);
   end Reply;

   procedure Execute (Size : Natural) is
      Request : constant Unsigned_8 := Shift_Right (Byte_Of (1), 4);
      Address : constant Natural := Natural (Byte_Of (1) and 16#F#) * 65_536 +
        Natural (Byte_Of (2)) * 256 + Natural (Byte_Of (3));
      Count : constant Natural := (if Size > 3 then Natural (Byte_Of (4)) + 1 else 0);
   begin
      if Never_Done then
         Regs (0) := (Regs (0) and not 16#8000_0000#) or 16#1000_0000#;
         return;
      end if;
      if Receive_Error_Once then
         Receive_Error_Once := False;
         Regs (0) := (Regs (0) and not 16#8000_0000#) or 16#0200_0000#;
         return;
      end if;
      if Defers > 0 then
         Defers := Defers - 1;
         Reply ([0 => (if Request >= 8 then 16#20# else 16#80#)]);
         return;
      end if;
      if Nak_All then
         Reply ([0 => (if Request >= 8 then 16#10# else 16#40#)]);
         return;
      end if;
      case Request is
         when 16#9# =>  --  native read
            declare
               Data : Byte_Array (0 .. Count) := [others => 0];
            begin
               for I in 1 .. Count loop Data (I) := DPCD_Store ((Address + I - 1) mod 4096); end loop;
               Reply (Data);
            end;
         when 16#4# | 16#0# =>  --  I2C write (MOT or not)
            if Count >= 1 then
               if Address = 16#30# then Segment := Natural (Byte_Of (5));
               elsif Address = 16#50# then Pointer := Natural (Byte_Of (5));
               end if;
            end if;
            Reply ([0 => 0]);
         when 16#5# | 16#1# =>  --  I2C read
            declare
               Data : Byte_Array (0 .. Count) := [others => 0];
            begin
               for I in 1 .. Count loop
                  Data (I) := Store (Segment * 256 + Pointer);
                  Pointer := (Pointer + 1) mod 256;
               end loop;
               if Request = 16#1# and then Count = 0 then Segment := 0; end if;
               Reply (Data);
            end;
         when others => Reply ([0 => 16#10#]);
      end case;
   end Execute;

   procedure Read (Offset : R.Register_Offset; Value : out Unsigned_32) is
   begin
      Clock := Clock + 1;
      if Offset >= 16#64010# and then Offset <= 16#64024# then
         Value := Regs (Natural (Offset - 16#64010#) / 4);
      else
         Value := 0;
      end if;
   end Read;

   procedure Write (Offset : R.Register_Offset; Value : Unsigned_32; Written : out Boolean) is
   begin
      Clock := Clock + 1;
      Written := True;
      if Offset = 16#64010# then
         if (Value and 16#8000_0000#) /= 0 then
            Regs (0) := Value and not (16#4000_0000# or 16#1000_0000# or 16#0200_0000#);
            Execute (Natural (Shift_Right (Value, 20) and 16#1F#));
         else
            Regs (0) := Regs (0) and not (Value and (16#4000_0000# or 16#1000_0000# or 16#0200_0000#));
         end if;
      elsif Offset > 16#64010# and then Offset <= 16#64024# then
         Regs (Natural (Offset - 16#64010#) / 4) := Value;
      end if;
   end Write;

   procedure Now_Us (Value : out Unsigned_64) is
   begin
      Clock := Clock + 1;
      Value := Clock;
   end Now_Us;
end AUX_Model;
