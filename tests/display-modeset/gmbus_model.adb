package body GMBUS_Model is
   type Phase is (Idle, Reading, Writing, Wait_Phase);
   Clock : Unsigned_64 := 0;
   Pin, Wanted_Pin : Unsigned_32 := 0;
   Current : Phase := Idle;
   Active, Nak : Boolean := False;
   Device : Unsigned_32 := 0;
   Remaining : Natural := 0;
   Latched : Unsigned_32 := 0;
   Segment : Natural := 0;
   Pointer : Natural := 0;
   SCDC_Index : Natural := 0;
   First_Write_Byte : Boolean := True;
   SCDC : Byte_Array (0 .. 255) := [others => 0];
   Store : Byte_Array (0 .. 32_767) := [others => 0];
   Store_Length : Natural := 0;
   Stop_Count, Errors : Natural := 0;

   procedure Reset (EDID : Byte_Array; Connected_Pin : Unsigned_32) is
   begin
      Store := [others => 0];
      Store (0 .. EDID'Length - 1) := EDID;
      Store_Length := EDID'Length;
      Wanted_Pin := Connected_Pin;
      Pin := 0; Current := Idle; Active := False; Nak := False; Remaining := 0;
      Segment := 0; Pointer := 0; SCDC := [others => 0]; Stop_Count := 0; Errors := 0;
      No_Ready := False; Not_Writable := False;
   end Reset;

   procedure Consume (Word : Unsigned_32) is
      W : Unsigned_32 := Word;
   begin
      for I in 1 .. 4 loop
         exit when Remaining = 0;
         declare
            B : constant Unsigned_8 := Unsigned_8 (W and 16#FF#);
         begin
            case Device is
               when 16#30# => Segment := Natural (B);
               when 16#50# => Pointer := Natural (B);
               when 16#54# =>
                  if First_Write_Byte then
                     SCDC_Index := Natural (B);
                  else
                     SCDC (SCDC_Index mod 256) := B;
                     SCDC_Index := SCDC_Index + 1;
                  end if;
               when others => Errors := Errors + 1;
            end case;
            First_Write_Byte := False;
         end;
         W := Shift_Right (W, 8);
         Remaining := Remaining - 1;
      end loop;
      if Remaining = 0 then Current := Wait_Phase; end if;
   end Consume;

   procedure Read (Offset : R.Register_Offset; Value : out Unsigned_32) is
   begin
      Clock := Clock + 1;
      Value := 0;
      if Offset = R.GMBUS2 then
         if Nak then Value := Value or 16#0400#; end if;
         if not No_Ready and then Remaining > 0 and then Current in Reading | Writing then
            Value := Value or 16#0800#;
         end if;
         if Current = Wait_Phase then Value := Value or 16#4000#; end if;
         if Active then Value := Value or 16#0200#; end if;
      elsif Offset = R.GMBUS3 then
         if Current /= Reading or else Remaining = 0 or else No_Ready then
            Errors := Errors + 1;
            return;
         end if;
         for I in 0 .. 3 loop
            exit when Remaining = 0;
            declare
               B : Unsigned_8 := 0;
            begin
               if Device = 16#50# then
                  if Segment * 256 + Pointer < Store_Length then
                     B := Store (Segment * 256 + Pointer);
                  end if;
                  Pointer := (Pointer + 1) mod 256;
               elsif Device = 16#54# then
                  B := SCDC (SCDC_Index mod 256);
                  SCDC_Index := SCDC_Index + 1;
               end if;
               Value := Value or Shift_Left (Unsigned_32 (B), 8 * I);
            end;
            Remaining := Remaining - 1;
         end loop;
         if Remaining = 0 then Current := Wait_Phase; end if;
      end if;
   end Read;

   procedure Write (Offset : R.Register_Offset; Value : Unsigned_32; Written : out Boolean) is
   begin
      Clock := Clock + 1;
      Written := not Not_Writable;
      if Not_Writable then return; end if;
      if Offset = R.GMBUS0 then
         Pin := Value and 16#1F#;
         if Value = 0 then
            if Active then Errors := Errors + 1; end if;
            Current := Idle;
         end if;
      elsif Offset = R.GMBUS3 then
         Latched := Value;
         if Current = Writing and then Remaining > 0 then Consume (Value); end if;
      elsif Offset = R.GMBUS1 then
         if (Value and 16#8000_0000#) /= 0 then
            Nak := False; Active := False; Current := Idle; Remaining := 0;
         elsif Value = 0 then
            null;
         elsif (Value and 16#4000_0000#) = 0 then
            Errors := Errors + 1;
         elsif (Value and 16#0800_0000#) /= 0 then
            if Current /= Wait_Phase or else Nak then Errors := Errors + 1; end if;
            Stop_Count := Stop_Count + 1;
            Active := False;
            Current := Idle;
            Segment := 0;
         else
            Device := Shift_Right (Value, 1) and 16#7F#;
            Remaining := Natural (Shift_Right (Value, 16) and 16#1FF#);
            Active := True;
            if Pin /= Wanted_Pin or else Device not in 16#30# | 16#50# | 16#54# then
               Nak := True;
               Current := Idle;
               return;
            end if;
            if (Value and 1) /= 0 then
               if (Value and 16#0400_0000#) /= 0 then
                  if Device = 16#54# then
                     SCDC_Index := Natural (Shift_Right (Value, 8) and 16#FF#);
                  else
                     Pointer := Natural (Shift_Right (Value, 8) and 16#FF#);
                  end if;
               end if;
               Current := Reading;
            else
               Current := Writing;
               First_Write_Byte := True;
               Consume (Latched);
            end if;
         end if;
      end if;
   end Write;

   procedure Now_Us (Value : out Unsigned_64) is
   begin
      Clock := Clock + 1;
      Value := Clock;
   end Now_Us;

   function SCDC_Register (Index : Unsigned_8) return Unsigned_8 is (SCDC (Natural (Index)));
   function Stops return Natural is (Stop_Count);
   function Released return Boolean is (Pin = 0 and then not Active);
   function Protocol_Errors return Natural is (Errors);
end GMBUS_Model;
