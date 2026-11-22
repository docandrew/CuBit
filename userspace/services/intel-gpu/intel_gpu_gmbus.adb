package body Intel_GPU_GMBUS with SPARK_Mode is
   package R renames Intel_GPU_Display_Registers;

   --  GMBUS0: 100 kHz (rate field zero) and the pin pair.
   Rate_100_kHz : constant Unsigned_32 := 0;
   --  GMBUS1 (i915 intel_gmbus_regs.h).
   SW_Clear_Interrupt : constant Unsigned_32 := 16#8000_0000#;
   SW_Ready : constant Unsigned_32 := 16#4000_0000#;
   Cycle_Wait : constant Unsigned_32 := 16#0200_0000#;
   Cycle_Index : constant Unsigned_32 := 16#0400_0000#;
   Cycle_Stop : constant Unsigned_32 := 16#0800_0000#;
   Byte_Count_Shift : constant := 16;
   Index_Shift : constant := 8;
   Address_Shift : constant := 1;
   Direction_Read : constant Unsigned_32 := 1;
   --  GMBUS2.
   Wait_Phase : constant Unsigned_32 := 16#4000#;
   Hardware_Ready : constant Unsigned_32 := 16#0800#;
   Slave_Error : constant Unsigned_32 := 16#0400#;
   Active : constant Unsigned_32 := 16#0200#;
   --  i915 gmbus_wait (50 ms) and gmbus_wait_idle (10 ms).
   Ready_Timeout_Us : constant := 50_000;
   Idle_Timeout_Us : constant := 10_000;
   --  A stopped clock cannot hang the driver: at most this many polls per wait.
   Max_Polls : constant := 4_000_000;
   Bytes_Per_Data_Register : constant := 4;

   procedure Put (Offset : R.Register_Offset; Value : Unsigned_32; Result : in out Status) is
      Written : Boolean;
   begin
      if Result = OK then
         Write (Offset, Value, Written);
         if not Written then
            Result := Not_Writable;
         end if;
      end if;
   end Put;

   --  Wait until GMBUS2 shows Condition (or, when Clear, that it is gone).
   procedure Wait (Condition : Unsigned_32; Clear : Boolean; Timeout_Us : Unsigned_64;
                   Failure : Status; Result : in out Status) is
      Started, Now : Unsigned_64;
      Value : Unsigned_32;
   begin
      if Result /= OK then
         return;
      end if;
      Now_Us (Started);
      for Poll in 1 .. Max_Polls loop
         Read (R.GMBUS2, Value);
         if not Clear and then (Value and Slave_Error) /= 0 and then Value /= Unsigned_32'Last then
            Result := NAK;
            return;
         end if;
         if Value /= Unsigned_32'Last and then
           ((Value and Condition) /= 0) = not Clear
         then
            return;
         end if;
         Now_Us (Now);
         exit when Started = Unsigned_64'Last or else Now = Unsigned_64'Last or else
           Now < Started or else Now - Started >= Timeout_Us;
      end loop;
      Result := Failure;
   end Wait;

   procedure Start (Line : Pin; Result : out Status) is
   begin
      Result := OK;
      Put (R.GMBUS0, Rate_100_kHz or Pin_Code (Line), Result);
   end Start;

   --  STOP, wait for idle, release the pins. After a NAK, clear the
   --  controller first (i915 gmbus_xfer clear_err).
   procedure Finish (Result : in out Status) is
      Release : Status := OK;
   begin
      if Result = NAK then
         Put (R.GMBUS1, SW_Clear_Interrupt, Release);
         Put (R.GMBUS1, 0, Release);
      elsif Result = OK then
         Put (R.GMBUS1, Cycle_Stop or SW_Ready, Release);
         Wait (Active, True, Idle_Timeout_Us, Timeout_Idle, Release);
      end if;
      Release := (if Release = Not_Writable then Release else OK);
      Put (R.GMBUS0, 0, Release);
      if Result = OK then
         Result := Release;
      end if;
   end Finish;

   function Command (Cycle : Unsigned_32; Length : Transfer_Length; Index : Unsigned_8;
                     Device : Address; Read_Cycle : Boolean) return Unsigned_32 is
     (SW_Ready or Cycle or Shift_Left (Unsigned_32 (Length), Byte_Count_Shift) or
      Shift_Left (Unsigned_32 (Index), Index_Shift) or
      Shift_Left (Unsigned_32 (Device), Address_Shift) or
      (if Read_Cycle then Direction_Read else 0));

   --  The data word for Data (First .. First + 3), little-endian.
   function Pack (Data : Bytes; First : Positive) return Unsigned_32 is
      Word : Unsigned_32 := 0;
   begin
      for I in reverse 0 .. Bytes_Per_Data_Register - 1 loop
         Word := Shift_Left (Word, 8);
         if First <= Data'Last - I then
            Word := Word or Unsigned_32 (Data (First + I));
         end if;
      end loop;
      return Word;
   end Pack;

   procedure Index_Read (Device : Address; Index : Unsigned_8; Data : out Bytes; Result : in out Status)
     with Pre => Data'Length in Transfer_Length and then Data'First = 1
   is
      Word : Unsigned_32;
      Words : constant Positive := (Data'Length + Bytes_Per_Data_Register - 1) / Bytes_Per_Data_Register;
   begin
      Data := [others => 0];
      Put (R.GMBUS1, Command (Cycle_Index or Cycle_Wait, Data'Length, Index, Device, True), Result);
      for Item in 0 .. Words - 1 loop
         Wait (Hardware_Ready, False, Ready_Timeout_Us, Timeout_Ready, Result);
         exit when Result /= OK;
         Read (R.GMBUS3, Word);
         for Shift in 0 .. Bytes_Per_Data_Register - 1 loop
            exit when Item * Bytes_Per_Data_Register + Shift >= Data'Length;
            Data (Item * Bytes_Per_Data_Register + Shift + 1) :=
              Unsigned_8 (Shift_Right (Word, 8 * Shift) and 16#FF#);
         end loop;
      end loop;
      Wait (Wait_Phase, False, Ready_Timeout_Us, Timeout_Wait_Phase, Result);
   end Index_Read;

   procedure Write_Data (Device : Address; Data : Bytes; Result : in out Status)
     with Pre => Data'Length in Transfer_Length and then Data'First = 1
   is
      Position : Positive := 1;
   begin
      Put (R.GMBUS3, Pack (Data, Position), Result);
      Put (R.GMBUS1, Command (Cycle_Wait, Data'Length, 0, Device, False), Result);
      while Result = OK and then Data'Last - Position >= Bytes_Per_Data_Register loop
         pragma Loop_Variant (Increases => Position);
         Position := Position + Bytes_Per_Data_Register;
         Wait (Hardware_Ready, False, Ready_Timeout_Us, Timeout_Ready, Result);
         Put (R.GMBUS3, Pack (Data, Position), Result);
      end loop;
      Wait (Wait_Phase, False, Ready_Timeout_Us, Timeout_Wait_Phase, Result);
   end Write_Data;

   procedure Read_Indexed
     (Line : Pin; Device : Address; Index : Unsigned_8; Data : out Bytes; Result : out Status) is
   begin
      Start (Line, Result);
      Index_Read (Device, Index, Data, Result);
      Finish (Result);
   end Read_Indexed;

   procedure Write_Bytes (Line : Pin; Device : Address; Data : Bytes; Result : out Status) is
   begin
      Start (Line, Result);
      Write_Data (Device, Data, Result);
      Finish (Result);
   end Write_Bytes;

   procedure Read_EDID_Block
     (Line : Pin; Block : EDID_Block_Index; Data : out Bytes; Result : out Status)
   is
      Blocks_Per_Segment : constant := 2;
      Segment : constant Unsigned_8 := Unsigned_8 (Block / Blocks_Per_Segment);
   begin
      Start (Line, Result);
      --  Blocks 0 and 1 need no segment pointer (plain DDC monitors NAK it).
      if Segment /= 0 then
         Write_Data (DDC_Segment, [1 => Segment], Result);
      end if;
      Index_Read (DDC_EDID, Unsigned_8 ((Block mod Blocks_Per_Segment) * EDID_Block_Bytes), Data, Result);
      Finish (Result);
   end Read_EDID_Block;
end Intel_GPU_GMBUS;
