package body Intel_GPU_DP_AUX with SPARK_Mode is
   package R renames Intel_GPU_Display_Registers;

   AUX_CTL_A : constant := 16#6_4010#;
   AUX_Stride : constant := 16#100#;
   Data_Registers : constant := 5;
   function Control_Register (Line : Channel) return R.Register_Offset is
     (AUX_CTL_A + AUX_Stride * Channel'Pos (Line));
   function Data_Register (Line : Channel; Index : Positive) return R.Register_Offset is
     (Control_Register (Line) + 4 * Unsigned_32 (Index))
     with Pre => Index <= Data_Registers;

   --  DP_AUX_CH_CTL (i915 skl_get_aux_send_ctl).
   Send_Busy : constant Unsigned_32 := 16#8000_0000#;
   Done : constant Unsigned_32 := 16#4000_0000#;
   Interrupt : constant Unsigned_32 := 16#2000_0000#;
   Timeout_Error : constant Unsigned_32 := 16#1000_0000#;
   Timeout_Max : constant Unsigned_32 := 16#0C00_0000#;
   Receive_Error_Bit : constant Unsigned_32 := 16#0200_0000#;
   Size_Shift : constant := 20;
   Size_Mask : constant Unsigned_32 := 16#1F#;
   --  intel_dp_aux_sync_len (10 precharge + 16 preamble) and
   --  intel_dp_aux_fw_sync_len (10 + 8), each stored minus one.
   Sync_Pulses : constant Unsigned_32 := 26 - 1;
   FW_Sync_Pulses : constant Unsigned_32 := 18 - 1;
   FW_Sync_Shift : constant := 5;

   --  AUX request and reply codes (DisplayPort 1.4 section 2.7, drm_dp.h).
   I2C_Write : constant Unsigned_8 := 16#0#;
   I2C_Read : constant Unsigned_8 := 16#1#;
   I2C_MOT : constant Unsigned_8 := 16#4#;
   Native_Read : constant Unsigned_8 := 16#9#;
   Native_Reply_Mask : constant Unsigned_8 := 16#30#;
   Native_NAK : constant Unsigned_8 := 16#10#;
   Native_Defer : constant Unsigned_8 := 16#20#;
   I2C_Reply_Mask : constant Unsigned_8 := 16#C0#;
   I2C_NAK : constant Unsigned_8 := 16#40#;
   I2C_Defer : constant Unsigned_8 := 16#80#;
   EDID_Address : constant := 16#50#;
   Segment_Address : constant := 16#30#;

   Transfer_Tries : constant := 5;
   Defer_Tries : constant := 32;
   Busy_Timeout_Us : constant := 3_000;
   Done_Timeout_Us : constant := 10_000;
   Receive_Pause_Us : constant := 500;
   Max_Polls : constant := 4_000_000;
   --  A message is a 4-byte header plus up to 16 data bytes.
   Max_Message : constant := 20;
   subtype Message_Length is Positive range 1 .. Max_Message;
   type Message is array (1 .. Max_Message) of Unsigned_8;

   procedure Put (Offset : R.Register_Offset; Value : Unsigned_32; Result : in out Status) is
      Written : Boolean;
   begin
      if Result = OK then
         Write (Offset, Value, Written);
         if not Written then Result := Not_Writable; end if;
      end if;
   end Put;

   --  Wait until (Read (Control) and Mask) = 0, or the deadline.
   procedure Wait_Clear (Line : Channel; Mask : Unsigned_32; Timeout_Us : Unsigned_64; Value : out Unsigned_32;
                         Cleared : out Boolean) is
      Started, Now : Unsigned_64;
   begin
      Now_Us (Started);
      Cleared := False;
      Value := Unsigned_32'Last;
      for Poll in 1 .. Max_Polls loop
         Read (Control_Register (Line), Value);
         if Value /= Unsigned_32'Last and then (Value and Mask) = 0 then
            Cleared := True;
            return;
         end if;
         Now_Us (Now);
         exit when Started = Unsigned_64'Last or else Now = Unsigned_64'Last or else Now < Started or else
           Now - Started >= Timeout_Us;
      end loop;
   end Wait_Clear;

   procedure Pause_Us (Microseconds : Unsigned_64) is
      Started, Now : Unsigned_64;
   begin
      Now_Us (Started);
      for Poll in 1 .. Max_Polls loop
         Now_Us (Now);
         exit when Started = Unsigned_64'Last or else Now = Unsigned_64'Last or else Now < Started or else
           Now - Started >= Microseconds;
      end loop;
   end Pause_Us;

   --  One AUX transaction (intel_dp_aux_xfer): send Length bytes of
   --  Request, receive the reply (Reply (1) is its header byte). Transfer
   --  and Exchange carry contracts so the prover checks each once instead of
   --  inlining them into every caller (inlined, Read_EDID_Block's proof
   --  needed over 12 GB).
   procedure Transfer (Line : Channel; Request : Message; Length : Message_Length;
                       Reply : out Message; Reply_Length : out Natural; Result : out Status)
     with Post => Reply_Length <= Max_Message;
   procedure Transfer (Line : Channel; Request : Message; Length : Message_Length;
                       Reply : out Message; Reply_Length : out Natural; Result : out Status) is
      Value : Unsigned_32;
      Cleared : Boolean;
      Finished : Boolean := False;
   begin
      Reply := [others => 0];
      Reply_Length := 0;
      Result := OK;
      Wait_Clear (Line, Send_Busy, Busy_Timeout_Us, Value, Cleared);
      if not Cleared then
         Result := Busy;
         return;
      end if;
      for Try in 1 .. Transfer_Tries loop
         for Word in 1 .. Data_Registers loop
            declare
               Packed : Unsigned_32 := 0;
            begin
               for B in 0 .. 3 loop
                  Packed := Shift_Left (Packed, 8);
                  if (Word - 1) * 4 + B + 1 <= Length then
                     Packed := Packed or Unsigned_32 (Request ((Word - 1) * 4 + B + 1));
                  end if;
               end loop;
               Put (Data_Register (Line, Word), Packed, Result);
            end;
         end loop;
         Put (Control_Register (Line),
              Send_Busy or Done or Interrupt or Timeout_Error or Timeout_Max or Receive_Error_Bit or
              Shift_Left (Unsigned_32 (Length), Size_Shift) or
              Shift_Left (FW_Sync_Pulses, FW_Sync_Shift) or Sync_Pulses, Result);
         if Result /= OK then
            return;
         end if;
         Wait_Clear (Line, Send_Busy, Done_Timeout_Us, Value, Cleared);
         --  Clear the sticky status bits (write one to clear).
         Put (Control_Register (Line), Value or Done or Timeout_Error or Receive_Error_Bit, Result);
         if Result /= OK then
            return;
         end if;
         if not Cleared or else (Value and Timeout_Error) /= 0 then
            Result := Timeout;
         elsif (Value and Receive_Error_Bit) /= 0 then
            Result := Receive_Error;
            Pause_Us (Receive_Pause_Us);
         elsif (Value and Done) /= 0 then
            Result := OK;
            Finished := True;
            exit;
         else
            Result := Timeout;
         end if;
         if Try < Transfer_Tries then
            Result := OK;
         end if;
      end loop;
      if not Finished then
         if Result = OK then Result := Timeout; end if;
         return;
      end if;
      Reply_Length := Natural (Shift_Right (Value, Size_Shift) and Size_Mask);
      if Reply_Length = 0 or else Reply_Length > Max_Message then
         Reply_Length := 0;
         Result := Short_Reply;
         return;
      end if;
      for Word in 1 .. Data_Registers loop
         Read (Data_Register (Line, Word), Value);
         for B in 0 .. 3 loop
            Reply ((Word - 1) * 4 + B + 1) := Unsigned_8 (Shift_Right (Value, 24 - 8 * B) and 16#FF#);
         end loop;
      end loop;
   end Transfer;

   function Header (Request : Unsigned_8; Address : Unsigned_32; Length : Natural) return Message is
     ([1 => Shift_Left (Request, 4) or Unsigned_8 (Shift_Right (Address, 16) and 16#F#),
       2 => Unsigned_8 (Shift_Right (Address, 8) and 16#FF#),
       3 => Unsigned_8 (Address and 16#FF#),
       4 => Unsigned_8 ((if Length = 0 then 0 else Length - 1) mod 256),
       others => 0])
     with Pre => Request <= 16#F# and then Length <= Max_Payload;

   --  A transaction with DEFER retries; I2C replies also checked.
   procedure Exchange (Line : Channel; Request : Message; Length : Message_Length; I2C : Boolean;
                       Reply : out Message; Reply_Length : out Natural; Result : out Status)
     with Post => Reply_Length <= Max_Message;
   procedure Exchange (Line : Channel; Request : Message; Length : Message_Length; I2C : Boolean;
                       Reply : out Message; Reply_Length : out Natural; Result : out Status) is
   begin
      Reply := [others => 0];
      Reply_Length := 0;
      Result := Defer_Exhausted;
      for Try in 1 .. Defer_Tries loop
         Transfer (Line, Request, Length, Reply, Reply_Length, Result);
         exit when Result /= OK;
         if (Reply (1) and Native_Reply_Mask) = Native_NAK then
            Result := NAK; exit;
         elsif (Reply (1) and Native_Reply_Mask) = Native_Defer then
            Result := Defer_Exhausted;
            Pause_Us (Receive_Pause_Us);
         elsif I2C and then (Reply (1) and I2C_Reply_Mask) = I2C_NAK then
            Result := NAK; exit;
         elsif I2C and then (Reply (1) and I2C_Reply_Mask) = I2C_Defer then
            Result := Defer_Exhausted;
            Pause_Us (Receive_Pause_Us);
         else
            Result := OK; exit;
         end if;
      end loop;
   end Exchange;

   procedure Read_DPCD (Line : Channel; Address : DPCD_Address; Data : out Bytes; Result : out Status) is
      Reply : Message;
      Got : Natural;
   begin
      Data := [others => 0];
      Exchange (Line, Header (Native_Read, Address, Data'Length), 4, False, Reply, Got, Result);
      if Result = OK and then Got < Data'Length + 1 then
         Result := Short_Reply;
      elsif Result = OK then
         for I in Data'Range loop
            Data (I) := Reply (I + 1);
         end loop;
      end if;
   end Read_DPCD;

   procedure I2C_Write_Byte (Line : Channel; Device : Unsigned_32; Value : Unsigned_8; Result : out Status) is
      Request : Message := Header (I2C_Write or I2C_MOT, Device, 1);
      Reply : Message;
      Got : Natural;
   begin
      Request (5) := Value;
      Exchange (Line, Request, 5, True, Reply, Got, Result);
   end I2C_Write_Byte;

   procedure Read_EDID_Block (Line : Channel; Block : EDID_Block_Index; Data : out Bytes; Result : out Status) is
      Reply : Message;
      Got : Natural;
      Stop : Status;
   begin
      Data := [others => 0];
      Result := OK;
      if Block >= 2 then
         I2C_Write_Byte (Line, Segment_Address, Unsigned_8 (Block / 2), Result);
      end if;
      if Result = OK then
         I2C_Write_Byte (Line, EDID_Address, Unsigned_8 ((Block mod 2) * EDID_Block_Bytes), Result);
      end if;
      for Chunk in 0 .. EDID_Block_Bytes / Max_Payload - 1 loop
         exit when Result /= OK;
         Exchange (Line, Header (I2C_Read or I2C_MOT, EDID_Address, Max_Payload), 4, True, Reply, Got, Result);
         if Result = OK and then Got < Max_Payload + 1 then
            Result := Short_Reply;
         elsif Result = OK then
            for I in 1 .. Max_Payload loop
               Data (Chunk * Max_Payload + I) := Reply (I + 1);
            end loop;
         end if;
      end loop;
      --  End the I2C transaction (address-only read without MOT).
      Exchange (Line, Header (I2C_Read, EDID_Address, 0), 3, True, Reply, Got, Stop);
      --  The data is read; only a broken channel (not a NAK of the stop)
      --  makes the block unusable.
      if Result = OK and then Stop not in OK | NAK then
         Result := Stop;
      end if;
   end Read_EDID_Block;
end Intel_GPU_DP_AUX;
