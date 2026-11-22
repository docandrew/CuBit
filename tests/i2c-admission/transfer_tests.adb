pragma Ada_2022;
with Ada.Text_IO;
with CuBit.Grant_References;
with I2C_Transfer_Wire;
with I2C_Native_Results;
with DW_I2C_Request_Types;
with DW_I2C_Transfer;
with I2C_Work_Policy;
with Intel_LPSS_I2C_Profile;
procedure Transfer_Tests is
   package W renames I2C_Transfer_Wire;
   package G renames CuBit.Grant_References;
   use type W.Word;
   use type W.Count;
   use type W.Decode_Status;
   use type W.Deadline;
   use type W.Request_Words;
   use type W.Completion;
   use type W.Completion_Decode_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Natural'Image (Checks); end if;
   end Check;
   Item : W.Request := (Input => (G.Global_Slot'Last, G.Generation'Last),
                        Output => (0, 1), Write_Count => 1, Read_Count => 1,
                        Limit => (Kind => W.Forever));
   Words : W.Request_Words;
   function Parse (Value : W.Message_Words; Available : W.Capacity := W.Capacity'Last)
                   return W.Decoding is
     (W.Decode (W.Transfer_Operation, W.Protocol_Version, Value, Available));
   procedure Round_Trip is
      D : constant W.Decoding := Parse (W.Encode (Item));
   begin
      Check (D.Status = W.Decoded);
      Check (W.Encode (D.Value) = W.Encode (Item));
      Check (D.Value.Limit = Item.Limit);
   end Round_Trip;
   procedure Test_Completions is
      Reply : W.Completion := (Status => W.Completed, Ticket => W.Word'Last,
         Hardware_Succeeded => False, Effects_Possible => False,
         Requested_Writes => 0, Delivered_Reads => 0);
      Reply_Words : W.Completion_Words;
      function Decode (Value : W.Message_Words) return W.Completion_Decoding is
        (W.Decode_Completion (W.Transfer_Reply_Operation, W.Protocol_Version, Value));
      procedure Round_Trip_Reply is
         D : constant W.Completion_Decoding := Decode (W.Encode_Completion (Reply));
      begin
         Check (D.Status = W.Completion_Decoded);
         Check (D.Value = Reply);
      end Round_Trip_Reply;
   begin
      for Status in W.Completion_Status loop
         for Success in Boolean loop
            for Effects in Boolean loop
               Reply.Status := Status; Reply.Hardware_Succeeded := Success;
               Reply.Effects_Possible := Effects;
               Reply.Requested_Writes := W.Count'Last;
               Reply.Delivered_Reads := 0; Round_Trip_Reply;
               Reply.Ticket := 0; Reply.Delivered_Reads := W.Count'Last;
               Round_Trip_Reply;
            end loop;
         end loop;
      end loop;
      Reply.Status := W.Output_Refused; Reply.Hardware_Succeeded := True;
      Reply.Effects_Possible := True;
      for N in W.Count loop
         Reply.Requested_Writes := N; Reply.Delivered_Reads := W.Count'Last - N;
         Round_Trip_Reply;
      end loop;
      Reply_Words := W.Encode_Completion (Reply);
      Check (W.Decode_Completion (W.Transfer_Operation, 1, Reply_Words).Status =
         W.Completion_Wrong_Operation);
      for P in W.Word range 0 .. 15 loop
         if P /= W.Protocol_Version then
            Check (W.Decode_Completion (W.Transfer_Reply_Operation, P, Reply_Words).Status =
               W.Completion_Wrong_Protocol);
         end if;
      end loop;
      Check (Decode (W.Message_Words'(1 .. 0 => 0)).Status = W.Completion_Wrong_Length);
      Check (Decode (W.Message_Words'(0 .. 4 => 0)).Status = W.Completion_Wrong_Length);
      for Code in W.Word range 13 .. W.Status_Radix - 1 loop
         Reply_Words := W.Encode_Completion (Reply); Reply_Words (0) := Code;
         Check (Decode (Reply_Words).Status = W.Unknown_Status);
      end loop;
      for Bit in 10 .. 63 loop
         Reply_Words := W.Encode_Completion (Reply); Reply_Words (0) := 2 ** Bit;
         Check (Decode (Reply_Words).Status = W.Completion_Reserved_Bits);
      end loop;
      for Bit in 32 .. 63 loop
         Reply_Words := W.Encode_Completion (Reply); Reply_Words (2) := 2 ** Bit;
         Check (Decode (Reply_Words).Status = W.Completion_Reserved_Bits);
      end loop;
      for Bit in 0 .. 63 loop
         Reply_Words := W.Encode_Completion (Reply); Reply_Words (3) := 2 ** Bit;
         Check (Decode (Reply_Words).Status = W.Completion_Reserved_Bits);
      end loop;
      declare Shifted : W.Message_Words (Natural'Last - 3 .. Natural'Last); begin
         Reply_Words := W.Encode_Completion (Reply);
         for N in Reply_Words'Range loop Shifted (Shifted'First + N) := Reply_Words (N); end loop;
         Check (Decode (Shifted).Value = Reply);
      end;
   end Test_Completions;
   procedure Test_Native_Results is
      package Q renames DW_I2C_Request_Types;
      package T renames DW_I2C_Transfer;
      use type W.Completion_Status;
      Expected : constant array (Q.Reason) of W.Completion_Status :=
        [W.Completed, W.Unauthorized, W.Invalid_Channel, W.Unsupported_Channel,
         W.Busy, W.Invalid_Size, W.Input_Refused, W.Input_Uncertain,
         W.Admission_Refused, W.Output_Refused, W.Output_Uncertain, W.Internal_Mismatch];
   begin
      for Reason in Q.Reason loop
         for Success in Boolean loop
            for Effects in Boolean loop
               declare
                  Item : constant Q.Response :=
                    (Status => Reason, ID => T.Ticket'Last,
                     Hardware_Succeeded => Success, Effects_Possible => Effects,
                     Write_Count => T.Count'Last, Read_Count => 17);
                  Encoded : constant W.Completion_Words :=
                    W.Encode_Completion (I2C_Native_Results.To_Wire (Item));
                  D : constant W.Completion_Decoding := W.Decode_Completion
                    (W.Transfer_Reply_Operation, W.Protocol_Version, Encoded);
               begin
                  Check (D.Status = W.Completion_Decoded);
                  Check (D.Value.Status = Expected (Reason));
                  Check (D.Value.Ticket = W.Word'Last);
                  Check (D.Value.Requested_Writes = W.Count'Last);
                  Check (D.Value.Delivered_Reads = 17);
                  Check (D.Value.Hardware_Succeeded = Success);
                  Check (D.Value.Effects_Possible = Effects);
               end;
            end loop;
         end loop;
      end loop;
   end Test_Native_Results;
   procedure Test_Work_Policy is
      package P renames Intel_LPSS_I2C_Profile;
      package Policy renames I2C_Work_Policy;
      use type Policy.Steps;
      use type P.Outcome;
      type Speeds is array (Positive range <>) of P.Hertz;
      Values : constant Speeds := [2100, 100_000, 400_000, 1_000_000];
      Previous, Current : Policy.Steps;
      Timing : P.Timing;
   begin
      for Speed of Values loop
         Timing := P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C1, Speed);
         Check (Timing.Status = P.Calculated);
         Previous := Policy.Base_Steps;
         for N in W.Count loop
            Current := Policy.Budget (N, 0, Timing);
            Check (Current >= Previous);
            Check (Current = Policy.Budget (0, N, Timing));
            Check (W.Word (Current) >= W.Word (Policy.Base_Steps) + 2 * W.Word (N));
            Previous := Current;
         end loop;
      end loop;
      Timing := P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 100_000);
      -- Nominal 65535-byte 100kHz wire time is almost six seconds. At
      -- 256 work steps/ms, the old sixteen-steps/byte policy was too short.
      Check (Policy.Budget (W.Count'Last, 0, Timing) > 1_600_000);
      Check (Policy.Budget (W.Count'Last, W.Count'Last,
        (Status => P.Calculated, High_Count => P.Counter'Last,
         Low_Count => P.Counter'Last, others => <>)) >= Policy.Base_Steps);
      Check (Policy.Budget (W.Count'Last, W.Count'Last,
        (Status => P.Counter_Overflow, others => <>)) = Policy.Base_Steps);
   end Test_Work_Policy;
begin
   for N in W.Capacity loop
      Item.Write_Count := N; Item.Read_Count := 0; Round_Trip;
      Item.Write_Count := 0; Item.Read_Count := N; Round_Trip;
      Item.Write_Count := N; Item.Read_Count := W.Count'Last - N;
      Round_Trip;
   end loop;
   Item.Write_Count := 1; Item.Read_Count := 1;
   Item.Limit := (Kind => W.Within, At_Millisecond => 0); Round_Trip;
   Item.Limit := (Kind => W.Within, At_Millisecond => W.Millisecond'Last); Round_Trip;
   Words := W.Encode (Item);
   Check (Parse (Words, 1).Status = W.Invalid_Size);
   Check (Parse (Words, 2).Status = W.Decoded);
   Check (W.Decode (W.Transfer_Reply_Operation, 1, Words, 2).Status = W.Wrong_Operation);
   for P in W.Word range 0 .. 15 loop
      if P /= W.Protocol_Version then
         Check (W.Decode (W.Transfer_Operation, P, Words, 2).Status = W.Wrong_Protocol);
      end if;
   end loop;
   Check (Parse (W.Message_Words'(1 .. 0 => 0)).Status = W.Wrong_Length);
   Check (Parse (W.Message_Words'(0 .. 4 => 0)).Status = W.Wrong_Length);
   declare Shifted : W.Message_Words (Natural'Last - 3 .. Natural'Last); begin
      for N in Words'Range loop Shifted (Shifted'First + N) := Words (N); end loop;
      Check (Parse (Shifted).Status = W.Decoded);
   end;
   for Bit in 32 .. 63 loop
      Words (W.Counts_Index) := 2 ** Bit;
      Check (Parse (Words).Status = W.Reserved_Count_Bits);
   end loop;
   Words := W.Encode (Item); Words (W.Counts_Index) := 0;
   Check (Parse (Words).Status = W.Invalid_Size);
   Words := W.Encode (Item); Words (W.Counts_Index) := W.Reserved_Base - 1;
   Check (Parse (Words).Status = W.Invalid_Size);
   for Index in W.Input_Index .. W.Output_Index loop
      for Bit in 20 .. 31 loop
         Words := W.Encode (Item); Words (Index) := Words (Index) or 2 ** Bit;
         Check (Parse (Words).Status =
           (if Index = W.Input_Index then W.Invalid_Input else W.Invalid_Output));
      end loop;
      Words := W.Encode (Item); Words (Index) := 0;
      Check (Parse (Words).Status =
        (if Index = W.Input_Index then W.Invalid_Input else W.Invalid_Output));
   end loop;
   Item.Write_Count := 0; Words := W.Encode (Item); Words (W.Input_Index) := 1;
   Check (Parse (Words).Status = W.Invalid_Input);
   Item.Write_Count := 1; Item.Read_Count := 0;
   Words := W.Encode (Item); Words (W.Output_Index) := 1;
   Check (Parse (Words).Status = W.Invalid_Output);
   Test_Completions;
   Test_Native_Results;
   Test_Work_Policy;
   Ada.Text_IO.Put_Line ("transfer wire checks" & Natural'Image (Checks));
end Transfer_Tests;
