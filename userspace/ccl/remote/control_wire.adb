with CBOR.Decoding;
with CBOR.Encoding;
with CCL.Sessions;
with CCL.Language;
with CCL.Interfaces.Clock;
with CCL.Periodic_Programs;

package body Control_Wire with SPARK_Mode is
   use CBOR;
   use type CBOR.SE_Offset;
   use type CBOR.Byte;
   use type Interfaces.Unsigned_64;
   use type CCL.Control.Operation;
   use type CCL.Language.Interpretation_Status;
   procedure Decode (Data : CBOR.Byte_Array; Value : out Request; Valid : out Boolean) is
   begin
      Value := (others => <>); Valid := False;
      if Data'First < 0 or else Data'Last > Decoding.Max_Data_Length or else
        Data'Length not in 1 .. Max_Request then return; end if;
      declare
         R : constant Decode_All_Result := Decoding.Decode_All_Strict
           (Data, Check_UTF8 => True, Max_String_Len => Max_Source, Max_Depth => 1);
      begin
         if R.Status /= OK or else R.Count not in 5 .. 6 or else
           R.Items (1).Kind /= MT_Array or else R.Items (1).Arr_Count /= UInt64 (R.Count - 1) or else
           R.Items (2).Kind /= MT_Unsigned_Integer or else R.Items (2).UInt_Value /= 1 or else
           R.Items (3).Kind /= MT_Unsigned_Integer or else R.Items (3).UInt_Value = 0 or else
           R.Items (4).Kind /= MT_Unsigned_Integer or else R.Items (4).UInt_Value not in 1 .. 6 or else
           R.Items (5).Kind /= MT_Text_String or else R.Items (5).TS_Ref.Length > Max_Source
         then return; end if;
         declare
            Ref : constant String_Ref := R.Items (5).TS_Ref;
         begin
            if Ref.First < Data'First or else Ref.First > Data'Last + 1 or else
              Ref.Length > Data'Last - Ref.First + 1 then return; end if;
            Value.Length := Natural (Ref.Length);
            for I in 1 .. Value.Length loop
               declare
                  C : constant Character := Character'Val (Data (Ref.First + SE_Offset (I - 1)));
               begin
                  if C not in ' ' .. '~' | ASCII.HT | ASCII.CR | ASCII.LF then return; end if;
                  Value.Source (I) := C;
               end;
            end loop;
         end;
         Value.Id := R.Items (3).UInt_Value;
         Value.Op := CCL.Control.Operation'Enum_Val (R.Items (4).UInt_Value);
         if Value.Op in CCL.Control.Start_Monitor | CCL.Control.Stop_Monitor |
           CCL.Control.Inspect_Monitor
         then
            if R.Count /= 6 or else R.Items (6).Kind /= MT_Unsigned_Integer then return; end if;
            Value.Target := R.Items (6).UInt_Value;
            Valid := (if Value.Op = CCL.Control.Stop_Monitor then Value.Target /= 0
                      else Value.Target = 0) and then
              (Value.Op = CCL.Control.Start_Monitor or else Value.Length = 0);
         else
            Valid := R.Count = 5 and then
              (Value.Op = CCL.Control.Evaluate_Expression or else Value.Length = 0);
         end if;
      end;
   end Decode;

   procedure Encode (Query : Request; Value : CCL.Control.Response; Data : out Response) is
      Failed : Boolean := False;
      procedure Put (Bytes : Byte_Array) is
      begin
         if Bytes'Length > SE_Offset (Max_Response - Data.Length) then
            Failed := True;
         else
            Data.Data (SE_Offset (Data.Length + 1) .. SE_Offset (Data.Length + Bytes'Length)) := Bytes;
            Data.Length := Data.Length + Bytes'Length;
         end if;
      end Put;
      procedure Number (N : Interfaces.Unsigned_64) is
      begin Put (Encoding.Encode_Unsigned (N)); end Number;
      procedure Flag (B : Boolean) is
      begin Put (Encoding.Encode_Simple (if B then Simple_True else Simple_False)); end Flag;
      procedure Text (S : String) is
      begin
         if S'Length > 4096 or else (for some C of S => Character'Pos (C) >= 128) then
            Failed := True;
         else
            declare
               Header : Byte_Array := Encoding.Encode_Unsigned (UInt64 (S'Length));
               Payload : Byte_Array (1 .. SE_Offset (S'Length));
            begin
               Header (Header'First) := Header (Header'First) or 16#60#;
               for I in Payload'Range loop
                  Payload (I) := Character'Pos (S (S'First + (Natural (I) - 1)));
               end loop;
               Put (Header); Put (Payload);
            end;
         end if;
      end Text;
      procedure Outcome (Item : CCL.Language.Interpretation_Result) is
      begin
         Flag (Item.Status = CCL.Language.Succeeded);
         Text (CCL.Sessions.Result_Image (Item));
         Number (case CCL.Sessions.Result_Type (Item) is
           when CCL.Language.Invalid_Type => 0, when CCL.Language.Integer_Type => 1,
           when CCL.Language.Boolean_Type => 2, when CCL.Language.String_Type => 3,
           when CCL.Language.Character_Type => 4);
         Number (Interfaces.Unsigned_64 (Item.Diagnostic_Position));
         Number (Interfaces.Unsigned_64 (Item.Fuel_Remaining));
      end Outcome;
   begin
      Data := (others => <>);
      Put (Encoding.Encode_Array (case Query.Op is
        when CCL.Control.Inspect_Bindings => 12,
        when CCL.Control.Read_Clock => 5, when CCL.Control.Evaluate_Expression => 8,
        when CCL.Control.Start_Monitor | CCL.Control.Stop_Monitor |
             CCL.Control.Inspect_Monitor => 15));
      Number (1); Number (Query.Id); Number (CCL.Control.Operation'Enum_Rep (Query.Op));
      case Query.Op is
         when CCL.Control.Inspect_Bindings =>
            Number (Value.Observed.Process_Id); Number (Value.Observed.Network_Process);
            Number (Value.Observed.Clock_Process); Flag (Value.Observed.Clock_Available);
            Number (Value.Observed.Monotonic_Ms);
            for Word of CCL.Interfaces.Clock.DESCRIPTOR_DIGEST loop Number (Word); end loop;
         when CCL.Control.Read_Clock =>
            Flag (Value.Observed.Clock_Available); Number (Value.Observed.Monotonic_Ms);
         when CCL.Control.Evaluate_Expression =>
            Outcome (Value.Outcome);
         when CCL.Control.Start_Monitor | CCL.Control.Stop_Monitor | CCL.Control.Inspect_Monitor =>
            Flag (Value.Accepted);
            Number (CCL.Periodic_Programs.Lifecycle'Pos (CCL.Periodic_Programs.State (Value.Monitor)));
            Number (CCL.Periodic_Programs.Identity (Value.Monitor));
            Number (CCL.Periodic_Programs.Completed_Runs (Value.Monitor));
            Number (Interfaces.Unsigned_64 (CCL.Periodic_Programs.Interval (Value.Monitor)));
            Text (CCL.Periodic_Programs.Source_Text (Value.Monitor));
            Outcome (CCL.Periodic_Programs.Last_Result (Value.Monitor));
            Number (CCL.Periodic_Programs.Next_Deadline (Value.Monitor));
      end case;
      if Failed then Data.Length := 0; end if;
   end Encode;
end Control_Wire;
