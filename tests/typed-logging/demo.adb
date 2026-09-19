with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Authority_Policy;
with CuBit.Log_Records; use CuBit.Log_Records;
with CuBit.Text_To_Log;
with CuBit.Protocols; use CuBit.Protocols;
with CuBit.Protocols.Stream_Policies;
with CuBit.Protocols.Stream_Connections; use CuBit.Protocols.Stream_Connections;
with CuBit.Protocols.Stream_Bindings; use CuBit.Protocols.Stream_Bindings;
procedure Demo is
   --  A Linux-hosted integration fixture, not a live IPC authority issuer.
   Text_Binding : Binding (1);
   Log_Binding : Binding (2);
   Adapter : CuBit.Text_To_Log.Adapter;
   Source, Converter_Input, Converter_Output, Collector : Port_Descriptor;
   Input_Request, Output_Request, Direct : Request;
   Outcome : Result;
   Ticket : Unsigned_64;
   Old_Reference : Binding_Reference;
   Accepted : Boolean;
   Clock : Unsigned_64 := 100;
   type Receipt is record
      Peer : Unsigned_64 := 0;
      Observed_Ms : Unsigned_64 := 0;
      Record_Value : Log_Record := Empty_Record;
   end record;
   A, B : array (1 .. 4) of Receipt;
   A_Count, B_Count : Natural := 0;
   function Approve (R : Request) return Approvals is
     ([others => (For_Connection => Key (R),
                  Outcome => CuBit.Authority_Policy.Approved)]);
   function Profile (Schema : Schema_Contract)
      return CuBit.Protocols.Stream_Policies.Policy is
     (Element => Schema,
      Delivery => (Kind => CuBit.Protocols.Stream_Policies.Ordered_With_Gaps,
                   When_Lossy_Full =>
                     CuBit.Protocols.Stream_Policies.Drop_Oldest_Pending),
      Capacity => (Slots => 4, Maximum_In_Flight => 1,
                   Payload_Bytes => Unsigned_64 (Schema.Wire_Size) * 4),
      Normal_Close => CuBit.Protocols.Stream_Policies.Drain_Accepted);

   procedure Collect
     (Peer : Unsigned_64; Expected : Binding_Reference;
      Bytes : Wire_Buffer; Used : Wire_Count; Success : out Boolean) is
      Current : constant View := Inspect (Log_Binding);
      Value : Decoded;
      Entry_Value : Receipt;
   begin
      Success := False;
      if Expected /= Reference (Log_Binding) or else
        not Current.Active.Present or else
        Current.Active.Value.Source.Reference.Process_Instance /= Peer
      then
         return;
      end if;
      Value := Decode (Bytes, Used);
      if not Value.Success then return; end if;
      --  Fixture stand-in for authenticated peer and collector clock metadata.
      --  Neither originates in the submitted log payload.
      Entry_Value := (Peer, Clock, Value.Value);
      if Current.Active.Value.Destination.Reference.Process_Instance = 300 then
         pragma Assert (A_Count < A'Length);
         A_Count := A_Count + 1;
         A (A_Count) := Entry_Value;
         Put_Line ("collector A <- adapter:" & Text (Value.Value));
      else
         pragma Assert
           (Current.Active.Value.Destination.Reference.Process_Instance = 400);
         pragma Assert (B_Count < B'Length);
         B_Count := B_Count + 1;
         B (B_Count) := Entry_Value;
         Put_Line ("collector B <- adapter:" & Text (Value.Value));
      end if;
      Success := True;
   end Collect;
   procedure Send_Text (Chunk : String) is
      Step : CuBit.Text_To_Log.Step;
      Bytes : Wire_Buffer;
      Used : Wire_Count;
      Current : constant View := Inspect (Text_Binding);
      use type CuBit.Text_To_Log.Step_Kind;
   begin
      pragma Assert (Chunk'Length <= CuBit.Text_To_Log.Maximum_Chunk_Bytes);
      pragma Assert (Current.Active.Present);
      pragma Assert
        (Current.Active.Value.Source.Reference.Process_Instance = 100 and
         Current.Active.Value.Destination.Reference.Process_Instance = 200);
      Clock := Clock + 1;
      for C of Chunk loop
         CuBit.Text_To_Log.Feed
           (Adapter, C, (Monotonic_Milliseconds, 1, Clock), Step);
         if CuBit.Text_To_Log.Kind (Step) = CuBit.Text_To_Log.Record_Ready then
            Encode (CuBit.Text_To_Log.Value (Step), Bytes, Used);
            Collect (200, Reference (Log_Binding), Bytes, Used, Accepted);
            pragma Assert (Accepted);
         else
            pragma Assert (CuBit.Text_To_Log.Kind (Step) = CuBit.Text_To_Log.Need_More);
         end if;
      end loop;
   end Send_Text;
begin
   Source := (Reference => (100, 1, 1), Direction => Output,
              Profile => Profile (CuBit.Text_To_Log.Input_Contract));
   Converter_Input := Source;
   Converter_Input.Reference := (200, 1, 1);
   Converter_Input.Direction := Input;
   Converter_Output := (Reference => (200, 2, 1), Direction => Output,
                        Profile => Profile (Contract));
   Collector := Converter_Output;
   Collector.Reference := (300, 1, 1);
   Collector.Direction := Input;
   Input_Request := (900, Reference (Text_Binding), Source, Converter_Input);
   Output_Request := (900, Reference (Log_Binding), Converter_Output, Collector);
   Direct := Output_Request;
   Direct.Source := Source;
   pragma Assert (Check (Direct, Approve (Direct)) = Incompatible_Profiles);
   Put_Line ("direct stdout -> log record sink: rejected (schema mismatch)");
   Prepare (Text_Binding, Input_Request, Approve (Input_Request), Ticket, Outcome);
   pragma Assert (Outcome = Succeeded);
   Commit (Text_Binding, 900, Ticket, Approve (Input_Request), True, Outcome);
   pragma Assert (Outcome = Succeeded);
   Prepare (Log_Binding, Output_Request, Approve (Output_Request), Ticket, Outcome);
   pragma Assert (Outcome = Succeeded);
   Commit (Log_Binding, 900, Ticket, Approve (Output_Request), True, Outcome);
   pragma Assert (Outcome = Succeeded);
   Send_Text ("service started" & ASCII.LF);
   Old_Reference := Reference (Log_Binding);
   Output_Request.Binding := Old_Reference;
   Output_Request.Destination.Reference.Process_Instance := 400;
   Prepare (Log_Binding, Output_Request, Approve (Output_Request), Ticket, Outcome);
   pragma Assert (Outcome = Succeeded);
   Send_Text ("old route while preparing" & ASCII.LF);
   Commit (Log_Binding, 900, Ticket, Approve (Output_Request), True, Outcome);
   pragma Assert (Outcome = Succeeded);
   Send_Text ("new route after commit" & ASCII.LF);
   pragma Assert (A_Count = 2 and B_Count = 1);
   pragma Assert (A (1).Peer = 200 and B (1).Peer = 200);
   pragma Assert (A (1).Observed_Ms < B (1).Observed_Ms);
   declare
      Bytes : Wire_Buffer;
      Used : Wire_Count;
   begin
      Encode (B (1).Record_Value, Bytes, Used);
      Collect (200, Old_Reference, Bytes, Used, Accepted);
      pragma Assert (not Accepted);
      Collect (100, Reference (Log_Binding), Bytes, Used, Accepted);
      pragma Assert (not Accepted);
      Bytes (6) := 255;
      Collect (200, Reference (Log_Binding), Bytes, Used, Accepted);
      pragma Assert (not Accepted);
   end;
   Finish_Retirement (Log_Binding, Ticket, False, Outcome);
   pragma Assert (Outcome = Resources_Not_Ready);
   Finish_Retirement (Log_Binding, Ticket, True, Outcome);
   pragma Assert (Outcome = Succeeded);
   pragma Assert (A_Count = 2 and B_Count = 1);
   Put_Line ("PASS: real encoded records, explicit adapter, authorized model rebind, stale/wrong-peer/malformed rejection");
end Demo;
