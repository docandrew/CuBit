pragma Ada_2022;
with Ada.Text_IO;
with Firmware_Tables;
with System.Storage_Elements;
with ACPI_Host_Bridge_Channel;
with CuBit.Channels;
with CuBit.Memory_Grants;
with CuBit.Messages;
with CuBit.Handles;
with CuBit.Protocols;
with CuBit.Grant_References;
procedure Completion_Tests is
   package B renames ACPI_Host_Bridge_Channel; package W renames B.W;
   package K renames CuBit.Channels; package M renames CuBit.Messages;
   use type W.Word, B.Report, B.Phase, B.Failure, CuBit.Grant_References.Reference;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   Peer : constant B.P.Process_ID := B.P.From_Word (16#DEAD_BEEF_1234_0001#);
   Owner : constant B.P.Process_ID := B.P.From_Word (16#A123_4567_ABCD_0123#);
   Statuses : constant array (Positive range <>) of W.Word := [M.COMPLETION_TARGET_DIED, M.COMPLETION_CANCELLED];
   function Address (Value : System.Address) return W.Word is
     (W.Word (System.Storage_Elements.To_Integer (Value)));
begin
   for Status of Statuses loop
      for Mode in 1 .. 8 loop
         declare
            Own : aliased Firmware_Tables.Bytes (1 .. 3 * B.RC.Page_Bytes) := [others => 0] with Alignment => 4096;
            IO : B.Session;
            Empty_Rows : W.Rows (1 .. 0); Empty_Source : B.S.Bytes (1 .. 0);
            Item : M.CompletionEntry := (token => 100, from => B.P.To_Word (Peer), status => Status, valid => True, others => <>);
            Submitted, Handled, Closed : Boolean;
            Before : B.Report;
            Close_Count : Natural;
         begin
            M.Model_Owner := Owner; K.Mode := K.Good; K.Submit_OK := True;
            K.Open_Calls := 0; K.Finish_Calls := 0; K.Close_Calls := 0;
            K.Own_Base := Address (Own'Address);
            CuBit.Memory_Grants.Model_Retired := False;
            B.Begin_Open (IO, CuBit.Handles.Model_Endpoint, Peer,
              (Identity => B.M.Schema, Version => 1,
               Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => B.M.Message_Bytes),
              1, 100, 101, 75, 10, (State => B.S.M.Complete, Root_ID => 1, Sequence => 1, others => <>),
              Empty_Rows, Empty_Source, Submitted);
            Check (Submitted and K.Open_Calls = 1 and B.Observe (IO).Stage = B.Opening, "one original pending request");
            if Mode = 2 then B.Note_Time (IO, 75); end if;
            Before := B.Observe (IO);
            case Mode is
               when 3 => Item.from := Item.from xor 16#0100_0000_0000_0000#;
               when 4 => Item.token := 101;
               when 5 => Item.valid := False;
               when 6 => M.Model_Owner := Peer;
               when 7 => Item.status := M.COMPLETION_QUEUE_OVERFLOW;
               when 8 => Item.status := W.Word'Last;
               when others => null;
            end case;
            if Mode >= 3 then
               B.Complete_Open (IO, Item, Handled);
               Check (not Handled and K.Finish_Calls = 0 and K.Close_Calls = 0 and B.Observe (IO) = Before,
                  "wrong correlation or unknown status preserves original pending setup");
               B.Close (IO, Closed); Check (not Closed, "uncorrelated completion cannot retire pending request");
            end if;
            M.Model_Owner := Owner;
            Item := (token => 100, from => B.P.To_Word (Peer), status => Status, valid => True, others => <>);
            Item.msg.Words := [others => W.Word'Last];
            B.Complete_Open (IO, Item, Handled);
            Check (Handled and K.Finish_Calls = 0 and K.Close_Calls = 1,
               "known terminal completion closes locally without interpreting its message");
            Check (B.Observe (IO).Stage = B.Setup_Refused and not B.Observe (IO).Poisoned
              and B.Observe (IO).Completion_Status = Status, "exact terminal status retained for diagnostics");
            Check (B.Observe (IO).Why = (if Status = M.COMPLETION_TARGET_DIED then B.Open_Target_Died else B.Open_Cancelled),
               "target death and cancellation are distinct typed outcomes");
            B.Complete_Open (IO, Item, Handled);
            Check (not Handled and K.Close_Calls = 1, "original request completion consumed exactly once");
            B.Publish (IO, 10, Submitted); Check (not Submitted and not B.Observe (IO).Published, "terminal setup never publishes");
            B.Close (IO, Closed); Check (Closed and not B.Retired (IO), "closed channel still owes original grant retirement");
            Check (CuBit.Memory_Grants.Last_Query = K.Own_Reference, "retirement checks exact original generation and slot");
            Close_Count := K.Close_Calls;
            CuBit.Memory_Grants.Model_Retired := True;
            Check (B.Retired (IO), "original region retirement completes owner obligations");
            B.Close (IO, Closed); Check (Closed and K.Close_Calls = Close_Count, "terminal close is idempotent");
            B.Begin_Open (IO, CuBit.Handles.Model_Endpoint, Peer,
              (Identity => B.M.Schema, Version => 1,
               Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => B.M.Message_Bytes),
              1, 200, 201, 100, 10, (State => B.S.M.Complete, Root_ID => 1, Sequence => 2, others => <>),
              Empty_Rows, Empty_Source, Submitted);
            Check (not Submitted and K.Open_Calls = 1, "terminal request never restarts or renews deadline");
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Host bridge terminal completion checks" & Checks'Image);
end Completion_Tests;
