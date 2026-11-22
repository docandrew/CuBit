with Ada.Text_IO;
with Compositor_Input_Batch_Wire;
procedure Input_Batch_Wire_Tests is
   package W renames Compositor_Input_Batch_Wire;
   package B renames W.B;
   use type W.Word;
   use type B.Batch;
   Value : B.Batch;
   Wire : W.Snapshot_Words;
   Cases : Natural := 0;
   procedure Reject (Bad : W.Snapshot_Words) is
   begin
      pragma Assert (not W.Decode (Bad, 42, 123, 10).Accepted);
      Cases := Cases + 1;
   end Reject;
begin
   pragma Assert (W.Byte_Count = 320 and W.Snapshot_Words'Size = 320 * 8);
   for Length in B.Count loop
      for More in Boolean loop
         if Length > 0 or else not More then
            Value := (Length => Length, Through => 10 + W.Word (Length),
                      More => More, others => <>);
            for I in 1 .. Length loop
               Value.Items (I) := (True, 10 + W.Word (I), 6, 42, W.Word (I), 0);
            end loop;
            Wire := W.Encode (Value, 42, 123, 10);
            declare D : constant W.Decoding := W.Decode (Wire, 42, 123, 10); begin
               pragma Assert (D.Accepted and then D.Value = Value);
            end;
            pragma Assert (not W.Decode (Wire, 43, 123, 10).Accepted);
            pragma Assert (not W.Decode (Wire, 42, 124, 10).Accepted);
            pragma Assert (not W.Decode (Wire, 42, 123, 11).Accepted);
            Cases := Cases + 4;
            for Header in 0 .. 7 loop
               declare Bad : W.Snapshot_Words := Wire; begin
                  Bad (Header) := W.Word'Last; Reject (Bad);
               end;
            end loop;
            for I in B.Limit loop
               declare
                  Offset : constant Natural := W.Header_Words + (I - 1) * W.Event_Words;
                  Bad : W.Snapshot_Words := Wire;
               begin
                  Bad (Offset) := 11; Reject (Bad); -- unknown kind, or nonzero unused slot
                  Bad := Wire; Bad (Offset + 1) := 10; Reject (Bad); -- replayed serial
                  Bad := Wire; Bad (Offset + 2) := 256; Reject (Bad); -- invalid text
                  Bad := Wire; Bad (Offset + 3) := 1; Reject (Bad); -- invalid text tail
               end;
            end loop;
         end if;
      end loop;
   end loop;
   -- All event kinds accepted by the existing single-event codec also traverse
   -- the batch codec. Serial order, not event kind, defines delivery order.
   for Kind in W.Word range 1 .. 10 loop
      Value := (Length => 1, Through => 11, others => <>);
      Value.Items (1) := (True, 11, Kind, 42, 0, 0);
      Wire := W.Encode (Value, 42, 123, 10);
      pragma Assert (W.Decode (Wire, 42, 123, 10).Accepted);
      Wire (8) := 0; Reject (Wire);
      Cases := Cases + 1;
   end loop;
   -- A late malformed record must invalidate the entire transaction.
   Value := (Length => 8, Through => 18, More => True, others => <>);
   for I in B.Limit loop Value.Items (I) := (True, 10 + W.Word (I), 6, 42, 65, 0); end loop;
   Wire := W.Encode (Value, 42, 123, 10);
   pragma Assert (not W.Decode (Wire, 42, 123, 10, 7).Accepted);
   Wire (37) := 17; Reject (Wire); -- duplicate last serial
   Ada.Text_IO.Put_Line ("INPUT BATCH WIRE: PASS" & Cases'Image & " codec and malformed snapshot cases");
end Input_Batch_Wire_Tests;
