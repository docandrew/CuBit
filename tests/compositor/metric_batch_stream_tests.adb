with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Release_Metrics;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;
with Compositor_Metric_Batch_Policy;
with CuBit.Metric_Batches;
procedure Metric_Batch_Stream_Tests is
   package WM renames Compositor_Work_Metrics;
   package SM renames Compositor_Stage_Metrics;
   package M renames Compositor_Release_Metrics;
   package R renames M.Records;
   package P renames Compositor_Metric_Batch_Policy;
   package B renames CuBit.Metric_Batches;
   use type P.Append_Kind, B.Page_Id, B.Page_Pair, R.Record_Kind, R.Metric_Key;
   Builder : B.Builder;
   Pages, Held : B.Page_Pair := [others => [others => 0]];
   Policy : P.State;
   Sealed : Boolean;
   Page : B.Page_Id;
   Bytes : Unsigned_64;
   Admitted : Natural := 0;

   procedure Write (Value : R.Metric_Record; Now : Unsigned_64) is
      Accepted : Boolean;
   begin
      B.Append (Builder, Pages, Value, Accepted);
      pragma Assert (Accepted);
      P.Accepted (Policy, Now);
   end Write;
   procedure Offer (Frame : Unsigned_64) is
      V : constant M.Sample := M.Prepare ((Natural (Frame mod 2), 1, Frame, Frame, Frame + 1));
      Accepted : Boolean;
   begin
      if not B.Has_Room (Builder) then
         -- One dropped measurement, not spurious dropped declarations.
         B.Append (Builder, Pages, V.Value, Accepted);
         pragma Assert (not Accepted);
         return;
      end if;
      if P.Next (Policy) = P.Describe_Output_0 then
         Write (M.Declaration (0), Frame + 1);
      end if;
      if P.Next (Policy) = P.Describe_Output_1 then
         Write (M.Declaration (1), Frame + 1);
      end if;
      for Kind in SM.Stage loop
         if P.Next (Policy) = P.Append_Kind'Val (2 + SM.Stage'Pos (Kind)) then
            Write (SM.Declaration (Kind), Frame + 1);
         end if;
      end loop;
      for Kind in WM.Work_Kind loop
         if P.Next (Policy) = P.Append_Kind'Val (6 + WM.Work_Kind'Pos (Kind)) then
            Write (WM.Declaration (Kind), Frame + 1);
         end if;
      end loop;
      pragma Assert (P.Next (Policy) = P.Measurement);
      Write (V.Value, Frame + 1); Admitted := Admitted + 1;
      if P.Due (Policy, Frame + 1) then
         B.Seal (Builder, Pages, Sealed, Page, Bytes);
         pragma Assert (Sealed);
         P.Submitted (Policy);
      end if;
   end Offer;
   procedure Check_Declarations (Which : B.Page_Id; Records : R.Batch_Record_Count) is
      Header : constant R.Decoded_Header := R.Decode_Header
        (R.Slot (Pages (Which), 0), R.Batch_Bytes (Records));
   begin
      pragma Assert (Header.Success and then Header.Value.Records = Records);
      for Output in 0 .. 1 loop
         declare D : constant R.Decoded_Record := R.Decode (R.Slot (Pages (Which), Output + 1));
         begin
            pragma Assert (D.Success and then D.Value.Kind = R.Describe and then
              D.Value.Key = M.Key (Output) and then D.Value.Declared = R.Span);
         end;
      end loop;
      for Kind in SM.Stage loop
         declare D : constant R.Decoded_Record := R.Decode (R.Slot (Pages (Which), 3 + SM.Stage'Pos (Kind)));
         begin
            pragma Assert (D.Success and then D.Value.Kind = R.Describe and then
              D.Value.Key = SM.Key (Kind) and then D.Value.Declared = R.Latency);
         end;
      end loop;
      for Kind in WM.Work_Kind loop
         declare D : constant R.Decoded_Record := R.Decode (R.Slot (Pages (Which), 7 + WM.Work_Kind'Pos (Kind)));
         begin
            pragma Assert (D.Success and then D.Value.Kind = R.Describe and then
              D.Value.Key = WM.Key (Kind) and then D.Value.Declared = R.Counter);
         end;
      end loop;
   end Check_Declarations;
begin
   for Frame in 1 .. 110 loop Offer (Unsigned_64 (Frame)); end loop;
   pragma Assert (Admitted = 110 and B.In_Flight (Builder, 1) and B.In_Flight (Builder, 2));
   pragma Assert (P.Used (Policy) = 0);
   Check_Declarations (1, 63); Check_Declarations (2, 63);
   Held := Pages;
   for Frame in 111 .. 1000 loop Offer (Unsigned_64 (Frame)); end loop;
   pragma Assert (Admitted = 110 and B.Dropped (Builder) = 890 and Pages = Held);
   pragma Assert (P.Used (Policy) = 0);
   -- A confirmed out-of-order release makes precisely that page writable.
   B.Complete (Builder, 2);
   Offer (1001);
   pragma Assert (Admitted = 111 and P.Used (Policy) = 9 and P.Samples (Policy) = 1);
   pragma Assert (P.Due (Policy, 101_002));
   B.Seal (Builder, Pages, Sealed, Page, Bytes);
   pragma Assert (Sealed and Page = 2 and Bytes = R.Batch_Bytes (9));
   P.Submitted (Policy);
   Check_Declarations (2, 9);
   declare H : constant R.Decoded_Header := R.Decode_Header (R.Slot (Pages (2), 0), Bytes);
   begin
      pragma Assert (H.Success and then H.Value.Sequence = 3 and then H.Value.Producer_Dropped = 890);
   end;
   for I in R.Page_Word_Index loop
      pragma Assert (Pages (1) (I) = Held (1) (I));
   end loop;
   Ada.Text_IO.Put_Line ("METRIC-BATCH-STREAM: PASS two real pages, 890 overload drops, retained-page immutability and out-of-order release/redeclaration");
end Metric_Batch_Stream_Tests;
