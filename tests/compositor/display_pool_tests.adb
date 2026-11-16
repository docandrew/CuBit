with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Display_Pool_Protocol;
with CuBit.Display_Pool_Registry;
with CuBit.Output_Discovery;
procedure Display_Pool_Tests is
   package P renames CuBit.Display_Pool_Protocol;
   package R renames CuBit.Display_Pool_Registry;
   package D renames P.D;
   use type P.Attachment, P.Frame, P.Completion, D.Attachment_Request, D.DP.Protocol_Flags;
   S : R.State;
   Accepted : Boolean;
   Wire, Bad : D.Wire_Message;
   A : P.Attachment := (1, ((1, 2), (32, 24, 128)));
   F : P.Frame := (1, (7, 9, (1, 2, 3, 4)));
   C : P.Completion := (1, (7, 9, D.Published, D.Released, others => <>));
begin
   for Op in D.Operation loop
      pragma Assert (D.Code (Op) /= P.Attach_Buffer and D.Code (Op) /= P.Open_Session and
                     D.Code (Op) /= P.Submit_Frame);
   end loop;
   for Query in CuBit.Output_Discovery.Operation loop
      pragma Assert (CuBit.Output_Discovery.Code (CuBit.Output_Discovery.Display_Broker, Query) /= P.Attach_Buffer and
        CuBit.Output_Discovery.Code (CuBit.Output_Discovery.Display_Broker, Query) /= P.Open_Session and
        CuBit.Output_Discovery.Code (CuBit.Output_Discovery.Display_Broker, Query) /= P.Submit_Frame);
   end loop;
   for B in P.Buffer_Slot loop
      A.Buffer := B; A.Source.Grant.slot := Unsigned_64 (B);
      Wire := P.Encode (A);
      pragma Assert (P.Decode_Attachment (Wire).Valid and then P.Decode_Attachment (Wire).Value = A);
      for Flags in D.DP.Protocol_Flags loop
         Bad := Wire; Bad.Flags := Flags;
         pragma Assert (P.Decode_Attachment (Bad).Valid = (Flags in 1 .. 3));
      end loop;
      for Bit in 0 .. 15 loop
         Bad := Wire; Bad.Reserved := Shift_Left (Unsigned_16 (1), Bit);
         pragma Assert (not P.Decode_Attachment (Bad).Valid);
      end loop;
      R.Open (S, Accepted); pragma Assert (not Accepted);
      R.Register (S, A, Accepted); pragma Assert (Accepted and R.Count (S) = B);
      R.Register (S, A, Accepted); pragma Assert (not Accepted and R.Item (S, B) = A.Source);
      F.Buffer := B;
      Wire := P.Encode (F);
      pragma Assert (P.Decode_Frame (Wire).Valid and then P.Decode_Frame (Wire).Value = F);
      for Flags in D.DP.Protocol_Flags loop
         Bad := Wire; Bad.Flags := Flags;
         pragma Assert (P.Decode_Frame (Bad).Valid = (Flags in 1 .. 3));
      end loop;
      for Bit in 0 .. 63 loop
         Bad := Wire; Bad.Words (3) := Shift_Left (Unsigned_64 (1), Bit);
         pragma Assert (not P.Decode_Frame (Bad).Valid);
      end loop;
      for Outcome in D.Frame_Outcome loop
         for Disposition in D.Buffer_Disposition loop
            C := (B, (7, 9, Outcome, Disposition, others => <>));
            Wire := P.Encode (C);
            pragma Assert (P.Decode_Completion (Wire).Valid and then P.Decode_Completion (Wire).Value = C);
            for Flags in D.DP.Protocol_Flags loop
               Bad := Wire; Bad.Flags := Flags;
               pragma Assert (P.Decode_Completion (Bad).Valid = (Flags in 1 .. 3));
            end loop;
         end loop;
      end loop;
   end loop;
   R.Open (S, Accepted); pragma Assert (Accepted and R.Active (S));
   R.Open (S, Accepted); pragma Assert (not Accepted);
   for B in P.Buffer_Slot loop
      A.Buffer := B; A.Source.Grant.slot := 100;
      R.Register (S, A, Accepted); pragma Assert (not Accepted);
      pragma Assert (R.Item (S, B).Grant.slot = Unsigned_64 (B));
   end loop;
   R.Quarantine (S);
   R.Open (S, Accepted); pragma Assert (not Accepted and R.Faulted (S));
   for B in P.Buffer_Slot loop
      A.Buffer := B; R.Register (S, A, Accepted); pragma Assert (not Accepted);
   end loop;
   declare
      Fresh : R.State;
   begin
      A := (1, ((1, 2), (32, 24, 128)));
      R.Register (Fresh, A, Accepted); pragma Assert (Accepted);
      A.Buffer := 2;
      R.Register (Fresh, A, Accepted); pragma Assert (not Accepted); -- duplicate grant identity
      A.Source.Grant.slot := 2; A.Source.Layout.Pitch := 132;
      R.Register (Fresh, A, Accepted); pragma Assert (not Accepted); -- inconsistent layout
      A.Source.Layout := (32, 24, 0);
      R.Register (Fresh, A, Accepted); pragma Assert (not Accepted);
      pragma Assert (R.Count (Fresh) = 1);
   end;
   --  UI-020 loans: the request is an attachment under its own label; the
   --  reply carries the slot and whether its buffer is on screen. Neither
   --  decodes as the other, nor as an attachment, frame or completion.
   for B in P.Buffer_Slot loop
      for Shown in Boolean loop
         declare
            Item : constant P.Loan := (B, ((Unsigned_64 (B) + 10, 3), (32, 24, 128)), Shown);
            Request : constant P.Attachment := (B, ((Unsigned_64 (B), 1), (32, 24, 128)));
            Decoded : P.Loan_Decoding;
            use type P.Loan;
         begin
            Wire := P.Encode (Item);
            Decoded := P.Decode_Loan (Wire);
            pragma Assert (Decoded.Valid and then Decoded.Value = Item);
            pragma Assert (not P.Decode_Attachment (Wire).Valid and not P.Decode_Frame (Wire).Valid and
                           not P.Decode_Completion (Wire).Valid);
            --  Request and reply share the label (as frames and completions
            --  do): they travel opposite ways and are told apart by that.
            pragma Assert (P.Decode_Lend_Request (Wire).Valid = not Shown);
            for Flags in D.DP.Protocol_Flags loop
               Bad := Wire; Bad.Flags := Flags;
               pragma Assert (P.Decode_Loan (Bad).Valid = (Flags in 1 .. 3 | 5 .. 7));
               if Flags in 1 .. 3 | 5 .. 7 then
                  pragma Assert (P.Decode_Loan (Bad).Value.On_Screen = (Flags > 4) and
                                 Natural (P.Decode_Loan (Bad).Value.Buffer) = Natural (Flags mod 4));
               end if;
            end loop;
            for Bit in 0 .. 15 loop
               Bad := Wire; Bad.Reserved := Shift_Left (Unsigned_16 (1), Bit);
               pragma Assert (not P.Decode_Loan (Bad).Valid);
            end loop;
            Bad := Wire; Bad.Words (1) := 0;  --  generation 0 names no grant
            pragma Assert (not P.Decode_Loan (Bad).Valid);
            Wire := P.Encode_Lend_Request (Request);
            pragma Assert (P.Decode_Lend_Request (Wire).Valid and then
                           P.Decode_Lend_Request (Wire).Value = Request);
            pragma Assert (not P.Decode_Attachment (Wire).Valid);
            Bad := Wire; Bad.Flags := 5;
            pragma Assert (not P.Decode_Lend_Request (Bad).Valid);
         end;
      end loop;
   end loop;
   Put_Line ("display pool: PASS slot codecs, hostile envelopes, immutable registration/session lifecycle, loans");
end Display_Pool_Tests;
