with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Trace_Archive;
procedure Archive_Check is
   package A renames Compositor_Trace_Archive;
   package S renames A.S;
   package W renames A.W;
   use type S.Capture;
   Capture : S.Capture :=
     (True, Unsigned_64'Last, Unsigned_64'Last, S.P.Publisher_Tag (S.P.Issuance'Last),
      Unsigned_64'Last, Unsigned_64'Last - 3, Unsigned_64'Last,
      Unsigned_64'Last, (W.Input_Event, 1, (1, 1, 1, 0)));
   C, Mutated : A.Chunk;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Reject (Index : Natural; Value : Unsigned_64) is
   begin
      Mutated := C; Mutated (Index) := Value;
      Mutated (31) := A.Checksum (A.Prefix (Mutated));
      Check (not A.Decode (Mutated).Success);
   end Reject;
begin
   for I in 1 .. 1000 loop
      Capture.Value := (case I mod 5 is
         when 0 => (W.Input_Event, Unsigned_64 (I), (1, Unsigned_64 (I), 1, Unsigned_64 (I))),
         when 1 => (W.Source_Event, Unsigned_64 (I), (1, 2, Unsigned_64 (I), 0, Unsigned_64 (I))),
         when 2 => (W.Render_Event, Unsigned_64 (I),
           (W.RT.Submit, 1, 3, Unsigned_64'Last, Unsigned_64 (I), 0, 0, 0, 7, Unsigned_64 (I), Unsigned_64 (I))),
         when 3 => (W.Render_Event, Unsigned_64 (I),
           (W.RT.Draw, 1, 3, Unsigned_64'Last, Unsigned_64 (I), Unsigned_64'Last, Unsigned_64'Last, Unsigned_64'Last, 0, 0, Unsigned_64 (I))),
         when others => (W.Frame_Event, Unsigned_64 (I), (1, 7, Unsigned_64 (I), 0, Unsigned_64 (I))));
      C := A.Encode (Unsigned_64 (I), Capture);
      Check (A.Decode (C).Success and then A.Decode (C).Value = Capture and then
             A.Decode (C).Sequence = Unsigned_64 (I));
      for Index in C'Range loop
         for Bit in 0 .. 63 loop
            Mutated := C; Mutated (Index) := Mutated (Index) xor Shift_Left (1, Bit);
            Check (not A.Decode (Mutated).Success);
         end loop;
      end loop;
   end loop;
   C := A.Encode (Unsigned_64'Last - 1, Capture);
   Check (A.Decode (C).Sequence = Unsigned_64'Last - 1);
   Reject (0, 0); Reject (1, 255); Reject (2, 0); Reject (2, Unsigned_64'Last);
   Reject (3, 0); Reject (4, 0); Reject (5, 0); Reject (6, 0); Reject (7, 0);
   Reject (7, Unsigned_64'Last - 2); Reject (12, 0);
   for I in 26 .. 30 loop Reject (I, 1); end loop;
   Ada.Text_IO.Put_Line ("PASS archive chunks checks=" & Checks'Image);
   Ada.Text_IO.Put_Line ("CHECKSUM_VECTOR=" & A.Checksum ((others => 0))'Image);
end Archive_Check;
