pragma Ada_2022;
with Interfaces; with Ada.Text_IO; with GNAT.Source_Info;
with ACPI_Lock_Trace; with ACPI_Lock_Trace_Protocol; with ACPI_Global_Lock_Protocol;
procedure Lock_Trace_Tests is
   package G renames ACPI_Global_Lock_Protocol;
   package W renames ACPI_Lock_Trace_Protocol;
   package T is new ACPI_Lock_Trace (Interfaces.Unsigned_64, 0, Interfaces.Unsigned_64, 35);
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32;
   use type T.Dump_Outcome, T.Signal_Result, G.Lock_Outcome, W.Outcome;
   Owner : constant Interfaces.Unsigned_64 := 16#ABCD_0000_0000_0001#;
   Other_Owner : constant Interfaces.Unsigned_64 := 16#1234_0000_0000_0001#;
   C : T.Context; D : T.Dump_Outcome; S : T.Snapshot; Found : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Line : Positive := GNAT.Source_Info.Line) is
   begin
      Checks := Checks+1;
      if not Good then raise Program_Error with "check" & Checks'Image & " line" & Line'Image; end if;
   end Check;
   procedure Ready (State : in out T.Context; Caller : Interfaces.Unsigned_64 := Owner;
                    Scope : Interfaces.Unsigned_64 := 35; Valid : Boolean := True) is
   begin
      T.Begin_Request (State,Caller,Scope,G.Prepare);
      T.Finish_Request (State,(Valid,G.Ready,G.Configured,(1,1)));
   end Ready;
begin
   T.Request_Dump (C,Owner,35,D); Check (D = T.Unauthorized);
   Ready (C,Scope => 36); Check (T.Length (C) = 0);
   Ready (C,Caller => 0); Check (T.Length (C) = 0);
   Ready (C,Valid => False); T.Request_Dump (C,Owner,35,D); Check (D = T.Unauthorized);
   Ready (C); Check (T.Length (C) = 1);
   Ready (C,Other_Owner); Check (T.Length (C) = 1);
   T.Request_Dump (C,Other_Owner,35,D); Check (D = T.Unauthorized);
   T.Request_Dump (C,Owner,36,D); Check (D = T.Unauthorized and not T.Pending (C));
   T.Begin_Maintenance (C); T.Finish_Maintenance (C,G.Not_Ready); Check (T.Length (C) = 1);
   T.Begin_Request (C,Owner,35,G.Acquire);
   T.Observe_Read (C,2,True); T.Observe_CAS (C,2,3,2,True,True,True);
   T.Observe_CAS (C,3,1,0,False,True,False);
   T.Finish_Request (C,(True,G.Recorded,G.Atomic_Indeterminate,(1,1)));
   T.Read (C,2,S,Found);
   Check (Found and S.Read_Present and S.Read_Available and S.Read_Word = 2);
   Check (S.CAS_Present and S.CAS_Count = 2 and not S.CAS_Known and S.CAS_Attempted and not S.CAS_Exchanged);
   Check (S.Outcome = G.Atomic_Indeterminate);
   T.Begin_Maintenance (C); T.Observe_Signal (C,T.Signal_Not_Sent,9,7);
   T.Finish_Maintenance (C,G.Signal_Deferred); T.Read (C,3,S,Found);
   Check (Found and S.Maintenance and S.Signal = T.Signal_Not_Sent and S.Signal_Reads = 2 and S.Signal_Writes = 2);
   for I in 1 .. 20 loop
      T.Begin_Request (C,Owner,35,G.Check_Access); T.Observe_Read (C,Interfaces.Unsigned_32 (I),I /= 20);
      T.Finish_Request (C,(True,G.Checked,G.Acquired,(1,1)));
   end loop;
   Check (T.Length (C) = T.Maximum_Records and T.Dropped (C) = 11);
   T.Read (C,1,S,Found); Check (Found and S.Read_Word = 9);
   T.Read (C,12,S,Found); Check (Found and S.Read_Word = 20 and not S.Read_Available);
   T.Request_Dump (C,Owner,35,D); Check (D = T.Accepted and T.Pending (C));
   T.Request_Dump (C,Owner,35,D); Check (D = T.Already_Dumped);
   T.Begin_Maintenance (C); T.Observe_Signal (C,T.Signaled,1,1); T.Finish_Maintenance (C,G.Released);
   Check (T.Dropped (C) = 12);
   T.Finish_Dump (C); Check (not T.Pending (C));
   Ready (C); T.Begin_Maintenance (C); T.Observe_Read (C,999,True); T.Finish_Maintenance (C,G.Released);
   Check (T.Dropped (C) = 12); T.Read (C,12,S,Found); Check (Found and S.Signal = T.Signaled);
   T.Request_Dump (C,Owner,35,D); Check (D = T.Already_Dumped);
   declare Fresh : T.Context; begin T.Read (Fresh,1,S,Found); Check (not Found); end;
   Check (W.Valid_Request (W.Request));
   for K in W.Outcome loop
      declare R : constant W.Reply_Result := W.Decode (W.Reply (K,100)); begin
         Check (R.Valid and R.Status = K and R.Records = T.Maximum_Records);
      end;
   end loop;
   for I in 1 .. 9 loop
      declare P : W.Packet := W.Request; R : W.Packet := W.Reply (W.Accepted,1); begin
         case I is
            when 1 => P.Label := P.Label+1; R.Label := R.Label+1;
            when 2 => P.Length := 3; R.Length := 3;
            when 3 => P.Flags := 1; R.Flags := 1;
            when 4 => P.Reserved := 1; R.Reserved := 1;
            when 5 => P.Data (0) := 2; R.Data (0) := 2;
            when 6 => P.Data (1) := 4; R.Data (1) := 4;
            when 7 => P.Data (2) := 13; R.Data (2) := 13;
            when 8 => P.Data (3) := 1; R.Data (3) := 1;
            when 9 => P.Data (1) := Interfaces.Unsigned_64'Last; R.Data (1) := Interfaces.Unsigned_64'Last;
         end case;
         Check (not W.Valid_Request (P)); Check (not W.Decode (R).Valid);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Lock trace checks" & Checks'Image);
end Lock_Trace_Tests;
