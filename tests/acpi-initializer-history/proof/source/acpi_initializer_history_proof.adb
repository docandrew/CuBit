pragma Ada_2022;
with ACPI_Initializer_History;
procedure ACPI_Initializer_History_Proof with SPARK_Mode is
   Native_Namespace_Capacity : constant Positive := 16_384;
   package Native is new ACPI_Initializer_History (Native_Namespace_Capacity);
   package Small is new ACPI_Initializer_History (3);
   use type Native.Invocation_Number, Small.Invocation_Number, Native.Digest_Value;
   N : Native.State;
   S : Small.State;
   Accepted : Boolean;
   Item : constant Native.Invocation :=
     (Scope => 0, Method => 1, Access_Result => 0, Execution => 4, Reported_Charge => 1);
   Tiny : constant Small.Invocation :=
     (Scope => 0, Method => 1, Access_Result => 0, Execution => 4, Reported_Charge => 1);
begin
   pragma Assert (Native.Count (N) = 0 and Native.Complete (N));
   Native.Append (N, Item, Accepted);
   pragma Assert (Accepted and Native.Count (N) = 1 and Native.Complete (N));
   pragma Assert (Native.Digest (N) = Native.Next_Digest (Native.Empty_Digest, Item));
   Native.Invalidate (N);
   declare Before : constant Native.State := N; begin
      Native.Append (N, Item, Accepted);
      pragma Assert (not Accepted and Native.Count (N) = Native.Count (Before)
        and Native.Digest (N) = Native.Digest (Before) and not Native.Complete (N));
   end;
   for I in Small.Invocation_Count range 1 .. 3 loop
      pragma Loop_Invariant (Small.Count (S) = I - 1 and Small.Complete (S));
      Small.Append (S, Tiny, Accepted);
      pragma Assert (Accepted and Small.Count (S) = I and Small.Complete (S));
   end loop;
   declare Before : constant Small.State := S; begin
      Small.Append (S, Tiny, Accepted);
      pragma Assert (not Accepted and not Small.Complete (S)
        and Small.Count (S) = Small.Count (Before) and Small.Digest (S) = Small.Digest (Before));
      Small.Append (S, Tiny, Accepted);
      pragma Assert (not Accepted and not Small.Complete (S)
        and Small.Count (S) = Small.Count (Before) and Small.Digest (S) = Small.Digest (Before));
   end;
end ACPI_Initializer_History_Proof;
