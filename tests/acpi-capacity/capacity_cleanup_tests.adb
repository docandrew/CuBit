with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with Capacity_Fixture; use Capacity_Fixture;
procedure Capacity_Cleanup_Tests is
   package NS is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 8, Aggregate_Method_Capacity => 70000);
   use type NS.Load_Status;
   use type Integer_Value;
   Tree : NS.State := NS.Empty;
   Saved : NS.State;
   Status : NS.Load_Status;
   R : Execution_Result;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   Late : constant Bytes := Method ("\LATE", [16#A4#,16#0A#,42]);
   Inner_If : constant Bytes := [16#68#] & Late & [16#A4#,0];
   Outer_Body : constant Bytes := [16#A0#, Byte (Inner_If'Length + 1)] & Inner_If &
      Text ("INNR") & [16#A4#] & Text ("\LATE");
   Outer_Method : Bytes := Method ("OUTR", Outer_Body);
   Inner_Body : constant Bytes := Method ("\TEMP", [16#A4#,1]) & Text ("OUTR") & [1,16#A4#,0];
begin
   Outer_Method (7) := 1;
   NS.Load_Names (Tree, Method ("PADD", Body_Of_Size (65536)) & Outer_Method & Method ("INNR", Inner_Body), Bits_64, Status);
   Check (Status = NS.Loaded and then NS.Method_Usage (Tree) > 65536, "aggregate beyond former limit");
   Saved := Tree;
   -- OUTR(0) calls INNR, which appends TEMP then re-enters active OUTR(1).
   -- OUTR(1) appends LATE, but its return only decrements OUTR's active count.
   -- INNR now retires interior TEMP before surviving LATE. OUTR(0) resolves and
   -- invokes LATE's current method identity/body, then finally removes LATE.
   for Repeat in 1 .. 3 loop
      NS.Invoke_Mutable (Tree, 2, [others => 0], 1, 100, R);
      Check (R.Status = Returned and then R.Value = 42, "later live method identity/body survives interior cleanup");
      Check (NS.Cleanup_Frame (Tree, Saved), "exact static records/code preserved except monotonic issuer");
      Check (NS.Method_Usage (Tree) = 65536 + Outer_Body'Length + Inner_Body'Length,
         "dynamic aggregate high watermark reclaimed after outer return");
   end loop;
   Ada.Text_IO.Put_Line ("CAPACITY-CLEANUP: PASS" & Checks'Image);
end Capacity_Cleanup_Tests;
