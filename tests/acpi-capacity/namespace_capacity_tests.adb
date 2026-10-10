with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with Capacity_Fixture; use Capacity_Fixture;
procedure Namespace_Capacity_Tests is
   use type Integer_Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   generic
      Nodes, Pool : Positive;
   procedure Boundary;
   procedure Boundary is
      package NS is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => Nodes, Aggregate_Method_Capacity => Pool);
      use type NS.Load_Status;
      use type NS.State;
      Tree : NS.State := NS.Empty;
      Saved : NS.State;
      Status : NS.Load_Status;
   begin
      NS.Load_Names (Tree, Method ("FULL", Body_Of_Size (Pool)), Bits_64, Status);
      Check (Status = NS.Loaded and then NS.Method_Usage (Tree) = Pool, "exact aggregate fit");
      Check (NS.Method_Data (Tree, 1) = Body_Of_Size (Pool), "exact body retained");
      NS.Load_Names (Tree, Method ("ZERO", []), Bits_64, Status);
      Check (Status = NS.Loaded and then NS.Method_Data (Tree, 2)'Length = 0, "empty method at full pool");
      Saved := Tree;
      NS.Load_Names (Tree, Method ("OVER", [16#A3#]), Bits_64, Status);
      Check (Status = NS.Value_Limit and then Tree = Saved, "aggregate one over atomic");
   end Boundary;
   procedure Small is new Boundary (3, 4);
   procedure Per_Method_Exact is new Boundary (3, Max_Method_Bytes);
   procedure Large is
      package NS is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 8, Aggregate_Method_Capacity => 70000);
      use type NS.Load_Status;
      use type NS.State;
      Tree : NS.State := NS.Empty;
      Saved : NS.State;
      Status : NS.Load_Status;
      R : Execution_Result;
   begin
      NS.Load_Names (Tree, Method ("AAAA", Body_Of_Size (40000)) & Method ("BBBB", Body_Of_Size (30000)), Bits_64, Status);
      Check (Status = NS.Loaded and then NS.Method_Usage (Tree) = 70000, "aggregate above individual limit");
      Check (NS.Method_Data (Tree, 1)'Length = 40000 and then NS.Method_Data (Tree, 2)'Length = 30000, "independent method spans");
      R := NS.Invoke (Tree, 2, [others => 0], 0, 10);
      Check (R.Status = Returned and then R.Value = 1, "invoke above aggregate offset 40000");
      Saved := NS.Empty; Tree := NS.Empty;
      NS.Load_Names (Tree, Method ("HUGE", Body_Of_Size (Max_Method_Bytes + 1)), Bits_64, Status);
      Check (Status = NS.Value_Limit and then Tree = Saved, "individual overlarge rejected even with aggregate room");
   end Large;
   procedure Default_Rejection is
      package NS is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 4);
      use type NS.Load_Status;
      use type NS.State;
      Tree : NS.State := NS.Empty;
      Status : NS.Load_Status;
   begin
      NS.Load_Names (Tree, Method ("AAAA", Body_Of_Size (40000)) & Method ("BBBB", Body_Of_Size (30000)), Bits_64, Status);
      Check (Status = NS.Value_Limit and then Tree = NS.Empty, "default aggregate remains 65536 and atomic");
   end Default_Rejection;
   procedure Nodes_And_Cleanup is
      package NS is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 4, Aggregate_Method_Capacity => 128);
      use type NS.Load_Status;
      use type NS.State;
      Tree : NS.State := NS.Empty;
      Saved : NS.State;
      Status : NS.Load_Status;
      R : Execution_Result;
      Code : constant Bytes := Method ("TEMP", [16#A4#,1]) & [16#A4#,1];
   begin
      NS.Load_Names (Tree, Method ("MAKE", Code) & Method ("LATE", [16#A4#,1]), Bits_64, Status);
      Check (Status = NS.Loaded, "cleanup load");
      for Repeat in 1 .. 3 loop
         NS.Invoke_Mutable (Tree, 1, [others => 0], 0, 100, R);
         Check (R.Status = Returned and then NS.Count (Tree) = 2 and then NS.Method_Usage (Tree) = Code'Length + 2,
           "dynamic method cleanup reclaims aggregate tail");
         Check (NS.Method_Data (Tree, 2) = Bytes'[16#A4#,1], "later static method survives cleanup");
      end loop;
      NS.Load_Names (Tree, Method ("THRD", []) & Method ("FRTH", []), Bits_64, Status);
      Check (Status = NS.Loaded and then NS.Count (Tree) = 4, "node exact fit");
      Saved := Tree;
      NS.Load_Names (Tree, Method ("FIFT", []), Bits_64, Status);
      Check (Status = NS.Storage_Full and then Tree = Saved, "node one over atomic");
   end Nodes_And_Cleanup;
begin
   Small; Per_Method_Exact; Large; Default_Rejection; Nodes_And_Cleanup;
   Ada.Text_IO.Put_Line ("CAPACITY-NAMESPACE: PASS" & Checks'Image);
end Namespace_Capacity_Tests;
