with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Runtime_Package_GC_Data;
procedure Runtime_Package_GC_Tests is
   package N is new AML_Namespace
     (64, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 16);
   package C is new N.Owned.Collecting;
   use type Bytes;
   use type Integer_Value;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type C.Access_Status;
   use type C.Value_Kind;
   use type C.Value_Handle;
   use type C.Collection_Count;
   use type AML_Execute.Execution_Status;
   type Method_List is array (Positive range <>) of String (1 .. 4);
   Methods : constant Method_List := ["NAM0", "RET0", "CAL0", "CEL0"];
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
begin
   for Width in Integer_Width loop
      declare
         A : C.Arena;
         Input : aliased AML_Table_Backing.State (1, 1);
         Status : C.Access_Status;
         Loaded : N.Load_Status;
         Members : N.Initialization_Report;
         Prior, Current : C.Value_Handle := C.No_Value;
         Result : C.Result;
         procedure Release (Handle : in out C.Value_Handle) is
         begin
            C.Release (A, Handle, Status);
            Check (Status = C.Available, "release retained handle");
         end Release;
         procedure Invoke (Name : String; Expected : AML_Execute.Execution_Status) is
            Found : N.Lookup_Result;
         begin
            C.Find_Object (A, N.Root, Name, Found, Status);
            Check (Status = C.Available and then Found.Status = N.Found, "find " & Name);
            C.Invoke (A, Input, Found.Node, [others => (C.Immediate_Argument, 0)],
              0, 20_000, Result, Status);
            Check (Status = C.Available and then Result.Status = Expected,
              Name & " returned " & Result.Status'Image);
         end Invoke;
         procedure Expect_Package (Handle : C.Value_Handle; Length : Natural) is
            Description : C.Value_Description;
         begin
            C.Describe (A, Handle, Description, Status);
            Check (Status = C.Available and then Description.Kind = C.Package_Description
              and then Description.Length = Length, "complete package shape");
         end Expect_Package;
         procedure Expect_Buffer (Parent : C.Value_Handle; Index : Natural; Expected : Bytes) is
            Child : C.Value_Handle := C.No_Value;
            Description : C.Value_Description;
            Actual : Bytes (7 .. 6 + Expected'Length);
            Copied : Natural;
         begin
            C.Read_Element (A, Parent, Index, Child, Status);
            Check (Status = C.Available, "read buffer element");
            C.Describe (A, Child, Description, Status);
            Check (Status = C.Available and then Description.Kind = C.Buffer_Description
              and then Description.Length = Expected'Length, "buffer length and kind");
            C.Read_Bytes (A, Child, 0, Actual, Copied, Status);
            Check (Status = C.Available and then Copied = Expected'Length
              and then Actual = Expected, "every retained buffer byte");
            Release (Child);
         end Expect_Buffer;
         procedure Full_Tree (Handle : C.Value_Handle) is
            Middle, Inner : C.Value_Handle := C.No_Value;
         begin
            Expect_Package (Handle, 3);
            Expect_Buffer (Handle, 0, [16#55#,16#AA#,0,16#FF#]);
            C.Read_Element (A, Handle, 1, Middle, Status);
            Check (Status = C.Available, "middle package handle");
            Expect_Package (Middle, 2);
            Expect_Buffer (Middle, 0, [16#11#,16#22#,16#33#]);
            C.Read_Element (A, Middle, 1, Inner, Status);
            Check (Status = C.Available, "inner package handle");
            Expect_Package (Inner, 1);
            Expect_Buffer (Inner, 0, [16#77#,16#88#,16#99#,16#AA#,16#BB#]);
            Release (Inner); Release (Middle);
            Expect_Buffer (Handle, 2, [16#CC#,16#DD#]);
         end Full_Tree;
         procedure Check_Marker is
            Found : N.Lookup_Result;
            Observation : C.Result;
         begin
            C.Find_Object (A, N.Root, "MARK", Found, Status);
            Check (Status = C.Available and then Found.Status = N.Found, "marker exists");
            C.Observe_Named_Value (A, Found.Node, Observation, Status);
            Check (Status = C.Available and then Observation.Status = AML_Execute.Returned
              and then Observation.Number = 4, "exact oracle count-call total");
         end Check_Marker;
         procedure Check_Temporary_Removed is
            Found : N.Lookup_Result;
            Child : N.Node_ID;
         begin
            C.Find_Object (A, N.Root, "NAM0", Found, Status);
            Check (Status = C.Available and then Found.Status = N.Found, "named maker survives");
            C.Find_Child (A, Found.Node, "TEMP", Child, Status);
            Check (Status = C.Available and then Child = N.Root, "method-local package retired");
         end Check_Temporary_Removed;
      begin
         C.Initialize (A, Status); Check (Status = C.Available, "initialize");
         C.Load (A, Runtime_Package_GC_Data.Program, Width, Loaded, Status);
         Check (Status = C.Available and then Loaded = N.Loaded, "load exact normal AML body");
         C.Seal (A, Members, Status); Check (Status = C.Available, "seal");
         for Repetition in 1 .. 2 loop
            for Name of Methods loop
               Invoke ("RSET", AML_Execute.No_Return);
               declare
                  Before : constant C.Collection_Statistics := C.Reclamation_Metrics (A);
                  After : C.Collection_Statistics;
               begin
                  Invoke (Name, AML_Execute.Object_Returned); Current := Result.Handle;
                  After := C.Reclamation_Metrics (A);
                  Check (After.Completed > Before.Completed, "actual completed collection in " & Name);
                  Check (After.Freed_Objects > Before.Freed_Objects
                    and then After.Freed_Bytes > Before.Freed_Bytes,
                    "actual garbage reclaimed during count/argument calls in " & Name);
                  Check (After.Rejected = Before.Rejected, "no rejected root census");
                  Full_Tree (Current);
                  -- The preceding result remains pinned through all allocations,
                  -- nested count calls and caller/callee handoffs of this run.
                  if Prior /= C.No_Value then Full_Tree (Prior); end if;
                  Check_Marker; Check_Temporary_Removed;
                  Check (C.Retained_Count (A) = (if Prior = C.No_Value then 1 else 2),
                    "only explicit package roots remain");
                  if Prior /= C.No_Value then Release (Prior); end if;
                  Prior := Current; Current := C.No_Value;
                  pragma Assert (C.Module_Transient_Free (A));
               end;
            end loop;
         end loop;
         Release (Prior); Check (C.Retained_Count (A) = 0, "all result roots released");
         declare Before : constant C.Collection_Statistics := C.Reclamation_Metrics (A); begin
            Invoke ("PRES", AML_Execute.Returned);
            Invoke ("PRES", AML_Execute.Returned);
            -- PRES allocates only buffers. Reclaimed element slots therefore
            -- witness package reclamation, not merely PRES's own garbage.
            Check (C.Reclamation_Metrics (A).Freed_Elements - Before.Freed_Elements >= 6
              and then C.Reclamation_Metrics (A).Freed_Objects > Before.Freed_Objects
              and then C.Reclamation_Metrics (A).Freed_Bytes > Before.Freed_Bytes,
              "released 3+2+1 package elements and payload are reclaimed");
            Check (C.Observe_Usage (A).Values.Elements = 0,
              "no package element storage remains after releasing all products");
            Check (C.Reclamation_Metrics (A).Rejected = Before.Rejected,
              "cleanup/reuse preserves valid census");
         end;
         Check (C.Retained_Count (A) = 0, "pressure returned no compound root");
         pragma Assert (C.Module_Transient_Free (A));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("runtime package GC checks" & Checks'Image);
end Runtime_Package_GC_Tests;
