with Ada.Text_IO;
with Interfaces;
with CCL.Types;
with CCL.Objects;
with Config_Authority;
with Config_Collections;
with Config_Objects;
with Config_Typed_Store;
with Config_Worker_Protocol;

procedure Managed_Tests is
   package A renames Config_Authority;
   package C renames Config_Collections;
   package S renames Config_Typed_Store;
   package V renames Config_Objects;
   package P renames Config_Worker_Protocol;
   use type Interfaces.Unsigned_64;
   use type A.Install_Result;
   use type A.Operation;
   use type C.Result;
   use type C.Collection_ID;
   use type C.Management_Kind;
   use type V.Outcome;
   use type V.Read_Result;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   Catalog : C.State;
   Store : S.State;
   Authority : A.Authority_State;
   Rules, Activator_Rules : A.Rule_Set;
   Types : CCL.Types.Registry;
   Contract, Retrieved : CCL.Objects.Binding;
   Schema : constant CCL.Objects.Schema_Key := [1, 2, 3, 4];
   Value, Actual : CCL.Objects.Image;
   ID, Actual_ID : C.Collection_ID;
   Token, Reader : C.Handle;
   Result : C.Result;
   Outcome : V.Outcome;
   Read_Result : V.Read_Result;
   Revision : S.Number;
   Installed : A.Install_Result;
   Built : CCL.Objects.Build_Result;
   Good, Authorized, Available : Boolean;
   Request, Response : P.Frame;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "managed check" & Checks'Image; end if;
   end Check;
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Schema, Contract, Good); Check (Good);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built);
   Check (Built = CCL.Objects.Added);
   -- Even an explicitly installed wildcard read/write grant is not activation.
   A.Append (Rules, "", A.Read_Write, Good); Check (Good);
   A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
   A.Append (Activator_Rules, "", [others => True], Good); Check (Good);
   A.Install (Authority, 43, Activator_Rules, Installed); Check (Installed = A.Installed);
   for Kind in C.Management_Kind loop
      declare
         Name : constant String := (if Kind = C.Application_State then "test.state" else "test.managed");
      begin
         C.Register (Catalog, Name, Contract, ID, Result, Kind);
         Check (Result = C.Registered);
         Check (C.Management (Catalog, ID) = Kind);
         -- Activation never becomes a collection-handle operation, even for a
         -- subject explicitly granted it and even on application state.
         C.Open (Catalog, Authority, 43, Name, 0,
           [A.Activate_Config => True, others => False], Schema, Token, Result);
         Check (Result = C.Denied and Token = 0);
         C.Open (Catalog, Authority, 43, Name, 0,
           [others => True], Schema, Token, Result);
         Check (Result = C.Denied and Token = 0);
         C.Register (Catalog, Name, Contract, Actual_ID, Result, Kind);
         Check (Result = C.Already_Registered and Actual_ID = ID);
         C.Check_Registration (Catalog, Name, Contract, Actual_ID, Result,
           (if Kind = C.Application_State then C.Declaration_Managed else C.Application_State));
         Check (Result = C.Management_Conflict and Actual_ID = C.No_Collection);
         C.Register (Catalog, Name, Contract, Actual_ID, Result,
           (if Kind = C.Application_State then C.Declaration_Managed else C.Application_State));
         Check (Result = C.Management_Conflict and Actual_ID = C.No_Collection);
         Check (C.Management (Catalog, ID) = Kind);
         for Read_Right in Boolean loop
            for Write_Right in Boolean loop
               C.Open (Catalog, Authority, 42, Name, 0,
                 [A.Read_Config => Read_Right, A.Write_Config => Write_Right,
                  A.Activate_Config => False], Schema, Token, Result);
               if (Read_Right or Write_Right) and
                 (Kind = C.Application_State or not Write_Right)
               then
                  Check (Result = C.Opened and Token /= 0);
                  for Op in A.Operation loop
                     C.Resolve (Catalog, Authority, 42, Token, Op, Actual_ID, Result);
                     if (case Op is
                           when A.Read_Config => Read_Right,
                           when A.Write_Config => Write_Right,
                           when A.Activate_Config => False)
                     then
                        Check (Result = C.Resolved and Actual_ID = ID);
                     else
                        Check (Result = C.Denied and Actual_ID = C.No_Collection);
                     end if;
                  end loop;
   C.Close (Catalog, 42, Token, Result); Check (Result = C.Closed);
               else
                  Check (Result = C.Denied and Token = 0);
               end if;
            end loop;
         end loop;
      end;
   end loop;

   -- Exercise the actual typed store, not just catalog admission. Recovery of
   -- a trusted stored revision is allowed; public Set must not reach the worker.
   S.Register (Store, "test.managed", Contract, ID, Result, C.Declaration_Managed);
   Check (Result = C.Registered);
   S.Check_Registration (Store, "test.managed", Contract, Actual_ID, Result);
   Check (Result = C.Management_Conflict and Actual_ID = C.No_Collection);
   S.Register (Store, "test.managed", Contract, Actual_ID, Result);
   Check (Result = C.Management_Conflict and Actual_ID = C.No_Collection);
   S.Open (Store, Authority, 42, "test.managed", 0, A.Read_Only, Schema, Reader, Result);
   Check (Result = C.Opened);
   Check (S.Check_Access (Store, Authority, 42, Reader, A.Read_Config));
   Check (not S.Check_Access (Store, Authority, 42, Reader, A.Write_Config));
   S.Restore (Store, ID, 1, 1, Outcome); Check (Outcome = V.Accepted);
   S.Pending (Store, Request, Retrieved, Available); Check (Available);
   P.Make_Reply (Request, P.Loaded, 7, Retrieved, Value, Response, Good); Check (Good);
   S.Complete (Store, Response, Outcome); Check (Outcome = V.Published);
   S.Get (Store, Authority, 42, Reader, Actual, Revision, Authorized, Read_Result);
   Check (Authorized and Read_Result = V.Found and Revision = 7 and Actual = Value);
   S.Set (Store, Authority, 42, Reader, Value, 7, 2, Authorized, Outcome);
   Check (not Authorized and Outcome = V.Rejected);
   S.Pending (Store, Request, Retrieved, Available); Check (not Available);
   S.Get (Store, Authority, 42, Reader, Actual, Revision, Authorized, Read_Result);
   Check (Authorized and Read_Result = V.Found and Revision = 7 and Actual = Value);
   S.Open (Store, Authority, 42, "test.managed", 0, A.Read_Write, Schema, Token, Result);
   Check (Result = C.Denied and Token = 0);
   S.Open (Store, Authority, 99, "test.managed", 0, A.Read_Only, Schema, Token, Result);
   Check (Result = C.Denied and Token = 0);
   A.Revoke (Authority, 42);
   S.Get (Store, Authority, 42, Reader, Actual, Revision, Authorized, Read_Result);
   Check (not Authorized and Revision = 0);
   A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
   Check (not S.Check_Access (Store, Authority, 42, Reader, A.Read_Config));
   S.Open (Store, Authority, 42, "test.managed", 0, A.Read_Write, Schema, Token, Result);
   Check (Result = C.Denied and Token = 0);
   S.Open (Store, Authority, 42, "test.managed", 0, A.Read_Only, Schema, Reader, Result);
   Check (Result = C.Opened);
   Check (S.Check_Access (Store, Authority, 42, Reader, A.Read_Config));
   Ada.Text_IO.Put_Line ("Managed Config enforcement: PASS" & Checks'Image & " checks");
end Managed_Tests;
