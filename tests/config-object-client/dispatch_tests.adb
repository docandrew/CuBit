with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CuBit.Messages; use CuBit.Messages;
with Config_Authority;
with Config_Collections;
with Config_Objects;
with Config_Typed_Store;
with Config_Worker_Protocol;
with Config_Object_Messages;
with Config_Object_Dispatch;

procedure Dispatch_Tests is
   package A renames Config_Authority;
   package C renames Config_Collections;
   package V renames Config_Objects;
   package Store renames Config_Typed_Store;
   package P renames Config_Worker_Protocol;
   package W renames Config_Object_Messages;
   package D renames Config_Object_Dispatch;
   use type A.Install_Result;
   use type C.Result;
   use type V.Outcome;
   use type D.Disposition;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   Object : D.State;
   Data : Store.State;
   Authority : A.Authority_State;
   Write_Rules, Read_Rules : A.Rule_Set;
   Types : CCL.Types.Registry;
   Contract, Binding : CCL.Objects.Binding;
   Schema : constant CCL.Objects.Schema_Key := [1, 2, 3, 4];
   First, Second, Value : CCL.Objects.Image;
   Input : W.Frame;
   Request, Reply : Message;
   Job, Receipt, Saved : P.Frame;
   ID : C.Collection_ID;
   Writer, Reader : Unsigned_64;
   Access_Result : C.Result;
   Installed : A.Install_Result;
   Result : V.Outcome;
   Built : CCL.Objects.Build_Result;
   Next : D.Disposition;
   Good, Ready, Available : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "Config dispatch check" & Checks'Image; end if;
   end Check;
   procedure Call
     (Sender : ProcessID; Op : W.Operation; Handle : Unsigned_64 := 0;
      Revision : Unsigned_64 := 0; Token : Unsigned_64 := 0; Reserved : Boolean := False) is
   begin
      Request := W.Request (Op, (7, 9), Handle, Revision);
      D.Handle (Object, Data, Authority, Sender, Op, Request, Input, Token, Reserved, Reply, Value, Next);
      if Next = D.Reply_Now then Check (W.Valid_Reply (Reply, Op, Revision));
      else Check (Reply = NULL_MESSAGE and D.Waiting (Object)); end if;
   end Call;
   procedure Is_Reply (Code : W.Status) is
   begin
      Check (Next = D.Reply_Now and Reply.tag.label = W.Status'Enum_Rep (Code));
   end Is_Reply;
   procedure Build_Receipt (Code : P.Reply_Kind; Revision : Unsigned_64; Value : CCL.Objects.Image) is
   begin
      Store.Pending (Data, Job, Binding, Available); Check (Available);
      P.Make_Reply (Job, Code, Revision, Binding, Value, Receipt, Good); Check (Good);
   end Build_Receipt;
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Schema, Contract, Good); Check (Good);
   First := CCL.Objects.Empty (Contract); Second := First;
   CCL.Objects.Append (First, CCL.Objects.Integer_Cell (41), Built); Check (Built = CCL.Objects.Added);
   CCL.Objects.Append (Second, CCL.Objects.Integer_Cell (42), Built); Check (Built = CCL.Objects.Added);
   Store.Register (Data, "org.cubit.settings", Contract, ID, Access_Result); Check (Access_Result = C.Registered);
   A.Append (Write_Rules, "org.cubit", A.Read_Write, Good); Check (Good);
   A.Append (Read_Rules, "org.cubit", A.Read_Only, Good); Check (Good);
   A.Install (Authority, 42, Write_Rules, Installed); Check (Installed = A.Installed);
   A.Install (Authority, 43, Read_Rules, Installed); Check (Installed = A.Installed);
   W.Describe ("org.cubit.settings", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
   Call (99, W.Open_Collection); Is_Reply (W.Denied);
   Call (43, W.Open_Collection); Is_Reply (W.Denied);
   Call (42, W.Open_Collection); Is_Reply (W.Success); Writer := Reply.words (0);
   W.Describe ("org.cubit.settings", W.Read_Only, 0, Schema, Input.Control, Good); Check (Good);
   Call (43, W.Open_Collection); Is_Reply (W.Success); Reader := Reply.words (0);
   Call (43, W.Get_Object, Writer); Is_Reply (W.Denied); Check (Value = CCL.Objects.Image'(others => <>));
   Call (42, W.Get_Object, Writer); Is_Reply (W.Unavailable);
   Store.Restore (Data, ID, 10, 1, Result); Check (Result = V.Accepted);
   Build_Receipt (P.Absent, 0, First);
   D.Finish (Object, Data, Receipt, Reply, Ready); Check (not Ready and Reply = NULL_MESSAGE);
   Call (42, W.Get_Object, Writer); Is_Reply (W.Missing);
   Input.Value := First;
   Call (42, W.Set_Object, Writer, 0, 2); Is_Reply (W.Unavailable);
   Check (Store.Pending_Token (Data) = 0 and not D.Waiting (Object));
   Call (43, W.Set_Object, Reader, 0, 2, True); Is_Reply (W.Denied);
   Call (42, W.Set_Object, Writer, 0, 2, True); Check (Next = D.Await_Storage);
   Call (99, W.Set_Object, Writer, 0, 3, True); Is_Reply (W.Denied);
   Call (42, W.Set_Object, Writer, 0, 3, True); Is_Reply (W.Busy);
   Call (43, W.Get_Object, Reader); Is_Reply (W.Missing);
   Build_Receipt (P.Committed, 1, First); Saved := Receipt;
   Receipt.Session := 9;
   D.Finish (Object, Data, Receipt, Reply, Ready); Check (not Ready and D.Waiting (Object));
   D.Finish (Object, Data, Saved, Reply, Ready);
   Check (Ready and not D.Waiting (Object) and W.Valid_Reply (Reply, W.Set_Object, 0));
   Check (Reply.tag.label = W.Status'Enum_Rep (W.Success) and Reply.words (0) = 1);
   D.Finish (Object, Data, Saved, Reply, Ready); Check (not Ready and Reply = NULL_MESSAGE);
   Call (43, W.Get_Object, Reader); Is_Reply (W.Success); Check (Value = First);
   Input.Value := Second;
   Call (42, W.Set_Object, Writer, 0, 3, True); Is_Reply (W.Conflict);
   Call (42, W.Set_Object, Writer, 1, 4, True); Check (Next = D.Await_Storage);
   Call (43, W.Get_Object, Reader); Is_Reply (W.Success); Check (Value = First);
   A.Revoke (Authority, 42);
   Call (42, W.Get_Object, Writer); Is_Reply (W.Denied);
   Call (42, W.Set_Object, Writer, 1, 5, True); Is_Reply (W.Denied);
   Build_Receipt (P.Committed, 2, Second);
   D.Finish (Object, Data, Receipt, Reply, Ready);
   Check (Ready and Reply.tag.label = W.Status'Enum_Rep (W.Success) and Reply.words (0) = 2);
   Call (43, W.Get_Object, Reader); Is_Reply (W.Success); Check (Value = Second);
   Call (42, W.Close_Collection, Writer); Is_Reply (W.Success);
   A.Install (Authority, 42, Write_Rules, Installed); Check (Installed = A.Installed);
   W.Describe ("org.cubit.settings", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
   Call (42, W.Open_Collection); Is_Reply (W.Success); Writer := Reply.words (0);
   Input.Value := First;
   Call (42, W.Set_Object, Writer, 2, 5, True); Check (Next = D.Await_Storage);
   Build_Receipt (P.Committed, 3, First); Saved := Receipt;
   D.Lost (Object, Data, 9, Reply, Ready); Check (not Ready and D.Waiting (Object));
   D.Lost (Object, Data, 10, Reply, Ready); Check (Ready and not D.Waiting (Object));
   Check (Reply.tag.label = W.Status'Enum_Rep (W.Uncertain));
   D.Lost (Object, Data, 10, Reply, Ready); Check (not Ready);
   D.Finish (Object, Data, Saved, Reply, Ready); Check (not Ready);
   Call (43, W.Get_Object, Reader); Is_Reply (W.Stale); Check (Value = Second);
   Store.Restore (Data, ID, 11, 6, Result); Check (Result = V.Accepted);
   Build_Receipt (P.Loaded, 3, First);
   D.Finish (Object, Data, Receipt, Reply, Ready); Check (not Ready);
   Call (43, W.Get_Object, Reader); Is_Reply (W.Success); Check (Value = First);
   -- Pending receipts can be explicitly indeterminate or malformed after the
   -- worker committed. Neither permits publishing a candidate or claiming a
   -- definite rejection. Recovery can reveal the newer durable revision.
   for Scenario in 1 .. 2 loop
      declare
         Token : constant Unsigned_64 := Unsigned_64 (5 + 2 * Scenario);
         Previous : constant Unsigned_64 := Unsigned_64 (2 + Scenario);
      begin
         Input.Value := Second;
         Call (42, W.Set_Object, Writer, Previous, Token, True);
         Check (Next = D.Await_Storage);
         if Scenario = 1 then Build_Receipt (P.Uncertain, 0, Second);
         else
            Build_Receipt (P.Committed, Previous + 1, Second);
            Receipt.Reserved := 1;
         end if;
         Saved := Receipt;
         D.Finish (Object, Data, Receipt, Reply, Ready);
         Check (Ready and not D.Waiting (Object) and W.Valid_Reply (Reply, W.Set_Object, Previous));
         Check (Reply.tag.label = W.Status'Enum_Rep (W.Uncertain) and Reply.words (0) = 0);
         D.Finish (Object, Data, Saved, Reply, Ready); Check (not Ready);
         Call (43, W.Get_Object, Reader); Is_Reply (W.Stale);
         Check (Reply.words (0) = Previous);
         Store.Restore (Data, ID, Unsigned_64 (11 + Scenario), Token + 1, Result);
         Check (Result = V.Accepted);
         Build_Receipt (P.Loaded, Previous + 1, Second);
         D.Finish (Object, Data, Receipt, Reply, Ready); Check (not Ready);
         Call (43, W.Get_Object, Reader); Is_Reply (W.Success);
         Check (Reply.words (0) = Previous + 1 and Value = Second);
      end;
   end loop;
   W.Describe ("org.cubit.settings", W.Read_Only, 1, Schema, Input.Control, Good); Check (Good);
   Call (43, W.Open_Collection); Is_Reply (W.Invalid_Request);
   Request := W.Request (W.Get_Object, (7, 9), Reader); Request.tag.flags := 1;
   D.Handle (Object, Data, Authority, 43, W.Get_Object, Request, Input, 0, False, Reply, Value, Next);
   Is_Reply (W.Invalid_Request); Check (Value = CCL.Objects.Image'(others => <>));
   -- A broad namespace write grant is not permission to mutate declarations.
   Store.Register (Data, "org.cubit.managed", Contract, ID, Access_Result, C.Declaration_Managed);
   Check (Access_Result = C.Registered);
   W.Describe ("org.cubit.managed", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
   Call (42, W.Open_Collection); Is_Reply (W.Denied);
   W.Describe ("org.cubit.managed", W.Read_Only, 0, Schema, Input.Control, Good); Check (Good);
   Call (42, W.Open_Collection); Is_Reply (W.Success); Reader := Reply.words (0);
   Input.Value := First;
   Call (42, W.Set_Object, Reader, 0, 30, True); Is_Reply (W.Denied);
   Check (not D.Waiting (Object));
   Store.Pending (Data, Job, Binding, Available); Check (not Available);
   Ada.Text_IO.Put_Line ("Config native-object dispatch/deferred replies: PASS" & Checks'Image & " checks");
end Dispatch_Tests;
