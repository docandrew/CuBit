with Ada.Command_Line;
with Ada.Text_IO;
with System;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects.Schemas;
with CCL.Objects.Persistence;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Database.Schemas;
with Config_Schema_Protocol;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker_Channel; use Config_Worker_Channel;
with Config_Worker_Receiver;

-- Linux-hosted real Turso, native channel/executor, modeled IPC/grants.
procedure Schema_Store_Turso is
   package P renames Config_Schema_Protocol;
   package Grants renames CuBit.Memory_Grants;
   use type P.Operation;
   use type P.Reply_Kind;
   use type CCL.Objects.Binding;
   use type System.Address;
   function Open_Database (Path : System.Address; Length : Unsigned_64) return System.Address
     with Import, Convention => C, External_Name => "cubit_config_test_open";
   function Close_Database (DB : System.Address) return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_config_test_close";
   function Seed_Managed (Path : System.Address; Length : Unsigned_64) return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_config_test_seed_managed";
   Database : System.Address := System.Null_Address;
   Path : constant String := Ada.Command_Line.Argument (1);
   Registry : CCL.Types.Registry;
   Integer_Type, Boolean_Type, Changed_Type, Padded_Integer, Empty : CCL.Objects.Binding;
   Nominal, Shifted_Nominal : CCL.Objects.Binding;
   Checks, Calls : Natural := 0;
   Token : Unsigned_64 := 0;
   Good : Boolean;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with "schema Turso check" & Checks'Image; end if;
   end Check;
   procedure Pair_Type (Shifted : Boolean; Contract : out CCL.Objects.Binding) is
      use CCL.Types;
      Types : CCL.Types.Registry;
      A, B, Root, Noise : Type_Reference;
      Defined_As : Definition_Result;
      Accepted : Boolean;
      procedure Leaf (Name : String; Ref : out Type_Reference) is
      begin
         Define (Types, (Identifier => Named (Name), Form => Product, others => <>), Ref, Defined_As);
         Check (Defined_As = Defined);
      end Leaf;
   begin
      if Shifted then
         Leaf ("Noise", Noise); Leaf ("B", B); Leaf ("A", A);
      else
         Leaf ("A", A); Leaf ("B", B);
      end if;
      Define (Types, (Identifier => Named ("Pair"), Form => Product, Count => 2,
        Parts => [1 => (Named ("a"), A), 2 => (Named ("b"), B), others => <>]), Root, Defined_As);
      Check (Defined_As = Defined);
      CCL.Objects.Bind (Types, Root, [9, 10, 11, 12], Contract, Accepted); Check (Accepted);
   end Pair_Type;
   function Trusted (Source : Process_ID; Tag : Unsigned_64) return Boolean is
     (Source = 42 and Tag = 77);
   procedure Invoke_Value
     (Action : Config_Worker_Protocol.Operation; Name, Context : String;
      Expected_Revision : Unsigned_64; Schema : CCL.Objects.Schema_Key;
      Input : CCL.Objects.Persistence.Packet; Output : out Config_Worker_Storage.Reply)
   is
      pragma Unreferenced (Action, Name, Context, Expected_Revision, Schema, Input, Output);
   begin
      raise Program_Error with "metadata test must not create a value";
   end Invoke_Value;
   procedure Invoke_Type
     (Action : P.Operation; Name, Context : String; Contract : CCL.Objects.Binding;
      Recovered : out CCL.Objects.Binding; Result : out P.Reply_Kind)
   is
   begin
      Check (Grants.Active_Acquisitions = 0);
      Check (CCL.Objects.Is_Bound (Contract) = (Action = P.Create));
      Calls := Calls + 1;
      Config_Database.Schemas.Invoke (Database, Action, Name, Context, Contract, Recovered, Result);
   end Invoke_Type;
   package Receiver is new Config_Worker_Receiver (12, Trusted, Invoke_Value, Invoke_Type);
begin
   CCL.Objects.Bind (Registry, CCL.Types.Integer_Type, [1,2,3,4], Integer_Type, Good); Check (Good);
   CCL.Objects.Bind (Registry, CCL.Types.Boolean_Type, [5,6,7,8], Boolean_Type, Good); Check (Good);
   CCL.Objects.Bind (Registry, CCL.Types.Boolean_Type, [1,2,3,4], Changed_Type, Good); Check (Good);
   Pair_Type (False, Nominal); Pair_Type (True, Shifted_Nominal);
   Check (Nominal /= Shifted_Nominal and CCL.Objects.Same_Schema (Nominal, Shifted_Nominal));
   declare
      Ref : CCL.Types.Type_Reference;
      Defined_As : CCL.Types.Definition_Result;
      use type CCL.Types.Definition_Result;
   begin
      CCL.Types.Define (Registry, (Identifier => CCL.Types.Named ("Unrelated"),
        Form => CCL.Types.Product, others => <>), Ref, Defined_As);
      Check (Defined_As = CCL.Types.Defined);
      CCL.Objects.Bind (Registry, CCL.Types.Integer_Type, [1,2,3,4], Padded_Integer, Good);
      Check (Good and Padded_Integer /= Integer_Type);
   end;
   Check (Seed_Managed (Path'Address, Path'Length) = 1);
   for Phase in 1 .. 3 loop
      declare
         Client : Channel;
         Server : Receiver.State;
         procedure Exchange
           (Action : P.Operation; Name : String; Contract : CCL.Objects.Binding;
            Expected : P.Reply_Kind; Lose_Reply : Boolean := False)
         is
            Input, Output : P.Frame;
            Envelope, Reply : Message;
            Recovered : CCL.Objects.Binding;
            Valid, Taken, Ok : Boolean;
            Sent : Submission;
            Done : Completion_Result;
         begin
            Token := Token + 1;
            P.Make_Request (Action, Unsigned_64 (Phase), Token, Name, "machine", Contract, Input, Ok);
            Check (Ok);
            Submit_Type (Client, Input, Sent); Check (Sent = Submitted);
            Grants.Acquisitions := 0; Grants.Returns := 0;
            Grants.Deny_Acquisition := (if Lose_Reply then 2 else 0);
            Envelope := Last_Request; Envelope.authorityTag := 77;
            Receiver.Handle (Server, 42, Envelope, Reply);
            Check (Receiver.Needs_Recovery (Server) = Lose_Reply);
            Complete (Client, (requestId => Token, token => Token, msg => Reply,
              from => 42, status => COMPLETION_OK, valid => True), Done);
            Check (Done = Completed);
            Take_Type_Result (Client, Output, Valid, Taken);
            Check (Taken and (Valid = (not Lose_Reply)));
            if Valid then
               Check (Output.Reply = P.Reply_Kind'Enum_Rep (Expected));
               if Expected in P.Loaded | P.Loaded_Managed then
                  CCL.Objects.Schemas.Read (Output.Metadata, Recovered, Ok);
                  Check (Ok and CCL.Objects.Same_Schema (Recovered, Contract));
               end if;
            else
               Check (Status (Client) = Failed);
            end if;
         end Exchange;
      begin
         Database := Open_Database (Path'Address, Path'Length); Check (Database /= System.Null_Address);
         Grants.Expected_Pages := Loan_Bytes_Count / 4096;
         Grants.Expected_Transfer_Bytes := P.Frame_Bytes;
         Grants.Active_Acquisitions := 0; Grants.Fail_Return := 0; Grants.Deny_Acquisition := 0;
         Initialize (Client, 4, Good); Check (Good);
         -- Persisted class survives three fresh worker/database lifetimes.
         -- Create, including an equivalent schema, cannot downscope it to state.
         Exchange (P.Recover, "org.cubit.managed", Integer_Type, P.Loaded_Managed);
         Exchange (P.Create, "org.cubit.managed", Integer_Type, P.Management_Conflict);
         Exchange (P.Create, "org.cubit.managed", Padded_Integer, P.Management_Conflict);
         if Phase = 1 then
            Exchange (P.Create, "org.cubit.created", Integer_Type, P.Created);
            Exchange (P.Create, "org.cubit.created", Integer_Type, P.Already_Exists);
            Exchange (P.Create, "org.cubit.created", Changed_Type, P.Definition_Conflict);
            Exchange (P.Create, "org.cubit.nominal", Nominal, P.Created);
         end if;
         Exchange (P.Recover, "org.cubit.created", Integer_Type, P.Loaded);
         -- Exercise the byte-different, semantically identical declaration
         -- both in the original session and after independent database reopen.
         Exchange (P.Create, "org.cubit.created", Padded_Integer, P.Already_Exists);
         Exchange (P.Recover, "org.cubit.created", Integer_Type, P.Loaded);
         Exchange (P.Create, "org.cubit.nominal", Shifted_Nominal, P.Already_Exists);
         Exchange (P.Recover, "org.cubit.nominal", Nominal, P.Loaded);
         Exchange (P.Recover, "org.cubit.missing", Empty, P.Absent);
         if Phase = 2 then
            Exchange (P.Create, "org.cubit.lostack", Boolean_Type, P.Created, Lose_Reply => True);
         elsif Phase = 3 then
            -- Recover rather than blindly retrying the operation whose reply was lost.
            Exchange (P.Recover, "org.cubit.lostack", Boolean_Type, P.Loaded);
         end if;
         Check (Close_Database (Database) = 1); Database := System.Null_Address;
         Grants.Is_Retired := True; Retire (Client, Good); Check (Good);
      end;
   end loop;
   Check (Calls = 33);
   Ada.Text_IO.Put_Line ("Native schema channel -> real Turso create/recover/lost reply: PASS" & Checks'Image & " checks");
end Schema_Store_Turso;
