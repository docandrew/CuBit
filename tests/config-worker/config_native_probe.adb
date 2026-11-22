with CCL.Types;
with CCL.Objects.Persistence;
with Config_Database;
with Config_Database.Schemas;
with Config_Objects;
with Config_Worker;
with Config_Worker_Protocol;
with Config_Worker_Storage;

package body Config_Native_Probe is
   function Run (Database : System.Address; Phase : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32
   is
      package P renames Config_Worker_Protocol;
      package O renames Config_Objects;
      use type System.Address;
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;
      use type O.Outcome;
      use type O.Read_Result;
      use type CCL.Objects.Image;
      use type CCL.Objects.Build_Result;
      use type CCL.Objects.Binding;
      use type Config_Database.Schemas.Creation;
      use type Config_Database.Schemas.Recovery;
      procedure Invoke
        (Action : P.Operation; Name, Context : String; Expected_Revision : P.Number;
         Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
         Output : out Config_Worker_Storage.Reply)
      is
      begin
         Config_Database.Invoke (Database, Action, Name, Context,
                                 Expected_Revision, Schema, Input, Output);
      end Invoke;
      package Executor is new Config_Worker (Invoke);
      Worker : Executor.State;
      Client : O.State;
      Types : CCL.Types.Registry;
      Contract : CCL.Objects.Binding;
      Schema : constant CCL.Objects.Schema_Key := [1, 2, 3, 4];
      Initial, Candidate, Value : CCL.Objects.Image;
      Request, Response : P.Frame;
      Valid : Boolean;
      Built : CCL.Objects.Build_Result;
      Outcome : O.Outcome;
      Reading : O.Read_Result;
      Revision, Expected : P.Number;
      Step : Boot_Phase;
      Saved_Contract, Different : CCL.Objects.Binding;
      Created : Config_Database.Schemas.Creation;
      Recovered : Config_Database.Schemas.Recovery;
   begin
      if Database = System.Null_Address or else Phase > Boot_Phase'Enum_Rep (Verify_Only)
      then return 1; end if;
      Step := Boot_Phase'Enum_Val (Phase);
      Expected := (case Step is when Seed => 0, when Advance => 1, when Verify_Only => 2);
      CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Schema, Contract, Valid);
      if not Valid then return 2; end if;
      Config_Database.Schemas.Recover
        (Database, "org.cubit.publication", "test", Saved_Contract, Recovered);
      if Step = Seed then
         if Recovered /= Config_Database.Schemas.Absent or else
           CCL.Objects.Is_Bound (Saved_Contract) then return 20; end if;
      elsif Recovered /= Config_Database.Schemas.Loaded or else Saved_Contract /= Contract then
         return 21;
      end if;
      Config_Database.Schemas.Create
        (Database, "org.cubit.publication", "test", Contract, Created);
      if Created /= (if Step = Seed then Config_Database.Schemas.Created else
                     Config_Database.Schemas.Already_Exists) then return 22; end if;
      Config_Database.Schemas.Create
        (Database, "org.cubit.publication", "test", Contract, Created);
      if Created /= Config_Database.Schemas.Already_Exists then return 23; end if;
      CCL.Objects.Bind (Types, CCL.Types.Boolean_Type, Schema, Different, Valid);
      if not Valid then return 24; end if;
      Config_Database.Schemas.Create
        (Database, "org.cubit.publication", "test", Different, Created);
      if Created /= Config_Database.Schemas.Definition_Conflict then return 25; end if;
      Config_Database.Schemas.Recover
        (Database, "org.cubit.publication", "test", Saved_Contract, Recovered);
      if Recovered /= Config_Database.Schemas.Loaded or else Saved_Contract /= Contract then
         return 26;
      end if;
      Initial := CCL.Objects.Empty (Contract);
      if Step /= Seed then
         CCL.Objects.Append
           (Initial, CCL.Objects.Integer_Cell (if Step = Advance then 41 else 42), Built);
         if Built /= CCL.Objects.Added then return 3; end if;
      end if;
      O.Initialize (Client, Contract, Valid);
      if not Valid then return 4; end if;
      O.Attach (Client, 1, Outcome);
      if Outcome /= O.Accepted then return 5; end if;
      O.Begin_Load (Client, 1, Outcome);
      if Outcome /= O.Accepted then return 6; end if;
      P.Make_Request (P.Load, 1, 1, 0, "org.cubit.publication", "test", Contract,
                      CCL.Objects.Empty (Contract), Request, Valid);
      if not Valid then return 7; end if;
      Executor.Handle (Worker, Request, Contract, Response, Valid);
      if not Valid or else Executor.Needs_Recovery (Worker) then return 8; end if;
      if Response.Reply /= P.Reply_Kind'Enum_Rep
        (if Step = Seed then P.Absent else P.Loaded) or else
        Response.Revision /= Expected or else Response.Value /= Initial
      then return 9; end if;
      O.Finish_Load (Client, 1, 1, (if Step = Seed then O.Absent else O.Loaded),
                     Expected, Response.Value, Outcome);
      if Outcome /= (if Step = Seed then O.Accepted else O.Published) then return 10; end if;
      O.Read (Client, Schema, Value, Revision, Reading);
      if Reading /= (if Step = Seed then O.Missing else O.Found) or else
        Revision /= Expected or else (Step /= Seed and then Value /= Initial)
      then return 11; end if;
      if Step = Verify_Only then return 0; end if;
      Candidate := CCL.Objects.Empty (Contract);
      CCL.Objects.Append
        (Candidate, CCL.Objects.Integer_Cell (if Step = Seed then 41 else 42), Built);
      if Built /= CCL.Objects.Added then return 12; end if;
      O.Begin_Commit (Client, Candidate, Expected, 2, Outcome);
      if Outcome /= O.Accepted then return 13; end if;
      O.Export_Pending (Client, Value, Revision, Valid);
      if not Valid or else Value /= Candidate or else Revision /= Expected then return 14; end if;
      P.Make_Request (P.Commit, 1, 2, Revision, "org.cubit.publication", "test",
                      Contract, Value, Request, Valid);
      if not Valid then return 15; end if;
      Executor.Handle (Worker, Request, Contract, Response, Valid);
      if not Valid or else Executor.Needs_Recovery (Worker) or else
        Response.Reply /= P.Reply_Kind'Enum_Rep (P.Committed) or else
        Response.Revision /= Expected + 1
      then return 16; end if;
      --  Disk commit is not publication: the old cache must still be visible.
      O.Read (Client, Schema, Value, Revision, Reading);
      if Reading /= (if Step = Seed then O.Missing else O.Found) or else
        Revision /= Expected or else (Step /= Seed and then Value /= Initial)
      then return 17; end if;
      O.Finish_Commit (Client, 1, 2, O.Committed, Response.Revision, Outcome);
      if Outcome /= O.Published then return 18; end if;
      O.Read (Client, Schema, Value, Revision, Reading);
      if Reading /= O.Found or else Revision /= Expected + 1 or else Value /= Candidate
      then return 19; end if;
      return 0;
   end Run;
end Config_Native_Probe;
