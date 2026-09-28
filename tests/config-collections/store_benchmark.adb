with Ada.Real_Time;
with Ada.Text_IO;
with CCL.Objects;
with CCL.Types;
with Config_Authority;
with Config_Collections;
with Config_Objects;
with Config_Typed_Store;
with Config_Worker_Protocol;

-- Linux-hosted CPU/copy baseline, NOT an IPC, database or disk benchmark.
-- Replies are synthesized by the real protocol builder. The production store
-- still performs authorization, validation and acknowledged publication.
procedure Store_Benchmark is
   package A renames Config_Authority;
   package C renames Config_Collections;
   package V renames Config_Objects;
   package P renames Config_Worker_Protocol;
   package S renames Config_Typed_Store;
   use type Ada.Real_Time.Time;
   use type A.Install_Result;
   use type C.Result;
   use type V.Outcome;
   use type V.Read_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Image;
   use type S.Number;
   Iterations : constant := 10_000;
   Store : S.State;
   Authority : A.Authority_State;
   Rules : A.Rule_Set;
   Types : CCL.Types.Registry;
   Contract, Retrieved : CCL.Objects.Binding;
   Value, Actual : CCL.Objects.Image;
   Request, Response : P.Frame;
   ID : C.Collection_ID;
   Handle : C.Handle;
   Catalog_Result : C.Result;
   Result : V.Outcome;
   Read_Result : V.Read_Result;
   Installed : A.Install_Result;
   Built : CCL.Objects.Build_Result;
   Authorized, Good, Available : Boolean;
   Revision : S.Number;
   Token : S.Number := 1;
   Checksum : S.Number := 0;

   procedure Require (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "benchmark invariant"; end if;
   end Require;

   procedure Finish (Kind : P.Reply_Kind; New_Revision : S.Number) is
   begin
      S.Pending (Store, Request, Retrieved, Available);
      Require (Available);
      P.Make_Reply (Request, Kind, New_Revision, Retrieved, Value, Response, Good);
      Require (Good);
      S.Complete (Store, Response, Result);
      Require (Result in V.Accepted | V.Published);
   end Finish;

   procedure Report (Label : String; Started : Ada.Real_Time.Time) is
      Elapsed : constant Duration := Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Started);
   begin
      Ada.Text_IO.Put_Line
        (Label & " ns/op" & Long_Long_Integer'Image
           (Long_Long_Integer (Elapsed * 1_000_000_000 / Iterations)));
   end Report;

   procedure Measure (Label : String) is
      Started : Ada.Real_Time.Time;
   begin
      -- First acknowledged value; setup and allocation are not timed.
      S.Set (Store, Authority, 42, Handle, Value, 0, Token, Authorized, Result);
      Token := Token + 1;
      Require (Authorized and Result = V.Accepted);
      Finish (P.Committed, 1);
      for Run in 1 .. 5 loop
         Started := Ada.Real_Time.Clock;
         for I in 1 .. Iterations loop
            S.Get (Store, Authority, 42, Handle, Actual, Revision, Authorized, Read_Result);
            Require (Authorized and Read_Result = V.Found);
            Checksum := Checksum + Revision;
         end loop;
         Report (Label & " get" & Run'Image, Started);
         Require (Actual = Value);
         Started := Ada.Real_Time.Clock;
         for I in 1 .. Iterations loop
            S.Set (Store, Authority, 42, Handle, Value, Revision, Token, Authorized, Result);
            Token := Token + 1;
            Require (Authorized and Result = V.Accepted);
            Revision := Revision + 1;
            Finish (P.Committed, Revision);
         end loop;
         Report (Label & " set/pending/reply/complete" & Run'Image, Started);
      end loop;
      S.Get (Store, Authority, 99, Handle, Actual, Revision, Authorized, Read_Result);
      Require (not Authorized and Revision = 0 and Actual = CCL.Objects.Image'(others => <>));
      S.Close (Store, 42, Handle, Catalog_Result);
      Require (Catalog_Result = C.Closed);
   end Measure;

   procedure Setup (Name : String) is
   begin
      S.Register (Store, Name, Contract, ID, Catalog_Result);
      Require (Catalog_Result = C.Registered);
      S.Open (Store, Authority, 42, Name, 0, A.Read_Write,
        CCL.Objects.Identity (Contract), Handle, Catalog_Result);
      Require (Catalog_Result = C.Opened);
      S.Restore (Store, ID, 10, Token, Result);
      Token := Token + 1;
      Require (Result = V.Accepted);
      Finish (P.Absent, 0);
   end Setup;
begin
   A.Append (Rules, "org.cubit", A.Read_Write, Good); Require (Good);
   A.Install (Authority, 42, Rules, Installed); Require (Installed = A.Installed);
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good);
   Require (Good);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built);
   Require (Built = CCL.Objects.Added);
   Setup ("org.cubit.integer");
   Measure ("integer");
   CCL.Objects.Bind (Types, CCL.Types.String_Type, [5, 6, 7, 8], Contract, Good);
   Require (Good);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append_Text (Value, [1 .. CCL.Objects.Maximum_Text_Bytes => 'x'], Built);
   Require (Built = CCL.Objects.Added);
   Setup ("org.cubit.text");
   Measure ("full-text");
   Ada.Text_IO.Put_Line ("checksum" & Checksum'Image);
end Store_Benchmark;
