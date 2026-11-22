with Ada.Text_IO;
with Interfaces; use Interfaces;
with CBOR;
with CCL.Types;
with CCL.Objects;
with CCL.Objects.Persistence;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker;

procedure Execution_Tests is
   package P renames Config_Worker_Protocol;
   package Codec renames CCL.Objects.Persistence;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Image;
   use type Codec.Outcome;
   use type P.Operation;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   Value, Decoded : CCL.Objects.Image;
   Built : CCL.Objects.Build_Result;
   Packet : Codec.Packet;
   Encoded : Codec.Outcome;
   Raw : Config_Worker_Storage.Reply;
   Calls, Count : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Count := Count + 1;
      if not OK then raise Program_Error with "worker execution check" & Count'Image; end if;
   end Check;
   procedure Invoke
     (Action : P.Operation; Name, Context : String; Expected_Revision : P.Number;
      Schema : CCL.Objects.Schema_Key; Input : Codec.Packet;
      Output : out Config_Worker_Storage.Reply)
   is
      use type CCL.Objects.Schema_Key;
   begin
      Calls := Calls + 1;
      Check (Name = "org.cubit.test" and Context = "default" and Schema = [1, 2, 3, 4]);
      if Action = P.Commit then
         Check (Expected_Revision = 1);
         Codec.Decode (Input.Data (1 .. CBOR.SE_Offset (Input.Length)), Contract, Decoded, Encoded);
         Check (Encoded = Codec.Success and Decoded = Value);
      else Check (Expected_Revision = 0 and Input.Length = 0);
      end if;
      Output := Raw;
   end Invoke;
   package Worker is new Config_Worker (Invoke);
   Request, Response : P.Frame;
   OK : Boolean;
   type Revisions is array (Positive range <>) of P.Number;
   Revision_Cases : constant Revisions := [0, 1, 2, P.Maximum_Revision, Unsigned_64'Last];
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, OK); Check (OK);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built); Check (Built = CCL.Objects.Added);
   Codec.Encode (Value, Contract, Packet, Encoded); Check (Encoded = Codec.Success);
   -- Every backend operation/status/revision combination. Error replies are
   -- canonical, and poisoned instances must never issue a second operation.
   for Action in P.Operation loop
      P.Make_Request (Action, 10, 20, (if Action = P.Load then 0 else 1),
                      "org.cubit.test", "default", Contract, Value, Request, OK); Check (OK);
      for Code in Unsigned_32 range 0 .. 8 loop
         for Rev of Revision_Cases loop
            declare
               S : Worker.State;
               Expected : P.Frame;
               Valid_Backend : Boolean;
               Poison : Boolean;
            begin
               Raw := (Code => Code, Revision => Rev, others => <>);
               if Code = P.Reply_Kind'Enum_Rep (P.Loaded) then
                  Raw.Length := Unsigned_32 (Packet.Length); Raw.Data := Packet.Data;
               end if;
               Expected := Request; Expected.Reply := Code; Expected.Revision := Rev;
               Expected.Value := (if Code = P.Reply_Kind'Enum_Rep (P.Loaded)
                                  then Value else CCL.Objects.Empty (Contract));
               Valid_Backend := P.Valid_Reply (Expected, Request, Contract);
               Poison := not Valid_Backend or Code in P.Reply_Kind'Enum_Rep (P.Load_Failed) |
                                                     P.Reply_Kind'Enum_Rep (P.Uncertain);
               Calls := 0;
               Worker.Handle (S, Request, Contract, Response, OK);
               Check (OK and Calls = 1 and P.Valid_Reply (Response, Request, Contract));
               Check (Worker.Needs_Recovery (S) = Poison);
               Check (Response.Reply =
                 (if Poison then (if Action = P.Load then P.Reply_Kind'Enum_Rep (P.Load_Failed)
                                  else P.Reply_Kind'Enum_Rep (P.Uncertain)) else Code));
               if Poison then
                  Worker.Handle (S, Request, Contract, Response, OK);
                  Check (OK and Calls = 1 and Worker.Needs_Recovery (S));
               end if;
            end;
         end loop;
      end loop;
   end loop;
   -- Malformed encoded data, oversized raw lengths and unexpected payloads.
   P.Make_Request (P.Load, 10, 20, 0, "org.cubit.test", "default", Contract, Value, Request, OK);
   for Fault in 1 .. 4 loop
      declare
         S : Worker.State;
      begin
         Raw := (Code => P.Reply_Kind'Enum_Rep (P.Loaded), Revision => 1,
                 Length => Unsigned_32 (Packet.Length), Data => Packet.Data);
         case Fault is
            when 1 => Raw.Length := Unsigned_32'Last;
            when 2 => Raw.Length := 0;
            when 3 => Raw.Data := [others => 0];
            when 4 => Raw.Code := P.Reply_Kind'Enum_Rep (P.Absent); Raw.Revision := 0;
         end case;
         Calls := 0;
         Worker.Handle (S, Request, Contract, Response, OK);
         Check (OK and Calls = 1 and Worker.Needs_Recovery (S));
         Check (Response.Value = CCL.Objects.Empty (Contract));
         Worker.Handle (S, Request, Contract, Response, OK); Check (OK and Calls = 1);
      end;
   end loop;
   declare
      S : Worker.State;
   begin
      Calls := 0;
      Request.Name_Length := Unsigned_32'Last;
      Worker.Handle (S, Request, Contract, Response, OK);
      Check (not OK and Calls = 0 and not Worker.Needs_Recovery (S));
   end;
   Ada.Text_IO.Put_Line ("Config worker execution:" & Count'Image & " checks PASS");
end Execution_Tests;
