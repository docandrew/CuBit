with CBOR;
with CCL.Objects.Schemas.Persistence;

package body Config_Database.Schemas is
   package Codec renames CCL.Objects.Schemas.Persistence;
   use type System.Address;
   use type Codec.Outcome;
   use type Config_Schema_Protocol.Operation;
   type Operation is (Create_Type, Recover_Type);
   for Operation use (Create_Type => 1, Recover_Type => 2);
   type Wire_Result is
     (Created_Type, Existing_Type, Conflicting_Type, Loaded_Type,
      Absent_Type, Rejected_Type, Uncertain_Type, Failed_Type);
   for Wire_Result use
     (Created_Type => 1, Existing_Type => 2, Conflicting_Type => 3,
      Loaded_Type => 4, Absent_Type => 5, Rejected_Type => 6,
      Uncertain_Type => 7, Failed_Type => 8);
   type Reply is record
      Code, Length : Unsigned_32 := 0;
      Data : CBOR.Byte_Array (1 .. Codec.Maximum_Encoded_Bytes) := [others => 0];
   end record with Size => (8 + Codec.Maximum_Encoded_Bytes) * 8, Alignment => 4;
   for Reply use record
      Code at 0 range 0 .. 31;
      Length at 4 range 0 .. 31;
      Data at 8 range 0 .. Codec.Maximum_Encoded_Bytes * 8 - 1;
   end record;
   procedure Execute (Database, Input, Output : System.Address)
     with Import, Convention => C, External_Name => "cubit_config_schema_execute";

   procedure Prepare
     (Database : System.Address; Action : Operation; Name, Context : String;
      Call : out Request; Good : out Boolean)
   is
   begin
      Call := (others => <>);
      Good := Database /= System.Null_Address and then
        Config_Worker_Protocol.Valid_Name (Name) and then
        Config_Worker_Protocol.Valid_Name (Context);
      if not Good then return; end if;
      Call.Action := Operation'Enum_Rep (Action);
      Call.Name_Length := Name'Length;
      Call.Context_Length := Context'Length;
      Call.Name (1 .. Name'Length) := Name;
      Call.Context (1 .. Context'Length) := Context;
   end Prepare;

   procedure Create
     (Database : System.Address; Name, Context : String;
      Contract : CCL.Objects.Binding; Result : out Creation)
   is
      Call : Request;
      Output : Reply;
      Packet : Codec.Packet;
      Encoded : Codec.Outcome;
      Good : Boolean;
   begin
      Result := Rejected;
      Prepare (Database, Create_Type, Name, Context, Call, Good);
      if not Good then return; end if;
      Codec.Encode (Contract, Packet, Encoded);
      if Encoded /= Codec.Success then return; end if;
      Call.Schema := CCL.Objects.Identity (Contract);
      Call.Input_Length := Unsigned_64 (Packet.Length);
      Call.Input := Packet.Data'Address;
      Execute (Database, Call'Address, Output'Address);
      Result := Uncertain;
      if Output.Length /= 0 then return; end if;
      case Output.Code is
         when Wire_Result'Enum_Rep (Created_Type) => Result := Created;
         when Wire_Result'Enum_Rep (Existing_Type) => Result := Already_Exists;
         when Wire_Result'Enum_Rep (Conflicting_Type) =>
            --  Rust compares the stored bytes. Equivalent nominal schemas can
            --  have different local IDs/declaration order/unrelated metadata.
            --  Definitions are immutable and this adapter exclusively owns the
            --  database, so a validated read can settle that comparison without
            --  rewriting the declaration or retrying an uncertain transaction.
            declare
               Existing : CCL.Objects.Binding;
               Loaded_As : Recovery;
            begin
               Recover (Database, Name, Context, Existing, Loaded_As);
               if Loaded_As = Loaded then
                  Result := (if CCL.Objects.Same_Schema (Existing, Contract) then
                                Already_Exists else Definition_Conflict);
               end if;
               --  Absent/malformed/failed recovery contradicts the conflict
               --  response: leave Uncertain, retiring the worker session.
            end;
         when Wire_Result'Enum_Rep (Rejected_Type) => Result := Rejected;
         when others => null;
      end case;
   end Create;

   procedure Recover
     (Database : System.Address; Name, Context : String;
      Contract : out CCL.Objects.Binding; Result : out Recovery)
   is
      Call : Request;
      Output : Reply;
      Decoded : Codec.Outcome;
      Good : Boolean;
      Empty_Contract : CCL.Objects.Binding;
   begin
      -- Default initialization is unbound; no partial import
      -- escapes on absence, bad metadata or a malformed database reply.
      Contract := Empty_Contract;
      Result := Load_Failed;
      Prepare (Database, Recover_Type, Name, Context, Call, Good);
      if not Good then return; end if;
      Execute (Database, Call'Address, Output'Address);
      if Output.Code = Wire_Result'Enum_Rep (Absent_Type) and then Output.Length = 0 then
         Result := Absent;
      elsif Output.Code = Wire_Result'Enum_Rep (Loaded_Type) and then
        Output.Length in 1 .. Codec.Maximum_Encoded_Bytes
      then
         Codec.Decode (Output.Data (1 .. CBOR.SE_Offset (Output.Length)), Contract, Decoded);
         if Decoded = Codec.Success then Result := Loaded; end if;
      end if;
   end Recover;

   procedure Invoke
     (Database : System.Address; Action : Config_Schema_Protocol.Operation;
      Name, Context : String; Contract : CCL.Objects.Binding;
      Recovered : out CCL.Objects.Binding; Result : out Config_Schema_Protocol.Reply_Kind)
   is
      package P renames Config_Schema_Protocol;
      Made : Creation;
      Read : Recovery;
      Empty : CCL.Objects.Binding;
   begin
      Recovered := Empty;
      if Action = P.Create then
         Create (Database, Name, Context, Contract, Made);
         Result := (case Made is when Created => P.Created, when Already_Exists => P.Already_Exists,
           when Definition_Conflict => P.Definition_Conflict, when Rejected => P.Rejected,
           when Uncertain => P.Uncertain);
      else
         Recover (Database, Name, Context, Recovered, Read);
         Result := (case Read is when Loaded => P.Loaded, when Absent => P.Absent,
           when Load_Failed => P.Load_Failed);
      end if;
   end Invoke;
end Config_Database.Schemas;
