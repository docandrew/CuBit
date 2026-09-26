with CCL.Types.Correspondence;
with CCL.Objects.Catalog;
with CCL.Objects.Values;

package body Config_Object_Outcomes with SPARK_Mode is
   use CCL.Types;
   use type Interfaces.Unsigned_64;
   use type Config_Object_Messages.Status;
   use type CCL.Objects.Catalog.Publication_Result;
   function Definitions return Registry is
      Types : Registry;
      Ref : Type_Reference;
      Result : Definition_Result;
      Description : CCL.Types.Description :=
        (Identifier => Named ("ConfigWrite"), Form => Sum,
         Count => Write_Alternative'Pos (Write_Alternative'Last) + 1, others => <>);
   begin
      for Choice in Write_Alternative loop
         Description.Parts (Write_Alternative'Enum_Rep (Choice)) :=
           (Identifier => Named (Name (Choice)),
            Payload => (if Choice = Committed then Integer_Type else Unit_Type));
      end loop;
      Define (Types, Description, Ref, Result);
      return Types;
   end Definitions;
   Types : constant Registry := Definitions;
   Root : constant Type_Reference := Find (Types, Named ("ConfigWrite"));
   function Bound_Schema return CCL.Objects.Binding is
      Contract : CCL.Objects.Binding;
      Accepted : Boolean;
   begin
      CCL.Objects.Bind (Types, Root, Key, Contract, Accepted);
      return Contract;
   end Bound_Schema;
   Contract : constant CCL.Objects.Binding := Bound_Schema;
   function Schema return CCL.Objects.Binding is (Contract);
   procedure Publish (Catalog : in out CCL.Catalog.Interface_Catalog; Accepted : out Boolean) is
      Result : CCL.Objects.Catalog.Publication_Result;
   begin
      CCL.Catalog.Publish_Schema (Catalog, Contract, Result);
      Accepted := Result = CCL.Objects.Catalog.Published;
   end Publish;
   procedure To_VM
     (Local_Types : CCL.Types.Registry; Transport_Valid : Boolean;
      Code : Config_Object_Messages.Status; Revision : Interfaces.Unsigned_64;
      Value : out CCL.VM.Value; Accepted : out Boolean)
   is
      Ref : constant Type_Reference := CCL.Types.Correspondence.Resolve (Types, Root, Local_Types);
      Choice : Write_Alternative := Uncertain;
   begin
      Value := CCL.VM.Integer_Constant (0);
      Accepted := Ref in Declared_Type;
      if not Accepted then return; end if;
      if Transport_Valid then
         if Code = Config_Object_Messages.Success and Revision in 1 .. Config_Object_Messages.Maximum_Revision then
            Choice := Committed;
         elsif Revision = 0 then
            case Code is
               when Config_Object_Messages.Invalid_Request => Choice := Invalid_Request;
               when Config_Object_Messages.Denied => Choice := Denied;
               when Config_Object_Messages.Busy => Choice := Busy;
               when Config_Object_Messages.Unavailable => Choice := Unavailable;
               when Config_Object_Messages.Conflict => Choice := Conflict;
               when Config_Object_Messages.Rejected => Choice := Rejected;
               when Config_Object_Messages.Uncertain => Choice := Uncertain;
               when others => null;
            end case;
         end if;
      end if;
      Value := (Kind => CCL.VM.Variant_Value, Data_Type => Ref,
        Alternative => Write_Alternative'Enum_Rep (Choice),
        Integer => (if Choice = Committed then Interfaces.Integer_64 (Revision) else 0), others => <>);
   end To_VM;
   procedure To_Host
     (Transport_Valid : Boolean; Code : Config_Object_Messages.Status;
      Revision : Interfaces.Unsigned_64; Reply : out CCL.Host_Values.Call_Result)
   is
      Value : CCL.VM.Value;
      Image : CCL.Objects.Image;
      Accepted : Boolean;
   begin
      Reply := (others => <>);
      To_VM (Types, Transport_Valid, Code, Revision, Value, Accepted);
      if not Accepted then return; end if;
      CCL.Objects.Values.From_VM (Contract, Types, Value, Image, Accepted);
      if Accepted then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True);
      end if;
   end To_Host;
end Config_Object_Outcomes;
