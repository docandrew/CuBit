pragma Ada_2022;
with CuBit.Child_Specs;
with Child_Parameters;

--  A device-class launch's typed check (SUP-001), when procmgr starts the
--  driver: the plan's Device_Class_Launch value (CCL.Configurations
--  Class_Block) against the driver's own parameter signature and the
--  supervisor's facts, by CCL's type checker (CCL.Typed_Values), then the
--  merge (Child_Parameters). The merged record is what procmgr then checks
--  against the driver's full signature, as for any typed parameters.
package Class_Launch_Checks is
   package CS renames CuBit.Child_Specs;

   type Outcome is
     (Merged,
      Schema_Refused,      --  the signature has no root record, or a fact is no field of it
      Not_Typed,           --  the value is not a Device_Class_Launch of the driver's policy
      Facts_Differ,        --  its facts are not exactly the supervisor's
      Too_Long);           --  the merged record does not fit one value

   Maximum_Message : constant := 200;
   type Result is record
      Status : Outcome := Not_Typed;
      Merged_Value : String (1 .. Child_Parameters.Maximum_Merged_Bytes) := [others => ' '];
      Length : Natural range 0 .. Child_Parameters.Maximum_Merged_Bytes := 0;
      Message : String (1 .. Maximum_Message) := [others => ' '];
      Message_Length : Natural range 0 .. Maximum_Message := 0;
   end record;

   --  Signature, Root: the driver's .cubit.parameters (canonical
   --  declarations, its root record's name). Launch: the plan's value.
   procedure Check
     (Signature, Root, Launch : String;
      Facts : CS.Fact_Array; Fact_Total : CS.Fact_Count;
      Item : out Result)
   with Pre => Signature'Length <= Child_Parameters.Maximum_Signature_Bytes
               and then Signature'Last < Positive'Last
               and then Root'Length in 1 .. Child_Parameters.Maximum_Root_Bytes;
end Class_Launch_Checks;
