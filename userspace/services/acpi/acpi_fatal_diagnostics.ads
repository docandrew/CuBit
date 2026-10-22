with AML_Fatal;
package ACPI_Fatal_Diagnostics with SPARK_Mode, Pure is
   -- Copied diagnostic positions only, never namespace or hardware authority.
   type Origin is (Definition_Block, Device_Initializer);
   type Catalog_Identifier is new Natural;
   type Namespace_Position is new Natural;
   type Location (Kind : Origin := Definition_Block) is record
      case Kind is
         when Definition_Block => Table_ID : Catalog_Identifier;
         -- Scope whose initializer was invoked, not the nested fatal opcode's
         -- exact instruction/method location. Zero denotes the namespace root.
         when Device_Initializer => Initializer_Scope : Namespace_Position;
      end case;
   end record;
   type Observation (Present : Boolean := False) is record
      case Present is
         when True => Item : AML_Fatal.Request; Source : Location;
         when False => null;
      end case;
   end record;
   -- Below the existing CuBit.Log_Records.Maximum_Text_Bytes=512. No dependency
   -- on native logging, no I/O and no claim that formatting delivers a record.
   Maximum_Text_Length : constant Positive := 160;
   function Format (Seen : Observation) return String
     with Global => null, Post => Format'Result'Length <= Maximum_Text_Length;
end ACPI_Fatal_Diagnostics;
