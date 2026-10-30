with AML_Decode;
package AML_Mutexes with SPARK_Mode, Pure is
   type Sync_Level is range 0 .. 15;
   Canonical_Flags_Last : constant AML_Decode.Byte := AML_Decode.Byte (Sync_Level'Last);
   Declaration_Opcode : constant AML_Decode.Byte := 16#01#;
   Acquire_Opcode : constant AML_Decode.Byte := 16#23#;
   Release_Opcode : constant AML_Decode.Byte := 16#27#;
   type Operation is (Acquire_Mutex, Release_Mutex);
   type Timeout is range 0 .. 65_535;
   Timeout_Bytes : constant := 2;
   Object_Type_Code : constant := 9;
   function Canonical_Flags (Flags : AML_Decode.Byte) return Boolean is
     (Natural (Flags) <= Natural (Canonical_Flags_Last));
end AML_Mutexes;
