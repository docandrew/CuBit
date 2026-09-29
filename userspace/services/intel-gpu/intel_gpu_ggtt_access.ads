with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power;
package Intel_GPU_GGTT_Access with SPARK_Mode is
   Write_Request_Label : constant := 16#0233#;
   Write_Slot : constant Unsigned_64 := 26;
   type Grant_Plan is record
      Valid : Boolean := False;
      Physical, Bytes : Unsigned_64 := 0;
   end record;
   -- Broker-only inputs: Owner_Ready incorporates the designated live owner,
   -- frozen PCI identity, retained reset authorization and inspection grant.
   -- IRQ_Disabled is the completed executor state, not a caller assertion.
   -- Expected values are retained from the original admitted inspection.
   -- This is a grant validator, not authority issuance, a free-space proof,
   -- a power reference, or permission to overwrite arbitrary PTEs.
   function Plan_Write
     (Config : Intel_GPU_PCI_Power.Configuration;
      Expected_BAR, Expected_Table_Bytes : Unsigned_64;
      Owner_Ready, IRQ_Disabled : Boolean) return Grant_Plan
   with Global => null,
     Post => (if Plan_Write'Result.Valid then
       Owner_Ready and then IRQ_Disabled and then
       Plan_Write'Result.Bytes = Expected_Table_Bytes and then
       Plan_Write'Result.Bytes in 2_097_152 | 4_194_304 | 8_388_608 and then
       Plan_Write'Result.Physical > Expected_BAR and then
       Plan_Write'Result.Physical - Expected_BAR = 8_388_608 and then
       Plan_Write'Result.Bytes - 1 <= Unsigned_64'Last - Plan_Write'Result.Physical
       else Plan_Write'Result.Physical = 0 and then Plan_Write'Result.Bytes = 0);
end Intel_GPU_GGTT_Access;
