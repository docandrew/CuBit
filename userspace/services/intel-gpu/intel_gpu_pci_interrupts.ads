with Intel_GPU_PCI_Power;
with Interfaces;
package Intel_GPU_PCI_Interrupts with SPARK_Mode is
   Disable_Request_Label : constant := 16#0232#;
   type Snapshot is record
      Valid : Boolean := False;
      INTx_Disabled, MSI_Present, MSI_Enabled : Boolean := False;
      MSIX_Present, MSIX_Enabled, MSIX_Masked : Boolean := False;
   end record;
   -- Read-only Type-0 configuration snapshot. Valid describes the capability
   -- walk and recognized record bounds, NOT interrupt quiescence or ownership.
   -- No GPU source masks, pending vectors, MSI-X table or IRQ routing inspected.
   function Decode (Data : Intel_GPU_PCI_Power.Configuration) return Snapshot
     with Global => null;
   type Disable_Plan is private;
   function Plan_Disable (Data : Intel_GPU_PCI_Power.Configuration)
     return Disable_Plan with Global => null;
   function Valid (Plan : Disable_Plan) return Boolean;
   function Count (Plan : Disable_Plan) return Natural;
   function Offset (Plan : Disable_Plan; Index : Positive) return Natural
     with Pre => Valid (Plan) and then Index <= Count (Plan);
   function Before (Plan : Disable_Plan; Index : Positive) return Interfaces.Unsigned_16
     with Pre => Valid (Plan) and then Index <= Count (Plan);
   function After (Plan : Disable_Plan; Index : Positive) return Interfaces.Unsigned_16
     with Pre => Valid (Plan) and then Index <= Count (Plan);
   -- At most three 16-bit writes: command.INTx-disable, MSI.enable clear,
   -- MSI-X.enable clear/function-mask set. Never write the adjacent PCI status
   -- word or MSI address/data/table. Already-correct fields are omitted.
   -- A plan is not authority: native caller must recheck identity/D0/ownership,
   -- capability layout and baseline under serialization, use word writes and
   -- verify completion. Disabling delivery does not drain in-flight interrupts.
   -- Compact bootstrap observation, never an authorization token.
   function Encoding_Valid (Bits : Interfaces.Unsigned_8) return Boolean;
   function Pack (Value : Snapshot) return Interfaces.Unsigned_8
     with Post => Encoding_Valid (Pack'Result);
   function Unpack (Bits : Interfaces.Unsigned_8) return Snapshot
     with Pre => Encoding_Valid (Bits);
private
   type Word_Change is record
      Address : Natural range 0 .. 254 := 0;
      Prior, Proposed : Interfaces.Unsigned_16 := 0;
   end record;
   type Changes is array (Positive range 1 .. 3) of Word_Change;
   type Disable_Plan is record
      Ready : Boolean := False;
      Used : Natural range 0 .. 3 := 0;
      Words : Changes;
   end record;
end Intel_GPU_PCI_Interrupts;
