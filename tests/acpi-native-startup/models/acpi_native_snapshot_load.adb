pragma Ada_2022;
package body ACPI_Native_Snapshot_Load is
   package R renames L.R;
   -- The owner now uses the controlled child. Reaching the obsolete modeled
   -- entry is a test error; actual native source adapters have a separate suite.
   procedure Load
     (Deadline : R.Tick; Owner : out L.State_Access; Result : out Report) is
      pragma Unreferenced (Deadline, Owner, Result);
   begin raise Program_Error with "uncontrolled owner loader must not execute"; end Load;
   procedure Read_Catalog
     (Diagnostics : in out Report; Item : out L.Inventory; Status : out R.Copy_Status) is
      pragma Unreferenced (Diagnostics, Item, Status);
   begin raise Program_Error with "unmodeled native catalog adapter"; end Read_Catalog;
   procedure Read_Description
     (Diagnostics : in out Report; ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
      pragma Unreferenced (Diagnostics, ID, Item, Status);
   begin raise Program_Error with "unmodeled native descriptor adapter"; end Read_Description;
   procedure Read_Chunk
     (Diagnostics : in out Report; ID : R.S.Table_ID; Offset : Natural;
      Into : out R.F.Bytes; Copied : out R.Byte_Count; Status : out R.Copy_Status) is
      pragma Unreferenced (Diagnostics, ID, Offset, Into, Copied, Status);
   begin raise Program_Error with "unmodeled native chunk adapter"; end Read_Chunk;
   procedure Read_Clock (Now : out R.Tick; Available : out Boolean) is
      pragma Unreferenced (Now, Available);
   begin raise Program_Error with "unmodeled native clock adapter"; end Read_Clock;
end ACPI_Native_Snapshot_Load;
