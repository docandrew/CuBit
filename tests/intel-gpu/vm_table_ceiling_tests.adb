--  BUG-001 (laptop-15): opening Penny made intel-gpu refuse the desktop's
--  live VM bind for Penny's 1280 x 780 window surface while reserving page
--  tables ("VM update rejected stage= 4 status= 3"), and the desktop died.
--
--  This builds a desktop-shaped VM in the production image instance
--  (Intel_GPU_Application_State.VM, its real table ceiling) the way anv lays
--  out its address space: state pools in separate GiB regions, the high heap
--  allocated downwards from the top of the 48-bit space, 2 MiB alignment for
--  buffers of 1 MiB or more. The buffer list is a model of the desktop's
--  ~82 MiB (three 1920 x 1080 targets, staging, readback, backdrop, atlas,
--  four client windows, glyph strips, batches); the laptop's exact layout
--  is not known. It needs 63 tables, and Penny's surface 2 more: past the
--  former 64-table working size, inside the production ceiling.
--
--  Hosted model of the offline VM image and its growth planner; not the
--  native driver, not Intel hardware.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Application_State;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Refusals;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Table_Ceiling_Tests is
   package S renames Intel_GPU_Application_State;
   package VM renames S.VM;
   package G is new VM.Growth;
   package R renames Intel_GPU_VM_Refusals;
   use type G.Plan_Status, R.Cause;

   KiB : constant := 1024;
   MiB : constant := 1024 * KiB;
   GiB : constant := 1024 * MiB;
   Page : constant := 4 * KiB;
   --  The former working size of Intel_GPU_Application_State.Table_Pages.
   Former_Ceiling : constant := 64;
   --  Offline table backing this model may use (more than it needs).
   Offline_Tables : constant Positive := Positive'Min (S.Table_Pages, 96);

   --  anv's virtual address layout (anv_va.c), raw 48-bit addresses.
   High_Heap_Top : constant Unsigned_64 := 16#F000_0000_0000# - 4 * GiB;
   Window_Bytes : constant Unsigned_64 := 1280 * 780 * 4;
   Target_Bytes : constant Unsigned_64 := 1920 * 1080 * 4;

   Source : VM.Image;
   Mirror_Bytes : constant := (96 - 4) * Page;
   type Mirror_RAM is array (1 .. Mirror_Bytes) of Unsigned_8 with Alignment => 4096;
   Mirrors : Mirror_RAM := [others => 0];
   type Descriptor_RAM is array (1 .. 4096) of Unsigned_8 with Alignment => 4096;
   Descriptors : Descriptor_RAM := [others => 0];
   --  Every data page aliases one DMA page: the planner counts tables, and
   --  same-policy aliases are allowed within one VM.
   Data_DMA : constant Unsigned_64 := 16#4000_0000#;
   Cursor : Unsigned_64 := High_Heap_Top;
   OK : Boolean;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL: " & What);
         raise Program_Error with What;
      end if;
      Checks := Checks + 1;
   end Check;

   procedure Map (GPU, Bytes : Unsigned_64) is
      Data : constant VM.Data_Pages (1 .. Positive ((Bytes + Page - 1) / Page)) :=
        [others => Data_DMA];
   begin
      VM.Map_Pages (Source, GPU, Data, Write_Back, Read_Write, OK);
      Check (OK, "map" & Unsigned_64'Image (GPU) & Unsigned_64'Image (Bytes));
   end Map;

   --  anv's high heap: highest hole first; 2 MiB alignment from 1 MiB,
   --  64 KiB from 64 KiB.
   function Next_High (Bytes : Unsigned_64) return Unsigned_64 is
      Align : constant Unsigned_64 :=
        (if Bytes >= 1 * MiB then 2 * MiB elsif Bytes >= 64 * KiB then 64 * KiB else Page);
      Size : constant Unsigned_64 := (Bytes + Page - 1) / Page * Page;
   begin
      Cursor := (Cursor - Size) / Align * Align;
      return Cursor;
   end Next_High;

   procedure High (Bytes : Unsigned_64; Count : Positive := 1) is
   begin
      for I in 1 .. Count loop
         Map (Next_High (Bytes), Bytes);
      end loop;
   end High;

   procedure Extend_Mirrors is
      Bytes : Unsigned_64 := 0;
   begin
      while Bytes < Unsigned_64 (Offline_Tables - S.Bootstrap_Table_Mirrors) * Page loop
         Bytes := Unsigned_64'Min
           (Bytes + 64 * KiB, Unsigned_64 (Offline_Tables - S.Bootstrap_Table_Mirrors) * Page);
         VM.Extend_Metadata (Source, Unsigned_64 (To_Integer (Mirrors'Address)), Bytes, OK);
         Check (OK, "mirror metadata");
      end loop;
   end Extend_Mirrors;

   function Table_DMA (P : VM.Page_Number) return Unsigned_64 is
     (16#1000_0000# + Unsigned_64 (P) * Page);
   procedure Initialize is new VM.Initialize_From_Pages (Table_DMA);

   Before, Penny : G.Requirements;
   Penny_GPU : Unsigned_64;
begin
   --  Refusal classification: only capacity causes answer CAPACITY.
   Check (R.Classify (Planned => True, Occupied => False, Fits_Ceiling => False) = R.Table_Ceiling,
          "past the ceiling is a table-ceiling refusal");
   Check (R.Classify (True, False, True) = R.Reservation_Refused, "reservation refusal");
   Check (R.Classify (False, True, True) = R.Range_Occupied, "occupied range");
   Check (R.Classify (False, False, False) = R.Range_Invalid, "invalid range");
   Check (R.Is_Capacity (R.Table_Ceiling) and R.Is_Capacity (R.Reservation_Refused) and
          not R.Is_Capacity (R.Range_Occupied) and not R.Is_Capacity (R.Range_Invalid),
          "capacity causes");

   VM.Extend_Descriptors (Source, Unsigned_64 (To_Integer (Descriptors'Address)),
                          Descriptors'Length, OK);
   Check (OK, "descriptor metadata");
   Extend_Mirrors;
   Initialize (Source, Offline_Tables, OK);
   Check (OK, "initialize");

   --  State pools, one region each (anv_va.c): general state, low heap,
   --  binding tables, internal and bindless surface states, indirect
   --  descriptors, shaders, dynamic and dynamic-visible state.
   Map (2 * MiB, 1 * MiB);
   Map (2 * GiB, 64 * KiB);
   Map (3 * GiB, 1 * MiB);
   Map (4 * GiB, 1 * MiB);
   Map (5 * GiB, 1 * MiB);
   Map (8 * GiB, 1 * MiB);
   Map (12 * GiB, 2 * MiB);
   Map (15 * GiB, 1 * MiB);
   Map (16 * GiB, 1 * MiB);
   --  The desktop's buffers in the high heap.
   High (Target_Bytes, 3);          --  output targets
   High (16 * MiB);                 --  upload staging
   High (Target_Bytes);             --  readback
   High (Target_Bytes);             --  backdrop
   High (1 * MiB);                  --  icon atlas
   High (Window_Bytes, 4);          --  four client windows
   High (8 * KiB, 285);             --  glyph strips
   High (2 * MiB, 2);               --  batch and state buffers
   VM.Seal (Source, OK);
   Check (OK, "seal");
   Check (VM.Used (Source) = 63, "the desktop model uses 63 tables:" & Natural'Image (VM.Used (Source)));
   Check (VM.Used (Source) <= Former_Ceiling, "the desktop alone fit the former working size");

   --  An already mapped range is planned as occupied, never as capacity.
   Before := G.Inspect (Source, 16 * GiB, Page);
   Check (Before.Status = G.Occupied, "mapped range is occupied");

   --  Penny's window surface (laptop-15: "surface 6 ... 1280 x 780").
   Penny_GPU := Next_High (Window_Bytes);
   Penny := G.Inspect (Source, Penny_GPU, (Window_Bytes + Page - 1) / Page * Page);
   Check (Penny.Status = G.Ready, "Penny's range is free");
   Check (Penny.Additional_Tables = 2 and Penny.Required_Tables = 65,
          "Penny's surface needs 2 new tables, 65 in all:" &
          Natural'Image (Penny.Additional_Tables) & Natural'Image (Penny.Required_Tables));
   Check (Penny.Required_Tables > Former_Ceiling, "past the former 64-table working size");
   --  The regression: with the production ceiling the bind is admitted
   --  (fails while Table_Pages was 64, the laptop-15 refusal).
   Check (Penny.Fits_Quota, "the production ceiling admits Penny's surface");
   Check (R.Classify (Penny.Status = G.Ready, Penny.Status = G.Occupied, Penny.Fits_Quota) =
          R.Reservation_Refused,
          "a refusal for this plan could only be a reservation refusal, never the ceiling");
   Ada.Text_IO.Put_Line
     ("VM table ceiling PASS:" & Natural'Image (Checks) & " checks; desktop model" &
      Natural'Image (VM.Used (Source)) & " tables, Penny's surface +" &
      Natural'Image (Penny.Additional_Tables) & " within the ceiling of" &
      Natural'Image (S.Table_Pages) & " (hosted model, not hardware)");
end VM_Table_Ceiling_Tests;
