with Interfaces;
with Intel_GPU_ADLN_PPGTT;
package Intel_GPU_Initial_VM with SPARK_Mode is
   use Interfaces;
   use Intel_GPU_ADLN_PPGTT;
   type Level is (Root, Pointer_Directory, Directory, Leaves);
   type Table_Pages is array (Level) of Unsigned_64;
   -- Index selects the GPU page within this2MiB window; zero leaves a hole.
   -- Nonzero values are authorized retained DMA pages, not CPU addresses.
   type Data_Pages is array (Table_Index) of Unsigned_64;
   type Page_Table is array (Table_Index) of Unsigned_64;
   type Table_Set is array (Level) of Page_Table;
   type Plan is record
      Valid : Boolean := False;
      Root_DMA : Unsigned_64 := 0;
      Entries : Table_Set := [others => [others => 0]];
   end record;
   function Admissible
     (GPU_Base : Unsigned_64; Tables : Table_Pages; Data : Data_Pages;
      Access_Mode : Page_Access) return Boolean is
     (GPU_Base < 2 ** 48 and then GPU_Base mod (2 ** 21) = 0 and then
      (for some Page of Data => Page /= 0) and then Access_Mode = Read_Write and then
      (for all L in Level => Valid_DMA_Page (Tables (L)) and then
        (for all Other in Level => L = Other or else Tables (L) /= Tables (Other))) and then
      (for all Page of Data => Page = 0 or else
        (Valid_DMA_Page (Page) and then
         (for all L in Level => Page /= Tables (L)))));
   -- Offline initial rendering VM: one2MiB-aligned window of up to512 4KiB
   -- sparse pages in a four-level tree. Zero leaves remain nonpresent. This is
   -- not the eventual general VA allocator or a live bind/unbind operation.
   -- Callers must authorize/retain every supplied DMA page and establish
   -- cache visibility before context registration. No addresses come from IPC.
   -- Directory pages must be distinct and never appear as rendering data.
   function Build
     (GPU_Base : Unsigned_64; Tables : Table_Pages; Data : Data_Pages;
      Policy : Cache_Policy := Write_Back; Access_Mode : Page_Access := Read_Write)
      return Plan
   with Global => null,
     Post => Build'Result.Valid = Admissible (GPU_Base, Tables, Data, Access_Mode) and then
       (if Build'Result.Valid then Build'Result.Root_DMA = Tables (Root) and then
          (for all I in Table_Index => Build'Result.Entries (Leaves) (I) =
             Encode_Leaf (Data (I), Policy, Access_Mode))
              else Build'Result.Root_DMA = 0 and then
                (for all L in Level =>
                  (for all I in Table_Index => Build'Result.Entries (L) (I) = 0)));
end Intel_GPU_Initial_VM;
