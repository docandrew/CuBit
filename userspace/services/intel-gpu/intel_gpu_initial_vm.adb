package body Intel_GPU_Initial_VM with SPARK_Mode is
   function Build
     (GPU_Base : Unsigned_64; Tables : Table_Pages; Data : Data_Pages;
      Policy : Cache_Policy := Write_Back; Access_Mode : Page_Access := Read_Write)
      return Plan is
      Result : Plan;
      Path : constant Walk := Locate (GPU_Base);
   begin
      if not Admissible (GPU_Base, Tables, Data, Access_Mode) then return Result; end if;
      Result.Entries (Root) (Path.PML4) := Encode_Directory (Tables (Pointer_Directory));
      Result.Entries (Pointer_Directory) (Path.PDP) := Encode_Directory (Tables (Directory));
      Result.Entries (Directory) (Path.PD) := Encode_Directory (Tables (Leaves));
      for Index in Data'Range loop
         Result.Entries (Leaves) (Index) := Encode_Leaf (Data (Index), Policy, Access_Mode);
         pragma Loop_Invariant
           (for all I in Table_Index'First .. Index =>
              Result.Entries (Leaves) (I) = Encode_Leaf (Data (I), Policy, Access_Mode));
      end loop;
      Result.Root_DMA := Tables (Root);
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_Initial_VM;
