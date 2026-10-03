with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Table_Provenance.IO is
   type Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
   function Resolve
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Table : Positive;
      Expected_DMA : Unsigned_64) return Mapping
   is
      Item : Mapping;
   begin
      if not Exclusive then return (others => 0); end if;
      Item := Accessor.Lookup (Object, Session, Expected_Generation, Table);
      if Item.Ticket = 0 or else Item.DMA /= Expected_DMA or else not Exclusive
      then return (others => 0); end if;
      return Item;
   end Resolve;
   procedure Read_Word
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Table : Positive;
      Expected_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : out Unsigned_64; Accepted : out Boolean)
   is
      Item : constant Mapping := Resolve (Object, Session, Expected_Generation, Table, Expected_DMA);
   begin
      Value := 0; Accepted := False;
      if Item.Ticket = 0 then return; end if;
      declare
         Page : Words with Import, Volatile,
           Address => To_Address (Integer_Address (Item.CPU));
      begin Value := Page (Index); end;
      Accepted := Exclusive;
      if not Accepted then Value := 0; end if;
   end Read_Word;
   procedure Write_Word
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Table : Positive;
      Expected_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : Unsigned_64; Accepted : out Boolean)
   is
      Item : constant Mapping := Resolve (Object, Session, Expected_Generation, Table, Expected_DMA);
   begin
      Accepted := False;
      if Item.Ticket = 0 then return; end if;
      declare
         Page : Words with Import, Volatile,
           Address => To_Address (Integer_Address (Item.CPU));
      begin Page (Index) := Value; end;
      Accepted := Exclusive;
   end Write_Word;
   function Flush
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Table : Positive;
      Expected_DMA : Unsigned_64) return Boolean
   is
      Item : constant Mapping := Resolve (Object, Session, Expected_Generation, Table, Expected_DMA);
   begin
      return Item.Ticket /= 0 and then Flush_CPU_Page (Item.CPU) and then Exclusive;
   end Flush;
end Intel_GPU_Table_Provenance.IO;
