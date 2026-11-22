with Interfaces; use Interfaces;

package User_Buffer_Copy with SPARK_Mode, Pure is
   Page_Size : constant Unsigned_64 := 4096;
   User_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;
   function Valid_Range (Source, Length : Unsigned_64) return Boolean is
     (Source > 0 and then Source < User_Limit and then Length <= User_Limit - Source);

   generic
      -- Destination is kernel-owned. A callback reads only within one page,
      -- and must retain the physical source until that read completes (a pin
      -- for reclaimable frames, or an established permanent reservation).
      with procedure Read_Chunk
        (Page : Unsigned_64; Within_Page : Natural; Destination_Offset : Unsigned_64;
         Count : Positive; Success : out Boolean);
   procedure Copy (Source, Length : Unsigned_64; Success : out Boolean)
     with Post => (if Success then Valid_Range (Source, Length));

   generic
      with procedure Read_Byte
        (Address : Unsigned_64; Value : out Character; Success : out Boolean);
   procedure Copy_Name (Source : Unsigned_64; Name : out String; Success : out Boolean)
     with Post => (if Success and then Name'Length > 0 then Valid_Range (Source, 1));
end User_Buffer_Copy;
