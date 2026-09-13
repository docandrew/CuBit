package body Copy_Proof with SPARK_Mode is
   procedure Exercise_Copy (Source, Length : Unsigned_64; Allowed : Boolean; Success : out Boolean) is
      procedure Read_Chunk
        (Page : Unsigned_64; Within_Page : Natural; Destination_Offset : Unsigned_64;
         Count : Positive; Success : out Boolean)
      is
         pragma Unreferenced (Page, Within_Page, Destination_Offset, Count);
      begin
         Success := Allowed;
      end Read_Chunk;
      procedure Copy is new User_Buffer_Copy.Copy (Read_Chunk);
   begin
      Copy (Source, Length, Success);
   end Exercise_Copy;
   function Exercise_Walk (Root, Address, Physical_Last, Word : Unsigned_64) return Unsigned_64 is
      procedure Read_Entry
        (Table_Frame : Unsigned_64; Index : User_Page_Walk.Table_Index; Value : out Unsigned_64)
      is
         pragma Unreferenced (Table_Frame, Index);
      begin
         Value := Word;
      end Read_Entry;
      function Walk is new User_Page_Walk.Readable_Frame (Read_Entry);
   begin
      return Walk (Root, Address, Physical_Last);
   end Exercise_Walk;
   procedure Exercise_Name
     (Source : Unsigned_64; Byte : Character; Allowed : Boolean;
      Name : out String; Success : out Boolean)
   is
      procedure Read_Byte (Address : Unsigned_64; Value : out Character; Success : out Boolean) is
         pragma Unreferenced (Address);
      begin
         Value := Byte;
         Success := Allowed;
      end Read_Byte;
      procedure Copy_Name is new User_Buffer_Copy.Copy_Name (Read_Byte);
   begin
      Copy_Name (Source, Name, Success);
   end Exercise_Name;
end Copy_Proof;
