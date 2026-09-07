with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.File_Access; use CuBit.File_Access;

procedure Main is
   Item : Policy;
   Data : Wire_Bytes (1 .. 2 * Wire_Entry_Bytes) := [others => 0];
   OK : Boolean;
   Read_Only : constant Rights_Set := [Read_Objects => True, others => False];
   Write_Only : constant Rights_Set := [Write_Objects => True, others => False];
   Read_Write : constant Rights_Set :=
     [Read_Objects | Write_Objects => True, others => False];

   procedure Set_Entry (Number : Positive; Prefix : String; Rights : Unsigned_8) is
      Base : constant Positive := (Number - 1) * Wire_Entry_Bytes + 1;
   begin
      Data (Base .. Base + Wire_Entry_Bytes - 1) := [others => 0];
      Data (Base) := Rights;
      Data (Base + 1) := Prefix'Length;
      for Offset in 1 .. Prefix'Length loop
         Data (Base + 7 + Offset) :=
           Character'Pos (Prefix (Prefix'First + Offset - 1));
      end loop;
   end Set_Entry;
begin
   pragma Assert (not Allows (Item, "anything", Read_Only));
   Set_Entry (1, "@nvme:0/documents", 1);
   Set_Entry (2, "@nvme:0/projects/", 3);
   Decode (Data, Item, OK);
   pragma Assert (OK);
   pragma Assert (Allows (Item, "@nvme:0/documents", Read_Only));
   pragma Assert (Allows (Item, "@nvme:0/documents/a", Read_Only));
   pragma Assert (not Allows (Item, "@nvme:0/documents/a", Write_Only));
   pragma Assert (not Allows (Item, "@nvme:0/documents-private/a", Read_Only));
   pragma Assert (Allows (Item, "@nvme:0/projects/a", Read_Write));
   pragma Assert (not Allows (Item, "@nvme:0/projects/../secret", Read_Only));
   pragma Assert (not Allows (Item, "@nvme:0/projects/a" & ASCII.NUL, Read_Only));
   pragma Assert (not Allows (Item, "@mem:0/projects/a", Read_Only));
   pragma Assert (not Allows (Item, "@nvme:0/projects/a", No_Rights));

   --  Two separate scope entries cannot be combined to synthesize a stronger
   --  handle: one matching entry must grant every requested right.
   Set_Entry (1, "same", 1);
   Set_Entry (2, "same", 2);
   Decode (Data, Item, OK);
   pragma Assert (OK and then not Allows (Item, "same", Read_Write));
   for Raw in Unsigned_8 loop
      Set_Entry (1, "scope", Raw);
      Decode (Data (1 .. Wire_Entry_Bytes), Item, OK);
      pragma Assert (OK = (Raw <= 15));
      if OK then
         for Requested in Unsigned_8 range 1 .. 15 loop
            pragma Assert
              (Allows (Item, "scope/child", Rights_From_Wire (Requested)) =
               ((Raw and Requested) = Requested));
         end loop;
      else
         pragma Assert (not Allows (Item, "scope/child", Read_Only));
      end if;
   end loop;
   Set_Entry (1, "valid", 15);
   Set_Entry (2, "bad", 15);
   Data (Wire_Entry_Bytes + 2) := 65;
   Decode (Data, Item, OK);
   pragma Assert (not OK and then not Allows (Item, "valid", Read_Only));
   Set_Entry (2, "../bad", 15);
   Decode (Data, Item, OK);
   pragma Assert (not OK);
   Set_Entry (2, "bad" & ASCII.NUL, 15);
   Decode (Data, Item, OK);
   pragma Assert (not OK);
   Set_Entry (2, "valid", 15);
   Data (3) := 1;
   Decode (Data, Item, OK);
   pragma Assert (not OK);
   Decode (Data (1 .. 0), Item, OK);
   pragma Assert (not OK);
   Decode (Data (1 .. 71), Item, OK);
   pragma Assert (not OK);
   Allow_All_For_Bootstrap (Item);
   pragma Assert (Allows (Item, "anywhere", All_Rights));
   Clear (Item);
   pragma Assert (not Allows (Item, "anywhere", All_Rights));
   declare
      Shifted_Scope : constant String (30 .. 33) := "work";
      Shifted_Name : constant String (60 .. 69) := "work/child";
   begin
      pragma Assert (Scope_Matches (Shifted_Scope, Shifted_Name));
      pragma Assert (not Valid_Path (".."));
      pragma Assert (not Valid_Path ("a/.."));
      pragma Assert (not Valid_Path ("a/../"));
      pragma Assert (Valid_Path ("a/.hidden/.../b"));
   end;
   declare
      Extreme : constant String (Integer'Last .. Integer'Last) := "x";
   begin
      pragma Assert (Scope_Matches ("x", Extreme));
      pragma Assert (Scope_Matches (Extreme, "x"));
      pragma Assert (Valid_Path (Extreme));
   end;
   Put_Line ("file access: scope boundaries, rights subsets, atomic decode PASS");
end Main;
