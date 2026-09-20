package body CCL.Types.Encoding with SPARK_Mode is
   use Interfaces;
   type Wire_Shape is (Wire_Product, Wire_Sum);
   for Wire_Shape use (Wire_Product => 1, Wire_Sum => 2);
   subtype Name_Offset is Natural range 0 .. Definition_Size - Name_Size;
   function Encode (Item : Description) return Bytes is
      Data : Bytes := [others => 0];
      procedure Put_Name (Position : Name_Offset; Value : Name) is
      begin
         Data (Position) := Unsigned_8 (Value.Length);
         for I in 1 .. Value.Length loop
            Data (Position + I) := Character'Pos (Value.Data (I));
         end loop;
      end Put_Name;
   begin
      Put_Name (0, Item.Identifier);
      Data (Shape_Offset) := (case Item.Form is
        when Product => Wire_Shape'Enum_Rep (Wire_Product),
        when Sum => Wire_Shape'Enum_Rep (Wire_Sum), when Primitive => 0);
      Data (Count_Offset) := Unsigned_8 (Item.Count);
      for I in 1 .. Item.Count loop
         Put_Name (Parts_Offset + (I - 1) * Part_Size, Item.Parts (I).Identifier);
         Data (Parts_Offset + (I - 1) * Part_Size + Name_Size) := Unsigned_8 (Item.Parts (I).Payload);
      end loop;
      return Data;
   end Encode;

   procedure Decode (Data : Bytes; Item : out Description; Valid : out Boolean) is
      D : Description;
      Good : Boolean := True;
      procedure Get_Name (Position : Name_Offset; Value : out Name) is
      begin
         Value := (others => <>);
         if Data (Position) > Maximum_Name_Length then Good := False; return; end if;
         Value.Length := Natural (Data (Position));
         for I in 1 .. Maximum_Name_Length loop
            if I <= Value.Length then Value.Data (I) := Character'Val (Data (Position + I));
            elsif Data (Position + I) /= 0 then Good := False;
            end if;
         end loop;
      end Get_Name;
   begin
      Item := (others => <>); Valid := False;
      if Data (Reserved_Offset) /= 0 or else Data (Count_Offset) > Maximum_Components then return; end if;
      case Data (Shape_Offset) is
         when Wire_Shape'Enum_Rep (Wire_Product) => D.Form := Product;
         when Wire_Shape'Enum_Rep (Wire_Sum) => D.Form := Sum;
         when others => return;
      end case;
      D.Count := Natural (Data (Count_Offset));
      Get_Name (0, D.Identifier);
      for I in Component_Index loop
         if I <= D.Count then
            Get_Name (Parts_Offset + (I - 1) * Part_Size, D.Parts (I).Identifier);
            if Data (Parts_Offset + (I - 1) * Part_Size + Name_Size) > Unsigned_8 (Type_Reference'Last) then
               Good := False;
            else
               D.Parts (I).Payload := Type_Reference (Data (Parts_Offset + (I - 1) * Part_Size + Name_Size));
            end if;
         else
            for B in 0 .. Part_Size - 1 loop
               if Data (Parts_Offset + (I - 1) * Part_Size + B) /= 0 then Good := False; end if;
            end loop;
         end if;
      end loop;
      if Good then Item := D; Valid := True; end if;
   end Decode;
end CCL.Types.Encoding;
