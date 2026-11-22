with Desktop_Icon_Pixels.Atlases;
with System.Storage_Elements;
package body Desktop_Icon_Mapping with SPARK_Mode => Off is
   procedure Copy_Chunk (Item : Desktop_Icon_Pixels.Asset;
      Mapping : System.Address; Bytes : Compositor_Upload.Byte_Count;
      Plan : Compositor_Upload.Plan; Complete : out Boolean) is
      use type System.Address, System.Storage_Elements.Integer_Address;
   begin
      Complete := False;
      if Mapping = System.Null_Address or else Bytes < 4 or else
        System.Storage_Elements.To_Integer (Mapping) mod Desktop_Icon_Pixels.Pixels'Alignment /= 0
      then return; end if;
      declare
         Target : Desktop_Icon_Pixels.Pixels (0 .. Bytes / 4 - 1)
           with Import, Address => Mapping;
      begin
         Desktop_Icon_Pixels.Copy_Chunk (Item, Target, Plan, Complete);
      end;
   end Copy_Chunk;
   procedure Copy_Atlas (Kind : Desktop_Icon_Pixels.Family;
      Mapping : System.Address; Bytes : Compositor_Upload.Byte_Count;
      Plan : Compositor_Upload.Plan; Complete : out Boolean) is
      use type System.Address, System.Storage_Elements.Integer_Address;
   begin
      Complete := False;
      if Mapping = System.Null_Address or else Bytes < 4 or else
        System.Storage_Elements.To_Integer (Mapping) mod Desktop_Icon_Pixels.Pixels'Alignment /= 0
      then return; end if;
      declare
         Target : Desktop_Icon_Pixels.Pixels (0 .. Bytes / 4 - 1)
           with Import, Address => Mapping;
      begin
         Desktop_Icon_Pixels.Atlases.Copy_Chunk (Kind, Target, Plan, Complete);
      end;
   end Copy_Atlas;
end Desktop_Icon_Mapping;
