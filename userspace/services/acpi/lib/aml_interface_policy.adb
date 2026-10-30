pragma Ada_2022;
package body AML_Interface_Policy with SPARK_Mode is
   use type AML_Decode.Byte;
   function Count (Item : Policy) return Interface_Count is (Item.Used);
   function Supports (Item : Policy; Name : AML_Decode.Bytes) return Boolean is
   begin
      if Name'Length = 0 or else Name'Length > Max_Interface_Bytes then return False; end if;
      for I in 1 .. Item.Used loop
         if Item.Items (I).Length = Name'Length
           and then (for all J in 1 .. Name'Length =>
             Item.Items (I).Data (J) = Name (Name'First + (J - 1))) then
            return True;
         end if;
      end loop;
      return False;
   end Supports;
   procedure Add (Item : in out Policy; Name : AML_Decode.Bytes;
                  Status : out Add_Status) is
   begin
      Status := Invalid_Name;
      if Name'Length = 0 or else Name'Length > Max_Interface_Bytes
        or else (for some B of Name => B = 0) then return; end if;
      if Supports (Item, Name) then Status := Duplicate; return; end if;
      if Item.Used = Max_Interfaces then Status := Full; return; end if;
      Item.Used := Item.Used + 1;
      Item.Items (Item.Used).Length := Name'Length;
      for J in 1 .. Name'Length loop
         Item.Items (Item.Used).Data (J) := Name (Name'First + (J - 1));
      end loop;
      Status := Added;
   end Add;
end AML_Interface_Policy;
