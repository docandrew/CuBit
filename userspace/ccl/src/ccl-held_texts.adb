with Ada.Unchecked_Deallocation;

package body CCL.Held_Texts with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   function Empty return Text is (Ada.Finalization.Controlled with Data => null);

   function To_Text (Item : String) return Text is
     (Ada.Finalization.Controlled with
      Data => (if Item'Length = 0 then null else new String'(Item)));

   function Length (Item : Text) return Natural is (if Item.Data = null then 0 else Item.Data'Length);

   function Image (Item : Text) return String is
   begin
      if Item.Data = null then
         return "";
      end if;
      return Result : constant String (1 .. Item.Data'Length) := Item.Data.all;
   end Image;

   function Slice (Item : Text; First, Last : Natural) return String is
   begin
      if Item.Data = null or else Last < First or else First < 1 or else Last > Item.Data'Length then
         return "";
      end if;
      return Result : constant String (1 .. Last - First + 1) :=
        Item.Data (Item.Data'First + First - 1 .. Item.Data'First + Last - 1);
   end Slice;

   function Element (Item : Text; Index : Positive) return Character is
     (if Item.Data = null or else Index > Item.Data'Length then Character'Val (0)
      else Item.Data (Item.Data'First + Index - 1));

   function "=" (Left, Right : Text) return Boolean is (Image (Left) = Image (Right));

   overriding procedure Adjust (Item : in out Text) is
   begin
      if Item.Data /= null then
         Item.Data := new String'(Item.Data.all);
      end if;
   end Adjust;

   overriding procedure Finalize (Item : in out Text) is
   begin
      Free (Item.Data);
   end Finalize;
end CCL.Held_Texts;
