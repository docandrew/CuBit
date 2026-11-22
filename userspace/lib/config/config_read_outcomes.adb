with CCL.Objects.Catalog;

package body Config_Read_Outcomes with SPARK_Mode is
   use CCL.Types;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type CCL.Objects.Schema_Key;
   use type CCL.Objects.Catalog.Publication_Result;
   package Wire renames Config_Object_Messages;

   procedure Define
     (Value_Type : CCL.Objects.Binding;
      Snapshot_Name, Result_Name : CCL.Types.Name;
      Result_Key : CCL.Objects.Schema_Key;
      Item : out Description; Accepted : out Boolean)
   is
      Catalog : CCL.Objects.Catalog.Schema_Catalog;
      Published : CCL.Objects.Catalog.Publication_Result;
      Types : Registry;
      Root, Snapshot, Result_Root : Type_Reference;
      Definition : CCL.Types.Description;
      Status : Definition_Result;
      Bound : CCL.Objects.Binding;
   begin
      Item := (others => <>); Accepted := False;
      if not CCL.Objects.Is_Bound (Value_Type) or else
        Result_Key = CCL.Objects.No_Schema or else Result_Key = CCL.Objects.Identity (Value_Type)
      then return; end if;
      CCL.Objects.Catalog.Publish (Catalog, Value_Type, Published);
      if Published /= CCL.Objects.Catalog.Published then return; end if;
      Types := CCL.Objects.Catalog.Visible_Types (Catalog);
      Root := CCL.Objects.Catalog.Root_Of (Catalog, CCL.Objects.Identity (Value_Type));
      Definition := (Identifier => Snapshot_Name, Form => Product, Count => 2, others => <>);
      Definition.Parts (1) := (Named ("revision"), Integer_Type);
      Definition.Parts (2) := (Named ("value"), Root);
      CCL.Types.Define (Types, Definition, Snapshot, Status);
      if Status /= Defined then return; end if;
      Definition := (Identifier => Result_Name, Form => Sum,
        Count => Alternative'Pos (Alternative'Last) + 1, others => <>);
      for Choice in Alternative loop
         Definition.Parts (Alternative'Enum_Rep (Choice)) :=
           (Named (Name (Choice)), (if Choice in Found | Stale then Snapshot else Unit_Type));
      end loop;
      CCL.Types.Define (Types, Definition, Result_Root, Status);
      if Status /= Defined then return; end if;
      CCL.Objects.Bind (Types, Result_Root, Result_Key, Bound, Accepted);
      if Accepted then Item := (Value_Type, Bound, True); end if;
   end Define;

   function Matches (Item : Description; Value_Type : CCL.Objects.Binding) return Boolean is
     (Item.Defined and then CCL.Objects.Same_Schema (Item.Value_Type, Value_Type));

   procedure Build
     (Item : Description; Transport_Valid : Boolean;
      Code : Wire.Status; Revision : Interfaces.Unsigned_64;
      Value : CCL.Objects.Image;
      Output : out CCL.Objects.Image; Accepted : out Boolean)
   is
      use type Wire.Status;
      Choice : Alternative := Invalid_Completion;
   begin
      Output := CCL.Objects.Empty (Item.Result_Type); Accepted := False;
      if not Item.Defined then return; end if;
      if Transport_Valid then
         if Code in Wire.Success | Wire.Stale then
            if Revision in 1 .. Wire.Maximum_Revision and then
              CCL.Objects.Validate (Value, Item.Value_Type) and then
              Value.Used_Cells <= CCL.Objects.Maximum_Cells - 3
            then Choice := (if Code = Wire.Success then Found else Stale); end if;
         elsif Revision = 0 then
            Choice := (case Code is
              when Wire.Missing => Missing, when Wire.Denied => Denied,
              when Wire.Busy => Busy, when Wire.Unavailable => Unavailable,
              when Wire.Schema_Mismatch => Schema_Mismatch,
              when Wire.Invalid_Request => Invalid_Request,
              when others => Invalid_Completion);
         end if;
      end if;
      Output.Cells (1) := CCL.Objects.Variant_Cell (Alternative'Enum_Rep (Choice));
      if Choice in Found | Stale then
         Output.Cells (2) := CCL.Objects.Product_Cell (2);
         Output.Cells (3) := CCL.Objects.Integer_Cell (Interfaces.Integer_64 (Revision));
         Output.Used_Cells := Value.Used_Cells + 3;
         Output.Cells (4 .. 3 + Natural (Value.Used_Cells)) :=
           Value.Cells (1 .. Natural (Value.Used_Cells));
         -- String offsets are relative to the object's text block, not its
         -- cell positions. The envelope adds no text; nested offsets remain
         -- exact, including empty strings and a completely full text block.
         Output.Used_Bytes := Value.Used_Bytes;
         Output.Text := Value.Text;
      else
         Output.Used_Cells := 2;
         Output.Cells (2) := CCL.Objects.Unit_Cell;
      end if;
      Accepted := CCL.Objects.Validate (Output, Item.Result_Type);
   end Build;
end Config_Read_Outcomes;
