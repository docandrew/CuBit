pragma Ada_2022;
package body HID_Field_Usages with SPARK_Mode is
   use type Schema.Status;
   use type Schema.Bit_Count;
   use type Schema.Count;
   use type Schema.Wide;
   Usage_Page_Base : constant Schema.Word := 2 ** 16;
   Constant_Flag : constant Schema.Word := 1;
   Variable_Flag : constant Schema.Word := 2;
   procedure Resolve
     (Layout : Schema.Result; Field : Schema.Field_Span;
      Usages : Schema.Usage_Table; Element : Schema.Bit_Count;
      Usage : out Schema.Word; Outcome : out Status)
   is
      First, Last, Index, Offset : Natural;
      Remaining : Schema.Wide := Schema.Wide (Element);
      Length : Schema.Wide;
      Selected, Final_Usage : Schema.Word := 0;
      Found : Boolean := False;
      Term : Schema.Usage_Term;
   begin
      Usage := 0; Outcome := Invalid_Schema;
      if Layout.Outcome /= Schema.Decoded then return; end if;
      Outcome := Invalid_Field;
      if not Layout.Present (Field.Kind, Field.ID) or else Field.Size = 0 or else
         Schema.Wide (Field.Offset) + Schema.Wide (Field.Size) * Schema.Wide (Field.Elements) >
           Schema.Wide (Layout.Bits (Field.Kind, Field.ID)) then return; end if;
      if Element >= Field.Elements then Outcome := Element_Out_Of_Range; return; end if;
      if (Field.Flags and Constant_Flag) /= 0 then Outcome := No_Usage; return; end if;
      if (Field.Flags and Variable_Flag) = 0 then Outcome := Array_Field; return; end if;
      Outcome := Invalid_Usage_Table;
      if Field.Usage_Terms = 0 then
         if Field.Usage_First = 0 then Outcome := No_Usage; end if;
         return;
      end if;
      if Field.Usage_First = 0 or else
         Schema.Wide (Field.Usage_First) + Schema.Wide (Field.Usage_Terms) - 1 >
           Schema.Wide (Layout.Usages_Required) then return; end if;
      First := Natural (Field.Usage_First); Last := First + Natural (Field.Usage_Terms) - 1;
      if Schema.Wide (Last) > Schema.Wide (Usages'Length) then return; end if;
      for N in First .. Last loop
         Offset := N - 1;
         if Offset > Natural'Last - Usages'First then return; end if;
         Index := Usages'First + Offset;
         if Index not in Usages'Range then return; end if;
         Term := Usages (Index);
         if Term.Uses_Current_Page or else Term.First > Term.Last or else
            Term.First / Usage_Page_Base /= Term.Last / Usage_Page_Base then return; end if;
         Length := Schema.Wide (Term.Last) - Schema.Wide (Term.First) + 1;
         if not Found then
            if Remaining < Length then Selected := Term.First + Schema.Word (Remaining); Found := True;
            else Remaining := Remaining - Length; end if;
         end if;
         Final_Usage := Term.Last;
      end loop;
      Usage := (if Found then Selected else Final_Usage); Outcome := Resolved;
   end Resolve;
end HID_Field_Usages;
