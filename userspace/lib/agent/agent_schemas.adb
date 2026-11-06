pragma Ada_2022;
with Interfaces;

package body Agent_Schemas with SPARK_Mode is
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Shape;
   use type CCL.Types.Default_Kind;

   function Image (Value : Interfaces.Integer_64) return String
   with Post => Image'Result'Length <= 20
   is
      Text : constant String := Interfaces.Integer_64'Image (Value);
   begin
      pragma Assert (Text'Length <= 20);
      return (if Text'Length > 0 and then Text (Text'First) = ' ' then Text (Text'First + 1 .. Text'Last)
              else Text);
   end Image;

   procedure Input_Schema
     (Types : CCL.Types.Registry; Item : Parameter; Label : String;
      Into : out String; Last : out Natural; Result : out Schema_Result)
   is
      Overflow : Boolean := False;
      Bad_Type : Boolean := False;

      procedure Put (Text : String)
      with Pre => Last <= Into'Last and then Into'First = 1, Post => Last <= Into'Last
      is
      begin
         for C of Text loop
            pragma Loop_Invariant (Last <= Into'Last);
            if Last < Into'Last then
               Last := Last + 1;
               Into (Last) := C;
            else
               Overflow := True;
            end if;
         end loop;
      end Put;

      --  A name as a JSON string (names hold no quote or backslash, but
      --  escaping costs nothing).
      procedure Put_Name (Name : String)
      with Pre => Last <= Into'Last and then Into'First = 1, Post => Last <= Into'Last
      is
      begin
         Put ("""");
         for C of Name loop
            pragma Loop_Invariant (Last <= Into'Last);
            if C = '"' or else C = '\' then
               Put ("\" & C);
            elsif C in ' ' .. '~' then
               Put ([1 => C]);
            end if;
         end loop;
         Put ("""");
      end Put_Name;

      procedure Value_Schema (Ref : CCL.Types.Type_Reference; Text_Limit : Natural; Depth : Natural)
      with Pre => Depth <= Maximum_Depth and then Last <= Into'Last and then Into'First = 1,
           Post => Last <= Into'Last,
           Subprogram_Variant => (Decreases => Maximum_Depth - Depth)
      is
         Shape : CCL.Types.Description;
      begin
         if Overflow or else Bad_Type then
            return;
         elsif Ref = CCL.Types.Integer_Type then
            Put ("{""type"":""integer""}");
            return;
         elsif Ref = CCL.Types.Boolean_Type then
            Put ("{""type"":""boolean""}");
            return;
         elsif Ref = CCL.Types.Character_Type then
            Put ("{""type"":""string"",""minLength"":1,""maxLength"":1}");
            return;
         elsif Ref = CCL.Types.String_Type then
            Put ("{""type"":""string"",""maxLength"":" & Image (Interfaces.Integer_64 (Text_Limit)) & "}");
            return;
         elsif Ref = CCL.Types.Unit_Type then
            Put ("{""type"":""null""}");
            return;
         elsif not CCL.Types.Known (Types, Ref) or else Ref not in CCL.Types.Declared_Type
           or else Depth = Maximum_Depth
         then
            Bad_Type := True;
            return;
         end if;
         Shape := CCL.Types.Describe (Types, Ref);
         case Shape.Form is
            when CCL.Types.Bounded =>
               Put ("{""type"":""integer"",""minimum"":" & Image (CCL.Types.Low_Of (Types, Ref)) &
                    ",""maximum"":" & Image (CCL.Types.High_Of (Types, Ref)) & "}");
            when CCL.Types.Product =>
               Put ("{""type"":""object"",""properties"":{");
               for F in 1 .. Shape.Count loop
                  pragma Loop_Invariant (Last <= Into'Last);
                  if F > 1 then
                     Put (",");
                  end if;
                  Put_Name (CCL.Types.Image (Shape.Parts (F).Identifier));
                  Put (":");
                  Value_Schema (Shape.Parts (F).Payload, Typed_Text_Limit, Depth + 1);
               end loop;
               Put ("},""required"":[");
               declare
                  First : Boolean := True;
               begin
                  for F in 1 .. Shape.Count loop
                     pragma Loop_Invariant (Last <= Into'Last);
                     if CCL.Types.Default_Of (Types, Ref, F).Kind = CCL.Types.No_Default then
                        if not First then
                           Put (",");
                        end if;
                        First := False;
                        Put_Name (CCL.Types.Image (Shape.Parts (F).Identifier));
                     end if;
                  end loop;
               end;
               Put ("],""additionalProperties"":false}");
            when CCL.Types.Sum =>
               if CCL.Types.Is_Enumeration (Types, Ref) then
                  Put ("{""type"":""string"",""enum"":[");
                  for A in 1 .. Shape.Count loop
                     pragma Loop_Invariant (Last <= Into'Last);
                     if A > 1 then
                        Put (",");
                     end if;
                     Put_Name (CCL.Types.Image (Shape.Parts (A).Identifier));
                  end loop;
                  Put ("]}");
               else
                  Put ("{""type"":""object"",""properties"":{");
                  for A in 1 .. Shape.Count loop
                     pragma Loop_Invariant (Last <= Into'Last);
                     if A > 1 then
                        Put (",");
                     end if;
                     Put_Name (CCL.Types.Image (Shape.Parts (A).Identifier));
                     Put (":");
                     if Shape.Parts (A).Payload = CCL.Types.Invalid_Type then
                        Put ("{""type"":""null""}");
                     else
                        Value_Schema (Shape.Parts (A).Payload, Typed_Text_Limit, Depth + 1);
                     end if;
                  end loop;
                  Put ("},""minProperties"":1,""maxProperties"":1,""additionalProperties"":false}");
               end if;
            when CCL.Types.Sequence =>
               Put ("{""type"":""array"",""items"":");
               Value_Schema (CCL.Types.Element_Of (Types, Ref), Typed_Text_Limit, Depth + 1);
               Put ("}");
            when others =>
               Bad_Type := True;
         end case;
      end Value_Schema;
   begin
      Into := [others => ' '];
      Last := 0;
      case Item.Kind is
         when No_Parameter =>
            Put ("{""type"":""object"",""properties"":{},""additionalProperties"":false}");
         when Integer_Parameter | Boolean_Parameter | Text_Parameter =>
            Put ("{""type"":""object"",""properties"":{");
            Put_Name (Label);
            Put (":");
            Value_Schema
              ((case Item.Kind is
                  when Integer_Parameter => CCL.Types.Integer_Type,
                  when Boolean_Parameter => CCL.Types.Boolean_Type,
                  when others => CCL.Types.String_Type), Item.Text_Limit, 0);
            Put ("},""required"":[");
            Put_Name (Label);
            Put ("],""additionalProperties"":false}");
         when Typed_Parameter =>
            if Item.Root in CCL.Types.Declared_Type and then CCL.Types.Known (Types, Item.Root)
              and then CCL.Types.Describe (Types, Item.Root).Form = CCL.Types.Product
            then
               Value_Schema (Item.Root, Typed_Text_Limit, 0);
            else
               --  Not a record: one property, as for a scalar.
               Put ("{""type"":""object"",""properties"":{");
               Put_Name (Label);
               Put (":");
               Value_Schema (Item.Root, Typed_Text_Limit, 0);
               Put ("},""required"":[");
               Put_Name (Label);
               Put ("],""additionalProperties"":false}");
            end if;
      end case;
      Result := (if Bad_Type then Unsupported elsif Overflow then Too_Large else Generated);
   end Input_Schema;
end Agent_Schemas;
