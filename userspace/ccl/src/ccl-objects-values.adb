with CCL.Types.Correspondence;

package body CCL.Objects.Values with SPARK_Mode is
   use CCL.Types;

   procedure From_Host
     (Contract : Binding; Value : CCL.Host_Values.Value;
      Object : out Image; Accepted : out Boolean) is
      Result : Build_Result;
   begin
      Object := Empty (Contract);
      Accepted := False;
      case Value.Kind is
         when CCL.Host_Values.Integer_Value =>
            if Contract.Root /= Integer_Type then return; end if;
            Append (Object, Integer_Cell (Value.Integer), Result);
         when CCL.Host_Values.Boolean_Value =>
            if Contract.Root /= Boolean_Type then return; end if;
            Append (Object, Boolean_Cell (Value.Boolean), Result);
         when CCL.Host_Values.Text_Value =>
            if Contract.Root /= String_Type then return; end if;
            Append_Text (Object, Value.Content.Data (1 .. Value.Content.Length), Result);
         when CCL.Host_Values.Handler_Value | CCL.Host_Values.Resource_Value => return;
         when CCL.Host_Values.Object_Value =>
            if not Validate (Value.Object, Contract) then return; end if;
            Object := Value.Object;
            Accepted := True;
            return;
      end case;
      Accepted := Result = Added and then Validate (Object, Contract);
   end From_Host;

   function To_Host (Contract : Binding; Object : Image) return Host_Result is
      Text : CCL.Host_Values.Text;
   begin
      if not Validate (Object, Contract) then return (Available => False); end if;
      case Contract.Root is
         when Integer_Type => return (True, CCL.Host_Values.Integer_Constant (Integer_Of (Object.Cells (1))));
         when Boolean_Type => return (True, CCL.Host_Values.Boolean_Constant (Object.Cells (1).First = 1));
         when String_Type =>
            if Object.Cells (1).Second > CCL.Host_Values.Maximum_Text_Length then
               return (True, CCL.Host_Values.Object_Constant (Object));
            end if;
            Text.Length := Natural (Object.Cells (1).Second);
            Text.Data (1 .. Text.Length) := Object.Text (1 .. Text.Length);
            return (True, CCL.Host_Values.Text_Constant (Text));
         when others => return (True, CCL.Host_Values.Object_Constant (Object));
      end case;
   end To_Host;

   procedure From_VM
     (Contract : Binding; Local_Types : CCL.Types.Registry; Value : CCL.VM.Value;
      Object : out Image; Accepted : out Boolean) is
      Result : Build_Result;
      D : Description;
      Local_Root : constant Type_Reference :=
        CCL.Types.Correspondence.Resolve (Contract.Types, Contract.Root, Local_Types);
   begin
      Object := Empty (Contract);
      Accepted := False;
      if not Is_Bound (Contract) or else Local_Root = Invalid_Type then return; end if;
      --  Do not strip ownership metadata to manufacture unrestricted data.
      if not Value.Copyable or else Value.Type_Tag /= 0 or else
        not CCL.VM.Well_Typed (Local_Types, Value)
      then return; end if;
      case Value.Kind is
         when CCL.VM.Object_Value | CCL.VM.Resource_Value => return;
         -- Objects require their owning native VM store; resources can never
         -- become persistence images.
         when CCL.VM.Integer_Value =>
            if Contract.Root /= Integer_Type or Value.Data_Type /= Invalid_Type then return; end if;
            Append (Object, Integer_Cell (Value.Integer), Result);
         when CCL.VM.Boolean_Value =>
            if Contract.Root /= Boolean_Type or Value.Data_Type /= Invalid_Type then return; end if;
            Append (Object, Boolean_Cell (Value.Boolean), Result);
         when CCL.VM.Variant_Value =>
            if Value.Data_Type /= Local_Root or else
              not Is_Scalar_Sum (Contract.Types, Contract.Root)
            then return; end if;
            D := Describe (Contract.Types, Contract.Root);
            if Value.Alternative > D.Count then return; end if;
            Append (Object, Variant_Cell (Value.Alternative), Result);
            if Result /= Added then return; end if;
            case D.Parts (Value.Alternative).Payload is
               when Integer_Type => Append (Object, Integer_Cell (Value.Integer), Result);
               when Boolean_Type => Append (Object, Boolean_Cell (Value.Boolean), Result);
               when Unit_Type => Append (Object, Unit_Cell, Result);
               when others => return;
            end case;
      end case;
      Accepted := Result = Added and then Validate (Object, Contract);
   end From_VM;

   procedure To_VM
     (Contract : Binding; Local_Types : CCL.Types.Registry; Object : Image;
      Value : out CCL.VM.Value; Accepted : out Boolean) is
      D : Description;
      Local_Root : constant Type_Reference :=
        CCL.Types.Correspondence.Resolve (Contract.Types, Contract.Root, Local_Types);
   begin
      Value := CCL.VM.Integer_Constant (0);
      Accepted := False;
      if Local_Root = Invalid_Type or else not Validate (Object, Contract) then return; end if;
      if Contract.Root = Integer_Type then
         Value := CCL.VM.Integer_Constant (Integer_Of (Object.Cells (1)));
      elsif Contract.Root = Boolean_Type then
         Value := CCL.VM.Boolean_Constant (Object.Cells (1).First = 1);
      elsif Is_Scalar_Sum (Contract.Types, Contract.Root) then
         D := Describe (Contract.Types, Contract.Root);
         for Choice in 1 .. D.Count loop
            if Object.Cells (1).First = Unsigned_64 (Choice) then
               Value := (Kind => CCL.VM.Variant_Value, Data_Type => Local_Root,
                         Alternative => Choice, others => <>);
               case D.Parts (Choice).Payload is
                  when Integer_Type => Value.Integer := Integer_Of (Object.Cells (2));
                  when Boolean_Type => Value.Boolean := Object.Cells (2).First = 1;
                  when Unit_Type => null;
                  when others => return;
               end case;
               Accepted := True;
               return;
            end if;
         end loop;
         return;
      else return;
      end if;
      Accepted := True;
   end To_VM;
end CCL.Objects.Values;
