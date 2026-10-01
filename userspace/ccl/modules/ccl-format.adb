with CBOR;
with CBOR.Decoding;
with CBOR.Encoding;
with CCL.VM; use CCL.VM;
with CCL.Imports;
with CCL.Host_Values;
with CCL.Objects;
with CCL.Ownership;
with CCL.Types;

package body CCL.Format with
   SPARK_Mode => On
is
   use type CCL.Ownership.Disposition_Effect;
   use type CCL.Ownership.Disposition;
   use type CCL.Catalog.Intern_Result;
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Definition_Result;
   use type CCL.Types.Shape;
   use type CBOR.SE_Offset;
   use type CBOR.Decode_Status;
   use type CBOR.Major_Type;
   use type CBOR.Byte;
   package E renames CBOR.Encoding;
   package D renames CBOR.Decoding;

   subtype Wire_Buffer is CBOR.Byte_Array (1 .. MAX_MODULE_SIZE);
   type Digest_Words is array (Natural range 0 .. 3) of Unsigned_64;

   --  32 bytes, most significant first, as CCL.Objects.Persistence does.
   function Encoded_Digest (Words : Digest_Words) return CBOR.Byte_Array
     with Post => Encoded_Digest'Result'First = 1 and then
                  Encoded_Digest'Result'Length = DIGEST_BYTES;
   function Encoded_Digest (Words : Digest_Words) return CBOR.Byte_Array is
      Result : CBOR.Byte_Array (1 .. DIGEST_BYTES) := [others => 0];
   begin
      for Word in Words'Range loop
         for B in 0 .. 7 loop
            Result (CBOR.SE_Offset (Word * 8 + B + 1)) := CBOR.Byte
              (Shift_Right (Words (Word), (7 - B) * 8) and 16#FF#);
         end loop;
      end loop;
      return Result;
   end Encoded_Digest;

   function From_Descriptor (Item : CCL.Catalog.Descriptor_Digest) return Digest_Words is
     ([for Word in Digest_Words'Range => Item (Word)]);
   function From_Schema (Item : CCL.Objects.Schema_Key) return Digest_Words is
     ([for Word in Digest_Words'Range => Item (Word)]);
   function To_Descriptor (Item : Digest_Words) return CCL.Catalog.Descriptor_Digest is
     ([for Word in CCL.Catalog.Descriptor_Digest'Range => Item (Word)]);
   function To_Schema (Item : Digest_Words) return CCL.Objects.Schema_Key is
     ([for Word in CCL.Objects.Schema_Key'Range => Item (Word)]);

   function Shape_Code (Form : CCL.Types.Shape) return Unsigned_64 is
     (case Form is
         when CCL.Types.Product => SHAPE_PRODUCT,
         when CCL.Types.Sum => SHAPE_SUM,
         when CCL.Types.Resource => SHAPE_RESOURCE,
         when CCL.Types.Sequence => SHAPE_SEQUENCE,
         when CCL.Types.Callable => SHAPE_CALLABLE,
         when CCL.Types.Bounded => SHAPE_BOUNDED,
         when CCL.Types.Primitive => 0);

   function Op_Number (Item : Op_Code) return Unsigned_8 is
     (Unsigned_8 (Op_Code'Enum_Rep (Item)));

   function Kind_Number (Item : Value_Kind) return Unsigned_8 is
     (Unsigned_8 (Value_Kind'Enum_Rep (Item)));

   function Authority_Number (Item : Authority_Class) return Unsigned_8 is
     (Unsigned_8 (Authority_Class'Enum_Rep (Item)));

   function Transfer_Number
     (Item : CCL.Imports.Transfer_Mode) return Unsigned_8 is
     (Unsigned_8 (CCL.Imports.Transfer_Mode'Enum_Rep (Item)));

   function Cancellation_Number
     (Item : CCL.Imports.Cancellation_Mode) return Unsigned_8 is
     (Unsigned_8 (CCL.Imports.Cancellation_Mode'Enum_Rep (Item)));

   function Mode_Number
     (Item : CCL.Ownership.Ownership_Mode) return Unsigned_8 is
     (Unsigned_8 (CCL.Ownership.Ownership_Mode'Enum_Rep (Item)));

   function Effect_Number
     (Item : CCL.Ownership.Disposition_Effect) return Unsigned_8 is
     (Unsigned_8 (CCL.Ownership.Disposition_Effect'Enum_Rep (Item)));

   function Ownership_Metadata_Valid (Item : Program) return Boolean is
   begin
      if Item.Locals_Length > 0 and then Item.Types_Length = 0 then
         return False;
      end if;
      if Item.Locals_Length > 0 then
         for Local in 0 .. Item.Locals_Length - 1 loop
            if Natural (Item.Local_Types (Local)) >= Item.Types_Length then
               return False;
            end if;
         end loop;
      end if;
      if Item.Types_Length > 0 then
         for T in 0 .. Item.Types_Length - 1 loop
            for D in 0 .. CCL.Ownership.MAX_DISPOSITIONS - 1 loop
               if D >= Item.Types (T).Dispositions_Length then
                  if Item.Types (T).Dispositions (D) /=
                    (Verb => 0, Effect => CCL.Ownership.Consume,
                     Next_Type => 0)
                  then
                     return False;
                  end if;
               elsif Item.Types (T).Dispositions (D).Effect =
                 CCL.Ownership.Transition
               then
                  if Natural (Item.Types (T).Dispositions (D).Next_Type) >=
                    Item.Types_Length
                  then
                     return False;
                  end if;
               elsif Item.Types (T).Dispositions (D).Next_Type /= 0 then
                  return False;
               end if;
               if D < Item.Types (T).Dispositions_Length and then D > 0 then
                  for Prior in 0 .. D - 1 loop
                     if Item.Types (T).Dispositions (Prior).Verb =
                       Item.Types (T).Dispositions (D).Verb
                     then
                        return False;
                     end if;
                  end loop;
               end if;
            end loop;
         end loop;
      end if;
      return True;
   end Ownership_Metadata_Valid;

   function Digest_Present
     (Item : CCL.Catalog.Descriptor_Digest) return Boolean
   is
   begin
      for Word of Item loop
         if Word /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Digest_Present;

   function Portable_Linkage_Valid
     (Item : Program; Linkage : CCL.Catalog.Linkage_Table) return Boolean
   is
      Resolved : CCL.Catalog.Resolved_Operation;
   begin
      if Item.Imports_Length /= CCL.Catalog.Length (Linkage) then
         return False;
      end if;
      if Item.Imports_Length > 0 then
         for I in 0 .. Item.Imports_Length - 1 loop
            Resolved := CCL.Catalog.Element (Linkage, I);
            if Item.Imports (I).Binding /= 0 or else
              not CCL.Host_Values.Matches_Bytecode (Item.Imports (I), Resolved.Import) or else
              Resolved.Interface_Major = 0 or else
              not Digest_Present (Resolved.Interface_Digest)
            then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Portable_Linkage_Valid;

   function Runtime_Binding_Present
     (Item : Program; Linkage : CCL.Catalog.Linkage_Table) return Boolean
   is
   begin
      if Item.Imports_Length > 0 then
         for I in 0 .. Item.Imports_Length - 1 loop
            if Item.Imports (I).Binding /= 0 then
               return True;
            end if;
         end loop;
      end if;
      if CCL.Catalog.Length (Linkage) > 0 then
         for I in 0 .. CCL.Catalog.Length (Linkage) - 1 loop
            if CCL.Catalog.Element (Linkage, I).Import.Binding /= 0 then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Runtime_Binding_Present;

   function Limits_Valid (Item : Resource_Limits) return Boolean is
     (Item.Fuel > 0);

   function Canonical (Item : Instruction) return Boolean is
     ((if Item.Op in Make_Variant | Equal_Variant | Project_Field then
          Item.Data_Type in CCL.Types.Declared_Type
       else Item.Data_Type = CCL.Types.Invalid_Type and then Item.Alternative = 0) and then
      (case Item.Op is
         when Project_Field =>
           Item.Immediate in 1 .. Integer_64 (CCL.Types.Maximum_Components) and then
           Item.Target = 0 and then Item.Import = 0 and then Item.Local = 0 and then
           Item.Verb = 0 and then Item.Alternative = 0,
         when Make_Variant | Equal_Variant =>
           Item.Immediate = 0 and then Item.Target = 0 and then Item.Import = 0 and then
           Item.Local = 0 and then Item.Verb = 0 and then
           (if Item.Op = Make_Variant then Item.Alternative > 0 else Item.Alternative = 0),
         when Switch_Variant | Copy_Stack | Call_Function | Push_Text | Text_Builtin =>
           Item.Immediate >= 0 and then Item.Target = 0 and then Item.Import = 0 and then
           Item.Local = 0 and then Item.Verb = 0,
         when Push_Integer => Item.Target = 0 and then Item.Import = 0 and then
           Item.Local = 0 and then Item.Verb = 0,
         when Push_Boolean =>
           (Item.Immediate = 0 or else Item.Immediate = 1) and then
           Item.Target = 0 and then Item.Import = 0 and then Item.Local = 0 and then Item.Verb = 0,
         when Jump | Jump_If_False =>
           Item.Immediate = 0 and then Item.Import = 0 and then Item.Local = 0 and then Item.Verb = 0,
         when Invoke_Import => Item.Immediate = 0 and then Item.Target = 0 and then Item.Local = 0 and then Item.Verb = 0,
         when Initialize_Local | Copy_Local | Move_Local | Drop_Local |
              Borrow_Local_RO |
              Return_Local_RO | Borrow_Local_RW | Return_Local_RW =>
           Item.Immediate = 0 and then Item.Target = 0 and then
           Item.Import = 0 and then Item.Verb = 0,
         when Apply_Local_Disposition =>
           Item.Immediate = 0 and then Item.Target = 0 and then Item.Import = 0,
         when others =>
           Item.Immediate = 0 and then Item.Target = 0 and then
           Item.Import = 0 and then Item.Local = 0 and then Item.Verb = 0));

   procedure Encode
     (Candidate  : Program;
      Linkage    : CCL.Catalog.Linkage_Table;
      Limits     : Resource_Limits;
      Data       : out Byte_Array;
      Length     : out Module_Length;
      Error      : out Format_Error;
      Validation : out Validation_Error)
   is
      Checked : Validated_Program;
      Output  : Wire_Buffer := [others => 0];
      Used    : Natural range 0 .. MAX_MODULE_SIZE := 0;
      Fits    : Boolean := True;

      procedure Put (Bytes : CBOR.Byte_Array) with Pre => Bytes'First = 1;
      procedure Put (Bytes : CBOR.Byte_Array) is
      begin
         if not Fits then return; end if;
         if Bytes'Length > CBOR.SE_Offset (MAX_MODULE_SIZE - Used) then
            Fits := False;
         else
            Output (CBOR.SE_Offset (Used) + 1 .. CBOR.SE_Offset (Used) + Bytes'Length) := Bytes;
            Used := Used + Natural (Bytes'Length);
         end if;
      end Put;
      procedure Put_Array (Count : Natural) is
      begin
         Put (E.Encode_Array (CBOR.UInt64 (Count)));
      end Put_Array;
      procedure Put_Unsigned (Value : Unsigned_64) is
      begin
         Put (E.Encode_Unsigned (Value));
      end Put_Unsigned;
      procedure Put_Integer (Value : Integer_64) is
         Encoded : constant CBOR.Byte_Array := E.Encode_Integer (Value);
         Head : constant CBOR.Byte_Array (1 .. Encoded'Length) := Encoded;
      begin
         Put (Head);
      end Put_Integer;
      --  Names, the magic, digests and text constants.
      procedure Put_Bytes (Bytes : CBOR.Byte_Array)
        with Pre => Bytes'First = 1 and then Bytes'Length <= MAX_CONSTANT_BYTES;
      procedure Put_Bytes (Bytes : CBOR.Byte_Array) is
         --  Byte string head (major type 2): the length in the head itself
         --  below 24, else in one following byte. Shortest form either way.
         BYTE_STRING : constant := 16#40#;
         ONE_BYTE_LENGTH : constant := 24;
         TWO_BYTE_LENGTH : constant := 25;
         Length : constant Natural := Natural (Bytes'Length);
      begin
         if Length < ONE_BYTE_LENGTH then
            Put ([1 => CBOR.Byte (BYTE_STRING + Length)]);
         elsif Length <= 16#FF# then
            Put ([1 => CBOR.Byte (BYTE_STRING + ONE_BYTE_LENGTH), 2 => CBOR.Byte (Length)]);
         else
            Put ([1 => CBOR.Byte (BYTE_STRING + TWO_BYTE_LENGTH),
                  2 => CBOR.Byte (Length / 16#100#), 3 => CBOR.Byte (Length mod 16#100#)]);
         end if;
         Put (Bytes);
      end Put_Bytes;
      procedure Put_Name (Item : CCL.Types.Name) is
         Bytes : CBOR.Byte_Array (1 .. CBOR.SE_Offset (CCL.Types.Maximum_Name_Length)) :=
           [others => 0];
      begin
         for I in 1 .. Item.Length loop
            Bytes (CBOR.SE_Offset (I)) := CBOR.Byte (Character'Pos (Item.Data (I)));
         end loop;
         Put_Bytes (Bytes (1 .. CBOR.SE_Offset (Item.Length)));
      end Put_Name;
      procedure Put_Kind (Item : Value_Kind) is
      begin
         Put_Unsigned (Unsigned_64 (Kind_Number (Item)));
      end Put_Kind;
      procedure Put_Type (Item : CCL.Types.Type_Reference) is
      begin
         Put_Unsigned (Unsigned_64 (Item));
      end Put_Type;
   begin
      Data := [others => 0];
      Length := 0;
      Error := Format_Valid;
      Verify (Candidate, Checked, Validation);
      if Validation /= Valid then
         Error := Bytecode_Invalid;
         return;
      elsif not Ownership_Metadata_Valid (Candidate) then
         Error := Invalid_Ownership_Metadata;
         return;
      elsif Runtime_Binding_Present (Candidate, Linkage) then
         Error := Runtime_Binding_In_Module;
         return;
      elsif not Portable_Linkage_Valid (Candidate, Linkage) then
         Error := Invalid_Linkage;
         return;
      elsif not Limits_Valid (Limits) then
         Error := Invalid_Resource_Limit;
         return;
      end if;
      for I in 0 .. Natural (Candidate.Length) - 1 loop
         if not Canonical (Candidate.Code (Instruction_Index (I))) then
            Error := Noncanonical_Instruction;
            return;
         end if;
      end loop;

      Put_Array (11);
      declare
         Magic : CBOR.Byte_Array (1 .. FORMAT_MAGIC'Length) := [others => 0];
      begin
         for I in FORMAT_MAGIC'Range loop
            Magic (CBOR.SE_Offset (I - FORMAT_MAGIC'First + 1)) :=
              CBOR.Byte (Character'Pos (FORMAT_MAGIC (I)));
         end loop;
         Put_Bytes (Magic);
      end;
      Put_Unsigned (FORMAT_VERSION);
      Put_Array (3);
      Put_Unsigned (Unsigned_64 (Limits.Fuel));
      Put_Unsigned (Unsigned_64 (Limits.Memory));
      Put_Unsigned (Unsigned_64 (Limits.In_Flight));

      --  Ownership types.
      Put_Array (Candidate.Types_Length);
      for T in 0 .. Candidate.Types_Length - 1 loop
         Put_Array (2);
         Put_Unsigned (Unsigned_64 (Mode_Number (Candidate.Types (T).Mode)));
         Put_Array (Candidate.Types (T).Dispositions_Length);
         for D in 0 .. Candidate.Types (T).Dispositions_Length - 1 loop
            Put_Array (3);
            Put_Unsigned (Unsigned_64 (Candidate.Types (T).Dispositions (D).Verb));
            Put_Unsigned (Unsigned_64 (Effect_Number (Candidate.Types (T).Dispositions (D).Effect)));
            Put_Unsigned (Unsigned_64 (Candidate.Types (T).Dispositions (D).Next_Type));
         end loop;
      end loop;

      --  Data types, in registry order.
      Put_Array (Natural (CCL.Types.Last (Candidate.Data_Types) - CCL.Types.Unit_Type));
      for T in CCL.Types.Declared_Type'First .. CCL.Types.Last (Candidate.Data_Types) loop
         declare
            Item : constant CCL.Types.Description := CCL.Types.Describe (Candidate.Data_Types, T);
         begin
            Put_Array (5);
            Put_Unsigned (Shape_Code (Item.Form));
            Put_Name (Item.Identifier);
            Put_Array (Item.Count);
            for P in 1 .. Item.Count loop
               Put_Array (2);
               Put_Name (Item.Parts (P).Identifier);
               Put_Type (Item.Parts (P).Payload);
            end loop;
            Put_Integer (if Item.Form = CCL.Types.Bounded
                         then CCL.Types.Low_Of (Candidate.Data_Types, T) else 0);
            Put_Integer (if Item.Form = CCL.Types.Bounded
                         then CCL.Types.High_Of (Candidate.Data_Types, T) else 0);
         end;
      end loop;

      --  Match tables.
      Put_Array (Candidate.Matches_Length);
      for M in 0 .. Candidate.Matches_Length - 1 loop
         Put_Array (2);
         Put_Type (Candidate.Matches (M).Data_Type);
         Put_Array (CCL.Types.Maximum_Components);
         for A in CCL.Types.Component_Index loop
            Put_Unsigned (Unsigned_64 (Candidate.Matches (M).Targets (A)));
         end loop;
      end loop;

      --  Locals.
      Put_Array (2);
      Put_Unsigned (Unsigned_64 (Candidate.Dynamic_Locals_Length));
      Put_Array (Candidate.Locals_Length);
      for L in 0 .. Candidate.Locals_Length - 1 loop
         Put_Array (3);
         Put_Kind (Candidate.Local_Kinds (L));
         Put_Unsigned (Unsigned_64 (Candidate.Local_Types (L)));
         Put_Type (Candidate.Local_Data_Types (L));
      end loop;

      --  Imports, with their descriptor-pinned linkage.
      Put_Array (Candidate.Imports_Length);
      for I in 0 .. Candidate.Imports_Length - 1 loop
         declare
            Resolved : constant CCL.Catalog.Resolved_Operation := CCL.Catalog.Element (Linkage, I);
            Item : constant Import_Declaration := Candidate.Imports (I);
         begin
            Put_Array (IMPORT_FIELDS);
            Put_Kind (Item.Argument);
            Put_Kind (Item.Result);
            Put_Unsigned (Unsigned_64 (Authority_Number (Item.Authority)));
            Put_Unsigned (if Item.Ownership_Argument then 1 else 0);
            Put_Unsigned (Unsigned_64 (Item.Local));
            Put_Unsigned (Unsigned_64 (Transfer_Number (Item.Transfer)));
            Put_Unsigned (Unsigned_64 (Cancellation_Number (Item.Cancellation)));
            Put_Unsigned (Unsigned_64 (Resolved.Parameters));
            Put_Unsigned (Unsigned_64 (Item.Success_Verb));
            Put_Unsigned (Unsigned_64 (Item.Failure_Verb));
            Put_Unsigned (Unsigned_64 (Item.Cancel_Verb));
            Put_Unsigned (Unsigned_64 (Resolved.Interface_Major));
            Put_Unsigned (Unsigned_64 (Resolved.Interface_Minor));
            Put_Unsigned (Unsigned_64 (Resolved.Operation));
            Put_Type (Item.Argument_Data_Type);
            Put_Type (Item.Result_Data_Type);
            Put_Bytes (Encoded_Digest (From_Descriptor (Resolved.Interface_Digest)));
            Put_Bytes (Encoded_Digest (From_Schema (Resolved.Import.Argument_Schema)));
            Put_Bytes (Encoded_Digest (From_Schema (Resolved.Import.Result_Schema)));
         end;
      end loop;

      --  Functions.
      Put_Array (Candidate.Functions_Length);
      for F in 0 .. Candidate.Functions_Length - 1 loop
         Put_Array (4);
         Put_Unsigned (Unsigned_64 (Candidate.Functions (F).Entry_PC));
         Put_Array (Candidate.Functions (F).Count);
         for P in 1 .. Candidate.Functions (F).Count loop
            Put_Array (2);
            Put_Kind (Candidate.Functions (F).Kinds (P));
            Put_Type (Candidate.Functions (F).Data_Types (P));
         end loop;
         Put_Kind (Candidate.Functions (F).Result);
         Put_Type (Candidate.Functions (F).Result_Data_Type);
      end loop;

      --  Text constants.
      Put_Array (Candidate.Constants_Length);
      for C in 0 .. Candidate.Constants_Length - 1 loop
         declare
            Item : constant Text_Constant := Candidate.Constants (C);
            Bytes : CBOR.Byte_Array (1 .. MAX_CONSTANT_BYTES) := [others => 0];
         begin
            if Item.Length > MAX_CONSTANT_BYTES - (Item.First - 1) then
               Error := Invalid_Operand;
               return;
            end if;
            for I in 1 .. Item.Length loop
               Bytes (CBOR.SE_Offset (I)) :=
                 CBOR.Byte (Character'Pos (Candidate.Constant_Text (Item.First + I - 1)));
            end loop;
            Put_Bytes (Bytes (1 .. CBOR.SE_Offset (Item.Length)));
         end;
      end loop;

      --  Code.
      Put_Array (Natural (Candidate.Length));
      for I in 0 .. Natural (Candidate.Length) - 1 loop
         declare
            Op : constant Instruction := Candidate.Code (Instruction_Index (I));
         begin
            Put_Array (INSTRUCTION_FIELDS);
            Put_Unsigned (Unsigned_64 (Op_Number (Op.Op)));
            Put_Unsigned (Unsigned_64 (Op.Local));
            Put_Unsigned (Unsigned_64 (Op.Verb));
            Put_Type (Op.Data_Type);
            Put_Unsigned (Unsigned_64 (Op.Alternative));
            Put_Integer (Op.Immediate);
            Put_Unsigned (Unsigned_64 (Op.Target));
            Put_Unsigned (Unsigned_64 (Op.Import));
         end;
      end loop;

      if not Fits then
         Error := Buffer_Too_Small;
         return;
      end if;
      for I in 1 .. Used loop
         Data (I - 1) := Unsigned_8 (Output (CBOR.SE_Offset (I)));
      end loop;
      Length := Used;
   end Encode;

   procedure Encode
     (Candidate  : Program;
      Limits     : Resource_Limits;
      Data       : out Byte_Array;
      Length     : out Module_Length;
      Error      : out Format_Error;
      Validation : out Validation_Error)
   is
      Empty : CCL.Catalog.Linkage_Table;
   begin
      CCL.Catalog.Initialize (Empty);
      Encode (Candidate, Empty, Limits, Data, Length, Error, Validation);
   end Encode;

   procedure Decode
     (Data       : Byte_Array;
      Length     : Module_Length;
      Program    : out CCL.VM.Program;
      Linkage    : out CCL.Catalog.Linkage_Table;
      Limits     : out Resource_Limits;
      Error      : out Format_Error;
      Validation : out Validation_Error)
   is
      Candidate : CCL.VM.Program;
      Checked   : Validated_Program;
      Input     : Wire_Buffer := [others => 0];
      Last      : constant CBOR.SE_Offset := CBOR.SE_Offset (Length);
      Position  : CBOR.SE_Offset := 1;
      Count     : Natural;
      Value     : Unsigned_64;

      --  One head of the profile, or Malformed_Encoding. Indefinite
      --  lengths never pass: their counts exceed every bound below.
      procedure Take (Item : out CBOR.Decode_Result) is
      begin
         Item := (Status => CBOR.Err_Truncated, others => <>);
         if Error /= Format_Valid then return; end if;
         if Position not in 1 .. Last then
            Error := Malformed_Encoding;
            return;
         end if;
         Item := D.Decode (Input (1 .. Last), Position);
         if Item.Status /= CBOR.OK or else Input (Position) mod 32 = 31 then
            Error := Malformed_Encoding;
            return;
         end if;
         Position := Item.Next;
      end Take;

      procedure Get_Array (Items : out Natural; Maximum : Natural)
        with Post => Items <= Maximum and then (if Error /= Format_Valid then Items = 0);
      procedure Get_Array (Items : out Natural; Maximum : Natural) is
         Item : CBOR.Decode_Result;
      begin
         Items := 0;
         Take (Item);
         if Error /= Format_Valid then return; end if;
         if Item.Item.Kind /= CBOR.MT_Array then
            Error := Malformed_Encoding;
            return;
         end if;
         declare
            Found : constant Unsigned_64 := Unsigned_64 (Item.Item.Arr_Count);
         begin
            --  Convert against a static bound first, then compare as Natural.
            if Found > Unsigned_64 (Natural'Last) then
               Error := Malformed_Encoding;
               return;
            end if;
            declare
               Count : constant Natural := Natural (Found);
            begin
               if Count > Maximum then
                  Error := Malformed_Encoding;
               else
                  Items := Count;
               end if;
            end;
         end;
      end Get_Array;

      procedure Expect_Array (Items : Natural) is
         Found : Natural;
      begin
         Get_Array (Found, Items);
         if Error = Format_Valid and then Found /= Items then
            Error := Malformed_Encoding;
         end if;
      end Expect_Array;

      procedure Get_Unsigned (Item_Value : out Unsigned_64; Maximum : Unsigned_64)
        with Post => Item_Value <= Maximum;
      procedure Get_Unsigned (Item_Value : out Unsigned_64; Maximum : Unsigned_64) is
         Item : CBOR.Decode_Result;
      begin
         Item_Value := 0;
         Take (Item);
         if Error /= Format_Valid then return; end if;
         if Item.Item.Kind /= CBOR.MT_Unsigned_Integer or else
           Item.Item.UInt_Value > CBOR.UInt64 (Maximum)
         then
            Error := Malformed_Encoding;
         else
            Item_Value := Unsigned_64 (Item.Item.UInt_Value);
         end if;
      end Get_Unsigned;

      --  A bounded count or index. The conversion is checked here, in a small
      --  context; in the large decode loop it does not discharge at level 1.
      procedure Get_Natural (Item_Value : out Natural; Maximum : Natural)
        with Post => Item_Value <= Maximum;
      procedure Get_Natural (Item_Value : out Natural; Maximum : Natural) is
         Wide : Unsigned_64;
      begin
         Get_Unsigned (Wide, Unsigned_64 (Maximum));
         pragma Assert (Wide <= Unsigned_64 (Natural'Last));
         Item_Value := Natural (Wide);
         pragma Assert (Unsigned_64 (Item_Value) <= Unsigned_64 (Maximum));
      end Get_Natural;

      procedure Get_Integer (Item_Value : out Integer_64) is
         Item : CBOR.Decode_Result;
      begin
         Item_Value := 0;
         Take (Item);
         if Error /= Format_Valid then return; end if;
         if Item.Item.Kind not in CBOR.MT_Unsigned_Integer | CBOR.MT_Negative_Integer then
            Error := Malformed_Encoding;
            return;
         end if;
         declare
            --  The magnitude: the value itself, or N for the CBOR value -1 - N.
            Found : constant Unsigned_64 :=
              (if Item.Item.Kind = CBOR.MT_Unsigned_Integer
               then Unsigned_64 (Item.Item.UInt_Value)
               else Unsigned_64 (Item.Item.NInt_Arg));
         begin
            if Found > Unsigned_64 (Integer_64'Last) then
               Error := Malformed_Encoding;
               return;
            end if;
            declare
               --  Proved at level 3 (prove-ccl-format): at level 1 the solvers
               --  do not relate the 63-bit static bound to this conversion.
               Magnitude : constant Integer_64 := Integer_64 (Found);
            begin
               Item_Value := (if Item.Item.Kind = CBOR.MT_Unsigned_Integer
                              then Magnitude else -1 - Magnitude);
            end;
         end;
      end Get_Integer;

      --  A byte string of at most Maximum bytes into Bytes (1 .. Used).
      procedure Get_Bytes
        (Bytes : out CBOR.Byte_Array; Used : out Natural; Maximum : Natural)
        with Pre => Bytes'First = 1 and then Bytes'Length = CBOR.SE_Offset (Maximum),
             Post => Used <= Maximum
      is
         Item : CBOR.Decode_Result;
      begin
         Bytes := [others => 0];
         Used := 0;
         Take (Item);
         if Error /= Format_Valid then return; end if;
         if Item.Item.Kind /= CBOR.MT_Byte_String or else
           Item.Item.BS_Ref.Length > CBOR.SE_Offset (Maximum)
         then
            Error := Malformed_Encoding;
            return;
         end if;
         declare
            Payload : constant CBOR.Byte_Array := D.Get_String (Input (1 .. Last), Item.Item.BS_Ref);
         begin
            Bytes (1 .. Payload'Length) := Payload;
            Used := Natural (Payload'Length);
         end;
      end Get_Bytes;

      procedure Get_Name (Item : out CCL.Types.Name) is
         Bytes : CBOR.Byte_Array (1 .. CBOR.SE_Offset (CCL.Types.Maximum_Name_Length));
         Used : Natural;
      begin
         Item := (others => <>);
         Get_Bytes (Bytes, Used, CCL.Types.Maximum_Name_Length);
         if Error /= Format_Valid then return; end if;
         Item.Length := Used;
         for I in 1 .. Used loop
            Item.Data (I) := Character'Val (Bytes (CBOR.SE_Offset (I)));
         end loop;
      end Get_Name;

      procedure Get_Digest (Words : out Digest_Words) is
         Bytes : CBOR.Byte_Array (1 .. DIGEST_BYTES);
         Used : Natural;
      begin
         Words := [others => 0];
         Get_Bytes (Bytes, Used, DIGEST_BYTES);
         if Error /= Format_Valid then return; end if;
         if Used /= DIGEST_BYTES then
            Error := Malformed_Encoding;
            return;
         end if;
         for Word in Words'Range loop
            for B in 0 .. 7 loop
               Words (Word) := Shift_Left (Words (Word), 8) or
                 Unsigned_64 (Bytes (CBOR.SE_Offset (Word * 8 + B + 1)));
            end loop;
         end loop;
      end Get_Digest;

      procedure Get_Kind (Kind : out Value_Kind) is
      begin
         Kind := Integer_Value;
         Get_Unsigned (Value, Unsigned_64 (Kind_Number (Value_Kind'Last)));
         if Error = Format_Valid then
            Kind := Value_Kind'Enum_Val (Value);
         elsif Error = Malformed_Encoding then
            Error := Invalid_Value_Kind;
         end if;
      end Get_Kind;

      procedure Get_Type (Kind : out CCL.Types.Type_Reference) is
      begin
         Kind := CCL.Types.Invalid_Type;
         Get_Unsigned (Value, Unsigned_64 (CCL.Types.Type_Reference'Last));
         if Error = Format_Valid then
            Kind := CCL.Types.Type_Reference (Value);
         end if;
      end Get_Type;
   begin
      Program := (others => <>);
      CCL.Catalog.Initialize (Linkage);
      Limits := (others => 0);
      Error := Format_Valid;
      Validation := Valid;
      if Length = 0 then
         Error := Buffer_Too_Small;
         return;
      end if;
      for I in 1 .. Length loop
         Input (CBOR.SE_Offset (I)) := CBOR.Byte (Data (I - 1));
      end loop;

      Expect_Array (11);
      declare
         Magic : CBOR.Byte_Array (1 .. FORMAT_MAGIC'Length);
         Used : Natural;
      begin
         Get_Bytes (Magic, Used, FORMAT_MAGIC'Length);
         if Error /= Format_Valid then return; end if;
         if Used /= FORMAT_MAGIC'Length then Error := Bad_Magic; return; end if;
         for I in FORMAT_MAGIC'Range loop
            if Magic (CBOR.SE_Offset (I - FORMAT_MAGIC'First + 1)) /=
              CBOR.Byte (Character'Pos (FORMAT_MAGIC (I)))
            then
               Error := Bad_Magic;
               return;
            end if;
         end loop;
      end;
      Get_Unsigned (Value, Unsigned_64'Last);
      if Error /= Format_Valid then return; end if;
      if Value /= FORMAT_VERSION then Error := Unsupported_Version; return; end if;

      Expect_Array (3);
      declare
         Fuel, Memory, In_Flight : Unsigned_64;
      begin
         Get_Unsigned (Fuel, Unsigned_64'Last);
         Get_Unsigned (Memory, Unsigned_64'Last);
         Get_Unsigned (In_Flight, Unsigned_64'Last);
         if Error /= Format_Valid then return; end if;
         if Fuel not in 1 .. MAX_MODULE_FUEL or else Memory > MAX_MODULE_MEMORY or else
           In_Flight > MAX_MODULE_IN_FLIGHT
         then
            Error := Invalid_Resource_Limit;
            return;
         end if;
         Limits := (Fuel => Natural (Fuel), Memory => Natural (Memory),
                    In_Flight => Natural (In_Flight));
      end;

      --  Ownership types.
      Get_Array (Count, CCL.Ownership.MAX_TYPES);
      if Error /= Format_Valid then return; end if;
      Candidate.Types_Length := Count;
      for T in 0 .. Count - 1 loop
         Expect_Array (2);
         Get_Unsigned (Value, Unsigned_64 (Mode_Number (CCL.Ownership.Ownership_Mode'Last)));
         if Error /= Format_Valid then Error := Invalid_Ownership_Metadata; return; end if;
         Candidate.Types (T).Mode := CCL.Ownership.Ownership_Mode'Enum_Val (Value);
         declare
            Dispositions : Natural;
         begin
            Get_Array (Dispositions, CCL.Ownership.MAX_DISPOSITIONS);
            if Error /= Format_Valid then Error := Invalid_Ownership_Metadata; return; end if;
            Candidate.Types (T).Dispositions_Length := Dispositions;
            for Disposition in 0 .. Dispositions - 1 loop
               declare
                  Verb, Effect, Next : Unsigned_64;
               begin
                  Expect_Array (3);
                  Get_Unsigned (Verb, Unsigned_64 (CCL.Ownership.Disposition_Id'Last));
                  Get_Unsigned (Effect, Unsigned_64 (Effect_Number (CCL.Ownership.Disposition_Effect'Last)));
                  Get_Unsigned (Next, Unsigned_64 (CCL.Ownership.Type_Id'Last));
                  if Error /= Format_Valid or else Natural (Next) >= Count then
                     Error := Invalid_Ownership_Metadata;
                     return;
                  end if;
                  Candidate.Types (T).Dispositions (Disposition) :=
                    (Verb => CCL.Ownership.Disposition_Id (Verb),
                     Effect => CCL.Ownership.Disposition_Effect'Enum_Val (Effect),
                     Next_Type => CCL.Ownership.Type_Id (Next));
               end;
            end loop;
         end;
      end loop;

      --  Data types. A part naming a later type is a list of its own type
      --  (CCL.Types.Complete_Self_List): defined with a Unit placeholder
      --  first, completed once every definition exists.
      Get_Array (Count, CCL.Types.Maximum_Declarations);
      if Error /= Format_Valid then return; end if;
      declare
         type Pending_Part is record
            Owner : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
            Part : CCL.Types.Component_Index := 1;
            List_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
         end record;
         Pending : array (1 .. CCL.Types.Maximum_Declarations) of Pending_Part;
         Pending_Count : Natural range 0 .. CCL.Types.Maximum_Declarations := 0;
      begin
         for T in 1 .. Count loop
            declare
               Item : CCL.Types.Description;
               Shape, Parts : Natural;
               Low, High : Integer_64;
               Ref : CCL.Types.Type_Reference;
               Status : CCL.Types.Definition_Result;
               Own : constant CCL.Types.Type_Reference := CCL.Types.Unit_Type + CCL.Types.Type_Reference (T);
            begin
               Expect_Array (5);
               Get_Unsigned (Value, SHAPE_BOUNDED);
               Shape := Natural (Value);
               Get_Name (Item.Identifier);
               Get_Array (Parts, CCL.Types.Maximum_Components);
               if Error /= Format_Valid then Error := Invalid_Type_Metadata; return; end if;
               Item.Form := (case Shape is
                  when SHAPE_PRODUCT => CCL.Types.Product,
                  when SHAPE_SUM => CCL.Types.Sum,
                  when SHAPE_RESOURCE => CCL.Types.Resource,
                  when SHAPE_SEQUENCE => CCL.Types.Sequence,
                  when SHAPE_CALLABLE => CCL.Types.Callable,
                  when SHAPE_BOUNDED => CCL.Types.Bounded,
                  when others => CCL.Types.Primitive);
               Item.Count := Parts;
               for P in 1 .. Parts loop
                  Expect_Array (2);
                  Get_Name (Item.Parts (P).Identifier);
                  Get_Type (Item.Parts (P).Payload);
                  if Error /= Format_Valid then Error := Invalid_Type_Metadata; return; end if;
                  if Item.Parts (P).Payload >= Own then
                     if Pending_Count = Pending'Last then
                        Error := Invalid_Type_Metadata; return;
                     end if;
                     Pending_Count := Pending_Count + 1;
                     Pending (Pending_Count) := (Own, P, Item.Parts (P).Payload);
                     Item.Parts (P).Payload := CCL.Types.Unit_Type;
                  end if;
               end loop;
               Get_Integer (Low);
               Get_Integer (High);
               if Error /= Format_Valid then Error := Invalid_Type_Metadata; return; end if;
               if Item.Form = CCL.Types.Bounded then
                  if Parts /= 0 then Error := Invalid_Type_Metadata; return; end if;
                  CCL.Types.Define_Range (Candidate.Data_Types, Item.Identifier, Low, High, Ref, Status);
               elsif Low /= 0 or else High /= 0 then
                  Error := Invalid_Type_Metadata; return;
               else
                  CCL.Types.Define (Candidate.Data_Types, Item, Ref, Status);
               end if;
               if Status /= CCL.Types.Defined or else Ref /= Own then
                  Error := Invalid_Type_Metadata;
                  return;
               end if;
            end;
         end loop;
         for P in 1 .. Pending_Count loop
            declare
               Completed : Boolean;
            begin
               CCL.Types.Complete_Self_List
                 (Candidate.Data_Types, Pending (P).Owner, Pending (P).Part,
                  Pending (P).List_Type, Completed);
               if not Completed then Error := Invalid_Type_Metadata; return; end if;
            end;
         end loop;
      end;

      --  Match tables.
      Get_Array (Count, Maximum_Matches);
      if Error /= Format_Valid then return; end if;
      Candidate.Matches_Length := Count;
      for M in 0 .. Count - 1 loop
         Expect_Array (2);
         Get_Type (Candidate.Matches (M).Data_Type);
         Expect_Array (CCL.Types.Maximum_Components);
         for A in CCL.Types.Component_Index loop
            Get_Unsigned (Value, MAX_INSTRUCTIONS - 1);
            if Error /= Format_Valid then
               if Error = Malformed_Encoding then Error := Invalid_Operand; end if;
               return;
            end if;
            Candidate.Matches (M).Targets (A) := Instruction_Index (Value);
         end loop;
      end loop;

      --  Locals.
      Expect_Array (2);
      Get_Unsigned (Value, CCL.Ownership.MAX_BINDINGS);
      Get_Array (Count, CCL.Ownership.MAX_BINDINGS);
      if Error /= Format_Valid then return; end if;
      if Natural (Value) > Count or else (Count > 0 and then Candidate.Types_Length = 0) then
         Error := Invalid_Ownership_Metadata;
         return;
      end if;
      Candidate.Dynamic_Locals_Length := Natural (Value);
      Candidate.Locals_Length := Count;
      for L in 0 .. Count - 1 loop
         Expect_Array (3);
         Get_Kind (Candidate.Local_Kinds (L));
         Get_Unsigned (Value, Unsigned_64 (CCL.Ownership.Type_Id'Last));
         if Error /= Format_Valid or else Natural (Value) >= Candidate.Types_Length then
            Error := Invalid_Ownership_Metadata;
            return;
         end if;
         Candidate.Local_Types (L) := CCL.Ownership.Type_Id (Value);
         Get_Type (Candidate.Local_Data_Types (L));
         if Error /= Format_Valid then Error := Invalid_Ownership_Metadata; return; end if;
      end loop;

      --  Imports and their linkage.
      Get_Array (Count, MAX_IMPORTS);
      if Error /= Format_Valid then return; end if;
      Candidate.Imports_Length := Count;
      for I in 0 .. Count - 1 loop
         declare
            Argument, Result : Value_Kind;
            Authority, Ownership, Local, Transfer, Cancellation, Parameters,
              Success, Failure, Cancel, Major, Minor, Operation : Unsigned_64;
            Argument_Type, Result_Type : CCL.Types.Type_Reference;
            Digest, Argument_Key, Result_Key : Digest_Words;
            Resolution : CCL.Catalog.Resolved_Operation;
            Link_Index : CCL.VM.Import_Index;
            Interned : CCL.Catalog.Intern_Result;
         begin
            Expect_Array (IMPORT_FIELDS);
            Get_Kind (Argument);
            Get_Kind (Result);
            if Error /= Format_Valid then return; end if;
            Get_Unsigned (Authority, Unsigned_64 (Authority_Number (Authority_Class'Last)));
            if Error /= Format_Valid then Error := Invalid_Authority; return; end if;
            Get_Unsigned (Ownership, 1);
            Get_Unsigned (Local, CCL.Ownership.MAX_BINDINGS - 1);
            if Error /= Format_Valid then Error := Invalid_Ownership_Metadata; return; end if;
            Get_Unsigned (Transfer, Unsigned_64 (Transfer_Number (CCL.Imports.Transfer_Mode'Last)));
            if Error /= Format_Valid then Error := Invalid_Transfer_Mode; return; end if;
            Get_Unsigned (Cancellation, Unsigned_64 (Cancellation_Number (CCL.Imports.Cancellation_Mode'Last)));
            if Error /= Format_Valid then Error := Invalid_Cancellation_Mode; return; end if;
            Get_Unsigned (Parameters, Unsigned_64 (CCL.Catalog.Parameter_Count'Last));
            Get_Unsigned (Success, Unsigned_64 (CCL.Ownership.Disposition_Id'Last));
            Get_Unsigned (Failure, Unsigned_64 (CCL.Ownership.Disposition_Id'Last));
            Get_Unsigned (Cancel, Unsigned_64 (CCL.Ownership.Disposition_Id'Last));
            if Error /= Format_Valid then Error := Invalid_Ownership_Metadata; return; end if;
            Get_Unsigned (Major, Unsigned_64 (Unsigned_16'Last));
            Get_Unsigned (Minor, Unsigned_64 (Unsigned_16'Last));
            if Error /= Format_Valid or else Major = 0 then Error := Invalid_Linkage; return; end if;
            Get_Unsigned (Operation, Unsigned_64 (CCL.Catalog.Operation_Index'Last));
            if Error /= Format_Valid then Error := Invalid_Ownership_Metadata; return; end if;
            Get_Type (Argument_Type);
            Get_Type (Result_Type);
            if Error /= Format_Valid then Error := Invalid_Type_Metadata; return; end if;
            Get_Digest (Digest);
            Get_Digest (Argument_Key);
            Get_Digest (Result_Key);
            if Error /= Format_Valid then return; end if;
            Candidate.Imports (I) :=
              (Argument => Argument, Result => Result,
               Argument_Data_Type => Argument_Type, Result_Data_Type => Result_Type,
               Authority => Authority_Class'Enum_Val (Authority),
               Binding => 0,
               Ownership_Argument => Ownership = 1,
               Local => CCL.Ownership.Binding_Id (Local),
               Transfer => CCL.Imports.Transfer_Mode'Enum_Val (Transfer),
               Cancellation => CCL.Imports.Cancellation_Mode'Enum_Val (Cancellation),
               Success_Verb => CCL.Ownership.Disposition_Id (Success),
               Failure_Verb => CCL.Ownership.Disposition_Id (Failure),
               Cancel_Verb => CCL.Ownership.Disposition_Id (Cancel),
               Result_Type_Tag => 0,
               Receiver_Data_Type => CCL.Types.Invalid_Type);
            if not CCL.Host_Values.Portable_Contract
              (Candidate.Imports (I), To_Schema (Argument_Key),
               To_Schema (Result_Key))
            then
               Error := Invalid_Linkage;
               return;
            end if;
            Resolution :=
              (Interface_Digest => To_Descriptor (Digest),
               Interface_Major => Unsigned_16 (Major),
               Interface_Minor => Unsigned_16 (Minor),
               Operation => CCL.Catalog.Operation_Index (Operation),
               Parameters => CCL.Catalog.Parameter_Count (Parameters),
               Import => CCL.Host_Values.From_Bytecode
                 (Candidate.Imports (I), To_Schema (Argument_Key),
                  To_Schema (Result_Key)));
            if not Digest_Present (Resolution.Interface_Digest) then
               Error := Invalid_Linkage;
               return;
            end if;
            CCL.Catalog.Intern (Linkage, Resolution, Link_Index, Interned);
            if Interned /= CCL.Catalog.Linkage_Added or else Link_Index /= I then
               Error := Invalid_Linkage;
               return;
            end if;
         end;
      end loop;

      --  Functions.
      Get_Array (Count, MAX_FUNCTIONS);
      if Error /= Format_Valid then return; end if;
      Candidate.Functions_Length := Count;
      for F in 0 .. Count - 1 loop
         declare
            Parameters : Natural;
         begin
            Expect_Array (4);
            Get_Unsigned (Value, MAX_INSTRUCTIONS - 1);
            if Error /= Format_Valid then Error := Invalid_Function; return; end if;
            Candidate.Functions (F).Entry_PC := Instruction_Index (Value);
            Get_Array (Parameters, MAX_PARAMETERS);
            if Error /= Format_Valid then Error := Invalid_Function; return; end if;
            Candidate.Functions (F).Count := Parameters;
            for P in 1 .. Parameters loop
               Expect_Array (2);
               Get_Kind (Candidate.Functions (F).Kinds (P));
               Get_Type (Candidate.Functions (F).Data_Types (P));
               if Error /= Format_Valid then return; end if;
            end loop;
            Get_Kind (Candidate.Functions (F).Result);
            Get_Type (Candidate.Functions (F).Result_Data_Type);
            if Error /= Format_Valid then return; end if;
         end;
      end loop;

      --  Text constants, packed into the pool in order.
      Get_Array (Count, MAX_CONSTANTS);
      if Error /= Format_Valid then return; end if;
      Candidate.Constants_Length := Count;
      declare
         Pool_Used : Natural range 0 .. MAX_CONSTANT_BYTES := 0;
         Bytes : CBOR.Byte_Array (1 .. MAX_CONSTANT_BYTES);
         Used : Natural;
      begin
         for C in 0 .. Count - 1 loop
            Get_Bytes (Bytes, Used, MAX_CONSTANT_BYTES);
            if Error /= Format_Valid then return; end if;
            if Used > MAX_CONSTANT_BYTES - Pool_Used then
               Error := Invalid_Operand;
               return;
            end if;
            Candidate.Constants (C) := (First => Pool_Used + 1, Length => Used);
            for I in 1 .. Used loop
               Candidate.Constant_Text (Pool_Used + I) :=
                 Character'Val (Bytes (CBOR.SE_Offset (I)));
            end loop;
            Pool_Used := Pool_Used + Used;
         end loop;
      end;

      --  Code.
      Get_Array (Count, MAX_INSTRUCTIONS);
      if Error /= Format_Valid then return; end if;
      Candidate.Length := Program_Length (Count);
      for I in 0 .. Count - 1 loop
         declare
            Op, Verb, Target, Import : Unsigned_64;
            Local : CCL.Ownership.Binding_Id;
            Alternative : CCL.Types.Component_Count;
            Data_Type : CCL.Types.Type_Reference;
            Immediate : Integer_64;
         begin
            Expect_Array (INSTRUCTION_FIELDS);
            Get_Unsigned (Op, Unsigned_64 (Op_Number (Op_Code'Last)));
            if Error /= Format_Valid then Error := Invalid_Opcode; return; end if;
            Get_Natural (Local, CCL.Ownership.Binding_Id'Last);
            Get_Unsigned (Verb, Unsigned_64 (CCL.Ownership.Disposition_Id'Last));
            Get_Type (Data_Type);
            Get_Natural (Alternative, CCL.Types.Component_Count'Last);
            Get_Integer (Immediate);
            Get_Unsigned (Target, MAX_INSTRUCTIONS - 1);
            Get_Unsigned (Import, MAX_IMPORTS - 1);
            if Error /= Format_Valid then Error := Invalid_Operand; return; end if;
            Candidate.Code (Instruction_Index (I)) :=
              (Op => Op_Code'Enum_Val (Op),
               Immediate => Immediate,
               Target => Instruction_Index (Target),
               Import => Import_Index (Import),
               Local => Local,
               Verb => CCL.Ownership.Disposition_Id (Verb),
               Data_Type => Data_Type,
               Alternative => Alternative);
            if not Canonical (Candidate.Code (Instruction_Index (I))) then
               Error := Noncanonical_Instruction;
               return;
            end if;
         end;
      end loop;

      --  Exactly one item: nothing may follow it.
      if Position /= Last + 1 then
         Error := Malformed_Encoding;
         return;
      end if;
      if not Ownership_Metadata_Valid (Candidate) then
         Error := Invalid_Ownership_Metadata;
         return;
      end if;
      Verify (Candidate, Checked, Validation);
      if Validation /= Valid then
         Error := Bytecode_Invalid;
      else
         Program := Candidate;
      end if;
   end Decode;

   procedure Decode
     (Data       : Byte_Array;
      Length     : Module_Length;
      Program    : out Validated_Program;
      Limits     : out Resource_Limits;
      Error      : out Format_Error;
      Validation : out Validation_Error)
   is
      Candidate : CCL.VM.Program;
      Linkage   : CCL.Catalog.Linkage_Table;
      Ignored_Validation : Validation_Error;
   begin
      Decode
        (Data, Length, Candidate, Linkage, Limits, Error, Validation);
      if Error = Format_Valid then
         if CCL.Catalog.Length (Linkage) /= 0 then
            Error := Invalid_Linkage;
            Verify ((others => <>), Program, Ignored_Validation);
         else
            Verify (Candidate, Program, Validation);
            if Validation /= Valid then
               Error := Bytecode_Invalid;
            end if;
         end if;
      else
         Verify ((others => <>), Program, Ignored_Validation);
      end if;
   end Decode;
end CCL.Format;
