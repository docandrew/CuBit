with Interfaces; use Interfaces;
with CCL.VM; use CCL.VM;
with CCL.Imports;

package body CCL.Format with
   SPARK_Mode => On
is
   use type CCL.Ownership.Disposition_Effect;
   use type CCL.Ownership.Disposition;
   use type CCL.Imports.Transfer_Mode;
   use type CCL.Imports.Cancellation_Mode;
   use type CCL.Catalog.Intern_Result;

   type Wire_Boolean is (Wire_False, Wire_True);
   for Wire_Boolean use (Wire_False => 0, Wire_True => 1);
   for Wire_Boolean'Size use 8;

   function Boolean_Number (Item : Boolean) return Unsigned_8 is
     (Unsigned_8
        (Wire_Boolean'Enum_Rep
           (if Item then Wire_True else Wire_False)));

   FORMAT_MAGIC : constant String := "CCLB";

   function Has_Valid_Magic (Data : Byte_Array) return Boolean is
   begin
      for Position in FORMAT_MAGIC'Range loop
         if Data (MAGIC_OFFSET + Position - FORMAT_MAGIC'First) /=
           Character'Pos (FORMAT_MAGIC (Position))
         then
            return False;
         end if;
      end loop;
      return True;
   end Has_Valid_Magic;
   procedure Put_U16
     (Data : in out Byte_Array; Offset : Byte_Index; Item : Unsigned_16) is
   begin
      Data (Offset) := Unsigned_8 (Item and 16#FF#);
      Data (Offset + 1) := Unsigned_8 (Shift_Right (Item, 8));
   end Put_U16;

   procedure Put_U32
     (Data : in out Byte_Array; Offset : Byte_Index; Item : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         Data (Offset + I) := Unsigned_8
           (Shift_Right (Item, 8 * I) and 16#FF#);
      end loop;
   end Put_U32;

   procedure Put_U64
     (Data : in out Byte_Array; Offset : Byte_Index; Item : Unsigned_64) is
   begin
      for I in 0 .. 7 loop
         Data (Offset + I) := Unsigned_8
           (Shift_Right (Item, 8 * I) and 16#FF#);
      end loop;
   end Put_U64;

   function Get_U16 (Data : Byte_Array; Offset : Byte_Index) return Unsigned_16 is
     (Unsigned_16 (Data (Offset)) or
      Shift_Left (Unsigned_16 (Data (Offset + 1)), 8));

   function Get_U32 (Data : Byte_Array; Offset : Byte_Index) return Unsigned_32 is
      Result : Unsigned_32 := 0;
   begin
      for I in 0 .. 3 loop
         Result := Result or Shift_Left (Unsigned_32 (Data (Offset + I)), 8 * I);
      end loop;
      return Result;
   end Get_U32;

   function Get_U64 (Data : Byte_Array; Offset : Byte_Index) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         Result := Result or Shift_Left (Unsigned_64 (Data (Offset + I)), 8 * I);
      end loop;
      return Result;
   end Get_U64;

   function To_Wire (Item : Integer_64) return Unsigned_64 is
     (if Item >= 0 then Unsigned_64 (Item)
      else Unsigned_64'Last - Unsigned_64 (-(Item + 1)));

   function From_Wire (Item : Unsigned_64) return Integer_64 is
     (if Item <= Unsigned_64 (Integer_64'Last) then Integer_64 (Item)
      else -1 - Integer_64 (Unsigned_64'Last - Item));

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
              Resolved.Import /= Item.Imports (I) or else
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
     (case Item.Op is
         when Push_Integer => Item.Target = 0 and then Item.Import = 0,
         when Push_Boolean =>
           (Item.Immediate = 0 or else Item.Immediate = 1) and then
           Item.Target = 0 and then Item.Import = 0,
         when Jump | Jump_If_False =>
           Item.Immediate = 0 and then Item.Import = 0,
         when Invoke_Import => Item.Immediate = 0 and then Item.Target = 0,
         when Initialize_Local | Copy_Local | Move_Local | Drop_Local |
              Borrow_Local_RO |
              Return_Local_RO | Borrow_Local_RW | Return_Local_RW =>
           Item.Immediate = 0 and then Item.Target = 0 and then
           Item.Import = 0 and then Item.Verb = 0,
         when Apply_Local_Disposition =>
           Item.Immediate = 0 and then Item.Target = 0 and then Item.Import = 0,
         when others =>
           Item.Immediate = 0 and then Item.Target = 0 and then
           Item.Import = 0 and then Item.Local = 0 and then Item.Verb = 0);

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
      Needed  : Natural;
      Offset  : Natural;
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

      Needed := HEADER_SIZE + Candidate.Types_Length * TYPE_SIZE +
        Candidate.Locals_Length * LOCAL_SIZE +
        Candidate.Imports_Length * IMPORT_SIZE +
        Natural (Candidate.Length) * INSTRUCTION_SIZE;
      Length := Module_Length (Needed);
      for Position in FORMAT_MAGIC'Range loop
         Data (MAGIC_OFFSET + Position - FORMAT_MAGIC'First) :=
           Character'Pos (FORMAT_MAGIC (Position));
      end loop;
      Put_U16 (Data, VERSION_OFFSET, FORMAT_VERSION);
      Put_U16 (Data, HEADER_SIZE_OFFSET, HEADER_SIZE);
      Put_U32 (Data, TOTAL_LENGTH_OFFSET, Unsigned_32 (Needed));
      Put_U16
        (Data, INSTRUCTION_COUNT_OFFSET, Unsigned_16 (Candidate.Length));
      Put_U16
        (Data, IMPORT_COUNT_OFFSET, Unsigned_16 (Candidate.Imports_Length));
      Put_U32 (Data, FUEL_LIMIT_OFFSET, Unsigned_32 (Limits.Fuel));
      Put_U32 (Data, MEMORY_LIMIT_OFFSET, Unsigned_32 (Limits.Memory));
      Put_U16
        (Data, IN_FLIGHT_LIMIT_OFFSET, Unsigned_16 (Limits.In_Flight));
      Data (LOCAL_COUNT_OFFSET) := Unsigned_8 (Candidate.Locals_Length);
      Data (TYPE_COUNT_OFFSET) := Unsigned_8 (Candidate.Types_Length);
      Data (DYNAMIC_LOCAL_COUNT_OFFSET) :=
        Unsigned_8 (Candidate.Dynamic_Locals_Length);

      Offset := HEADER_SIZE;
      if Candidate.Types_Length > 0 then
         for T in 0 .. Candidate.Types_Length - 1 loop
            Data (Offset + TYPE_MODE_OFFSET) :=
              Mode_Number (Candidate.Types (T).Mode);
            Data (Offset + TYPE_DISPOSITION_COUNT_OFFSET) :=
              Unsigned_8 (Candidate.Types (T).Dispositions_Length);
            for D in 0 .. CCL.Ownership.MAX_DISPOSITIONS - 1 loop
               Data
                 (Offset + TYPE_DISPOSITIONS_OFFSET +
                  D * DISPOSITION_SIZE + DISPOSITION_VERB_OFFSET) :=
                 Candidate.Types (T).Dispositions (D).Verb;
               Data
                 (Offset + TYPE_DISPOSITIONS_OFFSET +
                  D * DISPOSITION_SIZE + DISPOSITION_EFFECT_OFFSET) :=
                 Effect_Number
                 (Candidate.Types (T).Dispositions (D).Effect);
               Data
                 (Offset + TYPE_DISPOSITIONS_OFFSET +
                  D * DISPOSITION_SIZE + DISPOSITION_NEXT_TYPE_OFFSET) :=
                 Unsigned_8
                 (Candidate.Types (T).Dispositions (D).Next_Type);
            end loop;
            Offset := Offset + TYPE_SIZE;
         end loop;
      end if;
      if Candidate.Locals_Length > 0 then
         for Local in 0 .. Candidate.Locals_Length - 1 loop
            Data (Offset + LOCAL_KIND_OFFSET) :=
              Kind_Number (Candidate.Local_Kinds (Local));
            Data (Offset + LOCAL_TYPE_OFFSET) :=
              Unsigned_8 (Candidate.Local_Types (Local));
            Offset := Offset + LOCAL_SIZE;
         end loop;
      end if;
      if Candidate.Imports_Length > 0 then
         for I in 0 .. Candidate.Imports_Length - 1 loop
            declare
               Resolved : constant CCL.Catalog.Resolved_Operation :=
                 CCL.Catalog.Element (Linkage, I);
            begin
               Data (Offset + IMPORT_ARGUMENT_OFFSET) :=
                 Kind_Number (Candidate.Imports (I).Argument);
               Data (Offset + IMPORT_RESULT_OFFSET) :=
                 Kind_Number (Candidate.Imports (I).Result);
               Data (Offset + IMPORT_AUTHORITY_OFFSET) := Authority_Number
                 (Candidate.Imports (I).Authority);
               Data (Offset + IMPORT_OWNERSHIP_OFFSET) :=
                 Boolean_Number (Candidate.Imports (I).Ownership_Argument);
               Data (Offset + IMPORT_LOCAL_OFFSET) :=
                 Unsigned_8 (Candidate.Imports (I).Local);
               Data (Offset + IMPORT_TRANSFER_OFFSET) := Transfer_Number
                 (Candidate.Imports (I).Transfer);
               Data (Offset + IMPORT_CANCELLATION_OFFSET) :=
                 Cancellation_Number
                 (Candidate.Imports (I).Cancellation);
               Data (Offset + IMPORT_PARAMETER_COUNT_OFFSET) :=
                 Unsigned_8 (Resolved.Parameters);
               Data (Offset + IMPORT_SUCCESS_VERB_OFFSET) :=
                 Candidate.Imports (I).Success_Verb;
               Data (Offset + IMPORT_FAILURE_VERB_OFFSET) :=
                 Candidate.Imports (I).Failure_Verb;
               Data (Offset + IMPORT_CANCEL_VERB_OFFSET) :=
                 Candidate.Imports (I).Cancel_Verb;
               Put_U16
                 (Data, Offset + IMPORT_MAJOR_VERSION_OFFSET,
                  Resolved.Interface_Major);
               Put_U16
                 (Data, Offset + IMPORT_MINOR_VERSION_OFFSET,
                  Resolved.Interface_Minor);
               Data (Offset + IMPORT_OPERATION_OFFSET) :=
                 Unsigned_8 (Resolved.Operation);
               for Word in Resolved.Interface_Digest'Range loop
                  Put_U64
                    (Data,
                     Offset + IMPORT_DIGEST_OFFSET + Word * DIGEST_WORD_SIZE,
                     Resolved.Interface_Digest (Word));
               end loop;
            end;
            Offset := Offset + IMPORT_SIZE;
         end loop;
      end if;

      for I in 0 .. Candidate.Length - 1 loop
         if not Canonical (Candidate.Code (Instruction_Index (I))) then
            Error := Noncanonical_Instruction;
            Length := 0;
            return;
         end if;
         Data (Offset + INSTRUCTION_OPCODE_OFFSET) := Op_Number
           (Candidate.Code (Instruction_Index (I)).Op);
         Data (Offset + INSTRUCTION_LOCAL_OFFSET) := Unsigned_8
           (Candidate.Code (Instruction_Index (I)).Local);
         Data (Offset + INSTRUCTION_VERB_OFFSET) :=
           Candidate.Code (Instruction_Index (I)).Verb;
         Put_U64
           (Data, Offset + INSTRUCTION_IMMEDIATE_OFFSET,
            To_Wire (Candidate.Code (Instruction_Index (I)).Immediate));
         Put_U16
           (Data, Offset + INSTRUCTION_TARGET_OFFSET,
            Unsigned_16 (Candidate.Code (Instruction_Index (I)).Target));
         Data (Offset + INSTRUCTION_IMPORT_OFFSET) := Unsigned_8
           (Candidate.Code (Instruction_Index (I)).Import);
         Offset := Offset + INSTRUCTION_SIZE;
      end loop;
   end Encode;

   procedure Encode
     (Candidate  : Program;
      Limits     : Resource_Limits;
      Data       : out Byte_Array;
      Length     : out Module_Length;
      Error      : out Format_Error;
      Validation : out Validation_Error)
   is
      Empty_Linkage : CCL.Catalog.Linkage_Table;
   begin
      CCL.Catalog.Initialize (Empty_Linkage);
      Encode
        (Candidate, Empty_Linkage, Limits, Data, Length, Error, Validation);
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
      Instruction_Count : Natural;
      Import_Count : Natural;
      Local_Count : Natural;
      Type_Count : Natural;
      Dynamic_Local_Count : Natural;
      Expected : Natural;
      Offset : Natural;
      Op : Unsigned_8;
      Kind : Unsigned_8;
      Authority : Unsigned_8;
      Mode : Unsigned_8;
      Effect : Unsigned_8;
      Resolution : CCL.Catalog.Resolved_Operation;
      Link_Index : CCL.VM.Import_Index;
      Interned   : CCL.Catalog.Intern_Result;
   begin
      Program := (others => <>);
      CCL.Catalog.Initialize (Linkage);
      Limits := (others => 0);
      Error := Format_Valid;
      Validation := Valid;
      if Length < HEADER_SIZE then
         Error := Buffer_Too_Small;
         return;
      elsif not Has_Valid_Magic (Data) then
         Error := Bad_Magic;
         return;
      elsif Get_U16 (Data, VERSION_OFFSET) /= FORMAT_VERSION then
         Error := Unsupported_Version;
         return;
      elsif Get_U16 (Data, HEADER_SIZE_OFFSET) /= HEADER_SIZE then
         Error := Bad_Header_Size;
         return;
      end if;

      Instruction_Count := Natural
        (Get_U16 (Data, INSTRUCTION_COUNT_OFFSET));
      Import_Count := Natural (Get_U16 (Data, IMPORT_COUNT_OFFSET));
      Local_Count := Natural (Data (LOCAL_COUNT_OFFSET));
      Type_Count := Natural (Data (TYPE_COUNT_OFFSET));
      Dynamic_Local_Count := Natural (Data (DYNAMIC_LOCAL_COUNT_OFFSET));
      if Instruction_Count > MAX_INSTRUCTIONS or else
        Import_Count > MAX_IMPORTS or else
        Local_Count > CCL.Ownership.MAX_BINDINGS or else
        Type_Count > CCL.Ownership.MAX_TYPES or else
        Dynamic_Local_Count > Local_Count
      then
         Error := Bad_Total_Length;
         return;
      end if;
      Expected := HEADER_SIZE + Type_Count * TYPE_SIZE +
        Local_Count * LOCAL_SIZE + Import_Count * IMPORT_SIZE +
        Instruction_Count * INSTRUCTION_SIZE;
      if Get_U32 (Data, TOTAL_LENGTH_OFFSET) /= Unsigned_32 (Length) or else
        Length /= Expected
      then
         Error := Bad_Total_Length;
         return;
      elsif Data (HEADER_RESERVED_OFFSET) /= 0 or else
        Data (HEADER_RESERVED_OFFSET + 1) /= 0 or else
        Data (HEADER_RESERVED_OFFSET + 2) /= 0
      then
         Error := Bad_Reserved_Field;
         return;
      elsif Get_U32 (Data, FUEL_LIMIT_OFFSET) = 0 or else
        Get_U32 (Data, FUEL_LIMIT_OFFSET) >
          Unsigned_32 (MAX_MODULE_FUEL) or else
        Get_U32 (Data, MEMORY_LIMIT_OFFSET) >
          Unsigned_32 (MAX_MODULE_MEMORY) or else
        Get_U16 (Data, IN_FLIGHT_LIMIT_OFFSET) > MAX_MODULE_IN_FLIGHT
      then
         Error := Invalid_Resource_Limit;
         return;
      end if;
      Limits :=
        (Fuel => Natural (Get_U32 (Data, FUEL_LIMIT_OFFSET)),
         Memory => Natural (Get_U32 (Data, MEMORY_LIMIT_OFFSET)),
         In_Flight => Natural (Get_U16 (Data, IN_FLIGHT_LIMIT_OFFSET)));
      Candidate.Length := Program_Length (Instruction_Count);
      Candidate.Imports_Length := Import_Count;
      Candidate.Locals_Length := Local_Count;
      Candidate.Dynamic_Locals_Length := Dynamic_Local_Count;
      Candidate.Types_Length := Type_Count;

      Offset := HEADER_SIZE;
      if Type_Count > 0 then
         for T in 0 .. Type_Count - 1 loop
            Mode := Data (Offset + TYPE_MODE_OFFSET);
            if Mode > Unsigned_8
              (CCL.Ownership.Ownership_Mode'Enum_Rep
                 (CCL.Ownership.Ownership_Mode'Last)) or else
              Data (Offset + TYPE_DISPOSITION_COUNT_OFFSET) >
                CCL.Ownership.MAX_DISPOSITIONS or else
              Get_U16 (Data, Offset + TYPE_RESERVED_OFFSET) /= 0
            then
               Error := Invalid_Ownership_Metadata;
               return;
            end if;
            Candidate.Types (T).Mode :=
              CCL.Ownership.Ownership_Mode'Enum_Val (Mode);
            Candidate.Types (T).Dispositions_Length :=
              Natural (Data (Offset + TYPE_DISPOSITION_COUNT_OFFSET));
            for D in 0 .. CCL.Ownership.MAX_DISPOSITIONS - 1 loop
               Effect := Data
                 (Offset + TYPE_DISPOSITIONS_OFFSET +
                  D * DISPOSITION_SIZE + DISPOSITION_EFFECT_OFFSET);
               if Effect > Unsigned_8
                 (CCL.Ownership.Disposition_Effect'Enum_Rep
                    (CCL.Ownership.Disposition_Effect'Last)) or else
                 Data
                   (Offset + TYPE_DISPOSITIONS_OFFSET +
                    D * DISPOSITION_SIZE + DISPOSITION_RESERVED_OFFSET) /= 0
                 or else
                 Natural
                   (Data
                      (Offset + TYPE_DISPOSITIONS_OFFSET +
                       D * DISPOSITION_SIZE + DISPOSITION_NEXT_TYPE_OFFSET)) >=
                   Type_Count
               then
                  Error := Invalid_Ownership_Metadata;
                  return;
               end if;
               Candidate.Types (T).Dispositions (D) :=
                 (Verb => Data
                    (Offset + TYPE_DISPOSITIONS_OFFSET +
                     D * DISPOSITION_SIZE + DISPOSITION_VERB_OFFSET),
                  Effect => CCL.Ownership.Disposition_Effect'Enum_Val
                    (Effect),
                  Next_Type => CCL.Ownership.Type_Id
                    (Data
                       (Offset + TYPE_DISPOSITIONS_OFFSET +
                        D * DISPOSITION_SIZE +
                        DISPOSITION_NEXT_TYPE_OFFSET)));
            end loop;
            Offset := Offset + TYPE_SIZE;
         end loop;
      end if;
      if Local_Count > 0 then
         if Type_Count = 0 then
            Error := Invalid_Ownership_Metadata;
            return;
         end if;
         for Local in 0 .. Local_Count - 1 loop
            if Data (Offset + LOCAL_KIND_OFFSET) > Unsigned_8
              (Value_Kind'Enum_Rep (Value_Kind'Last)) or else
              Natural (Data (Offset + LOCAL_TYPE_OFFSET)) >= Type_Count or else
              Get_U16 (Data, Offset + LOCAL_RESERVED_OFFSET) /= 0
            then
               Error := Invalid_Ownership_Metadata;
               return;
            end if;
            Candidate.Local_Kinds (Local) :=
              Value_Kind'Enum_Val (Data (Offset + LOCAL_KIND_OFFSET));
            Candidate.Local_Types (Local) :=
              CCL.Ownership.Type_Id (Data (Offset + LOCAL_TYPE_OFFSET));
            Offset := Offset + LOCAL_SIZE;
         end loop;
      end if;
      if Import_Count > 0 then
         for I in 0 .. Import_Count - 1 loop
            Kind := Data (Offset + IMPORT_ARGUMENT_OFFSET);
            if Kind > Unsigned_8
              (Value_Kind'Enum_Rep (Value_Kind'Last)) or else
              Data (Offset + IMPORT_RESULT_OFFSET) > Unsigned_8
                (Value_Kind'Enum_Rep (Value_Kind'Last))
            then
               Error := Invalid_Value_Kind;
               return;
            end if;
            Authority := Data (Offset + IMPORT_AUTHORITY_OFFSET);
            if Authority > Unsigned_8
              (Authority_Class'Enum_Rep (Authority_Class'Last))
            then
               Error := Invalid_Authority;
               return;
            elsif Data (Offset + IMPORT_OWNERSHIP_OFFSET) > Unsigned_8
              (Wire_Boolean'Enum_Rep (Wire_Boolean'Last)) or else
              Data (Offset + IMPORT_LOCAL_OFFSET) >
                CCL.Ownership.MAX_BINDINGS - 1 or else
              Data (Offset + IMPORT_PARAMETER_COUNT_OFFSET) >
                Unsigned_8 (CCL.Catalog.Parameter_Count'Last) or else
              Data (Offset + IMPORT_OPERATION_OFFSET) >
                Unsigned_8 (CCL.Catalog.Operation_Index'Last)
            then
               Error := Invalid_Ownership_Metadata;
               return;
            elsif Data (Offset + IMPORT_TRANSFER_OFFSET) >
              CCL.Imports.Transfer_Mode'Enum_Rep
                (CCL.Imports.Transfer_Mode'Last)
            then
               Error := Invalid_Transfer_Mode;
               return;
            elsif Data (Offset + IMPORT_CANCELLATION_OFFSET) >
              CCL.Imports.Cancellation_Mode'Enum_Rep
                (CCL.Imports.Cancellation_Mode'Last)
            then
               Error := Invalid_Cancellation_Mode;
               return;
            elsif Data (Offset + IMPORT_RESERVED_OFFSET) /= 0 or else
              Data (Offset + IMPORT_IDENTITY_RESERVED_OFFSET) /= 0 or else
              Data (Offset + IMPORT_IDENTITY_RESERVED_OFFSET + 1) /= 0 or else
              Data (Offset + IMPORT_IDENTITY_RESERVED_OFFSET + 2) /= 0 or else
              Data (Offset + IMPORT_IDENTITY_RESERVED_OFFSET + 3) /= 0 or else
              Data (Offset + IMPORT_IDENTITY_RESERVED_OFFSET + 4) /= 0 or else
              Data (Offset + IMPORT_IDENTITY_RESERVED_OFFSET + 5) /= 0 or else
              Data (Offset + IMPORT_IDENTITY_RESERVED_OFFSET + 6) /= 0
            then
               Error := Bad_Reserved_Field;
               return;
            elsif Get_U16
              (Data, Offset + IMPORT_MAJOR_VERSION_OFFSET) = 0
            then
               Error := Invalid_Linkage;
               return;
            end if;
            Candidate.Imports (I) :=
               (Argument => Value_Kind'Enum_Val (Kind),
               Result => Value_Kind'Enum_Val
                 (Data (Offset + IMPORT_RESULT_OFFSET)),
               Authority => Authority_Class'Enum_Val (Authority),
               Binding => 0,
               Ownership_Argument =>
                 Wire_Boolean'Enum_Val
                   (Data (Offset + IMPORT_OWNERSHIP_OFFSET)) = Wire_True,
               Local => CCL.Ownership.Binding_Id
                 (Data (Offset + IMPORT_LOCAL_OFFSET)),
               Transfer => CCL.Imports.Transfer_Mode'Enum_Val
                 (Data (Offset + IMPORT_TRANSFER_OFFSET)),
               Cancellation => CCL.Imports.Cancellation_Mode'Enum_Val
                 (Data (Offset + IMPORT_CANCELLATION_OFFSET)),
               Success_Verb => Data (Offset + IMPORT_SUCCESS_VERB_OFFSET),
               Failure_Verb => Data (Offset + IMPORT_FAILURE_VERB_OFFSET),
               Cancel_Verb => Data (Offset + IMPORT_CANCEL_VERB_OFFSET));
            Resolution :=
              (Interface_Digest =>
                 [0 => Get_U64 (Data, Offset + IMPORT_DIGEST_OFFSET),
                  1 => Get_U64
                    (Data, Offset + IMPORT_DIGEST_OFFSET + DIGEST_WORD_SIZE),
                  2 => Get_U64
                    (Data,
                     Offset + IMPORT_DIGEST_OFFSET + 2 * DIGEST_WORD_SIZE),
                  3 => Get_U64
                    (Data,
                     Offset + IMPORT_DIGEST_OFFSET + 3 * DIGEST_WORD_SIZE)],
               Interface_Major => Get_U16
                 (Data, Offset + IMPORT_MAJOR_VERSION_OFFSET),
               Interface_Minor => Get_U16
                 (Data, Offset + IMPORT_MINOR_VERSION_OFFSET),
               Operation => CCL.Catalog.Operation_Index
                 (Data (Offset + IMPORT_OPERATION_OFFSET)),
               Parameters => CCL.Catalog.Parameter_Count
                 (Data (Offset + IMPORT_PARAMETER_COUNT_OFFSET)),
               Import => Candidate.Imports (I));
            if not Digest_Present (Resolution.Interface_Digest) then
               Error := Invalid_Linkage;
               return;
            end if;
            CCL.Catalog.Intern
              (Linkage, Resolution, Link_Index, Interned);
            if Interned /= CCL.Catalog.Linkage_Added or else Link_Index /= I
            then
               Error := Invalid_Linkage;
               return;
            end if;
            Offset := Offset + IMPORT_SIZE;
         end loop;
      end if;

      if Instruction_Count > 0 then
         for I in 0 .. Instruction_Count - 1 loop
            Op := Data (Offset + INSTRUCTION_OPCODE_OFFSET);
            if Op > Unsigned_8 (Op_Code'Enum_Rep (Op_Code'Last)) then
               Error := Invalid_Opcode;
               return;
            elsif Data (Offset + INSTRUCTION_RESERVED_OFFSET) /= 0 or else
              Data (Offset + INSTRUCTION_TRAILING_RESERVED_OFFSET) /= 0
            then
               Error := Bad_Reserved_Field;
               return;
            elsif Get_U16 (Data, Offset + INSTRUCTION_TARGET_OFFSET) >
              MAX_INSTRUCTIONS - 1 or else
              Data (Offset + INSTRUCTION_IMPORT_OFFSET) > MAX_IMPORTS - 1 or else
              Data (Offset + INSTRUCTION_LOCAL_OFFSET) >
                CCL.Ownership.MAX_BINDINGS - 1
            then
               Error := Invalid_Operand;
               return;
            end if;
            Candidate.Code (Instruction_Index (I)) :=
              (Op => Op_Code'Enum_Val (Op),
               Immediate => From_Wire
                 (Get_U64 (Data, Offset + INSTRUCTION_IMMEDIATE_OFFSET)),
               Target => Instruction_Index
                 (Get_U16 (Data, Offset + INSTRUCTION_TARGET_OFFSET)),
               Import => Import_Index
                 (Data (Offset + INSTRUCTION_IMPORT_OFFSET)),
               Local => CCL.Ownership.Binding_Id
                 (Data (Offset + INSTRUCTION_LOCAL_OFFSET)),
               Verb => Data (Offset + INSTRUCTION_VERB_OFFSET));
            if not Canonical
              (Candidate.Code (Instruction_Index (I)))
            then
               Error := Noncanonical_Instruction;
               return;
            end if;
            Offset := Offset + INSTRUCTION_SIZE;
         end loop;
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
