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
     (case Item is
         when Halt => 0, when Push_Integer => 1, when Push_Boolean => 2,
         when Add_Integer => 3, when Equal_Integer => 4,
         when Not_Boolean => 5, when Drop => 6, when Jump => 7,
         when Jump_If_False => 8, when Invoke_Import => 9,
         when Copy_Local => 10, when Move_Local => 11,
         when Drop_Local => 12, when Borrow_Local_RO => 13,
         when Return_Local_RO => 14, when Borrow_Local_RW => 15,
         when Return_Local_RW => 16, when Apply_Local_Disposition => 17);

   function Kind_Number (Item : Value_Kind) return Unsigned_8 is
     (if Item = Integer_Value then 0 else 1);

   function Authority_Number (Item : Authority_Class) return Unsigned_8 is
     (case Item is
         when No_Authority => 0, when Observe_Authority => 1,
         when Control_Authority => 2, when Secret_Use_Authority => 3,
         when Network_Authority => 4);

   function Mode_Number
     (Item : CCL.Ownership.Ownership_Mode) return Unsigned_8 is
     (case Item is
         when CCL.Ownership.Unrestricted => 0,
         when CCL.Ownership.Move_Only => 1,
         when CCL.Ownership.Must_Handle => 2);

   function Effect_Number
     (Item : CCL.Ownership.Disposition_Effect) return Unsigned_8 is
     (case Item is
         when CCL.Ownership.Consume => 0,
         when CCL.Ownership.Transfer => 1,
         when CCL.Ownership.Transition => 2);

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

   function Imports_Supported_By_V2 (Item : Program) return Boolean is
   begin
      if Item.Imports_Length > 0 then
         for I in 0 .. Item.Imports_Length - 1 loop
            if Item.Imports (I).Ownership_Argument or else
              Item.Imports (I).Transfer /= CCL.Imports.Copy_Argument or else
              Item.Imports (I).Cancellation /= CCL.Imports.Not_Cancellable or else
              Item.Imports (I).Success_Verb /= 0 or else
              Item.Imports (I).Failure_Verb /= 0 or else
              Item.Imports (I).Cancel_Verb /= 0
            then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Imports_Supported_By_V2;

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
         when Copy_Local | Move_Local | Drop_Local | Borrow_Local_RO |
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
      elsif not Imports_Supported_By_V2 (Candidate) then
         Error := Unsupported_Ownership_Metadata;
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
      Data (0) := Character'Pos ('C');
      Data (1) := Character'Pos ('C');
      Data (2) := Character'Pos ('L');
      Data (3) := Character'Pos ('B');
      Put_U16 (Data, 4, FORMAT_VERSION);
      Put_U16 (Data, 6, HEADER_SIZE);
      Put_U32 (Data, 8, Unsigned_32 (Needed));
      Put_U16 (Data, 12, Unsigned_16 (Candidate.Length));
      Put_U16 (Data, 14, Unsigned_16 (Candidate.Imports_Length));
      Put_U32 (Data, 16, Unsigned_32 (Limits.Fuel));
      Put_U32 (Data, 20, Unsigned_32 (Limits.Memory));
      Put_U16 (Data, 24, Unsigned_16 (Limits.In_Flight));
      Data (26) := Unsigned_8 (Candidate.Locals_Length);
      Data (27) := Unsigned_8 (Candidate.Types_Length);

      Offset := HEADER_SIZE;
      if Candidate.Types_Length > 0 then
         for T in 0 .. Candidate.Types_Length - 1 loop
            Data (Offset) := Mode_Number (Candidate.Types (T).Mode);
            Data (Offset + 1) :=
              Unsigned_8 (Candidate.Types (T).Dispositions_Length);
            for D in 0 .. CCL.Ownership.MAX_DISPOSITIONS - 1 loop
               Data (Offset + 4 + D * 4) :=
                 Candidate.Types (T).Dispositions (D).Verb;
               Data (Offset + 5 + D * 4) := Effect_Number
                 (Candidate.Types (T).Dispositions (D).Effect);
               Data (Offset + 6 + D * 4) := Unsigned_8
                 (Candidate.Types (T).Dispositions (D).Next_Type);
            end loop;
            Offset := Offset + TYPE_SIZE;
         end loop;
      end if;
      if Candidate.Locals_Length > 0 then
         for Local in 0 .. Candidate.Locals_Length - 1 loop
            Data (Offset) := Kind_Number (Candidate.Local_Kinds (Local));
            Data (Offset + 1) := Unsigned_8 (Candidate.Local_Types (Local));
            Offset := Offset + LOCAL_SIZE;
         end loop;
      end if;
      if Candidate.Imports_Length > 0 then
         for I in 0 .. Candidate.Imports_Length - 1 loop
            Data (Offset) := Kind_Number (Candidate.Imports (I).Argument);
            Data (Offset + 1) := Kind_Number (Candidate.Imports (I).Result);
            Data (Offset + 2) := Authority_Number
              (Candidate.Imports (I).Authority);
            Put_U32 (Data, Offset + 4, Candidate.Imports (I).Binding);
            Offset := Offset + IMPORT_SIZE;
         end loop;
      end if;

      for I in 0 .. Candidate.Length - 1 loop
         if not Canonical (Candidate.Code (Instruction_Index (I))) then
            Error := Noncanonical_Instruction;
            Length := 0;
            return;
         end if;
         Data (Offset) := Op_Number
           (Candidate.Code (Instruction_Index (I)).Op);
         Data (Offset + 1) := Unsigned_8
           (Candidate.Code (Instruction_Index (I)).Local);
         Data (Offset + 2) := Candidate.Code (Instruction_Index (I)).Verb;
         Put_U64
           (Data, Offset + 4,
            To_Wire (Candidate.Code (Instruction_Index (I)).Immediate));
         Put_U16
           (Data, Offset + 12,
            Unsigned_16 (Candidate.Code (Instruction_Index (I)).Target));
         Data (Offset + 14) := Unsigned_8
           (Candidate.Code (Instruction_Index (I)).Import);
         Offset := Offset + INSTRUCTION_SIZE;
      end loop;
   end Encode;

   procedure Decode
     (Data       : Byte_Array;
      Length     : Module_Length;
      Program    : out Validated_Program;
      Limits     : out Resource_Limits;
      Error      : out Format_Error;
      Validation : out Validation_Error)
   is
      Candidate : CCL.VM.Program;
      Instruction_Count : Natural;
      Import_Count : Natural;
      Local_Count : Natural;
      Type_Count : Natural;
      Expected : Natural;
      Offset : Natural;
      Op : Unsigned_8;
      Kind : Unsigned_8;
      Authority : Unsigned_8;
      Mode : Unsigned_8;
      Effect : Unsigned_8;
   begin
      Verify (Candidate, Program, Validation);
      Limits := (others => 0);
      Error := Format_Valid;
      Validation := Valid;
      if Length < HEADER_SIZE then
         Error := Buffer_Too_Small;
         return;
      elsif Data (0) /= Character'Pos ('C') or else
        Data (1) /= Character'Pos ('C') or else
        Data (2) /= Character'Pos ('L') or else
        Data (3) /= Character'Pos ('B')
      then
         Error := Bad_Magic;
         return;
      elsif Get_U16 (Data, 4) /= FORMAT_VERSION then
         Error := Unsupported_Version;
         return;
      elsif Get_U16 (Data, 6) /= HEADER_SIZE then
         Error := Bad_Header_Size;
         return;
      end if;

      Instruction_Count := Natural (Get_U16 (Data, 12));
      Import_Count := Natural (Get_U16 (Data, 14));
      Local_Count := Natural (Data (26));
      Type_Count := Natural (Data (27));
      if Instruction_Count > MAX_INSTRUCTIONS or else
        Import_Count > MAX_IMPORTS or else
        Local_Count > CCL.Ownership.MAX_BINDINGS or else
        Type_Count > CCL.Ownership.MAX_TYPES
      then
         Error := Bad_Total_Length;
         return;
      end if;
      Expected := HEADER_SIZE + Type_Count * TYPE_SIZE +
        Local_Count * LOCAL_SIZE + Import_Count * IMPORT_SIZE +
        Instruction_Count * INSTRUCTION_SIZE;
      if Get_U32 (Data, 8) /= Unsigned_32 (Length) or else Length /= Expected then
         Error := Bad_Total_Length;
         return;
      elsif Get_U32 (Data, 28) /= 0 then
         Error := Bad_Reserved_Field;
         return;
      elsif Get_U32 (Data, 16) = 0 or else
        Get_U32 (Data, 16) > Unsigned_32 (MAX_MODULE_FUEL) or else
        Get_U32 (Data, 20) > Unsigned_32 (MAX_MODULE_MEMORY) or else
        Get_U16 (Data, 24) > MAX_MODULE_IN_FLIGHT
      then
         Error := Invalid_Resource_Limit;
         return;
      end if;
      Limits :=
        (Fuel => Natural (Get_U32 (Data, 16)),
         Memory => Natural (Get_U32 (Data, 20)),
         In_Flight => Natural (Get_U16 (Data, 24)));
      Candidate.Length := Program_Length (Instruction_Count);
      Candidate.Imports_Length := Import_Count;
      Candidate.Locals_Length := Local_Count;
      Candidate.Types_Length := Type_Count;

      Offset := HEADER_SIZE;
      if Type_Count > 0 then
         for T in 0 .. Type_Count - 1 loop
            Mode := Data (Offset);
            if Mode > 2 or else
              Data (Offset + 1) > CCL.Ownership.MAX_DISPOSITIONS or else
              Get_U16 (Data, Offset + 2) /= 0
            then
               Error := Invalid_Ownership_Metadata;
               return;
            end if;
            Candidate.Types (T).Mode :=
              CCL.Ownership.Ownership_Mode'Val (Natural (Mode));
            Candidate.Types (T).Dispositions_Length :=
              Natural (Data (Offset + 1));
            for D in 0 .. CCL.Ownership.MAX_DISPOSITIONS - 1 loop
               Effect := Data (Offset + 5 + D * 4);
               if Effect > 2 or else Data (Offset + 7 + D * 4) /= 0 or else
                 Natural (Data (Offset + 6 + D * 4)) >= Type_Count
               then
                  Error := Invalid_Ownership_Metadata;
                  return;
               end if;
               Candidate.Types (T).Dispositions (D) :=
                 (Verb => Data (Offset + 4 + D * 4),
                  Effect => CCL.Ownership.Disposition_Effect'Val
                    (Natural (Effect)),
                  Next_Type => CCL.Ownership.Type_Id
                    (Data (Offset + 6 + D * 4)));
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
            if Data (Offset) > 1 or else
              Natural (Data (Offset + 1)) >= Type_Count or else
              Get_U16 (Data, Offset + 2) /= 0
            then
               Error := Invalid_Ownership_Metadata;
               return;
            end if;
            Candidate.Local_Kinds (Local) :=
              (if Data (Offset) = 0 then Integer_Value else Boolean_Value);
            Candidate.Local_Types (Local) :=
              CCL.Ownership.Type_Id (Data (Offset + 1));
            Offset := Offset + LOCAL_SIZE;
         end loop;
      end if;
      if Import_Count > 0 then
         for I in 0 .. Import_Count - 1 loop
            Kind := Data (Offset);
            if Kind > 1 or else Data (Offset + 1) > 1 then
               Error := Invalid_Value_Kind;
               return;
            end if;
            Authority := Data (Offset + 2);
            if Authority > 4 then
               Error := Invalid_Authority;
               return;
            elsif Data (Offset + 3) /= 0 then
               Error := Bad_Reserved_Field;
               return;
            end if;
            Candidate.Imports (I) :=
              (Argument => (if Kind = 0 then Integer_Value else Boolean_Value),
               Result => (if Data (Offset + 1) = 0 then Integer_Value else Boolean_Value),
               Authority => Authority_Class'Val (Natural (Authority)),
               Binding => Get_U32 (Data, Offset + 4), others => <>);
            Offset := Offset + IMPORT_SIZE;
         end loop;
      end if;

      if Instruction_Count > 0 then
         for I in 0 .. Instruction_Count - 1 loop
            Op := Data (Offset);
            if Op > 17 then
               Error := Invalid_Opcode;
               return;
            elsif Data (Offset + 3) /= 0 or else Data (Offset + 15) /= 0
            then
               Error := Bad_Reserved_Field;
               return;
            elsif Get_U16 (Data, Offset + 12) > MAX_INSTRUCTIONS - 1 or else
              Data (Offset + 14) > MAX_IMPORTS - 1 or else
              Data (Offset + 1) > CCL.Ownership.MAX_BINDINGS - 1
            then
               Error := Invalid_Operand;
               return;
            end if;
            Candidate.Code (Instruction_Index (I)) :=
              (Op => Op_Code'Val (Natural (Op)),
               Immediate => From_Wire (Get_U64 (Data, Offset + 4)),
               Target => Instruction_Index (Get_U16 (Data, Offset + 12)),
               Import => Import_Index (Data (Offset + 14)),
               Local => CCL.Ownership.Binding_Id (Data (Offset + 1)),
               Verb => Data (Offset + 2));
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

      Verify (Candidate, Program, Validation);
      if Validation /= Valid then
         Error := Bytecode_Invalid;
      end if;
   end Decode;
end CCL.Format;
