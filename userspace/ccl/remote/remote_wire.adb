pragma Ada_2022;

package body Remote_Wire with SPARK_Mode is

   Byte_Bits : constant := 8;

   ---------------------------------------------------------------------------
   --  Integers
   ---------------------------------------------------------------------------
   procedure Put_U32 (Data : in out Byte_Array; At_Index : Positive; Value : Unsigned_32) is
   begin
      for K in 0 .. 3 loop
         Data (At_Index + K) := Unsigned_8 (Shift_Right (Value, Byte_Bits * (3 - K)) and 16#FF#);
      end loop;
   end Put_U32;

   procedure Put_U64 (Data : in out Byte_Array; At_Index : Positive; Value : Unsigned_64) is
   begin
      for K in 0 .. 7 loop
         Data (At_Index + K) := Unsigned_8 (Shift_Right (Value, Byte_Bits * (7 - K)) and 16#FF#);
      end loop;
   end Put_U64;

   function Get_U32 (Data : Byte_Array; At_Index : Positive) return Unsigned_32 is
      Value : Unsigned_32 := 0;
   begin
      for K in 0 .. 3 loop
         Value := Shift_Left (Value, Byte_Bits) or Unsigned_32 (Data (At_Index + K));
      end loop;
      return Value;
   end Get_U32;

   function Get_U64 (Data : Byte_Array; At_Index : Positive) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for K in 0 .. 7 loop
         Value := Shift_Left (Value, Byte_Bits) or Unsigned_64 (Data (At_Index + K));
      end loop;
      return Value;
   end Get_U64;

   ---------------------------------------------------------------------------
   --  The token
   ---------------------------------------------------------------------------
   function Same_Token (Left, Right : Token) return Boolean is
      Difference : Unsigned_8 := 0;
   begin
      --  No early exit: every byte is compared whatever came before.
      for I in Token_Index loop
         Difference := Difference or (Left (I) xor Right (I));
         pragma Loop_Invariant
           ((Difference = 0) = (for all K in Token_Index'First .. I => Left (K) = Right (K)));
      end loop;
      return Difference = 0;
   end Same_Token;

   Symbol_Bits : constant := 5;
   Symbols : constant String (1 .. 32) := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
   Token_Bits : constant := Token_Bytes * Byte_Bits;              --  128

   --  Bit Index (0 = the first byte's most significant) of Item.
   function Bit (Item : Token; Index : Natural) return Unsigned_8 is
     (Shift_Right (Item (Index / Byte_Bits + 1), Byte_Bits - 1 - Index mod Byte_Bits) and 1)
     with Pre => Index < Token_Bits;

   function Token_Image (Item : Token) return Token_Text is
      Result : Token_Text := [others => '0'];
      Value : Natural;
   begin
      for S in 0 .. Token_Text_Length - 1 loop
         Value := 0;
         for B in 0 .. Symbol_Bits - 1 loop
            pragma Loop_Invariant (Value < 2 ** B);
            Value := Value * 2 +
              (if S * Symbol_Bits + B < Token_Bits then Natural (Bit (Item, S * Symbol_Bits + B)) else 0);
         end loop;
         Result (S + 1) := Symbols (Value + 1);
      end loop;
      return Result;
   end Token_Image;

   --  The value of symbol C, or 32 when it is not one (I and L read as 1,
   --  O as 0, either case).
   Not_A_Symbol : constant := 32;
   function Symbol_Value (C : Character) return Natural is
     (case C is
         when '0' | 'O' | 'o' => 0,
         when '1' | 'I' | 'i' | 'L' | 'l' => 1,
         when '2' .. '9' => Character'Pos (C) - Character'Pos ('0'),
         when 'A' .. 'H' => Character'Pos (C) - Character'Pos ('A') + 10,
         when 'a' .. 'h' => Character'Pos (C) - Character'Pos ('a') + 10,
         when 'J' | 'j' => 18,
         when 'K' | 'k' => 19,
         when 'M' | 'm' => 20,
         when 'N' | 'n' => 21,
         when 'P' .. 'T' => Character'Pos (C) - Character'Pos ('P') + 22,
         when 'p' .. 't' => Character'Pos (C) - Character'Pos ('p') + 22,
         when 'V' .. 'Z' => Character'Pos (C) - Character'Pos ('V') + 27,
         when 'v' .. 'z' => Character'Pos (C) - Character'Pos ('v') + 27,
         when others => Not_A_Symbol)
     with Post => Symbol_Value'Result <= Not_A_Symbol;

   procedure Decode_Token_Text (Text : String; Item : out Token; Valid : out Boolean) is
      Count : Natural := 0;
      Value : Natural;
      Position : Natural;
   begin
      Item := [others => 0];
      Valid := False;
      for C of Text loop
         if C = '-' or else C = ' ' then
            null;
         else
            Value := Symbol_Value (C);
            if Value = Not_A_Symbol or else Count = Token_Text_Length then
               Item := [others => 0];
               return;
            end if;
            for B in 0 .. Symbol_Bits - 1 loop
               Position := Count * Symbol_Bits + B;
               if (Value / 2 ** (Symbol_Bits - 1 - B)) mod 2 = 1 then
                  if Position >= Token_Bits then
                     --  A padding bit set: not the canonical text of a token.
                     Item := [others => 0];
                     return;
                  end if;
                  Item (Position / Byte_Bits + 1) :=
                    Item (Position / Byte_Bits + 1) or
                    Shift_Left (Unsigned_8'(1), Byte_Bits - 1 - Position mod Byte_Bits);
               end if;
            end loop;
            Count := Count + 1;
         end if;
         pragma Loop_Invariant (Count <= Token_Text_Length);
      end loop;
      Valid := Count = Token_Text_Length;
      if not Valid then
         Item := [others => 0];
      end if;
   end Decode_Token_Text;

   ---------------------------------------------------------------------------
   --  Names
   ---------------------------------------------------------------------------
   function Valid_Nonempty_Name (Name : String) return Boolean
     with Pre => Name'Length in 1 .. Maximum_Name_Bytes and then Name'Last < Positive'Last;

   function Valid_Name (Name : String) return Boolean is
   begin
      if Name'Length = 0 then
         return False;
      end if;
      return Valid_Nonempty_Name (Name);
   end Valid_Name;

   function Valid_Nonempty_Name (Name : String) return Boolean is
      Component_First : Positive := Name'First;
   begin
      if Name'Length >= Staging_Suffix'Length
        and then Name (Name'Last - Staging_Suffix'Length + 1 .. Name'Last) = Staging_Suffix
      then
         return False;
      end if;
      for I in Name'Range loop
         pragma Loop_Invariant (Component_First in Name'First .. I);
         if Name (I) = '/' then
            --  The component Name (Component_First .. I - 1) ends here.
            if I = Component_First
              or else Name (Component_First .. I - 1) = "."
              or else Name (Component_First .. I - 1) = ".."
              or else I = Name'Last
            then
               return False;
            end if;
            Component_First := I + 1;
         elsif not Name_Character (Name (I)) then
            return False;
         end if;
      end loop;
      return Name (Component_First .. Name'Last) /= "."
        and then Name (Component_First .. Name'Last) /= "..";
   end Valid_Nonempty_Name;

   function Encode_App_Response
     (Result : App_Result; Current, Previous : Generation; Instance : Instance_State;
      Stop : Stop_Kind; Current_Hash, Previous_Hash : Digest) return App_Response
   is
      Hashes_At : constant := 1 + 2 * Generation_Bytes + 2;
      Data : App_Response := [others => 0];
   begin
      Data (1) := App_Result'Enum_Rep (Result);
      Put_U32 (Data, 2, Current);
      Put_U32 (Data, 2 + Generation_Bytes, Previous);
      Data (2 + 2 * Generation_Bytes) := Instance_State'Enum_Rep (Instance);
      Data (3 + 2 * Generation_Bytes) := Stop_Kind'Enum_Rep (Stop);
      for I in Digest_Index loop
         Data (Hashes_At + I) := Current_Hash (I);
         Data (Hashes_At + Digest_Bytes + I) := Previous_Hash (I);
      end loop;
      return Data;
   end Encode_App_Response;

   function Valid_App_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_App_Name_Bytes
      and then (for all C of Name => C /= '/')
      and then Valid_Name (Name));

   function App_Of_Product (Product : String) return String is
      First : Positive := Positive'First;
   begin
      if Product'Length <= Product_Suffix'Length
        or else Product (Product'Last - Product_Suffix'Length + 1 .. Product'Last) /= Product_Suffix
      then
         return "";
      end if;
      First := Product'First;
      for I in Product'Range loop
         pragma Loop_Invariant (First in Product'First .. I);
         if Product (I) = '/' then
            First := I + 1;
         end if;
      end loop;
      if First > Product'Last - Product_Suffix'Length then
         return "";
      end if;
      declare
         Name : constant String := Product (First .. Product'Last - Product_Suffix'Length);
      begin
         return (if Valid_App_Name (Name) then Name else "");
      end;
   end App_Of_Product;

   ---------------------------------------------------------------------------
   --  Requests
   ---------------------------------------------------------------------------
   procedure Decode_Header
     (Data : Byte_Array; Header : out Request_Header; Result : out Status)
   is
      Op_Code : constant Unsigned_8 := Data (Operation_At);
      Place_Code : constant Unsigned_8 := Data (Place_At);
      Length : constant Unsigned_32 := Get_U32 (Data, Length_At);
   begin
      Header := (others => <>);
      if Data (1 .. Magic_Length) /= Magic or else Data (Place_At + 1) /= 0 then
         Result := Malformed;
         return;
      end if;
      if Data (Version_At) /= Protocol_Version then
         Result := Unsupported_Version;
         return;
      end if;
      if Op_Code not in Operation'Enum_Rep (Operation'First) .. Operation'Enum_Rep (Operation'Last)
        or else Place_Code > Place'Enum_Rep (Place'Last)
      then
         Result := Malformed;
         return;
      end if;
      if Length > Maximum_Request_Body then
         Result := Too_Large;
         return;
      end if;
      for Op in Operation loop
         if Operation'Enum_Rep (Op) = Op_Code then
            Header.Op := Op;
         end if;
      end loop;
      for P in Place loop
         if Place'Enum_Rep (P) = Place_Code then
            Header.Where := P;
         end if;
      end loop;
      for I in Token_Index loop
         Header.Credential (I) := Data (Token_At + I - 1);
      end loop;
      Header.Length := Natural (Length);
      Result := OK;
   end Decode_Header;

   --  Evaluate's source: printable ASCII, tab, CR and LF.
   function Source_Byte (B : Unsigned_8) return Boolean is
     (B in 16#20# .. 16#7E# | 16#09# | 16#0A# | 16#0D#);

   procedure Decode_Body
     (Header : Request_Header; Data : Byte_Array; Item : out Request; Result : out Status)
   is
      Length : constant Natural := Data'Length;
      After : Positive;   --  the first byte after the name

      --  The name at Data (1 .. 2 + its length): sets Item's name and After.
      procedure Take_Name (Empty_Allowed : Boolean; Taken : out Boolean)
        with Global => (Input => (Data, Length), Output => After, In_Out => Item),
             Pre  => Data'First = 1 and then Length = Data'Length,
             Post => (if Taken then After = Name_Length_Bytes + Item.Name_Last + 1 and then
                        After - 1 <= Length) and then
                     (if Taken and then not Empty_Allowed then Item.Name_Last > 0) and then
                     Item.Op = Item.Op'Old and then Item.Where = Item.Where'Old and then
                     Item.Data_Length = Item.Data_Length'Old and then
                     Item.Data_First = Item.Data_First'Old and then Item.Job = Item.Job'Old
      is
         Raw : Natural;
         Text : String (1 .. Maximum_Name_Bytes) := [others => ' '];
      begin
         After := 1;
         Taken := False;
         if Length < Name_Length_Bytes then
            return;
         end if;
         Raw := Natural (Data (1)) * 256 + Natural (Data (2));
         if Raw > Maximum_Name_Bytes or else Raw > Length - Name_Length_Bytes then
            return;
         end if;
         for I in 1 .. Raw loop
            Text (I) := Character'Val (Data (Name_Length_Bytes + I));
         end loop;
         if Raw = 0 then
            if not Empty_Allowed then
               return;
            end if;
         elsif not Valid_Name (Text (1 .. Raw)) then
            return;
         end if;
         Item.Name := Text;
         Item.Name_Last := Raw;
         After := Name_Length_Bytes + Raw + 1;
         Taken := True;
      end Take_Name;

      Taken : Boolean;
      Value : Unsigned_64;
   begin
      Item := (Op => Header.Op, Where => Header.Where, others => <>);
      Result := Malformed;
      case Header.Op is
         when Hello =>
            if Length = 0 then
               Result := OK;
            end if;

         when Evaluate =>
            if Length in 1 .. Maximum_Source_Bytes
              and then (for all I in Data'Range => Source_Byte (Data (I)))
            then
               Item.Data_First := 1;
               Item.Data_Length := Length;
               Result := OK;
            end if;

         when List | Stat | Remove | Build_Start =>
            Take_Name (Empty_Allowed => Header.Op = List, Taken => Taken);
            if Taken and then After - 1 = Length then
               Result := OK;
            end if;

         when App_Replace =>
            Take_Name (Empty_Allowed => False, Taken => Taken);
            if Taken and then After - 1 = Length
              and then App_Of_Product (Item.Name (1 .. Item.Name_Last))'Length > 0
            then
               Result := OK;
            end if;

         when App_Rollback | App_Status =>
            Take_Name (Empty_Allowed => False, Taken => Taken);
            if Taken and then After - 1 = Length
              and then Valid_App_Name (Item.Name (1 .. Item.Name_Last))
            then
               Result := OK;
            end if;

         when Get =>
            Take_Name (Empty_Allowed => False, Taken => Taken);
            if Taken and then Length - (After - 1) = Offset_Bytes then
               Value := Get_U64 (Data, After);
               if Value <= Maximum_File_Bytes then
                  Item.Offset := Value;
                  Result := OK;
               end if;
            end if;

         when Put =>
            Take_Name (Empty_Allowed => False, Taken => Taken);
            if Taken and then Length - (After - 1) >= Offset_Bytes
              and then Length - (After - 1) - Offset_Bytes <= Maximum_Chunk_Bytes
            then
               Value := Get_U64 (Data, After);
               if Value > Maximum_File_Bytes then
                  Result := Malformed;
               elsif Unsigned_64 (Length - (After - 1) - Offset_Bytes) > Maximum_File_Bytes - Value then
                  Result := Too_Large;
               else
                  Item.Offset := Value;
                  Item.Data_Length := Length - (After - 1) - Offset_Bytes;
                  Item.Data_First := (if Item.Data_Length = 0 then 1 else After + Offset_Bytes);
                  Result := OK;
               end if;
            end if;

         when Commit =>
            Take_Name (Empty_Allowed => False, Taken => Taken);
            if Taken and then Length - (After - 1) = Offset_Bytes + Digest_Bytes then
               Value := Get_U64 (Data, After);
               if Value <= Maximum_File_Bytes then
                  Item.Size := Value;
                  for I in Digest_Index loop
                     Item.Hash (I) := Data (After + Offset_Bytes + I - 1);
                  end loop;
                  Result := OK;
               else
                  Result := Too_Large;
               end if;
            end if;

         when Build_Poll =>
            if Length = Job_Bytes + Offset_Bytes then
               Item.Job := Get_U32 (Data, 1);
               Value := Get_U64 (Data, 1 + Job_Bytes);
               if Item.Job /= 0 and then Value <= Maximum_File_Bytes then
                  Item.Offset := Value;
                  Result := OK;
               else
                  Item.Job := 0;
               end if;
            end if;

         when Build_Cancel =>
            if Length = Job_Bytes then
               Item.Job := Get_U32 (Data, 1);
               if Item.Job /= 0 then
                  Result := OK;
               end if;
            end if;

         when System_Activate =>
            Take_Name (Empty_Allowed => False, Taken => Taken);
            if Taken and then Length - (After - 1) > Grace_Bytes + Health_Bytes
              and then Length - (After - 1) - Grace_Bytes - Health_Bytes <= Maximum_Service_Name_Bytes
            then
               declare
                  First : constant Positive := After + Grace_Bytes + Health_Bytes;
                  Service : String (1 .. Length - First + 1);
               begin
                  for I in Service'Range loop
                     Service (I) := Character'Val (Data (First + I - 1));
                  end loop;
                  if Valid_Service_Name (Service) then
                     Item.Grace := Get_U32 (Data, After);
                     Item.Health := Get_U32 (Data, After + Grace_Bytes);
                     Item.Data_First := First;
                     Item.Data_Length := Service'Length;
                     Result := OK;
                  end if;
               end;
            end if;

         when System_Rollback =>
            if Length = Grace_Bytes + Health_Bytes then
               Item.Grace := Get_U32 (Data, 1);
               Item.Health := Get_U32 (Data, 1 + Grace_Bytes);
               Result := OK;
            end if;

         when System_Generations =>
            if Length = 0 then
               Result := OK;
            end if;

         when Capture =>
            if Length = Capture_Request_Bytes then
               Item.Output := Output_Number (Data (1));
               Item.Area_X := Natural (Data (2)) * 256 + Natural (Data (3));
               Item.Area_Y := Natural (Data (4)) * 256 + Natural (Data (5));
               Item.Area_Width := Natural (Data (6)) * 256 + Natural (Data (7));
               Item.Area_Height := Natural (Data (8)) * 256 + Natural (Data (9));
               Result := OK;
            end if;
      end case;
      if Result /= OK then
         Item := (Op => Header.Op, Where => Header.Where, others => <>);
      end if;
   end Decode_Body;

   ---------------------------------------------------------------------------
   --  Responses
   ---------------------------------------------------------------------------
   function Encode_Response_Header (Result : Status; Length : Response_Body_Length)
     return Response_Header
   is
      Header : Response_Header := [others => 0];
   begin
      Header (1 .. Magic_Length) := Magic;
      Header (Version_At) := Protocol_Version;
      Header (Status_At) := Status'Enum_Rep (Result);
      Put_U32 (Header, Length_At, Unsigned_32 (Length));
      return Header;
   end Encode_Response_Header;

end Remote_Wire;
