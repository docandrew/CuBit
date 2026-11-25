pragma Ada_2022;

package body CuBit.Child_Specs with SPARK_Mode is

   Byte_Bits : constant := 8;
   Sign_Bit  : constant Unsigned_64 := 2 ** 63;

   --  Two's complement, both ways, without overflow.
   function To_Unsigned (Value : Integer_64) return Unsigned_64;
   function To_Integer (Word : Unsigned_64) return Integer_64;

   function To_Unsigned (Value : Integer_64) return Unsigned_64 is
     (if Value >= 0 then Unsigned_64 (Value)
      else Unsigned_64 (Value + Integer_64'Last + 1) or Sign_Bit);
   function To_Integer (Word : Unsigned_64) return Integer_64 is
     (if Word < Sign_Bit then Integer_64 (Word)
      else Integer_64 (Word - Sign_Bit) - Integer_64'Last - 1);

   function Encoded_Length (S : Spec) return Spec_Length is
      Total : Natural := Header_Bytes + S.Name_Bytes + S.Value_Bytes;
   begin
      for I in 1 .. S.Facts loop
         pragma Loop_Invariant
           (Total <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                     + (I - 1) * (Fact_Overhead + Maximum_Field_Bytes));
         Total := Total + Fact_Overhead + S.Fact_List (I).Field.Length;
      end loop;
      pragma Assert
        (Total <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                  + Maximum_Facts * (Fact_Overhead + Maximum_Field_Bytes));
      for I in 1 .. S.Bindings loop
         pragma Loop_Invariant
           (Total <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                     + Maximum_Facts * (Fact_Overhead + Maximum_Field_Bytes)
                     + (I - 1) * (Binding_Overhead + Maximum_Field_Bytes));
         Total := Total + Binding_Overhead + S.Binding_List (I).Label.Length;
      end loop;
      return Total + S.Dependencies * 8;
   end Encoded_Length;

   procedure Encode (S : Spec; Bytes : out Byte_Array; Length : out Spec_Length) is
      Next : Natural := 0;

      procedure Put (Value : Unsigned_64; Count : Positive)
        with Pre  => Bytes'First = 1 and then Bytes'Last = Maximum_Spec_Bytes
                     and then Count <= 8 and then Next <= Maximum_Spec_Bytes - Count,
             Post => Next = Next'Old + Count;

      procedure Put (Value : Unsigned_64; Count : Positive) is
         V : Unsigned_64 := Value;
      begin
         for K in 1 .. Count loop
            pragma Loop_Invariant (Next = Next'Loop_Entry + K - 1);
            Next := Next + 1;
            Bytes (Next) := Unsigned_8 (V and 16#FF#);
            V := Shift_Right (V, Byte_Bits);
         end loop;
      end Put;

      procedure Put_Text (Text : String)
        with Pre  => Bytes'First = 1 and then Bytes'Last = Maximum_Spec_Bytes
                     and then Text'Length <= Maximum_Spec_Bytes and then Next <= Maximum_Spec_Bytes - Text'Length,
             Post => Next = Next'Old + Text'Length;

      procedure Put_Text (Text : String) is
      begin
         for K in Text'Range loop
            pragma Loop_Invariant (Next = Next'Loop_Entry + (K - Text'First));
            Next := Next + 1;
            Bytes (Next) := Character'Pos (Text (K));
         end loop;
      end Put_Text;
   begin
      Bytes := [others => 0];
      Put (Unsigned_64 (Magic), 4);
      Put (Spec_Version, 2);
      Put (Restart_Kind'Enum_Rep (S.Restart), 1);
      Put (Child_Kind'Enum_Rep (S.Kind), 1);
      Put (Supervisor_Choice'Enum_Rep (S.Supervisor), 1);
      Put (0, 1);   --  reserved
      Put (Parameter_Source'Enum_Rep (S.Source), 1);
      Put (Unsigned_64 (S.Facts), 1);
      Put (Unsigned_64 (S.Bindings), 1);
      Put (Unsigned_64 (S.Dependencies), 1);
      Put (Start_Mode'Enum_Rep (S.Start), 1);
      Put (Sink_Kind'Enum_Rep (S.Sink), 1);
      Put (Unsigned_64 (S.Stop_Grace), 4);
      Put (Unsigned_64 (S.Intensity), 2);
      Put (Stop_Stage'Enum_Rep (S.Stage), 1);
      Put (0, 1);
      Put (Unsigned_64 (S.Window), 4);
      Put (Unsigned_64 (S.Name_Bytes), 2);
      Put (Unsigned_64 (S.Value_Bytes), 2);
      Put (S.Sink_Badge, 8);
      pragma Assert (Next = Header_Bytes);
      Put_Text (S.Name (1 .. S.Name_Bytes));
      Put_Text (S.Value (1 .. S.Value_Bytes));
      pragma Assert (Next <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes);
      for I in 1 .. S.Facts loop
         pragma Loop_Invariant
           (Next <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                    + (I - 1) * (Fact_Overhead + Maximum_Field_Bytes));
         Put (Unsigned_64 (S.Fact_List (I).Field.Length), 1);
         Put_Text (S.Fact_List (I).Field.Data (1 .. S.Fact_List (I).Field.Length));
         Put (To_Unsigned (S.Fact_List (I).Value), 8);
      end loop;
      pragma Assert
        (Next <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                 + Maximum_Facts * (Fact_Overhead + Maximum_Field_Bytes));
      for I in 1 .. S.Bindings loop
         pragma Loop_Invariant
           (Next <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                    + Maximum_Facts * (Fact_Overhead + Maximum_Field_Bytes)
                    + (I - 1) * (Binding_Overhead + Maximum_Field_Bytes));
         Put (Unsigned_64 (S.Binding_List (I).Label.Length), 1);
         Put_Text (S.Binding_List (I).Label.Data (1 .. S.Binding_List (I).Label.Length));
         Put (S.Binding_List (I).Badge, 8);
         Put (S.Binding_List (I).Provider, 8);
      end loop;
      pragma Assert
        (Next <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                 + Maximum_Facts * (Fact_Overhead + Maximum_Field_Bytes)
                 + Maximum_Bindings * (Binding_Overhead + Maximum_Field_Bytes));
      for I in 1 .. S.Dependencies loop
         pragma Loop_Invariant
           (Next <= Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
                    + Maximum_Facts * (Fact_Overhead + Maximum_Field_Bytes)
                    + Maximum_Bindings * (Binding_Overhead + Maximum_Field_Bytes)
                    + (I - 1) * 8);
         Put (S.Dependency_List (I), 8);
      end loop;
      Length := Next;
   end Encode;

   procedure Decode (Bytes : Byte_Array; S : out Spec; Status : out Decode_Status) is
      Last : constant Natural := Bytes'Length;
      At_Byte : Natural := 0;   --  bytes consumed

      function Get (Count : Positive) return Unsigned_64
        with Pre => Bytes'First = 1 and then Last = Bytes'Last
                    and then Count <= 8 and then At_Byte <= Last and then Count <= Last - At_Byte;

      function Get (Count : Positive) return Unsigned_64 is
         V : Unsigned_64 := 0;
      begin
         for K in reverse 1 .. Count loop
            V := Shift_Left (V, Byte_Bits) or Unsigned_64 (Bytes (At_Byte + K));
         end loop;
         return V;
      end Get;

      --  Take Count bytes as a number (nothing when they are not there).
      procedure Take (Count : Positive; Value : out Unsigned_64; Ok : out Boolean)
        with Pre  => Bytes'First = 1 and then Last = Bytes'Last and then Count <= 8 and then At_Byte <= Last,
             Post => At_Byte <= Last and then (if Ok then At_Byte = At_Byte'Old + Count);

      procedure Take (Count : Positive; Value : out Unsigned_64; Ok : out Boolean) is
      begin
         Value := 0;
         Ok := Count <= Last - At_Byte;
         if Ok then
            Value := Get (Count);
            At_Byte := At_Byte + Count;
         end if;
      end Take;

      --  Take Length bytes as characters into Into (1 .. Length).
      procedure Take_Text (Into : in out String; Length : Natural; Ok : out Boolean)
        with Pre  => Bytes'First = 1 and then Last = Bytes'Last
                     and then Into'First = 1 and then Length <= Into'Length and then At_Byte <= Last,
             Post => At_Byte <= Last;

      procedure Take_Text (Into : in out String; Length : Natural; Ok : out Boolean) is
      begin
         Ok := Length <= Last - At_Byte;
         if not Ok then
            return;
         end if;
         for K in 1 .. Length loop
            Into (K) := Character'Val (Bytes (At_Byte + K));
         end loop;
         At_Byte := At_Byte + Length;
      end Take_Text;

      Word : Unsigned_64;
      Ok : Boolean;
      Counts : array (1 .. 3) of Unsigned_64 := [others => 0];
      Name_Count, Value_Count : Unsigned_64;
   begin
      S := (others => <>);
      Status := Too_Short;
      if Last < Header_Bytes + 1 then
         return;
      end if;
      Take (4, Word, Ok);
      if Word /= Unsigned_64 (Magic) then
         Status := Bad_Magic;
         return;
      end if;
      Take (2, Word, Ok);
      if Word /= Spec_Version then
         Status := Bad_Version;
         return;
      end if;

      Status := Bad_Enumeration;
      Take (1, Word, Ok);
      case Word is
         when 1 => S.Restart := Temporary;
         when 2 => S.Restart := Transient;
         when 3 => S.Restart := Permanent;
         when others =>
            return;
      end case;
      Take (1, Word, Ok);
      case Word is
         when 1 => S.Kind := Worker;
         when 2 => S.Kind := Supervisor;
         when others =>
            return;
      end case;
      Take (1, Word, Ok);
      if Word /= 1 then
         return;
      end if;
      S.Supervisor := Caller;
      Take (1, Word, Ok);
      if Word /= 0 then
         Status := Bad_Reserved; return;
      end if;
      Take (1, Word, Ok);
      case Word is
         when 0 => S.Source := No_Parameters;
         when 1 => S.Source := Given;
         when 2 => S.Source := Device_Class;
         when others =>
            return;
      end case;

      Status := Bad_Count;
      for K in Counts'Range loop
         pragma Loop_Invariant (At_Byte <= Last);
         Take (1, Counts (K), Ok);
      end loop;
      if Counts (1) > Maximum_Facts or else Counts (2) > Maximum_Bindings
        or else Counts (3) > Maximum_Dependencies
      then
         return;
      end if;
      S.Facts := Fact_Count (Counts (1));
      S.Bindings := Binding_Count (Counts (2));
      S.Dependencies := Dependency_Count (Counts (3));

      Status := Bad_Enumeration;
      Take (1, Word, Ok);
      case Word is
         when 1 => S.Start := Run_At_Once;
         when 2 => S.Start := Held;
         when others =>
            return;
      end case;
      Take (1, Word, Ok);
      case Word is
         when 0 => S.Sink := No_Sink;
         when 1 => S.Sink := Write_Only_Sink;
         when others =>
            return;
      end case;
      Status := Bad_Number;
      Take (4, Word, Ok);
      if Word not in 1 .. Maximum_Grace_Ms then
         return;
      end if;
      S.Stop_Grace := Grace_Ms (Word);
      Take (2, Word, Ok);
      if Word > Maximum_Intensity then
         return;
      end if;
      S.Intensity := Intensity_Count (Word);
      Status := Bad_Enumeration;
      Take (1, Word, Ok);
      case Word is
         when 0 => S.Stage := Services;
         when 1 => S.Stage := Apps;
         when 2 => S.Stage := Power_Tail;
         when others =>
            return;
      end case;
      Take (1, Word, Ok);
      if Word /= 0 then
         Status := Bad_Reserved;
         return;
      end if;
      Status := Bad_Number;
      Take (4, Word, Ok);
      if Word > Maximum_Window_Ms then
         return;
      end if;
      S.Window := Window_Ms (Word);

      Status := Bad_Length;
      Take (2, Name_Count, Ok);
      Take (2, Value_Count, Ok);
      if Name_Count not in 1 .. Maximum_Name_Bytes or else Value_Count > Maximum_Value_Bytes then
         return;
      end if;
      S.Name_Bytes := Name_Length (Name_Count);
      S.Value_Bytes := Value_Length (Value_Count);
      Take (8, S.Sink_Badge, Ok);
      Take_Text (S.Name, S.Name_Bytes, Ok);
      if not Ok then
         return;
      end if;
      Take_Text (S.Value, S.Value_Bytes, Ok);
      if not Ok then
         return;
      end if;

      for I in 1 .. S.Facts loop
         pragma Loop_Invariant (At_Byte <= Last);
         Take (1, Word, Ok);
         if not Ok or else Word > Maximum_Field_Bytes then
            return;
         end if;
         S.Fact_List (I).Field.Length := Field_Length (Word);
         Take_Text (S.Fact_List (I).Field.Data, S.Fact_List (I).Field.Length, Ok);
         if not Ok then
            return;
         end if;
         Take (8, Word, Ok);
         if not Ok then
            return;
         end if;
         S.Fact_List (I).Value := To_Integer (Word);
      end loop;
      for I in 1 .. S.Bindings loop
         pragma Loop_Invariant (At_Byte <= Last);
         Take (1, Word, Ok);
         if not Ok or else Word > Maximum_Field_Bytes then
            return;
         end if;
         S.Binding_List (I).Label.Length := Field_Length (Word);
         Take_Text (S.Binding_List (I).Label.Data, S.Binding_List (I).Label.Length, Ok);
         if not Ok then
            return;
         end if;
         Take (8, Word, Ok);
         if not Ok then
            return;
         end if;
         S.Binding_List (I).Badge := Word;
         Take (8, Word, Ok);
         if not Ok then
            return;
         end if;
         S.Binding_List (I).Provider := Word;
      end loop;
      for I in 1 .. S.Dependencies loop
         pragma Loop_Invariant (At_Byte <= Last);
         Take (8, Word, Ok);
         if not Ok then
            return;
         end if;
         S.Dependency_List (I) := Word;
      end loop;
      if At_Byte /= Last then
         return;
      end if;
      if not Valid (S) then
         Status := Bad_Text;
         return;
      end if;
      Status := Decoded;
   end Decode;

   function Refusal_Of (Word : Unsigned_64) return Refusal is
   begin
      for Why in Refusal loop
         if Refusal'Enum_Rep (Why) = Word then
            return Why;
         end if;
      end loop;
      return Malformed_Request;
   end Refusal_Of;

   function Refusal_Name (Why : Refusal) return String is
     (case Why is
        when Malformed_Request    => "MALFORMED_REQUEST",
        when Grant_Unavailable    => "GRANT_UNAVAILABLE",
        when Malformed_Spec       => "MALFORMED_SPEC",
        when Not_Granted          => "NOT_GRANTED",
        when Class_Not_Granted    => "CLASS_NOT_GRANTED",
        when No_Class_Block       => "NO_CLASS_BLOCK",
        when Facts_Mismatch       => "FACTS_MISMATCH",
        when Parameters_Too_Long  => "PARAMETERS_TOO_LONG",
        when Bindings_Not_Granted => "BINDINGS_NOT_GRANTED",
        when Dependency_Unknown   => "DEPENDENCY_UNKNOWN",
        when Supervision_Capacity => "SUPERVISION_CAPACITY",
        when Launch_Refused       => "LAUNCH_REFUSED",
        when Sink_Not_Granted     => "SINK_NOT_GRANTED");

   function Status_Name (Status : Decode_Status) return String is
     (case Status is
        when Decoded         => "DECODED",
        when Too_Short       => "TOO_SHORT",
        when Bad_Magic       => "BAD_MAGIC",
        when Bad_Version     => "BAD_VERSION",
        when Bad_Enumeration => "BAD_ENUMERATION",
        when Bad_Number      => "BAD_NUMBER",
        when Bad_Reserved    => "BAD_RESERVED",
        when Bad_Length      => "BAD_LENGTH",
        when Bad_Text        => "BAD_TEXT",
        when Bad_Count       => "BAD_COUNT");
end CuBit.Child_Specs;
