pragma Ada_2022;
package body HID_Input_Values with SPARK_Mode is
   use type Schema.Status;
   use type Schema.Byte;
   use type Schema.Report_ID;
   use type Schema.Report_Kind;
   use type Schema.Bit_Count;
   use type Schema.Word;
   use type Schema.Wide;
   Bits_Per_Byte : constant := 8;
   Maximum_Scalar_Bits : constant := 32;
   Constant_Flag : constant Schema.Word := 1;
   Null_State_Flag : constant Schema.Word := 2 ** 6;
   type Signed_Wide is range -(2 ** 32) .. 2 ** 33;
   procedure Read_Element
     (Data : Schema.Bytes; Layout : Schema.Result; Field : Schema.Field_Span;
      Element : Schema.Bit_Count; Value : out Schema.Numeric_Value;
      Outcome : out Status)
   is
      Prefix_Bytes : constant Natural := (if Layout.Has_Report_IDs then 1 else 0);
      Bits, Required, Start, Finish, Raw, Weight : Schema.Wide;
      Number : Signed_Wide;
      Position, Index, Offset_Bytes : Natural;
      Byte_Offset : Schema.Wide;
   begin
      Value := 0; Outcome := Invalid_Schema;
      if Layout.Outcome /= Schema.Decoded then return; end if;
      if Field.Kind /= Schema.Input then Outcome := Not_Input; return; end if;
      if (Field.Flags and Constant_Flag) /= 0 then Outcome := Constant_Field; return; end if;
      if Field.Size not in 1 .. Maximum_Scalar_Bits then Outcome := Unsupported_Width; return; end if;
      if Element >= Field.Elements then Outcome := Element_Out_Of_Range; return; end if;
      if not Layout.Present (Schema.Input, Field.ID) or else
         (Layout.Has_Report_IDs and then Field.ID = 0) or else
         (not Layout.Has_Report_IDs and then Field.ID /= 0) or else
         Field.Logical_Minimum > Field.Logical_Maximum
      then Outcome := Invalid_Field_Range; return; end if;
      Bits := Schema.Wide (Layout.Bits (Schema.Input, Field.ID));
      Required := (Bits + Bits_Per_Byte - 1) / Bits_Per_Byte + Schema.Wide (Prefix_Bytes);
      if Schema.Wide (Data'Length) /= Required then Outcome := Invalid_Length; return; end if;
      if Prefix_Bytes /= 0 and then Data (Data'First) /= Schema.Byte (Field.ID) then
         Outcome := Report_ID_Mismatch; return;
      end if;
      Start := Schema.Wide (Field.Offset) + Schema.Wide (Element) * Schema.Wide (Field.Size);
      Finish := Start + Schema.Wide (Field.Size);
      if Finish > Bits or else
         Schema.Wide (Field.Offset) + Schema.Wide (Field.Elements) * Schema.Wide (Field.Size) > Bits
      then Outcome := Invalid_Field_Range; return; end if;
      Raw := 0; Weight := 1;
      for N in Natural range 0 .. Natural (Field.Size) - 1 loop
         Position := Natural (Start + Schema.Wide (N));
         Byte_Offset := Schema.Wide (Prefix_Bytes) + Schema.Wide (Position / Bits_Per_Byte);
         if Data'Length = 0 or else
            Byte_Offset > Schema.Wide (Data'Last) - Schema.Wide (Data'First)
         then Outcome := Invalid_Field_Range; return; end if;
         if Byte_Offset > Schema.Wide (Natural'Last) then
            Outcome := Invalid_Field_Range; return;
         end if;
         Offset_Bytes := Natural (Byte_Offset);
         if Offset_Bytes > Natural'Last - Data'First then
            Outcome := Invalid_Field_Range; return;
         end if;
         Index := Data'First + Offset_Bytes;
         if Index not in Data'Range or else Weight > 2 ** (Maximum_Scalar_Bits - 1) then
            Outcome := Invalid_Field_Range; return;
         end if;
         if (Schema.Wide (Data (Index)) /
             (2 ** (Position mod Bits_Per_Byte))) mod 2 /= 0
         then Raw := Raw + Weight; end if;
         Weight := Weight * 2;
         pragma Loop_Invariant (Weight <= 2 ** Maximum_Scalar_Bits);
         pragma Loop_Invariant (Raw < Weight);
      end loop;
      Number := Signed_Wide (Raw);
      if Field.Logical_Minimum < 0 and then Raw >= Weight / 2 then
         Number := Number - Signed_Wide (Weight);
      end if;
      if Number < Signed_Wide (Field.Logical_Minimum) or else Number > Signed_Wide (Field.Logical_Maximum) then
         Outcome := (if (Field.Flags and Null_State_Flag) /= 0 then Null_Value else Value_Outside_Logical_Range);
         return;
      end if;
      Value := Schema.Numeric_Value (Number); Outcome := Decoded;
   end Read_Element;
end HID_Input_Values;
