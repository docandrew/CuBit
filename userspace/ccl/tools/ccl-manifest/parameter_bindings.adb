------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with Type_Codecs;

package body Parameter_Bindings is
   use Ada.Strings.Unbounded;
   use type Interfaces.Unsigned_8;

   LF : constant Character := ASCII.LF;
   Hex : constant String := "0123456789ABCDEF";
   Readers : constant String := Type_Codecs.Readers;
   Header : constant String := "--  Generated with the ELF manifest; do not edit." & LF;

   procedure Write (Path, Text : String) is
      Output : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Put (Output, Text);
      Ada.Text_IO.Close (Output);
   end Write;

   function Byte_Image (Item : Interfaces.Unsigned_8) return String is
     ("16#" & Hex (Natural (Item / 16) + 1) & Hex (Natural (Item mod 16) + 1) & "#");

   function Digest_Text (Digest : CuBit.Parameter_Signatures.Identity) return String is
      Result : Unbounded_String;
   begin
      for B of Digest loop
         Append (Result, Hex (Natural (B / 16) + 1) & Hex (Natural (B mod 16) + 1));
      end loop;
      return To_String (Result);
   end Digest_Text;

   --  The Identity aggregate, eight bytes a line.
   function Digest_Aggregate (Digest : CuBit.Parameter_Signatures.Identity) return String is
      Per_Line : constant := 8;
      Result : Unbounded_String := To_Unbounded_String ("     [");
   begin
      for I in Digest'Range loop
         Append (Result, Byte_Image (Digest (I)));
         if I = Digest'Last then
            Append (Result, "];");
         elsif I mod Per_Line = 0 then
            Append (Result, "," & LF & "      ");
         else
            Append (Result, ", ");
         end if;
      end loop;
      return To_String (Result);
   end Digest_Aggregate;

   --  The signature as comment lines: one declaration a line, a long one
   --  broken at a space.
   function Signature_Comment (Signature : String) return String is
      Marker : constant String := " (type ";
      Indent : constant String := "--    ";
      Continued : constant String := "--      ";
      Line_Limit : constant := 100;
      Result : Unbounded_String := To_Unbounded_String (Indent);
      Column : Natural := Indent'Length;
   begin
      for I in Signature'Range loop
         if Signature'Last - I + 1 >= Marker'Length and then Signature (I .. I + Marker'Length - 1) = Marker then
            Append (Result, LF & Indent);
            Column := Indent'Length;
         elsif Signature (I) = ' ' and then Column >= Line_Limit then
            Append (Result, LF & Continued);
            Column := Continued'Length;
         else
            Append (Result, Signature (I));
            Column := Column + 1;
         end if;
      end loop;
      return To_String (Result) & LF;
   end Signature_Comment;

   procedure Emit
     (Spec_Path, Program, Root_Name, Signature : String;
      Digest : CuBit.Parameter_Signatures.Identity;
      Success : out Boolean; Detail : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Body_Path : constant String := Spec_Path (Spec_Path'First .. Spec_Path'Last - 1) & "b";
      Item : Type_Codecs.Generated;
   begin
      Success := True;
      Detail := Null_Unbounded_String;
      if Root_Name'Length = 0 then
         Write (Spec_Path,
                Header &
                "--  " & Program & " declares no typed parameters (docs/ccl-launch-parameters.md):" & LF &
                "--  it accepts no arguments, and procmgr refuses a launch that supplies any." & LF &
                "pragma Ada_2022;" & LF & LF &
                "package Parameters with SPARK_Mode, Pure, Elaborate_Body is" & LF &
                "end Parameters;" & LF);
         Write (Body_Path,
                Header &
                "pragma Ada_2022;" & LF & LF &
                "package body Parameters with SPARK_Mode is" & LF &
                "end Parameters;" & LF);
         return;
      end if;
      Type_Codecs.Generate (Signature, Root_Name, Item, Success, Detail);
      if not Success then
         return;
      end if;
      declare
         Root_Type : constant String := To_String (Item.Root_Type);
         Root_Default : constant String := To_String (Item.Root_Default);
      begin
         Write
           (Spec_Path,
            Header &
            "--  Typed parameters of " & Program & " (docs/ccl-launch-parameters.md): one" & LF &
            "--  " & Root_Name & ", given by its launcher and fixed for the process's lifetime." & LF &
            "--  Signature (.cubit.parameters), SHA-256" & LF &
            "--  " & Digest_Text (Digest) & ":" & LF &
            Signature_Comment (Signature) &
            "pragma Ada_2022;" & LF &
            (if Item.Integer_Field then "with Interfaces;" & LF else "") &
            "with " & Readers & ";" & LF &
            "with CuBit.Parameter_Signatures;" & LF & LF &
            "package Parameters with SPARK_Mode is" & LF &
            "   subtype Text is " & Readers & ".Text;" & LF &
            To_String (Item.Spec) & LF &
            "   --  The signature's identity: the SHA-256 of its text. procmgr hands over" & LF &
            "   --  only a value checked against the signature with this identity." & LF &
            "   Identity : constant CuBit.Parameter_Signatures.Identity :=" & LF &
            Digest_Aggregate (Digest) & LF & LF &
            "   --  Item is the value when Success: Source is exactly the canonical" & LF &
            "   --  source of a " & Root_Name & ". Otherwise Item is " & Root_Default & "." & LF &
            "   procedure Decode (Source : String; Item : out " & Root_Type & "; Success : out Boolean)" & LF &
            "   with Global => null, Always_Terminates;" & LF & LF &
            "   --  The value this process was launched with: read from its launch block" & LF &
            "   --  (CuBit.Launch_Parameters) and decoded. The block never changes, so" & LF &
            "   --  every call returns the same value; one without a valid value stops" & LF &
            "   --  the program, saying why." & LF &
            "   function Value return " & Root_Type & LF &
            "   with SPARK_Mode => Off;" & LF & LF &
            "   --  Value, for its check only: the Startup package calls it while it" & LF &
            "   --  elaborates, so a program without valid parameters stops before main." & LF &
            "   procedure Require" & LF &
            "   with SPARK_Mode => Off;" & LF &
            "end Parameters;" & LF);
         Write
           (Body_Path,
            Header &
            "pragma Ada_2022;" & LF &
            (if Item.Integer_Read and then not Item.Integer_Field then "with Interfaces;" & LF else "") &
            "with CuBit.Launch_Parameters;" & LF &
            "with CuBit.Parameter_Sections;" & LF & LF &
            "package body Parameters with SPARK_Mode is" & LF &
            "   package R renames " & Readers & ";" & LF & LF &
            To_String (Item.Bodies) &
            "   procedure Decode (Source : String; Item : out " & Root_Type & "; Success : out Boolean) is" & LF &
            "   begin" & LF &
            "      Item := " & Root_Default & ";" & LF &
            "      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;" & LF &
            "      if not Success then" & LF &
            "         return;" & LF &
            "      end if;" & LF &
            "      declare" & LF &
            "         Position : Positive := Source'First;" & LF &
            "      begin" & LF &
            "         " & To_String (Item.Root_Reader) & " (Source, Position, Success, Item);" & LF &
            "         R.Finish (Source, Position, Success);" & LF &
            "      end;" & LF &
            "      if not Success then" & LF &
            "         Item := " & Root_Default & ";" & LF &
            "      end if;" & LF &
            "   end Decode;" & LF & LF &
            "   function Value return " & Root_Type & LF &
            "   with SPARK_Mode => Off" & LF &
            "   is" & LF &
            "      use type CuBit.Launch_Parameters.Outcome;" & LF &
            "      Source : String (1 .. CuBit.Parameter_Sections.Maximum_Value_Bytes);" & LF &
            "      Length : Natural;" & LF &
            "      Found : CuBit.Launch_Parameters.Outcome;" & LF &
            "      Item : " & Root_Type & ";" & LF &
            "      Decoded : Boolean;" & LF &
            "   begin" & LF &
            "      CuBit.Launch_Parameters.Get (Identity, Source, Length, Found);" & LF &
            "      if Found /= CuBit.Launch_Parameters.Found then" & LF &
            "         CuBit.Launch_Parameters.Refuse (""" & Program & """, Found);" & LF &
            "      end if;" & LF &
            "      Decode (Source (1 .. Length), Item, Decoded);" & LF &
            "      if not Decoded then" & LF &
            "         CuBit.Launch_Parameters.Refuse (""" & Program & """, CuBit.Launch_Parameters.Undecodable_Value);" &
            LF &
            "      end if;" & LF &
            "      return Item;" & LF &
            "   end Value;" & LF & LF &
            "   procedure Require" & LF &
            "   with SPARK_Mode => Off" & LF &
            "   is" & LF &
            "      Checked : constant " & Root_Type & " := Value;" & LF &
            "      pragma Unreferenced (Checked);" & LF &
            "   begin" & LF &
            "      null;" & LF &
            "   end Require;" & LF &
            "end Parameters;" & LF);
      end;
   end Emit;
end Parameter_Bindings;
