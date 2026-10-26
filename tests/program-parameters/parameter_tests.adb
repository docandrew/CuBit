--  Hosted tests of a generated typed-parameter binding (package Parameters,
--  from acpi-sample.ccl: the former acpi.startup setting's cases, now a
--  program's parameters). Each value is checked the way a launch handles
--  it: the launcher's CCL check against the program's descriptor
--  (CCL.Typed_Values with its signature) renders the canonical source,
--  and the child's generated Decode reads it. Also: the descriptor and the
--  launch-block section codecs, Value through the launch block, and
--  malformed input (every truncation and one-byte change of a value, every
--  one-byte change of the descriptor).
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Interfaces;
with CCL.Typed_Values;
with CuBit.Parameter_Sections;
with CuBit.Parameter_Signatures;
with Launch_Double;
with Parameters;

procedure Parameter_Tests is
   package P renames Parameters;
   package PS renames CuBit.Parameter_Signatures;
   package PV renames CuBit.Parameter_Sections;
   use type P.ACPI_Deadline_Choice, P.Deadline_Milliseconds_Value, P.Namespace_Node_Count_Value,
     P.Method_Byte_Count_Value;
   use type PS.Validation, PS.Identity;
   use type PV.Validation;
   use type Interfaces.Unsigned_8;
   subtype Wide is Long_Long_Long_Integer;

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  The descriptor ccl-manifest emitted (run.sh extracts it).
   Descriptor : PS.Bytes (1 .. PS.Maximum_Descriptor_Bytes);
   Descriptor_Length : Natural := 0;
   procedure Read_Descriptor is
      use Ada.Streams;
      File : Stream_IO.File_Type;
      Data : Stream_Element_Array (1 .. Stream_Element_Offset (PS.Maximum_Descriptor_Bytes) + 1);
      Last : Stream_Element_Offset;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, "descriptor.bin");
      Stream_IO.Read (File, Data, Last);
      Stream_IO.Close (File);
      Descriptor_Length := Natural (Last);
      for I in 1 .. Descriptor_Length loop
         Descriptor (I) := Interfaces.Unsigned_8 (Data (Stream_Element_Offset (I)));
      end loop;
   end Read_Descriptor;

   function Root return String is (PS.Root_Of (Descriptor (1 .. Descriptor_Length)));
   function Signature return String is (PS.Signature_Of (Descriptor (1 .. Descriptor_Length)));

   --  The launcher's check: "" when refused.
   function Canonical (Source : String) return String is
      Result : CCL.Typed_Values.Check_Result;
   begin
      CCL.Typed_Values.Check (Signature, Root, Source, Result);
      return (if Result.Success then Result.Canonical (1 .. Result.Length) else "");
   end Canonical;
   function Decoded (Source : String) return Boolean is
      Item : P.ACPI_Parameters_Value;
      OK : Boolean;
   begin
      P.Decode (Source, Item, OK);
      return OK;
   end Decoded;

   Deadline_Ms : constant := 30_000;
   Nodes : constant := 512;
   Method_Bytes : constant := 65_536;
   --  The bounds acpi-sample.ccl declares: Integer_64'Last and Positive'Last.
   Minimum : constant := 1;
   Maximum_Deadline : constant Wide := Wide (Interfaces.Integer_64'Last);
   Maximum_Count : constant Wide := Wide (Positive'Last);

   function Image (Value : Wide) return String is
      Text : constant String := Wide'Image (Value);
   begin
      return (if Text (Text'First) = ' ' then Text (Text'First + 1 .. Text'Last) else Text);
   end Image;
   function Within (Milliseconds : Wide) return String is
     ("(ACPI_Deadline.Within " & Image (Milliseconds) & ")");
   function Author (Deadline, Namespace, Methods : String) return String is
     ("(ACPI_Parameters deferred_initialization => " & Deadline & " namespace_nodes => " & Namespace &
      " method_bytes => " & Methods & ")");
   function Author (Deadline : String; Namespace, Methods : Wide) return String is
     (Author (Deadline, Image (Namespace), Image (Methods)));

   procedure Refused (Source, What : String) is
   begin
      Check (Canonical (Source) = "", "launcher refuses " & What);
      Check (not Decoded (Source), "decoder refuses " & What);
   end Refused;
   procedure Accepted (Source, What : String) is
   begin
      Check (Canonical (Source) = Source, "renders as authored: " & What);
      Check (Decoded (Source), "decodes " & What);
   end Accepted;

   --  An independent statement of the root name rule: a letter, then
   --  letters, digits and single underscores, not ending with one.
   function Identifier (Name : String) return Boolean is
     (Name (Name'First) in 'A' .. 'Z' | 'a' .. 'z'
      and then (for all C of Name => C in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_')
      and then Name (Name'Last) /= '_'
      and then (for all K in Name'First .. Name'Last - 1 => Name (K .. K + 1) /= "__"));
   function Mutated_Root (Position : Positive; C : Character) return String is
      Name : String := Root;
   begin
      Name (Position) := C;
      return Name;
   end Mutated_Root;

   Item : P.ACPI_Parameters_Value;
   OK : Boolean;
begin
   Read_Descriptor;
   Check (PS.Validate (Descriptor (1 .. Descriptor_Length)) = PS.Valid, "the descriptor is well formed");
   Check (PS.Identity_Of (Descriptor (1 .. Descriptor_Length)) = P.Identity,
          "the binding's Identity is the descriptor's");
   Check (Root = "ACPI_Parameters", "the descriptor names the parameter record");

   --  The generated range types are the declared bounds (known at compile
   --  time, which is the point).
   pragma Warnings (Off, "condition is always True");
   Check (Wide (P.Deadline_Milliseconds_Value'First) = Minimum
          and then Wide (P.Deadline_Milliseconds_Value'Last) = Maximum_Deadline
          and then Wide (P.Namespace_Node_Count_Value'First) = Minimum
          and then Wide (P.Namespace_Node_Count_Value'Last) = Maximum_Count
          and then Wide (P.Method_Byte_Count_Value'First) = Minimum
          and then Wide (P.Method_Byte_Count_Value'Last) = Maximum_Count,
          "range types carry the declared bounds");
   pragma Warnings (On, "condition is always True");

   --  Round trips.
   declare
      Rendered : constant String := Canonical (Author (Within (Deadline_Ms), Nodes, Method_Bytes));
   begin
      Check (Rendered = Author (Within (Deadline_Ms), Nodes, Method_Bytes), "Within renders as authored: " & Rendered);
      P.Decode (Rendered, Item, OK);
      Check (OK and then Item.deferred_initialization.Choice = P.Within
             and then Item.deferred_initialization.Within_Payload = Deadline_Ms
             and then Item.namespace_nodes = Nodes and then Item.method_bytes = Method_Bytes,
             "Within N round-trips with its fields");
   end;
   declare
      Rendered : constant String := Canonical (Author ("ACPI_Deadline.Forever", Nodes, Method_Bytes));
   begin
      P.Decode (Rendered, Item, OK);
      Check (OK and then Item.deferred_initialization.Choice = P.Forever
             and then Item.namespace_nodes = Nodes and then Item.method_bytes = Method_Bytes,
             "Forever round-trips: " & Rendered);
   end;
   --  Named fields in any order: the launcher renders declaration order.
   Check (Canonical ("(ACPI_Parameters method_bytes => 1 namespace_nodes => 2 " &
                     "deferred_initialization => ACPI_Deadline.Forever)") =
          Author ("ACPI_Deadline.Forever", "2", "1"),
          "the launcher renders fields in declaration order");

   --  Boundaries.
   declare
      Lowest : constant String := Author (Within (Minimum), Minimum, Minimum);
      Highest : constant String := Author (Within (Maximum_Deadline), Maximum_Count, Maximum_Count);
   begin
      Accepted (Lowest, "every minimum");
      P.Decode (Lowest, Item, OK);
      Check (OK and then Wide (Item.deferred_initialization.Within_Payload) = Minimum
             and then Wide (Item.namespace_nodes) = Minimum and then Wide (Item.method_bytes) = Minimum,
             "minimums decode exactly");
      Accepted (Highest, "every maximum");
      P.Decode (Highest, Item, OK);
      Check (OK and then Wide (Item.deferred_initialization.Within_Payload) = Maximum_Deadline
             and then Wide (Item.namespace_nodes) = Maximum_Count
             and then Wide (Item.method_bytes) = Maximum_Count,
             "maximums decode exactly");
   end;
   Refused (Author (Within (Minimum - 1), Nodes, Method_Bytes), "a 0 ms deadline");
   Refused (Author (Within (Maximum_Deadline + 1), Nodes, Method_Bytes), "a deadline past Integer_64");
   Refused (Author (Within (-Deadline_Ms), Nodes, Method_Bytes), "a negative deadline");
   Refused (Author (Within (Deadline_Ms), Minimum - 1, Method_Bytes), "0 namespace nodes");
   Refused (Author (Within (Deadline_Ms), Maximum_Count + 1, Method_Bytes), "namespace nodes past Positive'Last");
   Refused (Author (Within (Deadline_Ms), -Nodes, Method_Bytes), "negative namespace nodes");
   Refused (Author (Within (Deadline_Ms), Nodes, Minimum - 1), "0 method bytes");
   Refused (Author (Within (Deadline_Ms), Nodes, Maximum_Count + 1), "method bytes past Positive'Last");
   Check (not Decoded (Author (Within (Deadline_Ms), Nodes, Maximum_Deadline + 1)),
          "decoder refuses method bytes past Integer_64");

   --  No defaults: a missing field is refused; the decoder leaves the default.
   declare
      Missing : constant String :=
        "(ACPI_Parameters deferred_initialization => ACPI_Deadline.Forever namespace_nodes => " & Image (Nodes) & ")";
   begin
      Refused (Missing, "a missing field");
      P.Decode (Missing, Item, OK);
      Check (not OK and then Item.deferred_initialization.Choice = P.ACPI_Deadline_Choice'First
             and then Item.namespace_nodes = P.Namespace_Node_Count_Value'First,
             "a refused value leaves the default");
      Refused ("(ACPI_Parameters)", "no fields");
   end;

   --  Wrong constructors.
   Refused (Author ("ACPI_Deadline.Never", Nodes, Method_Bytes), "an unknown deadline member");
   Check (not Decoded (Author ("ACPI_Deadline.Within", Nodes, Method_Bytes)), "refuses Within without milliseconds");
   Check (not Decoded (Author ("(ACPI_Deadline.Forever 1)", Nodes, Method_Bytes)), "refuses Forever with a payload");
   Check (not Decoded (Author ("Gateway.None", Nodes, Method_Bytes)), "refuses another type's member");
   Check (not Decoded ("(GPU_Rendering enabled => true)"), "refuses another record type");
   Check (Canonical ("(ACPI_Parameters deferred_initialization => ACPI_Deadline.Forever namespace_nodes => 1 " &
                     "method_bytes => 1 extra => 1)") = "", "launcher refuses an unknown field");

   --  Non-canonical spellings: the decoder reads only the rendering.
   Check (not Decoded ("(ACPI_Parameters namespace_nodes => " & Image (Nodes) & " deferred_initialization => " &
                       "ACPI_Deadline.Forever method_bytes => " & Image (Method_Bytes) & ")"),
          "decoder refuses fields out of declaration order");
   Check (not Decoded (Author ("(ACPI_Deadline.Within 030000)", Nodes, Method_Bytes)), "refuses a leading zero");
   Check (not Decoded (Author ("(ACPI_Deadline.Within  " & Image (Deadline_Ms) & ")", Nodes, Method_Bytes)),
          "refuses a doubled space");
   Check (not Decoded (Author ("Forever", Nodes, Method_Bytes)), "refuses an unqualified member");
   Check (not Decoded (Author (Within (Deadline_Ms), "-0", Image (Method_Bytes))), "refuses a signed zero");

   --  Malformed input: every proper prefix of a canonical value, and every
   --  one-byte change, is refused or decodes to a value whose rendering is
   --  exactly the changed source (Decode is total: it never raises).
   declare
      Good : constant String := Author (Within (Deadline_Ms), Nodes, Method_Bytes);
      Mutant : String := Good;
      Survivors : Natural := 0;
   begin
      for Last in Good'First - 1 .. Good'Last - 1 loop
         Check (not Decoded (Good (Good'First .. Last)), "refuses a truncation to" & Natural'Image (Last));
      end loop;
      for I in Good'Range loop
         for C in Character'Val (0) .. Character'Val (255) loop
            if C /= Good (I) then
               Mutant (I) := C;
               if Decoded (Mutant) then
                  Survivors := Survivors + 1;
                  Check (Canonical (Mutant) = Mutant, "an accepted mutant is canonical: " & Mutant);
               end if;
            end if;
         end loop;
         Mutant (I) := Good (I);
      end loop;
      Put_Line ("program-parameters: one-byte mutants that are other valid values:" & Natural'Image (Survivors));
   end;

   --  The descriptor: every one-byte change is refused, except in the
   --  identity (any 32 bytes) and within the signature's printable text.
   declare
      Copy : PS.Bytes (1 .. Descriptor_Length) := Descriptor (1 .. Descriptor_Length);
      Wrong : Natural := 0;
   begin
      for I in Copy'Range loop
         for B in Interfaces.Unsigned_8 loop
            if B /= Descriptor (I) then
               Copy (I) := B;
               declare
                  In_Identity : constant Boolean := I in PS.Identity_Offset + 1 .. PS.Identity_Offset + PS.Identity_Bytes;
                  In_Text : constant Boolean := I > PS.Header_Bytes + Root'Length and then PS.Printable (B);
                  In_Root : constant Boolean := I in PS.Header_Bytes + 1 .. PS.Header_Bytes + Root'Length
                    and then Identifier (Mutated_Root (I - PS.Header_Bytes, Character'Val (B)));
                  Valid : constant Boolean := PS.Validate (Copy) = PS.Valid;
               begin
                  if Valid /= (In_Identity or else In_Text or else In_Root) then
                     Wrong := Wrong + 1;
                  end if;
               end;
            end if;
         end loop;
         Copy (I) := Descriptor (I);
      end loop;
      Check (Wrong = 0, "every one-byte change of the descriptor is judged exactly;" & Natural'Image (Wrong) & " wrong");
      for Last in 0 .. Descriptor_Length - 1 loop
         Check (PS.Validate (Descriptor (1 .. Last)) /= PS.Valid, "refuses a descriptor cut to" & Natural'Image (Last));
      end loop;
   end;

   --  Value through the launch block: the section carries the identity and
   --  the canonical source; anything else stops the program.
   declare
      Section : PV.Bytes (1 .. PV.Maximum_Section_Bytes);
      Length : PV.Section_Length;
      Fits : Boolean;
      Source : constant String := Author (Within (Deadline_Ms), Nodes, Method_Bytes);
      procedure Place (Item : PV.Bytes) is
      begin
         Launch_Double.Present := True;
         Launch_Double.Trailer (1 .. Item'Length) := Item;
         Launch_Double.Trailer_Length := Item'Length;
      end Place;
      function Stops return Boolean is
      begin
         declare
            Got : constant P.ACPI_Parameters_Value := P.Value;
            pragma Unreferenced (Got);
         begin
            return False;
         end;
      exception
         when E : Launch_Double.Refused =>
            Put_Line ("  stopped: " & Ada.Exceptions.Exception_Message (E));
            return True;
      end Stops;
      Other : PS.Identity := P.Identity;
   begin
      Check (Stops, "Value stops a process without a launch block");
      Launch_Double.Present := True;
      Launch_Double.Trailer_Length := 0;
      Check (Stops, "Value stops a process launched without parameters");
      PV.Encode (P.Identity, Source, Section, Length, Fits);
      Check (Fits and then PV.Validate (Section (1 .. Length)) = PV.Valid, "a section encodes");
      Place (Section (1 .. Length));
      Item := P.Value;
      Check (Item.deferred_initialization.Choice = P.Within and then Item.namespace_nodes = Nodes
             and then Item.method_bytes = Method_Bytes, "Value reads the launch block's value");
      Other (1) := Interfaces."xor" (Other (1), 1);
      PV.Encode (Other, Source, Section, Length, Fits);
      Place (Section (1 .. Length));
      Check (Stops, "Value stops on a value checked against another signature");
      PV.Encode (P.Identity, Author (Within (Deadline_Ms), Nodes, Minimum - 1), Section, Length, Fits);
      Place (Section (1 .. Length));
      Check (Stops, "Value stops on an out-of-range value");
      PV.Encode (P.Identity, "", Section, Length, Fits);
      Check (not Fits and then Length = 0, "an empty value does not encode");
      PV.Encode (P.Identity, Source & ASCII.LF, Section, Length, Fits);
      Check (not Fits, "a value with a control character does not encode");
   end;

   --  The size limit: a value whose canonical source is over the launch
   --  block's budget for it is refused, saying so.
   declare
      Result : CCL.Typed_Values.Check_Result;
      Long : constant String (1 .. PV.Maximum_Value_Bytes) := [others => 'x'];
   begin
      CCL.Typed_Values.Check ("(type Note (record (text String)))", "Note", "(Note text => """ & Long & """)", Result);
      Check (not Result.Success
             and then Result.Message (1 .. Result.Message_Length) =
               "its canonical source is" & Natural'Image (Long'Length + 17) &
               " bytes, over the" & Natural'Image (PV.Maximum_Value_Bytes) & "-byte limit",
             "an over-long value is refused with the limit: " & Result.Message (1 .. Result.Message_Length));
   end;

   if Failures = 0 then
      Put_Line ("program-parameters: PASS" & Natural'Image (Checks) & " checks");
   else
      Put_Line ("program-parameters: FAIL" & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      raise Program_Error;
   end if;
end Parameter_Tests;
