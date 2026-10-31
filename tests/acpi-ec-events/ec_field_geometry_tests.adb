with Ada.Text_IO;
with Field_Plan_Tests;
with AML_Decode;
with AML_Field_Layout;
with AML_Field_Plans;
with ACPI_FADT.Transactions;
with ACPI_Field_Remote_Provider;
with ACPI_Region_Protocol;
procedure EC_Field_Geometry_Tests is
   package L renames AML_Field_Layout;
   package P renames AML_Field_Plans;
   package T renames ACPI_FADT.Transactions;
   use type AML_Decode.Integer_Value, AML_Decode.Byte, P.Datum_Count;
   use type T.Access_Width;
   type Transport_State is limited null record;
   procedure Exchange
     (Transport : in out Transport_State; Destination : Natural;
      Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Packet; Reply : out ACPI_Region_Protocol.Packet;
      Delivered : out ACPI_Region_Protocol.Delivery)
   is
      pragma Unreferenced (Transport, Destination, Deadline, Item);
   begin Reply := (others => <>); Delivered := ACPI_Region_Protocol.Not_Dispatched; end Exchange;
   procedure Window (Now, Deadline : out ACPI_Region_Protocol.Time_Instant) is
   begin Now := 1; Deadline := 100; end Window;
   package Provider is new ACPI_Field_Remote_Provider (Transport_State, Natural, 0, Exchange, Window);
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC geometry" & Checks'Image; end if; end Check;
   Field : L.Field_Geometry :=
     (Region => (Space => L.Embedded_Control_Space, Base => 0, Length => 256),
      Bit_Offset => 0, Bit_Length => 8, Flags => 1, Effective_Access => 1, others => <>);
   Register : L.Register_Geometry := (Region => Field.Region, Width => T.Byte_Access, others => <>);
   Item : P.Plan;
   Descriptor : Provider.Descriptor :=
     (Register => 1, Epoch => 1, Geometry => Register, Readable => True, others => <>);
   Original : constant L.Field_Geometry := Field;
begin
   Field_Plan_Tests;
   for Cell in 0 .. 255 loop
      Field := Original; Field.Bit_Offset := AML_Decode.Integer_Value (Cell * 8);
      Register := (Region => Field.Region, Byte_Offset => AML_Decode.Integer_Value (Cell), Width => T.Byte_Access);
      Check (L.Admitted (Field, Register));
      Item := P.Prepare (Field); Check (P.Valid (Item) and then P.Count (Item) = 1);
      Check (P.Matches (Item, 1, Register));
      Descriptor.Geometry := Register; Check (Provider.Valid (Descriptor));
   end loop;
   for Access_Code in AML_Decode.Byte range 0 .. 4 loop
      Field := Original; Field.Effective_Access := Access_Code; Field.Flags := Access_Code;
      Item := P.Prepare (Field); Check (P.Valid (Item) = (Access_Code = 1));
   end loop;
   Field := Original; Field.Bit_Offset := 7; Field.Bit_Length := 64;
   Item := P.Prepare (Field); Check (P.Valid (Item) and then P.Count (Item) = 9);
   for I in P.Datum_Position range 1 .. P.Count (Item) loop
      Register := (Region => Field.Region, Byte_Offset => AML_Decode.Integer_Value (I - 1), Width => T.Byte_Access);
      Check (P.Matches (Item, I, Register));
   end loop;
   for Mutation in 1 .. 8 loop
      Field := Original; Register := (Region => Field.Region, others => <>);
      case Mutation is
         when 1 => Field.Flags := 16#11#;
         when 2 => Field.Region.Base := 256;
         when 3 => Field.Region.Length := 257;
         when 4 => Field.Region.Base := 255; Field.Region.Length := 2;
         when 5 => Field.Bit_Offset := 255 * 8; Field.Bit_Length := 9;
         when 6 => Field.Region.Length := 0;
         when 7 => Field.Flags := 0;
         when 8 => Field.Effective_Access := 0;
      end case;
      Register.Region := Field.Region;
      Check (not L.Admitted (Field, Register)); Check (not P.Valid (P.Prepare (Field)));
   end loop;
   for Width in T.Access_Width loop
      Register := (Region => Original.Region, Width => Width, others => <>);
      Descriptor.Geometry := Register;
      Check (Provider.Valid (Descriptor) = (Width = T.Byte_Access));
   end loop;
   Register := (Region => (Space => L.Embedded_Control_Space, Base => 255, Length => 1), others => <>);
   Descriptor.Geometry := Register; Check (Provider.Valid (Descriptor));
   Descriptor.Geometry.Region.Base := 256; Check (not Provider.Valid (Descriptor));
   Descriptor.Geometry.Region.Base := 0; Descriptor.Geometry.Region.Length := 257; Check (not Provider.Valid (Descriptor));
   Descriptor.Geometry.Region := Original.Region; Descriptor.Geometry.Byte_Offset := 256; Check (not Provider.Valid (Descriptor));
   Ada.Text_IO.Put_Line ("EC field geometry checks:" & Checks'Image);
end EC_Field_Geometry_Tests;
