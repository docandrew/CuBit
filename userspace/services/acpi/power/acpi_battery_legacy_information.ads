with Interfaces;
generic
   Maximum_Text_Bytes : Positive := 128;
package ACPI_Battery_Legacy_Information with Pure, SPARK_Mode is
   use Interfaces;
   Text_Capacity : constant Positive := Maximum_Text_Bytes;
   type Field is (No_Field, Power_Unit, Design_Capacity, Last_Full_Charge,
      Technology, Design_Voltage, Warning_Capacity, Low_Capacity,
      Granularity_Low, Granularity_High, Model_Number, Serial_Number,
      Battery_Type, OEM_Information);
   subtype Numeric_Field is Field range Power_Unit .. Granularity_High;
   subtype Text_Field is Field range Model_Number .. OEM_Information;
   type Raw_Numbers is array (Numeric_Field) of Unsigned_64;
   subtype Text_Length is Natural range 0 .. Maximum_Text_Bytes;
   type Text_Buffer is array (Positive range 1 .. Maximum_Text_Bytes) of Unsigned_8;
   type Text is record
      Length : Text_Length := 0;
      Data : Text_Buffer := [others => 0];
   end record;
   type Texts is array (Text_Field) of Text;
   Empty_Texts : constant Texts := [others => (others => <>)];
   Expected_Elements : constant Positive := 13;
   Unknown_Word : constant Unsigned_64 := 16#FFFF_FFFF#;
   Maximum_Capacity : constant Unsigned_64 := 16#7FFF_FFFF#;
   type Unit_Kind is (Unspecified_Unit, Energy_And_Power, Charge_And_Current);
   type Technology_Kind is (Unspecified_Technology, Primary, Secondary);
   type Optional_Quantity is record
      Known : Boolean := False;
      Value : Unsigned_32 := 0;
   end record;
   type Information is record
      Units : Unit_Kind := Unspecified_Unit;
      Battery_Technology : Technology_Kind := Unspecified_Technology;
      Design, Last_Full, Voltage : Optional_Quantity;
      Warning, Low : Unsigned_32 := 0;
      Granularity_One, Granularity_Two : Unsigned_32 := 0;
      Identifiers : Texts := Empty_Texts;
   end record;
   Empty_Information : constant Information := (others => <>);
   -- Executable schema model used only in proof/assertion contexts. It adds
   -- no caller precondition and does not assume ordering between capacities.
   subtype Capacity_Field is Field range Design_Capacity .. Low_Capacity;
   subtype ASCII_Byte is Unsigned_8 range 1 .. 127;
   function Dwords_Fit (Numbers : Raw_Numbers) return Boolean is
     (for all Item in Numeric_Field => Numbers (Item) <= Unsigned_64 (Unsigned_32'Last))
     with Ghost;
   function Capacity_Valid (Item : Capacity_Field; Raw : Unsigned_64) return Boolean is
     (Item = Technology or else Raw <= Maximum_Capacity or else
       (Item in Design_Capacity | Last_Full_Charge | Design_Voltage and then Raw = Unknown_Word))
     with Ghost;
   function Numbers_Valid (Numbers : Raw_Numbers) return Boolean is
     (Dwords_Fit (Numbers) and then Numbers (Power_Unit) <= 1
       and then Numbers (Technology) <= 1 and then
       (for all Item in Capacity_Field => Capacity_Valid (Item, Numbers (Item))))
     with Ghost;
   function Text_Valid (Item : Text) return Boolean is
     (for all I in 1 .. Item.Length => Item.Data (I) in ASCII_Byte)
     with Ghost, Annotate => (GNATprove, Inline_For_Proof);
   function Inputs_Valid (Numbers : Raw_Numbers; Identifiers : Texts) return Boolean is
     (Numbers_Valid (Numbers) and then
       (for all Item in Text_Field => Text_Valid (Identifiers (Item))))
     with Ghost, Annotate => (GNATprove, Inline_For_Proof);
   -- Pointwise consequence of the quantified schema predicate; proved in the
   -- body, with no input assumptions or state effects.
   procedure Lemma_Text_Validity (Numbers : Raw_Numbers; Identifiers : Texts;
      Item : Text_Field)
     with Ghost, Global => null,
       Post => (if Inputs_Valid (Numbers, Identifiers) then Text_Valid (Identifiers (Item)));
   function Text_Matches (Value, Input : Text) return Boolean is
     (Value.Length = Input.Length and then
       (for all I in Text_Buffer'Range =>
          (if I <= Input.Length then Value.Data (I) = Input.Data (I)
             and then Input.Data (I) in ASCII_Byte else Value.Data (I) = 0)))
     with Ghost, Post => (if Text_Matches'Result then Text_Valid (Input));
   function Quantity_Matches (Value : Optional_Quantity; Raw : Unsigned_64) return Boolean is
     (Value.Known = (Raw /= Unknown_Word) and then
       Unsigned_64 (Value.Value) = (if Raw = Unknown_Word then 0 else Raw))
     with Ghost;
   function Matches (Value : Information; Numbers : Raw_Numbers; Identifiers : Texts)
      return Boolean is
     (Value.Units = (if Numbers (Power_Unit) = 0 then Energy_And_Power else Charge_And_Current)
       and then Value.Battery_Technology = (if Numbers (Technology) = 0 then Primary else Secondary)
       and then Quantity_Matches (Value.Design, Numbers (Design_Capacity))
       and then Quantity_Matches (Value.Last_Full, Numbers (Last_Full_Charge))
       and then Quantity_Matches (Value.Voltage, Numbers (Design_Voltage))
       and then Unsigned_64 (Value.Warning) = Numbers (Warning_Capacity)
       and then Unsigned_64 (Value.Low) = Numbers (Low_Capacity)
       and then Unsigned_64 (Value.Granularity_One) = Numbers (Granularity_Low)
       and then Unsigned_64 (Value.Granularity_Two) = Numbers (Granularity_High)
       and then (for all Item in Text_Field =>
          Text_Matches (Value.Identifiers (Item), Identifiers (Item))))
     with Ghost;
   type Outcome is (Decoded,
                    Integer_Too_Wide, Invalid_Units, Invalid_Technology,
                    Invalid_Capacity, Invalid_Text);
   type Decode_Result is record
      Status : Outcome := Invalid_Units;
      Failure_Field : Field := No_Field;
      Value : Information := Empty_Information;
   end record;
   -- Inputs have already been extracted as integers/strings. No narrowing,
   -- truncation, unit conversion or ordering assumptions are applied silently.
   -- Unknown differs from zero. Granularity remains a raw DWORD, without policy.
   -- Text capacity is an explicit instance budget, not an ACPI string limit.
   function Decode (Numbers : Raw_Numbers; Identifiers : Texts) return Decode_Result
     with Post => (Decode'Result.Status = Decoded) = Inputs_Valid (Numbers, Identifiers)
       and then (if Decode'Result.Status = Decoded then
          Decode'Result.Failure_Field = No_Field
            and then Matches (Decode'Result.Value, Numbers, Identifiers)
          else Decode'Result.Failure_Field /= No_Field
            and then Decode'Result.Value = Empty_Information);
end ACPI_Battery_Legacy_Information;
