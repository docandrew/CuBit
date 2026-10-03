pragma Ada_2022;
package body AML_Objects with SPARK_Mode is
   function Usage_Of (Store : State) return Usage is
     ((Objects => Store.Used, Bytes => Store.Bytes_Used, Elements => Store.Elements_Used));
   function Count (Store : State) return Object_ID is (Store.Used);
   function Byte_Count (Store : State) return Natural is (Store.Bytes_Used);
   function Element_Count (Store : State) return Natural is (Store.Elements_Used);
   function Extends (Store, Prior : State) return Boolean is
     (Store.Used >= Prior.Used
      and then Store.Bytes_Used >= Prior.Bytes_Used
      and then Store.Elements_Used >= Prior.Elements_Used
      and then Store.Objects (1 .. Prior.Used) = Prior.Objects (1 .. Prior.Used)
      and then Store.Bytes (1 .. Prior.Bytes_Used) = Prior.Bytes (1 .. Prior.Bytes_Used)
      and then Store.Elements (1 .. Prior.Elements_Used) = Prior.Elements (1 .. Prior.Elements_Used));
   function Valid (Store : State) return Boolean is
     ((for all I in 1 .. Store.Used =>
         (case Store.Objects (I).Tag is
            when Integer_Object => True,
            when String_Object | Buffer_Object =>
               Store.Objects (I).First <= Store.Bytes_Used and then
               Store.Objects (I).Size <= Store.Bytes_Used - Store.Objects (I).First,
            when Package_Object =>
               Store.Objects (I).First <= Store.Elements_Used and then
               Store.Objects (I).Size <= Store.Elements_Used - Store.Objects (I).First))
      and then (for all I in 1 .. Store.Elements_Used => Store.Elements (I) <= Store.Used));
   function Empty return State is ((others => <>));
   function Kind (Store : State; ID : Object_ID) return Object_Kind is (Store.Objects (ID).Tag);
   function Length (Store : State; ID : Object_ID) return Natural is (Store.Objects (ID).Size);
   function Integer_Data (Store : State; ID : Object_ID) return AML_Decode.Integer_Value is
     (Store.Objects (ID).Value);
   function Byte_Data (Store : State; ID : Object_ID) return AML_Decode.Bytes is
     (Store.Bytes (Store.Objects (ID).First + 1 .. Store.Objects (ID).First + Store.Objects (ID).Size));
   function Element (Store : State; ID : Object_ID; Index : Natural) return Object_ID is
     (Store.Elements (Store.Objects (ID).First + Index + 1));
   function Integer_Updated
     (Store, Prior : State; ID : Object_ID; Value : AML_Decode.Integer_Value)
      return Boolean is
     (Store = (Prior with delta Objects =>
       (Prior.Objects with delta ID =>
         (Prior.Objects (ID) with delta Value => Value))));
   function Stored_Byte (Store : State; ID : Object_ID; Index : Natural)
     return AML_Decode.Byte is
     (Store.Bytes (Store.Objects (ID).First + Index + 1));
   function Stored_Byte_Updated
     (Store, Prior : State; ID : Object_ID; Index : Natural;
      Value : AML_Decode.Byte) return Boolean is
     (Store = (Prior with delta Bytes =>
       (Prior.Bytes with delta Prior.Objects (ID).First + Index + 1 => Value)));
   procedure Set_Stored_Byte
     (Store : in out State; ID : Object_ID; Index : Natural; Value : AML_Decode.Byte)
   is
   begin
      Store.Bytes (Store.Objects (ID).First + Index + 1) := Value;
   end Set_Stored_Byte;
   procedure Set_Integer
     (Store : in out State; ID : Object_ID; Value : AML_Decode.Integer_Value) is
   begin
      Store.Objects (ID).Value := Value;
   end Set_Integer;
   procedure New_Integer (Store : in out State; Value : AML_Decode.Integer_Value;
                          ID : out Object_ID; Status : out Allocation_Status) is
   begin
      ID := 0;
      if Store.Used = Max_Objects then Status := Object_Limit; return; end if;
      Store.Used := Store.Used + 1;
      Store.Objects (Store.Used) := (Tag => Integer_Object, Value => Value, others => <>);
      ID := Store.Used;
      Status := Allocated;
   end New_Integer;
   procedure New_Bytes (Store : in out State; Tag : Byte_Kind; Data : AML_Decode.Bytes;
                        ID : out Object_ID; Status : out Allocation_Status) is
      Start : constant Natural := Store.Bytes_Used;
   begin
      ID := 0;
      if Store.Used = Max_Objects then Status := Object_Limit; return; end if;
      if Data'Length > Max_Bytes - Store.Bytes_Used then Status := Byte_Limit; return; end if;
      for I in 1 .. Data'Length loop
         pragma Loop_Invariant (Store.Bytes_Used = Start);
         pragma Loop_Invariant
           (Store.Bytes (1 .. Start) = Store.Bytes'Loop_Entry (1 .. Start));
         pragma Loop_Invariant (Store.Objects = Store.Objects'Loop_Entry);
         pragma Loop_Invariant (Valid (Store));
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 => Store.Bytes (Start + J) = Data (Data'First + (J - 1)));
         Store.Bytes (Start + I) := Data (Data'First + (I - 1));
      end loop;
      Store.Used := Store.Used + 1;
      Store.Objects (Store.Used) := (Tag => Tag, First => Start, Size => Data'Length, others => <>);
      Store.Bytes_Used := Store.Bytes_Used + Data'Length;
      ID := Store.Used;
      Status := Allocated;
   end New_Bytes;
   procedure New_Package (Store : in out State; Size : Natural;
                          ID : out Object_ID; Status : out Allocation_Status) is
      Start : constant Natural := Store.Elements_Used;
   begin
      ID := 0;
      if Store.Used = Max_Objects then Status := Object_Limit; return; end if;
      if Size > Max_Elements - Store.Elements_Used then Status := Element_Limit; return; end if;
      for I in 1 .. Size loop
         pragma Loop_Invariant (Store.Elements_Used = Start);
         pragma Loop_Invariant
           (Store.Elements (1 .. Start) = Store.Elements'Loop_Entry (1 .. Start));
         pragma Loop_Invariant (Store.Objects = Store.Objects'Loop_Entry);
         pragma Loop_Invariant (Valid (Store));
         pragma Loop_Invariant (for all J in 1 .. I - 1 => Store.Elements (Start + J) = 0);
         Store.Elements (Start + I) := 0;
      end loop;
      Store.Used := Store.Used + 1;
      Store.Objects (Store.Used) := (Tag => Package_Object, First => Start, Size => Size, others => <>);
      Store.Elements_Used := Store.Elements_Used + Size;
      ID := Store.Used;
      Status := Allocated;
   end New_Package;
   function Element_Updated
     (Store, Prior : State; ID : Object_ID; Index : Natural; Value : Object_ID)
     return Boolean is
     (Store = (Prior with delta Elements =>
       (Prior.Elements with delta Prior.Objects (ID).First + Index + 1 => Value)));
   procedure Set_Element (Store : in out State; ID : Object_ID; Index : Natural; Value : Object_ID) is
   begin
      Store.Elements (Store.Objects (ID).First + Index + 1) := Value;
   end Set_Element;
end AML_Objects;
