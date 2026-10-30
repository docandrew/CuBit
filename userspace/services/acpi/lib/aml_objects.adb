pragma Ada_2022;
package body AML_Objects with SPARK_Mode is
   use type AML_References.Reference_Kind;
   function Slot_Bound (Store : State) return Object_ID is (Store.Used);
   function Live_Count (Store : State) return Object_ID is (Store.Live_Used);
   function Is_Live (Store : State; ID : Object_ID) return Boolean is
     (ID /= No_Object and then ID <= Store.Used and then Store.Objects (ID).Occupied);
   function Last_Incarnation (Store : State; ID : Object_ID)
      return AML_Object_Identifiers.Slot_Incarnation is (Store.Objects (ID).Stamp);
   function Incarnation_Limit (Store : State) return AML_Object_Identifiers.Incarnation_Budget is
     (Store.Generation_Limit);
   function Address_Of (Store : State; ID : Object_ID) return AML_Object_Identifiers.Object_Address is
     (if not Is_Live (Store, ID) then AML_Object_Identifiers.No_Address
      else AML_Object_Identifiers.Make_Address (ID, Store.Objects (ID).Stamp));
   function Matches_Address (Store : State; Address : AML_Object_Identifiers.Object_Address) return Boolean is
     (AML_Object_Identifiers.Present (Address)
      and then Is_Live (Store, AML_Object_Identifiers.Slot_Of (Address))
      and then Store.Objects (AML_Object_Identifiers.Slot_Of (Address)).Stamp =
        AML_Object_Identifiers.Incarnation_Of (Address));
   function Usage_Of (Store : State) return Usage is
     ((Objects => Store.Live_Used, Bytes => Store.Bytes_Used, Elements => Store.Elements_Used));
   function Byte_Count (Store : State) return Natural is (Store.Bytes_Used);
   function Element_Count (Store : State) return Natural is (Store.Elements_Used);
   function Extends (Store, Prior : State) return Boolean is
     (Store.Generation_Limit = Prior.Generation_Limit
      and then Store.Used >= Prior.Used and then Store.Live_Used >= Prior.Live_Used
      and then Store.Bytes_Used >= Prior.Bytes_Used and then Store.Elements_Used >= Prior.Elements_Used
      and then (for all I in 1 .. Max_Objects =>
        Store.Objects (I).Stamp >= Prior.Objects (I).Stamp
        and then (if Prior.Objects (I).Occupied then Store.Objects (I) = Prior.Objects (I)))
      and then Store.Bytes (1 .. Prior.Bytes_Used) = Prior.Bytes (1 .. Prior.Bytes_Used)
      and then Store.Elements (1 .. Prior.Elements_Used) = Prior.Elements (1 .. Prior.Elements_Used));
   function Fresh_Allocation (Store, Prior : State; ID : Object_ID) return Boolean is
     (Is_Live (Store, ID) and then not Is_Live (Prior, ID)
      and then Prior.Objects (ID).Stamp < Prior.Generation_Limit
      and then Store.Objects (ID).Stamp = Prior.Objects (ID).Stamp + 1
      and then Store.Generation_Limit = Prior.Generation_Limit
      and then Store.Live_Used = Prior.Live_Used + 1
      and then Store.Used = Object_ID'Max (Prior.Used, ID)
      and then Extends (Store, Prior)
      and then (for all I in 1 .. Max_Objects => (if I /= ID then Store.Objects (I) = Prior.Objects (I)))
      and then (case Store.Objects (ID).Tag is
        when Integer_Object | Reference_Object | Metadata_Object | Metadata_Snapshot_Object | Event_Object | Mutex_Object =>
          Store.Bytes_Used = Prior.Bytes_Used and then Store.Elements_Used = Prior.Elements_Used
          and then Store.Objects (ID).First = 0 and then Store.Objects (ID).Size = 0,
        when String_Object | Buffer_Object =>
          Store.Objects (ID).First = Prior.Bytes_Used
          and then Store.Objects (ID).Size <= Max_Bytes - Prior.Bytes_Used
          and then Store.Bytes_Used = Prior.Bytes_Used + Store.Objects (ID).Size
          and then Store.Elements_Used = Prior.Elements_Used,
        when Package_Object =>
          Store.Objects (ID).First = Prior.Elements_Used
          and then Store.Objects (ID).Size <= Max_Elements - Prior.Elements_Used
          and then Store.Elements_Used = Prior.Elements_Used + Store.Objects (ID).Size
          and then Store.Bytes_Used = Prior.Bytes_Used));
   type Occupancy_Array is array (Positive range 1 .. Max_Objects) of Boolean;
   function Equal_Occupancy (Left, Right : Occupancy_Array) return Boolean is
     (for all I in 1 .. Max_Objects => Left (I) = Right (I))
     with Ghost, Annotate => (GNATprove, Logical_Equal);
   function Equal_Objects (Left, Right : Object_Array) return Boolean is
     (for all I in 1 .. Max_Objects => Left (I) = Right (I))
     with Ghost, Annotate => (GNATprove, Logical_Equal);
   function Equal_Elements (Left, Right : Element_Array) return Boolean is
     (for all I in Left'Range => Left (I) = Right (I))
     with Ghost, Annotate => (GNATprove, Logical_Equal);
   function Occupancy_Map (Objects : Object_Array) return Occupancy_Array is
     ([for I in 1 .. Max_Objects => Objects (I).Occupied])
     with Post => Occupancy_Map'Result = Occupancy_Array'([for I in 1 .. Max_Objects => Objects (I).Occupied]),
       Annotate => (GNATprove, Inline_For_Proof);
   function Occupied_Total (Present : Occupancy_Array) return Object_ID is
      Live : Object_ID := 0;
   begin
      for I in Present'Range loop
         pragma Loop_Invariant (Live <= I - 1);
         if Present (I) then Live := Live + 1; end if;
      end loop;
      return Live;
   end Occupied_Total;
   subtype Byte_Usage is Natural range 0 .. Max_Bytes;
   subtype Element_Usage is Natural range 0 .. Max_Elements;
   function Valid_Item (Used : Object_ID; Generation_Limit : AML_Object_Identifiers.Incarnation_Budget;
     Bytes_Used : Byte_Usage; Elements_Used : Element_Usage; Present : Occupancy_Array; Elements : Element_Array;
     Item : Object_Record; Position : Positive) return Boolean is
     (Item.Stamp <= Generation_Limit
      and then (if Position > Used then Item.Stamp = AML_Object_Identifiers.No_Incarnation)
      and then (if not Item.Occupied then Item = Object_Record'(Stamp => Item.Stamp, others => <>)
        else Position <= Used and then Item.Stamp /= AML_Object_Identifiers.No_Incarnation
          and then (if Item.Tag /= Integer_Object then Item.Origin = AML_Decode.Ordinary_Integer)
          and then (if Item.Tag not in Reference_Object | Metadata_Object then Item.Reference = AML_References.No_Reference)
          and then (if Item.Tag /= Metadata_Snapshot_Object then Item.Snapshot =
            AML_Metadata_Kinds.Snapshot_Description'(Object_Type => AML_Metadata_Kinds.Device_Metadata))
          and then (case Item.Tag is
            when Integer_Object => True,
            when Mutex_Object => Item.First = 0 and then Item.Size = 0
              and then Item.Value <= AML_Decode.Integer_Value (AML_Mutexes.Canonical_Flags_Last),
            when Event_Object => Item.First = 0 and then Item.Size = 0 and then Item.Value <= Max_Event_Units,
            when Reference_Object => Item.First = 0 and then Item.Size = 0 and then Item.Value = 0
              and then AML_References.Well_Formed (Item.Reference),
            when Metadata_Object => Item.First = 0 and then Item.Size = 0 and then Item.Value = 0
              and then Metadata_Identity (Item.Reference),
            when Metadata_Snapshot_Object => Item.First = 0 and then Item.Size = 0 and then Item.Value = 0
              and then AML_Metadata_Kinds.Supported (Item.Snapshot),
            when String_Object | Buffer_Object => Item.First <= Bytes_Used
              and then Item.Size <= Bytes_Used - Item.First,
            when Package_Object => Item.First <= Elements_Used
              and then Item.Size <= Elements_Used - Item.First
              and then (for all J in 1 .. Item.Size =>
                Elements (Item.First + J) = No_Object or else
                  (Elements (Item.First + J) <= Used
                    and then Present (Elements (Item.First + J)))))))
     with Annotate => (GNATprove, Inline_For_Proof);
   function Valid_Records (Used : Object_ID;
     Generation_Limit : AML_Object_Identifiers.Incarnation_Budget;
     Bytes_Used : Byte_Usage; Elements_Used : Element_Usage;
     Present : Occupancy_Array; Elements : Element_Array; Objects : Object_Array) return Boolean is
     (for all I in 1 .. Max_Objects =>
       Valid_Item (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Objects (I), I))
     with Annotate => (GNATprove, Inline_For_Proof);
   procedure Extend_Records (Used : Object_ID;
     Generation_Limit : AML_Object_Identifiers.Incarnation_Budget;
     Prior_Bytes, Bytes_Used : Byte_Usage; Elements_Used : Element_Usage;
     Present : Occupancy_Array; Elements : Element_Array;
     Objects : Object_Array)
     with Ghost,
       Pre => Prior_Bytes <= Bytes_Used and then
         Valid_Records (Used, Generation_Limit, Prior_Bytes, Elements_Used, Present, Elements, Objects),
       Post => Valid_Records (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Objects)
   is
   begin
      null;
   end Extend_Records;
   procedure Replace_Record (Used : Object_ID;
     Generation_Limit : AML_Object_Identifiers.Incarnation_Budget;
     Bytes_Used : Byte_Usage; Elements_Used : Element_Usage;
     Present : Occupancy_Array; Elements : Element_Array;
     Prior_Objects, Objects : Object_Array; Target : Object_ID)
     with Ghost,
       Pre => Target /= No_Object and then
         Valid_Records (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Prior_Objects)
         and then (for all I in 1 .. Max_Objects => (if I /= Target then Objects (I) = Prior_Objects (I)))
         and then Valid_Item (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Objects (Target), Positive (Target)),
       Post => Valid_Records (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Objects)
   is
   begin
      pragma Assert (for all I in 1 .. Max_Objects =>
        Valid_Item (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Prior_Objects (I), I));
      pragma Assert (for all I in 1 .. Max_Objects =>
        Valid_Item (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Objects (I), I));
   end Replace_Record;
   function Valid_Metadata (Used, Live_Used : Object_ID;
     Generation_Limit : AML_Object_Identifiers.Incarnation_Budget;
     Bytes_Used : Byte_Usage; Elements_Used : Element_Usage;
     Objects : Object_Array; Elements : Element_Array; Present : Occupancy_Array) return Boolean is
     (Valid_Records (Used, Generation_Limit, Bytes_Used, Elements_Used, Present, Elements, Objects)
      and then Occupied_Total (Present) = Live_Used)
     with Annotate => (GNATprove, Inline_For_Proof);
   function Valid (Store : State) return Boolean is
     (Valid_Metadata (Store.Used, Store.Live_Used, Store.Generation_Limit,
       Store.Bytes_Used, Store.Elements_Used, Store.Objects, Store.Elements, Occupancy_Map (Store.Objects)));
   procedure Unpack_Valid (Store : State; Present : Occupancy_Array;
     Objects : Object_Array; Elements : Element_Array)
     with Ghost,
       Pre => Valid (Store) and then Equal_Occupancy (Occupancy_Map (Store.Objects), Present)
         and then Equal_Objects (Store.Objects, Objects)
         and then Equal_Elements (Store.Elements, Elements),
       Post => Valid_Records (Store.Used, Store.Generation_Limit, Store.Bytes_Used,
         Store.Elements_Used, Present, Elements, Objects)
   is
   begin
      null;
   end Unpack_Valid;
   procedure Reassemble_Valid (Store : State; Present : Occupancy_Array)
     with Ghost,
       Pre => Equal_Occupancy (Occupancy_Map (Store.Objects), Present)
         and then Occupied_Total (Present) = Store.Live_Used
         and then Valid_Records (Store.Used, Store.Generation_Limit, Store.Bytes_Used,
           Store.Elements_Used, Present, Store.Elements, Store.Objects),
       Post => Valid (Store)
   is
   begin
      null;
   end Reassemble_Valid;
   procedure Preserve_Valid_Bytes (Prior, Store : State)
     with Ghost,
       Pre => Valid (Prior) and then Prior.Used = Store.Used
         and then Prior.Live_Used = Store.Live_Used and then Prior.Generation_Limit = Store.Generation_Limit
         and then Prior.Bytes_Used = Store.Bytes_Used and then Prior.Elements_Used = Store.Elements_Used
         and then Equal_Objects (Prior.Objects, Store.Objects)
         and then Equal_Elements (Prior.Elements, Store.Elements),
       Post => Valid (Store)
   is
   begin
      null;
   end Preserve_Valid_Bytes;
   function Empty (Max_Incarnation : AML_Object_Identifiers.Incarnation_Budget :=
      AML_Object_Identifiers.Slot_Incarnation'Last) return State is
     ((Generation_Limit => Max_Incarnation, others => <>));
   function Kind (Store : State; ID : Object_ID) return Object_Kind is (Store.Objects (ID).Tag);
   function Length (Store : State; ID : Object_ID) return Natural is (Store.Objects (ID).Size);
   function Origin_Of (Store : State; ID : Object_ID) return AML_Decode.Integer_Origin is
     (Store.Objects (ID).Origin);
   function Integer_Data (Store : State; ID : Object_ID) return AML_Decode.Integer_Value is
     (Store.Objects (ID).Value);
   function Reference_Data (Store : State; ID : Object_ID) return AML_References.Reference is
     (Store.Objects (ID).Reference);
   function Reference_Allocated
     (Store, Prior : State; ID : Object_ID; Value : AML_References.Reference)
      return Boolean is
     (Fresh_Allocation (Store, Prior, ID)
      and then Store = (Prior with delta Used => Store.Used, Live_Used => Prior.Live_Used + 1,
        Objects => (Prior.Objects with delta ID =>
          Object_Record'(Occupied => True, Stamp => Prior.Objects (ID).Stamp + 1,
                         Tag => Reference_Object, Reference => Value, others => <>))));
   function Metadata_Identity (Value : AML_References.Reference) return Boolean is
     (AML_References.Well_Formed (Value)
      and then AML_References.Kind (Value) in AML_References.Named_Cell | AML_References.Root_Namespace);
   function Metadata_Data (Store : State; ID : Object_ID) return AML_References.Reference is
     (Store.Objects (ID).Reference);
   function Metadata_Allocated
     (Store, Prior : State; ID : Object_ID; Value : AML_References.Reference)
      return Boolean is
     (Fresh_Allocation (Store, Prior, ID)
      and then Store = (Prior with delta Used => Store.Used, Live_Used => Prior.Live_Used + 1,
        Objects => (Prior.Objects with delta ID =>
          Object_Record'(Occupied => True, Stamp => Prior.Objects (ID).Stamp + 1,
                         Tag => Metadata_Object, Reference => Value, others => <>))));
   function Metadata_Snapshot_Data (Store : State; ID : Object_ID)
      return AML_Metadata_Kinds.Snapshot_Description is (Store.Objects (ID).Snapshot);
   function Metadata_Snapshot_Allocated
     (Store, Prior : State; ID : Object_ID;
      Value : AML_Metadata_Kinds.Snapshot_Description) return Boolean is
     (Fresh_Allocation (Store, Prior, ID)
      and then Store = (Prior with delta Used => Store.Used, Live_Used => Prior.Live_Used + 1,
        Objects => (Prior.Objects with delta ID =>
          Object_Record'(Occupied => True, Stamp => Prior.Objects (ID).Stamp + 1,
                         Tag => Metadata_Snapshot_Object, Snapshot => Value, others => <>))));
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
   function String_Replaced
     (Store, Prior : State; ID : Object_ID; Data : AML_Decode.Bytes)
      return Boolean is
     (Store.Used = Prior.Used
      and then Store.Live_Used = Prior.Live_Used
      and then Store.Generation_Limit = Prior.Generation_Limit
      and then Store.Elements_Used = Prior.Elements_Used
      and then Store.Elements = Prior.Elements
      and then Store.Objects =
        (Prior.Objects with delta ID =>
          (Prior.Objects (ID) with delta
             First => Prior.Bytes_Used, Size => Data'Length))
      and then Data'Length <= Max_Bytes - Prior.Bytes_Used
      and then Store.Bytes_Used = Prior.Bytes_Used + Data'Length
      and then Store.Bytes (1 .. Prior.Bytes_Used) =
        Prior.Bytes (1 .. Prior.Bytes_Used)
      and then Store.Bytes (Prior.Bytes_Used + 1 .. Store.Bytes_Used) = Data
      and then Store.Bytes (Store.Bytes_Used + 1 .. Max_Bytes) =
        Prior.Bytes (Store.Bytes_Used + 1 .. Max_Bytes));

   procedure Replace_String
     (Store : in out State; ID : Object_ID; Data : AML_Decode.Bytes;
      Status : out String_Update_Status)
   is
      Start : constant Natural := Store.Bytes_Used;
   begin
      if not Is_Live (Store, ID) then
         Status := Invalid_String_ID;
         return;
      end if;
      if Store.Objects (ID).Tag /= String_Object then
         Status := Not_A_String;
         return;
      end if;
      if Data'Length > Max_Bytes - Start then
         Status := String_Byte_Limit;
         return;
      end if;
      --  All rejection checks precede writes. Old extents remain reserved;
      --  references follow the stable object ID to the new extent.
      for I in 1 .. Data'Length loop
         Store.Bytes (Start + I) := Data (Data'First + (I - 1));
      end loop;
      Store.Objects (ID).First := Start;
      Store.Objects (ID).Size := Data'Length;
      Store.Bytes_Used := Start + Data'Length;
      Status := String_Updated;
   end Replace_String;

   subtype Storage_Bytes is AML_Decode.Bytes (1 .. Max_Bytes);
   function Stored_Bytes_Equal (Current, Prior : Storage_Bytes; Data : AML_Decode.Bytes;
     Start, Size : Natural) return Boolean is
     (for all J in Current'Range =>
       (if J > Start and then J - Start <= Size then
          (if J - Start <= Data'Length then Current (J) = Data (Data'First + (J - Start - 1))
           else Current (J) = 0)
        else Current (J) = Prior (J)))
     with Ghost, Pre => Start <= Max_Bytes and then Size <= Max_Bytes - Start;
   function Buffer_Stored
     (Store, Prior : State; ID : Object_ID; Data : AML_Decode.Bytes)
      return Boolean is
     (Store.Used = Prior.Used and then Store.Live_Used = Prior.Live_Used
      and then Store.Generation_Limit = Prior.Generation_Limit
      and then Store.Elements_Used = Prior.Elements_Used and then Store.Elements = Prior.Elements
      and then (if Prior.Objects (ID).Size = 0 then
          Data'Length <= Max_Bytes - Prior.Bytes_Used
          and then Store.Bytes_Used = Prior.Bytes_Used + Data'Length
          and then Store.Objects = (Prior.Objects with delta ID =>
            (Prior.Objects (ID) with delta First => Prior.Bytes_Used, Size => Data'Length))
          and then Stored_Bytes_Equal (Store.Bytes, Prior.Bytes, Data, Prior.Bytes_Used, Data'Length)
        else Store.Bytes_Used = Prior.Bytes_Used and then Store.Objects = Prior.Objects
          and then Stored_Bytes_Equal (Store.Bytes, Prior.Bytes, Data,
            Prior.Objects (ID).First, Prior.Objects (ID).Size)));

   procedure Store_Buffer
     (Store : in out State; ID : Object_ID; Data : AML_Decode.Bytes;
      Status : out Buffer_Update_Status)
   is
      Start, Size : Natural;
      Initial_Occupancy : constant Occupancy_Array := Occupancy_Map (Store.Objects) with Ghost;
      Initial_Objects : constant Object_Array := Store.Objects with Ghost;
      Initial_Elements : constant Element_Array := Store.Elements with Ghost;
      Initial_Byte_Count : constant Byte_Usage := Store.Bytes_Used with Ghost;
   begin
      if not Is_Live (Store, ID) then
         Status := Invalid_Buffer_ID; return;
      end if;
      if Store.Objects (ID).Tag /= Buffer_Object then
         Status := Not_A_Buffer; return;
      end if;
      Start := Store.Objects (ID).First; Size := Store.Objects (ID).Size;
      pragma Assert (Equal_Objects (Store.Objects, Initial_Objects));
      pragma Assert (Equal_Occupancy (Occupancy_Map (Store.Objects), Initial_Occupancy));
      pragma Assert (Equal_Elements (Store.Elements, Initial_Elements));
      Unpack_Valid (Store, Initial_Occupancy, Initial_Objects, Initial_Elements);
      pragma Assert (Valid_Records (Store.Used, Store.Generation_Limit, Initial_Byte_Count,
        Store.Elements_Used, Initial_Occupancy, Store.Elements, Initial_Objects));
      pragma Assert (for all I in 1 .. Max_Objects =>
        Valid_Item (Store.Used, Store.Generation_Limit, Initial_Byte_Count,
          Store.Elements_Used, Initial_Occupancy, Store.Elements, Initial_Objects (I), I));
      pragma Assert (Valid_Records (Store.Used, Store.Generation_Limit, Initial_Byte_Count,
        Store.Elements_Used, Initial_Occupancy, Store.Elements, Initial_Objects));
      if Size = 0 then
         if Data'Length > Max_Bytes - Store.Bytes_Used then
            Status := Buffer_Byte_Limit; return;
         end if;
         Start := Store.Bytes_Used; Size := Data'Length;
         Store.Objects (ID).First := Start;
         Store.Objects (ID).Size := Size;
         Store.Bytes_Used := Start + Size;
      end if;
      pragma Assert (Equal_Occupancy (Occupancy_Map (Store.Objects), Initial_Occupancy));
      pragma Assert (Occupied_Total (Initial_Occupancy) = Store.Live_Used);
      pragma Assert (Store.Bytes_Used >= Initial_Byte_Count);
      pragma Assert (Equal_Elements (Store.Elements, Initial_Elements));
      pragma Assert (for all I in 1 .. Max_Objects =>
        (if I /= ID then Store.Objects (I) = Initial_Objects (I)));
      pragma Assert (Valid_Item (Store.Used, Store.Generation_Limit, Store.Bytes_Used,
        Store.Elements_Used, Initial_Occupancy, Store.Elements, Store.Objects (ID), Positive (ID)));
      pragma Assert (Valid_Records (Store.Used, Store.Generation_Limit, Initial_Byte_Count,
        Store.Elements_Used, Initial_Occupancy, Store.Elements, Initial_Objects));
      Extend_Records (Store.Used, Store.Generation_Limit, Initial_Byte_Count,
        Store.Bytes_Used, Store.Elements_Used, Initial_Occupancy, Store.Elements, Initial_Objects);
      pragma Assert (for all I in 1 .. Max_Objects =>
        (if I /= ID then Valid_Item (Store.Used, Store.Generation_Limit, Store.Bytes_Used,
          Store.Elements_Used, Initial_Occupancy, Store.Elements, Initial_Objects (I), I)));
      Replace_Record (Store.Used, Store.Generation_Limit, Store.Bytes_Used,
        Store.Elements_Used, Initial_Occupancy, Store.Elements, Initial_Objects, Store.Objects, ID);
      pragma Assert (for all I in 1 .. Max_Objects =>
        Valid_Item (Store.Used, Store.Generation_Limit, Store.Bytes_Used,
          Store.Elements_Used, Initial_Occupancy, Store.Elements, Store.Objects (I), I));
      pragma Assert (Valid_Records (Store.Used, Store.Generation_Limit, Store.Bytes_Used,
        Store.Elements_Used, Initial_Occupancy, Store.Elements, Store.Objects));
      Reassemble_Valid (Store, Initial_Occupancy);
      pragma Assert (Valid (Store));
      declare Before_Bytes : constant State := Store with Ghost; begin
      if Size > 0 then
         if Data'Length >= Size then
            Store.Bytes (Start + 1 .. Start + Size) := Data (Data'First .. Data'First + (Size - 1));
         else
            if Data'Length > 0 then
               Store.Bytes (Start + 1 .. Start + Data'Length) := Data;
            end if;
            Store.Bytes (Start + Data'Length + 1 .. Start + Size) := [others => 0];
         end if;
      end if;
      Preserve_Valid_Bytes (Before_Bytes, Store);
      end;
      Status := Buffer_Updated;
   end Store_Buffer;

   procedure Set_Integer
     (Store : in out State; ID : Object_ID; Value : AML_Decode.Integer_Value) is
   begin
      Store.Objects (ID).Value := Value;
   end Set_Integer;
   procedure Select_Slot (Store : State; ID : out Object_ID; Status : out Allocation_Status) is
   begin
      ID := No_Object;
      for I in 1 .. Max_Objects loop
         if not Store.Objects (I).Occupied and then Store.Objects (I).Stamp < Store.Generation_Limit then
            ID := I; Status := Allocated; return;
         end if;
      end loop;
      Status := (if Store.Live_Used = Max_Objects then Object_Limit else Generation_Limit);
   end Select_Slot;
   procedure Occupy (Store : in out State; ID : Object_ID; Item : Object_Record) with
     Pre => ID /= No_Object and then not Is_Live (Store, ID)
       and then Store.Objects (ID).Stamp < Store.Generation_Limit and then Store.Live_Used < Max_Objects;
   procedure Occupy (Store : in out State; ID : Object_ID; Item : Object_Record) is
      Stamp : constant AML_Object_Identifiers.Slot_Incarnation := Store.Objects (ID).Stamp + 1;
   begin
      Store.Objects (ID) := (Item with delta Occupied => True, Stamp => Stamp);
      Store.Used := Object_ID'Max (Store.Used, ID);
      Store.Live_Used := Store.Live_Used + 1;
   end Occupy;
   procedure New_Reference (Store : in out State; Value : AML_References.Reference;
                            ID : out Object_ID; Status : out Allocation_Status) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      if not AML_References.Well_Formed (Value) then Status := Invalid_Reference; return; end if;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Reference_Object, Reference => Value, others => <>));
      ID := Slot;
   end New_Reference;
   procedure New_Metadata (Store : in out State; Value : AML_References.Reference;
                            ID : out Object_ID; Status : out Allocation_Status) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      if not Metadata_Identity (Value) then Status := Invalid_Reference; return; end if;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Metadata_Object, Reference => Value, others => <>));
      ID := Slot;
   end New_Metadata;
   procedure New_Metadata_Snapshot
     (Store : in out State; Value : AML_Metadata_Kinds.Snapshot_Description;
      ID : out Object_ID; Status : out Allocation_Status) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      if not AML_Metadata_Kinds.Supported (Value) then Status := Invalid_Metadata; return; end if;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Metadata_Snapshot_Object, Snapshot => Value, others => <>));
      ID := Slot;
   end New_Metadata_Snapshot;
   function Event_Units (Store : State; ID : Object_ID) return Event_Unit_Count is
     (Store.Objects (ID).Value);
   procedure Set_Event_Units (Store : in out State; ID : Object_ID; Units : Event_Unit_Count) is
   begin
      Store.Objects (ID).Value := Units;
   end Set_Event_Units;
   function Mutex_Flags (Store : State; ID : Object_ID) return AML_Decode.Byte is
     (AML_Decode.Byte (Store.Objects (ID).Value));
   function Mutex_Level (Store : State; ID : Object_ID) return AML_Mutexes.Sync_Level is
     (AML_Mutexes.Sync_Level (Store.Objects (ID).Value));
   procedure New_Mutex (Store : in out State; Flags : AML_Decode.Byte;
     ID : out Object_ID; Status : out Allocation_Status) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      if not AML_Mutexes.Canonical_Flags (Flags) then Status := Invalid_Mutex_Flags; return; end if;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Mutex_Object, Value => AML_Decode.Integer_Value (Flags), others => <>));
      ID := Slot;
   end New_Mutex;
   procedure New_Event (Store : in out State; ID : out Object_ID; Status : out Allocation_Status) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Event_Object, others => <>));
      ID := Slot;
   end New_Event;
   procedure New_Integer (Store : in out State; Value : AML_Decode.Integer_Value;
                          ID : out Object_ID; Status : out Allocation_Status;
                          Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Integer_Object, Value => Value, Origin => Origin, others => <>));
      ID := Slot;
   end New_Integer;
   procedure New_Bytes (Store : in out State; Tag : Byte_Kind; Data : AML_Decode.Bytes;
                        ID : out Object_ID; Status : out Allocation_Status) is
      Start : constant Natural := Store.Bytes_Used;
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      if Data'Length > Max_Bytes - Store.Bytes_Used then Status := Byte_Limit; return; end if;
      for I in 1 .. Data'Length loop
         Store.Bytes (Start + I) := Data (Data'First + (I - 1));
      end loop;
      Store.Bytes_Used := Store.Bytes_Used + Data'Length;
      Occupy (Store, Slot, (Tag => Tag, First => Start, Size => Data'Length, others => <>));
      ID := Slot;
   end New_Bytes;
   procedure New_Buffer
     (Store : in out State; Requested_Length : AML_Decode.Integer_Value;
      Initializer : AML_Decode.Bytes; ID : out Object_ID;
      Status : out Allocation_Status)
   is
      Effective : constant AML_Decode.Integer_Value := AML_Decode.Integer_Value'Max
        (Requested_Length, AML_Decode.Integer_Value (Initializer'Length));
      Start : constant Natural := Store.Bytes_Used;
      Size : Natural;
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      -- Check in the wide input domain before narrowing or touching the arena.
      if Effective > AML_Decode.Integer_Value (Max_Bytes - Start) then
         Status := Byte_Limit; return;
      end if;
      Size := Natural (Effective);
      for I in 1 .. Size loop
         Store.Bytes (Start + I) :=
           (if I <= Initializer'Length then Initializer (Initializer'First + (I - 1)) else 0);
      end loop;
      Store.Bytes_Used := Start + Size;
      Occupy (Store, Slot, (Tag => Buffer_Object, First => Start, Size => Size, others => <>));
      ID := Slot;
   end New_Buffer;
   package body Bit_Buffer_Initializers is
      function Window
        (Data : aliased Source_Type; First_Byte, Byte_Length, Offset : Natural;
         Bits : Positive) return AML_Decode.Byte
      is
         pragma Unreferenced (Byte_Length); -- Used by the public precondition.
         Address : constant Natural := First_Byte + Offset / 8;
         Value : Natural := Natural (Source_Byte (Data, Address)) / AML_Field_Data.Scale (Offset mod 8);
      begin
         if Bits + Offset mod 8 > 8 then
            Value := Value + Natural (Source_Byte (Data, Address + 1)) * AML_Field_Data.Scale (8 - Offset mod 8);
         end if;
         return AML_Decode.Byte (Value mod AML_Field_Data.Scale (Bits));
      end Window;
      procedure Allocate
        (Store : in out State; Data : aliased Source_Type;
         First_Byte, Byte_Length, Offset, Bits : Natural;
         ID : out Object_ID; Status : out Allocation_Status)
      is
         Size : constant Natural := Bits / 8 + (if Bits mod 8 = 0 then 0 else 1);
         Start : constant Natural := Store.Bytes_Used;
         Slot : Object_ID;
      begin
         ID := No_Object;
         Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
         if Size > Max_Bytes - Start then Status := Byte_Limit; return; end if;
         -- Initialize unpublished backing using only immutable byte readers.
         -- No allocation hooks, collection or whole-arena setter contracts
         -- occur inside this loop. Concrete readers must be side-effect free.
         for I in 1 .. Size loop
            Store.Bytes (Start + I) := Window
              (Data, First_Byte, Byte_Length, Offset + (I - 1) * 8,
               Positive'Min (8, Bits - (I - 1) * 8));
         end loop;
         Store.Bytes_Used := Start + Size;
         Occupy (Store, Slot, (Tag => Buffer_Object, First => Start, Size => Size, others => <>));
         ID := Slot;
      end Allocate;
   end Bit_Buffer_Initializers;
   package body String_Initializers is
      procedure Allocate
        (Store : in out State; Data : aliased Source_Type;
         First_Byte, Byte_Length : Natural; ID : out Object_ID;
         Status : out Allocation_Status)
      is
         Start : constant Natural := Store.Bytes_Used;
         Slot : Object_ID;
      begin
         ID := No_Object;
         Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
         if Byte_Length > Max_Bytes - Start then Status := Byte_Limit; return; end if;
         -- Readers are trusted immutable observers, as for bit initializers.
         -- No AML reentry, allocation hooks or collection occur in this loop.
         for I in 1 .. Byte_Length loop
            Store.Bytes (Start + I) := Source_Byte (Data, First_Byte + (I - 1));
         end loop;
         Store.Bytes_Used := Start + Byte_Length;
         Occupy (Store, Slot,
           (Tag => String_Object, First => Start, Size => Byte_Length, others => <>));
         ID := Slot;
      end Allocate;
   end String_Initializers;
   procedure New_Package (Store : in out State; Size : Natural;
                          ID : out Object_ID; Status : out Allocation_Status) is
      Start : constant Natural := Store.Elements_Used;
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      if Size > Max_Elements - Store.Elements_Used then Status := Element_Limit; return; end if;
      for I in 1 .. Size loop Store.Elements (Start + I) := No_Object; end loop;
      Store.Elements_Used := Store.Elements_Used + Size;
      Occupy (Store, Slot, (Tag => Package_Object, First => Start, Size => Size, others => <>));
      ID := Slot;
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
