pragma Ada_2022;
with Interfaces; use Interfaces;
with Multiboot_Memory_Map;

-- Immutable-after-publication boot payload catalog. No address overlays here.
package Boot_Modules with SPARK_Mode, Pure is
   Maximum_Modules : constant := 64;
   Maximum_Name : constant := 64;
   Page_Bytes : constant := 4096;
   Window_End : constant := 2 ** 30;
   subtype Address is Long_Long_Integer range 0 .. Window_End;
   subtype Module_Count is Natural range 0 .. Maximum_Modules;
   subtype Module_Index is Module_Count range 1 .. Maximum_Modules;
   subtype Name_Length is Natural range 0 .. Maximum_Name;
   type Module_Name is record
      Text : String (1 .. Maximum_Name) := [others => ' '];
      Length : Name_Length := 0;
   end record;
   type Image is record
      First, Limit, Page_Limit : Address := 0;
      Name : Module_Name;
   end record;
   function Valid (Item : Image) return Boolean is
     (Item.First > 0 and then Item.First < Item.Limit and then
      Item.Limit <= Item.Page_Limit and then
      Item.Page_Limit mod Page_Bytes = 0 and then
      Item.First mod Page_Bytes = 0 and then Item.Name.Length > 0);
   function Overlaps (First, Limit, Other_First, Other_Limit : Address)
     return Boolean is (First < Other_Limit and then Other_First < Limit);
   function In_RAM (Map : Multiboot_Memory_Map.Entries;
                    First, Limit : Address) return Boolean;
   procedure Clear_Padding (Data : in out Multiboot_Memory_Map.Bytes;
                            Payload_Bytes : Natural) with
     Pre => Payload_Bytes <= Data'Length,
     Post => (for all I in Data'Range =>
       (if I - Data'First < Payload_Bytes then Data (I) = Data'Old (I)
        else Data (I) = 0));

   type Catalog is private;
   type Status is (Success, Already_Sealed, Capacity_Exceeded, Invalid_Range,
                   Invalid_Name, Not_RAM, Overlapping_Payload, Duplicate_Name);
   function Sealed (State : Catalog) return Boolean;
   function Count (State : Catalog) return Module_Count;
   function Consistent (State : Catalog) return Boolean with Ghost;
   procedure Append (State : in out Catalog; Map : Multiboot_Memory_Map.Entries;
                     First, Limit : Unsigned_64; Protected_End : Address;
                     Name : Module_Name; Result : out Status) with
     Pre => Consistent (State),
     Post => Consistent (State) and then
       (if Result /= Success then State = State'Old) and then
       Sealed (State) = Sealed (State'Old);
   procedure Seal (State : in out Catalog) with
     Pre => Consistent (State),
     Post => Sealed (State) and then Consistent (State) and then
       (if Sealed (State'Old) then State = State'Old);
   function Get (State : Catalog; Index : Module_Index) return Image with
     Pre => Consistent (State) and then Sealed (State) and then Index <= Count (State),
     Post => Valid (Get'Result);
   function Reserved_End (State : Catalog) return Address with
     Pre => Consistent (State) and then Sealed (State),
     Post => (for all I in 1 .. Count (State) =>
                Get (State, I).Page_Limit <= Reserved_End'Result);
private
   type Image_Array is array (Module_Index) of Image;
   type Catalog is record
      Items : Image_Array;
      Used : Module_Count := 0;
      Published : Boolean := False;
   end record;
   function Sealed (State : Catalog) return Boolean is (State.Published);
   function Count (State : Catalog) return Module_Count is
     (if State.Published then State.Used else 0);
   function Consistent (State : Catalog) return Boolean is
     ((for all I in 1 .. State.Used => Valid (State.Items (I))) and then
      (for all I in 1 .. State.Used =>
         (for all J in 1 .. I - 1 =>
            not Overlaps (State.Items (I).First, State.Items (I).Page_Limit,
                          State.Items (J).First, State.Items (J).Page_Limit))));
   function Get (State : Catalog; Index : Module_Index) return Image is
     (State.Items (Index));
end Boot_Modules;
