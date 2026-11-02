------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Devmgr_Growable - implementation
------------------------------------------------------------------------------
pragma Ada_2022;


package body Devmgr_Growable is

   use type System.Address;

   type Elements is array (Positive range <>) of Element;

   Element_Bytes : constant Unsigned_64 :=
     Unsigned_64 ((Elements'Component_Size + System.Storage_Unit - 1) / System.Storage_Unit);
   Element_Alignment : constant Unsigned_64 := Unsigned_64 (Element'Alignment);
   Growth_Factor : constant := 2;

   function Length (S : Store) return Natural is (S.Used);
   function Capacity (S : Store) return Natural is (S.Reserved);
   function Block (S : Store) return System.Address is (S.Base);

   function Element_At (S : Store; Index : Positive) return Element is
      Items : constant Elements (1 .. S.Reserved) with Import, Address => S.Base;
   begin
      return Items (Index);
   end Element_At;

   procedure Set_At (S : in out Store; Index : Positive; Item : Element) is
      Items : Elements (1 .. S.Reserved) with Import, Address => S.Base;
   begin
      Items (Index) := Item;
   end Set_At;

   procedure Reserve (S : in out Store; Demand : Natural; Report : out Capacity_Report) is
      Next : Natural := S.Reserved;
      Replacement : System.Address;
   begin
      Report := (Status => Grown, Demand => Unsigned_64 (Demand),
                 Limit => Unsigned_64 (S.Reserved), Requested => 0);
      if Demand <= S.Reserved then
         return;
      end if;
      if Next = 0 then
         Next := Initial_Capacity;
      end if;
      while Next < Demand loop
         if Next > Natural'Last / Growth_Factor then
            Next := Natural'Last;
         else
            Next := Next * Growth_Factor;
         end if;
      end loop;
      if Element_Bytes = 0 or else Unsigned_64 (Next) > Unsigned_64'Last / Element_Bytes then
         Report.Status := Representation_Limit;
         return;
      end if;
      Report.Requested := Unsigned_64 (Next) * Element_Bytes;
      Replacement := Allocate (Report.Requested, Element_Alignment);
      if Replacement = System.Null_Address then
         Report.Status := Heap_Refused;
         return;
      end if;
      if S.Used > 0 then
         declare
            Fresh : Elements (1 .. Next) with Import, Address => Replacement;
            Old : constant Elements (1 .. S.Reserved) with Import, Address => S.Base;
         begin
            Fresh (1 .. S.Used) := Old (1 .. S.Used);
         end;
      end if;
      if S.Base /= System.Null_Address then
         Release (S.Base);
      end if;
      S.Base := Replacement;
      S.Reserved := Next;
      Report.Limit := Unsigned_64 (Next);
   end Reserve;

   procedure Append (S : in out Store; Item : Element; Report : out Capacity_Report) is
   begin
      if S.Used = Natural'Last then
         Report := (Status => Representation_Limit, Demand => Unsigned_64 (S.Used) + 1,
                    Limit => Unsigned_64 (S.Reserved), Requested => 0);
         return;
      end if;
      Reserve (S, S.Used + 1, Report);
      if Report.Status = Grown then
         S.Used := S.Used + 1;
         Set_At (S, S.Used, Item);
      end if;
   end Append;

   procedure Set_Length (S : in out Store; New_Length : Natural) is
   begin
      S.Used := New_Length;
   end Set_Length;

   procedure Clear (S : in out Store) is
   begin
      if S.Base /= System.Null_Address then
         Release (S.Base);
      end if;
      S.Base := System.Null_Address;
      S.Reserved := 0;
      S.Used := 0;
   end Clear;

end Devmgr_Growable;
