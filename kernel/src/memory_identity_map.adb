pragma Ada_2022;
with System.Address_To_Access_Conversions;
package body Memory_Identity_Map is
   use Interfaces;
   use System.Storage_Elements;
   use type System.Address;
   package Pointers is new System.Address_To_Access_Conversions (Entries);
   Page_Bytes : constant Unsigned_64 := 4096;
   function Bytes (Object : Map) return Unsigned_64 is (Object.Used_Bytes);
   function Index (Identity : Unsigned_64; Level : Natural) return Natural is
     (Natural (Shift_Right (Identity, Level * 8) and 255));

   function Find (Object : Map; Identity : Unsigned_64) return System.Address is
      Node : System.Address := Object.Root'Address;
   begin
      if Identity = 0 then return System.Null_Address; end if;
      for Level in reverse 1 .. 7 loop
         Node := Pointers.To_Pointer (Node) (Index (Identity, Level));
         if Node = System.Null_Address then return Node; end if;
      end loop;
      return Pointers.To_Pointer (Node) (Index (Identity, 0));
   end Find;

   type Path is array (Natural range 0 .. 7) of System.Address;
   -- Prune only the traversed suffix. All of its values must already be zero.
   procedure Prune
     (Object : in out Map; Nodes : Path; Identity : Unsigned_64;
      Deepest : Natural) is
      Empty : Boolean;
   begin
      for Depth in reverse 1 .. Deepest loop
         Empty := True;
         for Child of Pointers.To_Pointer (Nodes (Depth)).all loop
            if Child /= System.Null_Address then Empty := False; exit; end if;
         end loop;
         exit when not Empty;
         Pointers.To_Pointer (Nodes (Depth - 1))
           (Index (Identity, 8 - Depth)) := System.Null_Address;
         Release (Nodes (Depth));
         Object.Used_Bytes := Object.Used_Bytes - Page_Bytes;
      end loop;
   end Prune;

   procedure Insert
     (Object : in out Map; Identity : Unsigned_64;
      Value : System.Address; Byte_Limit : Unsigned_64;
      Status : out Insert_Result)
   is
      Nodes : Path := [others => System.Null_Address];
      Child : System.Address;
      Slot : Natural;
      Depth : Natural := 0;
   begin
      if Identity = 0 or else Value = System.Null_Address then
         Status := Invalid_Argument; return;
      end if;
      if Find (Object, Identity) /= System.Null_Address then
         Status := Already_Present; return;
      end if;
      Nodes (0) := Object.Root'Address;
      for Level in reverse 1 .. 7 loop
         Slot := Index (Identity, Level);
         Child := Pointers.To_Pointer (Nodes (Depth)) (Slot);
         if Child = System.Null_Address then
            if Object.Used_Bytes > Byte_Limit or else
              Page_Bytes > Byte_Limit - Object.Used_Bytes
            then
               Prune (Object, Nodes, Identity, Depth);
               Status := Metadata_Limit; return;
            end if;
            Child := Allocate;
            if Child = System.Null_Address then
               Prune (Object, Nodes, Identity, Depth);
               Status := No_Memory; return;
            end if;
            if To_Integer (Child) mod 4096 /= 0 or else
              To_Integer (Child) > Integer_Address'Last - 4095
            then
               Release (Child);
               Prune (Object, Nodes, Identity, Depth);
               Status := Invalid_Backing; return;
            end if;
            Pointers.To_Pointer (Child).all := [others => System.Null_Address];
            Pointers.To_Pointer (Nodes (Depth)) (Slot) := Child;
            Object.Used_Bytes := Object.Used_Bytes + Page_Bytes;
         end if;
         Depth := Depth + 1;
         Nodes (Depth) := Child;
      end loop;
      Pointers.To_Pointer (Nodes (7)) (Index (Identity, 0)) := Value;
      Status := Inserted;
   end Insert;

   procedure Remove
     (Object : in out Map; Identity : Unsigned_64;
      Expected : System.Address; Removed : out Boolean)
   is
      Nodes : Path := [others => System.Null_Address];
   begin
      Removed := False;
      if Identity = 0 or else Expected = System.Null_Address then return; end if;
      Nodes (0) := Object.Root'Address;
      for Depth in 1 .. 7 loop
         Nodes (Depth) := Pointers.To_Pointer (Nodes (Depth - 1))
           (Index (Identity, 8 - Depth));
         if Nodes (Depth) = System.Null_Address then return; end if;
      end loop;
      if Pointers.To_Pointer (Nodes (7)) (Index (Identity, 0)) /= Expected then
         return;
      end if;
      Pointers.To_Pointer (Nodes (7)) (Index (Identity, 0)) := System.Null_Address;
      Prune (Object, Nodes, Identity, 7);
      Removed := True;
   end Remove;
end Memory_Identity_Map;
