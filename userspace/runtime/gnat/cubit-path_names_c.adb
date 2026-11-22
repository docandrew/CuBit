------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Path_Names; use CuBit.Path_Names;

package body CuBit.Path_Names_C is

   use type System.Address;
   use type Interfaces.C.size_t;
   use type Interfaces.C.long;

   NUL : constant Character := Character'Val (0);

   function Resolve
     (Base : System.Address; Base_Length : Interfaces.C.size_t;
      Path : System.Address; Result : System.Address;
      Capacity : Interfaces.C.size_t) return Interfaces.C.long
   is
      --  The libc is built without run-time checks: check what the proved
      --  code's precondition needs, here.
      Path_Length : Natural := 0;
   begin
      if Path = System.Null_Address or else Result = System.Null_Address
        or else (Base = System.Null_Address and then Base_Length /= 0)
        or else Base_Length > Maximum_Path_Bytes
      then
         return -(if Path = System.Null_Address then ENOENT else EINVAL);
      end if;
      declare
         --  Read only up to the terminator, which must come within
         --  Maximum_Path_Bytes.
         Text : constant String (1 .. Maximum_Path_Bytes + 1)
         with Import, Address => Path;
      begin
         while Path_Length <= Maximum_Path_Bytes
           and then Text (Path_Length + 1) /= NUL
         loop
            Path_Length := Path_Length + 1;
         end loop;
      end;
      if Path_Length > Maximum_Path_Bytes then
         return -ENAMETOOLONG;
      end if;
      declare
         Path_Text : constant String (1 .. Path_Length)
         with Import, Address => Path;
         Base_Text : constant String (1 .. Natural (Base_Length))
         with Import, Address => Base;
         Name_Text : CuBit.Path_Names.Name;
         Length : Name_Length;
         Status : Resolution;
         Room : constant Name_Length :=
           (if Capacity >= Maximum_Name_Bytes then Maximum_Name_Bytes
            else Name_Length (Capacity));
      begin
         CuBit.Path_Names.Resolve
           (Base_Text, Path_Text, Room, Name_Text, Length, Status);
         case Status is
            when Resolved =>
               declare
                  Output : String (1 .. Length) with Import, Address => Result;
               begin
                  Output := Name_Text (1 .. Length);
               end;
               return Interfaces.C.long (Length);
            when Empty_Path   => return -ENOENT;
            when Too_Long     => return -ENAMETOOLONG;
            when Invalid_Base => return -EINVAL;
         end case;
      end;
   end Resolve;

   function Display
     (Item : System.Address; Length : Interfaces.C.size_t;
      Result : System.Address; Size : Interfaces.C.size_t)
      return Interfaces.C.long
   is
   begin
      if Length > Maximum_Name_Bytes
        or else (Item = System.Null_Address and then Length /= 0)
        or else Result = System.Null_Address
      then
         return -EINVAL;
      end if;
      declare
         Item_Text : constant String (1 .. Natural (Length))
         with Import, Address => Item;
         Shown : CuBit.Path_Names.Name;
         Shown_Length : Name_Length;
      begin
         CuBit.Path_Names.Display (Item_Text, Shown, Shown_Length);
         if Interfaces.C.size_t (Shown_Length) + 1 > Size then
            return -ERANGE;
         end if;
         declare
            Output : String (1 .. Shown_Length + 1)
            with Import, Address => Result;
         begin
            Output (1 .. Shown_Length) := Shown (1 .. Shown_Length);
            Output (Shown_Length + 1) := NUL;
         end;
         return Interfaces.C.long (Shown_Length + 1);
      end;
   end Display;

end CuBit.Path_Names_C;
