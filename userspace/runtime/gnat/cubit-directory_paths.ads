------------------------------------------------------------------------------
--  Bounded names used for filesystem policy checks and browser breadcrumbs.
--  A path is descriptive policy metadata, never a substitute for a handle.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Directory_Paths with SPARK_Mode => On is
   Maximum_Bytes : constant := 256;
   subtype Byte_Count is Natural range 0 .. Maximum_Bytes;
   type Path is private;

   function Value (Item : Path) return String;
   function Valid_Child_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Bytes - 1 and then
      Name /= "." and then Name /= ".." and then
      (for all C of Name =>
         C /= '/' and then C /= ':' and then C /= Character'Val (0)));

   procedure Set_Root
     (Name : String; Item : out Path; Success : out Boolean);

   --  Append exactly one component. Failure leaves Result equal to Parent.
   --  This does not resolve a name or confer authority to access that name.
   procedure Append_Child
     (Parent : Path; Name : String; Result : out Path;
      Success : out Boolean);

private
   type Path is record
      Data : String (1 .. Maximum_Bytes) := [others => ' '];
      Length : Byte_Count := 0;
   end record;
end CuBit.Directory_Paths;
