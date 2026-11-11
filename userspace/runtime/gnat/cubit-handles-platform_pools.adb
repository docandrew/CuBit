------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.Platform_Pools - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements;
with CuBit.Handles.Kernel;

package body CuBit.Handles.Platform_Pools is

   package K renames CuBit.Handles.Kernel;

   function Address_Word (A : System.Address) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (A)));

   function Map
     (Pool : Device_Memory; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean; Why : out Platform_Pool_ABI.Refusal_Record) return Unsigned_64
   is
      Refusal : aliased Platform_Pool_ABI.Refusal_Record := [others => 0];
      Answer  : Unsigned_64;
   begin
      Answer := K.Call
        (K.Map_Device, Physical, Virtual, Pages,
         (if Read_Only then Platform_Pool_ABI.Read_Only_Mode
          else Platform_Pool_ABI.Read_Write_Mode),
         K.Word (Pool), Address_Word (Refusal'Address));
      Why := Refusal;
      return Answer;
   end Map;

   function Declare_Pool (Pool : Device_Memory; Members, Claims : Unsigned_64) return Unsigned_64 is
      Words : aliased ABI.Derivation := [others => 0];
   begin
      Words (ABI.Source) := K.Word (Pool);
      Words (ABI.Flags) := ABI.Narrow;
      Words (ABI.Base) := Members;
      Words (ABI.Length) := Claims;
      return K.Call (K.Install_Capability, K.Word (Pool), Address_Word (Words'Address));
   end Declare_Pool;

   function Insert
     (Pool, Source : Device_Memory; Physical, Bytes : Unsigned_64;
      Kind : Platform_Pool_ABI.Pool_Member_Kind) return Unsigned_64
   is
      Words : aliased ABI.Derivation :=
        K.Derivation_Words (Narrowed (Source, Read_Write, Physical, Bytes));
   begin
      --  The member's kind rides in the Tag word (Platform_Pool_ABI).
      Words (ABI.Flags) := Words (ABI.Flags) or ABI.Set_Tag;
      Words (ABI.Tag) := Platform_Pool_ABI.Member_Kind_Word (Kind);
      return K.Call (K.Install_Capability, K.Word (Pool), Address_Word (Words'Address));
   end Insert;

end CuBit.Handles.Platform_Pools;
