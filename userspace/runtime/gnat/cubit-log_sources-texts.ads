------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A log source's names as text (CuBit.Log_Sources), for programs with a
--  secondary stack; CuBit.Log_Sources itself returns nothing unconstrained.
------------------------------------------------------------------------------
pragma Ada_2022;
package CuBit.Log_Sources.Texts with Pure, SPARK_Mode is
   function Identity_Of (Item : Source_Identity) return String is
     (Item.Identity (1 .. Item.Identity_Used));
   function Name_Of (Item : Source_Identity) return String is
     (Item.Name (1 .. Item.Name_Used));
   --  The short name a person types for it (Log_Sources.From_Name).
   function Label (Item : Source_Identity) return String is
     (if From_Name (Item) then Item.Name (1 .. Label_Last (Item))
      else Item.Identity (Label_First (Item) .. Item.Identity_Used))
   with Post => Label'Result'Length <= Identity_Bytes;
end CuBit.Log_Sources.Texts;
