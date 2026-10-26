------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Launch_Parameters - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Launch_Arguments;
with CuBit.Messages;
with CuBit.Outlet_Rings;

package body CuBit.Launch_Parameters is

   package LA renames CuBit.Launch_Arguments;
   package PR renames CuBit.Outlet_Rings;
   package PS renames CuBit.Parameter_Sections;
   use type LA.Validation;
   use type PS.Validation;
   use type CuBit.Parameter_Signatures.Identity;

   --  Kept by the start code: the launch arguments' length (the block
   --  without its startup-table envelope).
   Launch_Length : Unsigned_64
   with Import, Convention => C, External_Name => "__cubit_launch_arguments_length";

   procedure Get
     (Expected : CuBit.Parameter_Signatures.Identity;
      Value    : out String;
      Length   : out Natural;
      Result   : out Outcome)
   is
   begin
      Value := [others => ' '];
      Length := 0;
      Result := No_Launch_Block;
      if Launch_Length not in LA.Header_Bytes .. LA.Maximum_Block_Bytes then
         return;
      end if;
      declare
         Item : constant LA.Block (1 .. Natural (Launch_Length))
         with Import, Address => System'To_Address (LA.Block_Address);
      begin
         if LA.Validate (Item) /= LA.Valid then
            return;
         end if;
         Result := No_Parameters;
         declare
            First : constant Positive := LA.Strings_Last (Item) + 1;
            --  One copy of the trailer: everything below reads only it.
            Trailer : constant PR.Bytes (1 .. Item'Last - First + 1) :=
              [for K in 1 .. Item'Last - First + 1 => Item (First + K - 1)];
            Rings_Present, Present : Boolean;
            Ring_Length : PR.Table_Length;
            Section_Length : PS.Section_Length;
         begin
            PR.Measure (Trailer, Rings_Present, Ring_Length);
            declare
               Rest : constant PS.Bytes (1 .. Trailer'Length - Ring_Length) :=
                 [for K in 1 .. Trailer'Length - Ring_Length => Trailer (Ring_Length + K)];
            begin
               PS.Measure (Rest, Present, Section_Length);
               if not Present then
                  return;
               elsif PS.Validate (Rest (1 .. Section_Length)) /= PS.Valid then
                  Result := Malformed_Section;
                  return;
               elsif PS.Identity_Of (Rest (1 .. Section_Length)) /= Expected then
                  Result := Other_Signature;
                  return;
               end if;
               --  The value's bytes, copied without a function result (no
               --  secondary stack).
               Length := Section_Length - PS.Header_Bytes;
               for K in 1 .. Length loop
                  Value (K) := Character'Val (Rest (PS.Header_Bytes + K));
               end loop;
               Result := Found;
            end;
         end;
      end;
   end Get;

   procedure Refuse (Program : String; Why : Outcome) is
      Ignore : Unsigned_64;
   begin
      CuBit.Messages.debugPrint ("parameters: ");
      CuBit.Messages.debugPrint (Program);
      CuBit.Messages.debugPrint (": ");
      case Why is
         when Found =>
            CuBit.Messages.debugPrint ("its parameters were found");
         when No_Launch_Block =>
            CuBit.Messages.debugPrint ("it has no launch block");
         when No_Parameters =>
            CuBit.Messages.debugPrint ("its launch block carries no parameters, and every parameter is required");
         when Malformed_Section =>
            CuBit.Messages.debugPrint ("its parameters section is malformed");
         when Other_Signature =>
            CuBit.Messages.debugPrint ("its parameters were checked against another signature");
         when Undecodable_Value =>
            CuBit.Messages.debugPrint ("its parameter value is not the canonical source of its parameter record");
      end case;
      CuBit.Messages.debugPrint ("; stopping");
      CuBit.Messages.debugPrint ((1 => ASCII.LF));
      loop
         Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_EXIT, Parameters_Refused);
      end loop;
   end Refuse;

end CuBit.Launch_Parameters;
