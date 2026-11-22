------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Failures with SPARK_Mode is

   function Failed
     (Why : Reason; Detail : String; Remedy : String := "") return Failure
   is
      Result : Failure;
      D : constant Text_Length := Natural'Min (Detail'Length, MAXIMUM_TEXT);
      R : constant Text_Length := Natural'Min (Remedy'Length, MAXIMUM_TEXT);
   begin
      Result.Why := Why;
      Result.Detail (1 .. D) :=
        Detail (Detail'First .. Detail'First + (D - 1));
      Result.Detail_Length := D;
      Result.Remedy (1 .. R) :=
        Remedy (Remedy'First .. Remedy'First + (R - 1));
      Result.Remedy_Length := R;
      return Result;
   end Failed;

   function Phrase (Why : Reason) return String is
     (case Why is
         when Unspecified      => "failed, and gave no reason",
         when Not_Granted      => "is not granted to this program",
         when Outside_Scope    =>
           "reaches outside what this program was granted",
         when Refused          => "was refused by its service",
         when Not_Found        => "found nothing there",
         when Invalid_Argument => "rejected its argument",
         when Unavailable      => "could not reach its service",
         when Exhausted        => "ran out of room",
         when Device_Error     => "hit a device error");

   function Explain (Operation : String; Item : Failure) return String is
      Name : constant String (1 .. Operation'Length) := Operation;
      Detail : constant String := Item.Detail (1 .. Item.Detail_Length);
      Remedy : constant String :=
        Item.Remedy (1 .. Item.Remedy_Length);
   begin
      return Name & " " & Phrase (Item.Why) &
        (if Detail'Length > 0 then ": " & Detail else "") &
        (if Remedy'Length = 0 then ""
         elsif Item.Why in Not_Granted | Outside_Scope
         then ". To allow it: " & Remedy
         else ". Instead: " & Remedy);
   end Explain;
end CuBit.Failures;
