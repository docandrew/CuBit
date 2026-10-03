------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Why an operation failed, for the person who has to act on it
--  (docs/ccl-errors.md). On a locked-down system the first surprise is
--  usually a refusal: a Failure says what was refused, by whom, and how to
--  allow it. Every program reports failures the same way, so every front
--  end (the console, the Observatory, apps' own dialogs) shows them alike.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Failures with Pure, SPARK_Mode is

   type Reason is
     (Unspecified,
      Not_Granted,        --  this program holds no grant for the operation
      Outside_Scope,      --  granted, but the target lies outside the grant
      Refused,            --  the service's own policy said no
      Not_Found,
      Invalid_Argument,
      Unavailable,        --  the service is not running or not reachable
      Exhausted,          --  a bounded table or budget is full
      Device_Error);

   MAXIMUM_TEXT : constant := 200;
   subtype Text_Length is Natural range 0 .. MAXIMUM_TEXT;

   --  Detail: what happened, specifically. Remedy, when there is one: how
   --  to allow it (a manifest line, a grant to ask for, a limit to keep).
   type Failure is record
      Why : Reason := Unspecified;
      Detail : String (1 .. MAXIMUM_TEXT) := [others => ' '];
      Detail_Length : Text_Length := 0;
      Remedy : String (1 .. MAXIMUM_TEXT) := [others => ' '];
      Remedy_Length : Text_Length := 0;
   end record;

   --  A failure with its texts, each cut to MAXIMUM_TEXT.
   function Failed
     (Why : Reason; Detail : String; Remedy : String := "") return Failure;

   --  The reason as a short phrase completing "<operation> ...":
   --  "is not granted to this program".
   MAXIMUM_PHRASE : constant := 64;
   function Phrase (Why : Reason) return String
     with Post => Phrase'Result'First = 1
                  and then Phrase'Result'Length <= MAXIMUM_PHRASE;

   --  The whole message: "<operation> <phrase>: <detail>. To allow it:
   --  <remedy>" for a grant question, ". Instead: <remedy>" otherwise
   --  (parts left out when empty).
   MAXIMUM_OPERATION : constant := 1_024;
   function Explain (Operation : String; Item : Failure) return String
     with Pre => Operation'Length <= MAXIMUM_OPERATION;
end CuBit.Failures;
