-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- A builder's scope (SIGN-001, docs/signed-binaries.md section 2): the
-- identities it may sign, as reverse-DNS prefixes matched on label
-- boundaries ("com.example" covers com.example and com.example.app, not
-- com.examplefoo; "*" covers every identity), and its authority ceiling.
-- The ceiling can only refuse; it never grants.
-------------------------------------------------------------------------------
package CuBit.Signed_Admission.Scopes with SPARK_Mode => On is

   function Text (T : Identity_Text) return String is (T.Data (1 .. T.Length));

   Label_Separator : constant Character := '.';

   --  The reference definition the matcher is proved against.
   function Covers_Spec (Prefix, Identity : String) return Boolean is
     ((Prefix'Length = 1 and then Prefix (Prefix'First) = Every_Identity)
      or else (Prefix'Length > 0
               and then Identity'Length >= Prefix'Length
               and then Identity (Identity'First .. Identity'First + (Prefix'Length - 1)) = Prefix
               and then (Identity'Length = Prefix'Length
                         or else Identity (Identity'First + Prefix'Length) = Label_Separator)))
   with Ghost,
        Pre => Prefix'Length <= Max_Identity_Length and then Identity'Length <= Max_Identity_Length
               and then Prefix'First = 1 and then Identity'First = 1;

   function Covers (Prefix, Identity : Identity_Text) return Boolean
   with Post => Covers'Result = Covers_Spec (Text (Prefix), Text (Identity));

   --  Some prefix in Prefixes (First .. Last) covers Identity.
   function In_Scope
     (Prefixes : Prefix_Array; First : Prefix_Index; Last : Natural; Identity : Identity_Text)
      return Boolean
   with Post => In_Scope'Result =
                  (for some I in Prefixes'Range =>
                     I >= First and then I <= Last and then Covers (Prefixes (I), Identity));

   function Ceiling_Allows (Ceiling : Authority_Ceiling; Requested : Requested_Authority) return Boolean is
     (case Requested is
        when Application_Authority        => True,
        when Service_Authority            => Ceiling in Services | Hardware_And_System,
        when Hardware_Or_System_Authority => Ceiling = Hardware_And_System);

end CuBit.Signed_Admission.Scopes;
