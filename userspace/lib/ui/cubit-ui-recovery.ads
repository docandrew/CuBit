------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Recovery copies (PWR-003): where an app keeps its unsaved work when the
--  system stops, and how it names them. CuBit.UI.App.Run's Save_Recovery
--  hook writes them; the app offers them back at its next launch.
--
--  @description
--  Place: Root & the app's manifest identity & "/", for Text
--  "@system/Recovery/com.cubit.text/". Root is on the @system place alias
--  (the system volume: the installed root, the live medium's RAM root or
--  the development disk), never a device selector: an app does not name
--  a device (docs/locators.md, "Places and device selectors"). The app's
--  manifest grants itself that place (read, write, create); the disk
--  build makes the directory.
--  Format: one file per unsaved document, "recovery-<N>.txt", N from 1 in
--  the order the app keeps its documents, holding the document's bytes
--  exactly as the app would save them (UTF-8 text for Text). A copy never
--  overwrites the user's own file: it is a separate file in the app's own
--  place. Written within Budget_Milliseconds; the app's stop grace (its
--  plan entry or Start_Child spec) must be at least that.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.UI.Recovery with SPARK_Mode is

   Root : constant String := "@system/Recovery/";

   --  The most copies an app keeps (a named ceiling: later documents are
   --  left out and the app says so).
   Maximum_Copies : constant := 32;
   subtype Copy_Number is Positive range 1 .. Maximum_Copies;

   Copy_Prefix : constant String := "recovery-";
   Copy_Suffix : constant String := ".txt";

   --  The longest identity a place is made from.
   Maximum_Identity_Bytes : constant := 64;

   --  How long an app spends on its copies at most.
   Budget_Milliseconds : constant := 1_500;

   function Number_Image (N : Copy_Number) return String is
     (if N < 10 then [Character'Val (Character'Pos ('0') + N)]
      else [Character'Val (Character'Pos ('0') + N / 10), Character'Val (Character'Pos ('0') + N mod 10)]);

   function Place (Identity : String) return String is (Root & Identity & "/")
   with Pre => Identity'Length in 1 .. Maximum_Identity_Bytes;

   function Copy_Name (Identity : String; N : Copy_Number) return String is
     (Place (Identity) & Copy_Prefix & Number_Image (N) & Copy_Suffix)
   with Pre => Identity'Length in 1 .. Maximum_Identity_Bytes;
end CuBit.UI.Recovery;
