--  An instance of CuBit.Startup_Builder.Build with an abstract minter, for
--  gnatprove: Mint may answer anything, so Build's contract is proved for
--  every source's behaviour (the hosted tests use a scripted minter).
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Startup_Tables; use CuBit.Startup_Tables;
with CuBit.Startup_Builder; use CuBit.Startup_Builder;

package Builder_Proof with SPARK_Mode is
   type Source is record
      Calls : Natural := 0;
   end record;

   procedure Mint
     (Into : in out Source; Item : Startup_Entry; Index : Entry_Index;
      Handle : out Unsigned_64; Outcome : out Mint_Outcome)
   with Global => null;

   procedure Report
     (Into : in out Source; Item : Startup_Entry; Index : Entry_Index; Outcome : Mint_Outcome)
   with Global => null;

   procedure Build_Table is new Build (Source, Mint, Report);
end Builder_Proof;
