with Interfaces;

--  Arithmetic only. The caller authenticates the account and supplies an
--  exact, one-shot retirement receipt before permitting a release.
package Intel_GPU_Client_Budget_Policy with SPARK_Mode, Pure is
   use Interfaces;
   procedure Reserve
     (Charged : in out Unsigned_64; Limit, Bytes : Unsigned_64;
      Accepted : out Boolean)
   with Global => null,
     Post =>
       (Accepted = (Bytes /= 0 and then Bytes mod 4096 = 0 and then
          Charged'Old <= Limit and then Bytes <= Limit - Charged'Old)) and then
       (if Accepted then
          Charged = Charged'Old + Bytes and then Charged <= Limit and then
          Charged >= Charged'Old
        else Charged = Charged'Old);

   procedure Release
     (Charged : in out Unsigned_64; Bytes : Unsigned_64;
      Accepted : out Boolean)
   with Global => null,
     Post =>
       (Accepted = (Bytes /= 0 and then Bytes mod 4096 = 0 and then
          Bytes <= Charged'Old)) and then
       (if Accepted then
          Charged = Charged'Old - Bytes and then Charged <= Charged'Old
        else Charged = Charged'Old);
end Intel_GPU_Client_Budget_Policy;
