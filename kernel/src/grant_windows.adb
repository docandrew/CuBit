-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
pragma Ada_2022;
package body Grant_Windows with
    SPARK_Mode => On
is
    -- Two different bits do not overlap. Proved through a table of the 64
    -- powers of two: a shift by a symbolic amount is slow for the provers,
    -- a lookup in a constant table is not.
    type Power_Table is array (Bit_Index) of Unsigned_64;
    Powers : constant Power_Table :=
         [2**0, 2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7,
          2**8, 2**9, 2**10, 2**11, 2**12, 2**13, 2**14, 2**15,
          2**16, 2**17, 2**18, 2**19, 2**20, 2**21, 2**22, 2**23,
          2**24, 2**25, 2**26, 2**27, 2**28, 2**29, 2**30, 2**31,
          2**32, 2**33, 2**34, 2**35, 2**36, 2**37, 2**38, 2**39,
          2**40, 2**41, 2**42, 2**43, 2**44, 2**45, 2**46, 2**47,
          2**48, 2**49, 2**50, 2**51, 2**52, 2**53, 2**54, 2**55,
          2**56, 2**57, 2**58, 2**59, 2**60, 2**61, 2**62, 2**63]
      with Ghost;

    procedure Lemma_Bit_Is_Power (B : Bit_Index)
      with Ghost, Post => Bit (B) = Powers (B);
    procedure Lemma_Bit_Is_Power (B : Bit_Index) is null;

    procedure Lemma_Powers_Disjoint (B, C : Bit_Index)
      with Ghost, Pre => B /= C, Post => (Powers (B) and Powers (C)) = 0;
    procedure Lemma_Powers_Disjoint (B, C : Bit_Index) is null;

    procedure Lemma_Bits_Disjoint (B, C : Bit_Index)
      with Ghost,
           Pre  => B /= C,
           Post => (Bit (B) and Bit (C)) = 0;
    procedure Lemma_Bits_Disjoint (B, C : Bit_Index) is
    begin
        Lemma_Bit_Is_Power (B);
        Lemma_Bit_Is_Power (C);
        Lemma_Powers_Disjoint (B, C);
    end Lemma_Bits_Disjoint;

    -- Setting bit B of X leaves every other bit as it was.
    procedure Lemma_Or_Others (X : Unsigned_64; B : Bit_Index)
      with Ghost,
           Post => (for all C in Bit_Index =>
                      (if C /= B then ((X or Bit (B)) and Bit (C)) = (X and Bit (C))));
    procedure Lemma_Or_Others (X : Unsigned_64; B : Bit_Index) is
    begin
        for C in Bit_Index loop
            if C /= B then
                Lemma_Bits_Disjoint (B, C);
                pragma Assert (((X or Bit (B)) and Bit (C)) =
                               ((X and Bit (C)) or (Bit (B) and Bit (C))));
            end if;
            pragma Loop_Invariant
              (for all D in Bit_Index'First .. C =>
                 (if D /= B then ((X or Bit (B)) and Bit (D)) = (X and Bit (D))));
        end loop;
    end Lemma_Or_Others;

    -- Clearing bit B of X leaves every other bit as it was.
    procedure Lemma_And_Not_Others (X : Unsigned_64; B : Bit_Index)
      with Ghost,
           Post => (for all C in Bit_Index =>
                      (if C /= B then ((X and not Bit (B)) and Bit (C)) = (X and Bit (C))));
    procedure Lemma_And_Not_Others (X : Unsigned_64; B : Bit_Index) is
    begin
        for C in Bit_Index loop
            if C /= B then
                Lemma_Bits_Disjoint (B, C);
                pragma Assert (((not Bit (B)) and Bit (C)) = Bit (C));
                pragma Assert (((X and not Bit (B)) and Bit (C)) =
                               (X and ((not Bit (B)) and Bit (C))));
            end if;
            pragma Loop_Invariant
              (for all D in Bit_Index'First .. C =>
                 (if D /= B then ((X and not Bit (B)) and Bit (D)) = (X and Bit (D))));
        end loop;
    end Lemma_And_Not_Others;

    -- A window other than W in W's word is a different bit of that word.
    procedure Lemma_Same_Word (W : Window)
      with Ghost,
           Post => (for all V in Window =>
                      (if V /= W and then V / Word_Bits = W / Word_Bits then
                         V mod Word_Bits /= W mod Word_Bits));
    procedure Lemma_Same_Word (W : Window) is null;

    -- After S.Words (W / Word_Bits) changed only in bit W mod Word_Bits,
    -- every other window reads as before. No loop: tests run ghost code.
    procedure Lemma_Others_Unchanged (Old, S : Window_Set; W : Window)
      with Ghost,
           Pre  => (for all I in Word_Index =>
                      (if I /= W / Word_Bits then S.Words (I) = Old.Words (I)))
                   and then
                   (for all C in Bit_Index =>
                      (if C /= W mod Word_Bits then
                         (S.Words (W / Word_Bits) and Bit (C)) =
                         (Old.Words (W / Word_Bits) and Bit (C)))),
           Post => (for all V in Window =>
                      (if V /= W then Contains (S, V) = Contains (Old, V)));

    procedure Lemma_Others_Unchanged (Old, S : Window_Set; W : Window) is
    begin
        Lemma_Same_Word (W);
    end Lemma_Others_Unchanged;

    function Is_Empty (S : Window_Set) return Boolean is
    begin
        for I in Word_Index loop
            if S.Words (I) /= 0 then
                return False;
            end if;
            pragma Loop_Invariant (for all J in Word_Index'First .. I => S.Words (J) = 0);
        end loop;
        return True;
    end Is_Empty;

    procedure Allocate (S : in out Window_Set; W : out Window; Found : out Boolean) is
        Full : constant Unsigned_64 := Unsigned_64'Last;
    begin
        W := Window'First;
        Found := False;
        for I in Word_Index loop
            if S.Words (I) /= Full then
                for B in Bit_Index loop
                    if (S.Words (I) and Bit (B)) = 0 then
                        W := I * Word_Bits + B;
                        pragma Assert (W / Word_Bits = I and then W mod Word_Bits = B);
                        declare
                            Old : constant Window_Set := S with Ghost;
                        begin
                            Lemma_Or_Others (S.Words (I), B);
                            S.Words (I) := S.Words (I) or Bit (B);
                            Lemma_Others_Unchanged (Old, S, W);
                        end;
                        Found := True;
                        return;
                    end if;
                end loop;
            end if;
        end loop;
    end Allocate;

    procedure Release (S : in out Window_Set; W : Window) is
        Old : constant Window_Set := S with Ghost;
    begin
        Lemma_And_Not_Others (S.Words (W / Word_Bits), W mod Word_Bits);
        S.Words (W / Word_Bits) := S.Words (W / Word_Bits) and not Bit (W mod Word_Bits);
        Lemma_Others_Unchanged (Old, S, W);
    end Release;
end Grant_Windows;
