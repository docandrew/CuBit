pragma Ada_2022;
with Interfaces;
package BCD_Decimal_Lemmas with SPARK_Mode, Pure is
   Decimal_Radix : constant := 10;
   Decimal_Digits : constant := 16;
   type Decimal_Number is range 0 .. Decimal_Radix ** Decimal_Digits;
   subtype Decimal_Factor is Decimal_Number range 1 .. Decimal_Number'Last / Decimal_Radix;
   procedure Divide_Step (Value : Decimal_Number; Factor : Decimal_Factor) with
     Ghost,
     Post => (Value / Factor) / Decimal_Radix = Value / (Factor * Decimal_Radix);
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   subtype Word_Factor is Word range 1 .. Word (Decimal_Factor'Last);
   procedure Divide_Word_Step (Value : Word; Factor : Word_Factor) with
     Ghost, Pre => Value <= Word (Decimal_Number'Last),
     Post => (Value / Factor) / Decimal_Radix = Value / (Factor * Decimal_Radix);
   subtype Decimal_Digit is Word range 0 .. Decimal_Radix - 1;
   procedure Append_Bounded (Number : Word; Digit : Decimal_Digit; Factor : Word_Factor) with
     Ghost, Pre => Number < Factor,
     Post => Number * Decimal_Radix + Digit < Factor * Decimal_Radix
       and then (Number * Decimal_Radix + Digit) mod Decimal_Radix = Digit
       and then (Number * Decimal_Radix + Digit) / Decimal_Radix = Number;
   procedure Divide_Radix_First(Value:Word;Factor:Word_Factor) with Ghost,
     Pre=>Value<=Word(Decimal_Number'Last),
     Post=>(Value/Decimal_Radix)/Factor=Value/(Decimal_Radix*Factor);
   procedure Append_Preserves_Quotient(Number:Word;Digit:Decimal_Digit;Factor:Word_Factor) with Ghost,
     Pre=>Number<Word(Decimal_Factor'Last),
     Post=>(Number*Decimal_Radix+Digit)/(Factor*Decimal_Radix)=Number/Factor;
end BCD_Decimal_Lemmas;
