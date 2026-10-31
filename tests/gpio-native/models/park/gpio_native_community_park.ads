pragma Ada_2022;
with GPIO_Community_Park;
with GPIO_Native_Communities;
with GPIO_Native_Clock;
-- One startup instance for the actual frozen community backend. No guessed pin
-- inventory and no parent IRQ binding. Main must finish this before accepting
-- dynamic sink admissions or allowing any other controller register writer.
package GPIO_Native_Community_Park is new GPIO_Community_Park
  (GPIO_Native_Communities.Ready, GPIO_Native_Communities.Admit_Pin,
   GPIO_Native_Clock.Read, GPIO_Native_Communities.Read, GPIO_Native_Communities.Write);
