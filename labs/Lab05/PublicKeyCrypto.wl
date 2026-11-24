(* ::Package:: *)
(* Laboratory Work No. 5 - Public-Key Cryptography
   Daniel Moisei, FAF-241

   Run in Wolfram Cloud (https://www.wolframcloud.com, free account -> New Notebook)
   or in Wolfram Mathematica. Paste each CELL below into its own notebook cell,
   press Shift+Enter, and take the screenshot named in the CELL header
   (input + output visible). *)


(* ===================== CELL 1  ->  fig01_message_encoding.png ===================== *)
(* Message m = "Nume Prenume": ASCII codes -> 2 hex digits per character -> one decimal integer *)
plainText  = "Moisei Daniel";
asciiCodes = ToCharacterCode[plainText, "ASCII"];                  (* character -> ASCII code *)
hexBytes   = ToUpperCase[IntegerString[#, 16, 2]] & /@ asciiCodes;  (* code -> two hex digits  *)
messageHex = StringJoin[hexBytes];                                  (* bytes in original order *)
m          = FromDigits[messageHex, 16];                            (* hex string -> integer m *)

(* inverse conversion, used after every decryption: integer -> base-256 digits -> text *)
decode[x_Integer] := FromCharacterCode[IntegerDigits[x, 256], "ASCII"];
(* prints a 600-digit number as "first digits...last digits (bits)" so it fits on screen *)
show[x_Integer] := With[{s = IntegerString[x]},
  StringTake[s, Min[15, StringLength[s]]] <> "..." <> StringTake[s, -Min[15, StringLength[s]]] <>
  "  (" <> ToString[IntegerLength[x, 2]] <> " bits)"];

{asciiCodes, hexBytes, messageHex, m, decode[m] == plainText}


(* ===================== CELL 2  ->  fig02_rsa_keygen.png ===================== *)
(* Task 2.1 RSA, key generation: two 1024-bit primes give a 2048-bit modulus n *)
rsaE = 65537;                                         (* public exponent (Fermat prime F4) *)
While[True,
  rsaP   = RandomPrime[{2^1023, 2^1024 - 1}];         (* secret prime p *)
  rsaQ   = RandomPrime[{2^1023, 2^1024 - 1}];         (* secret prime q *)
  rsaN   = rsaP*rsaQ;                                 (* modulus n = p q *)
  rsaPhi = (rsaP - 1)*(rsaQ - 1);                     (* Euler's totient phi(n) *)
  (* repeat until p != q, n has exactly 2048 bits and e is invertible mod phi(n) *)
  If[rsaP != rsaQ && IntegerLength[rsaN, 2] == 2048 && GCD[rsaE, rsaPhi] == 1, Break[]]
];
rsaD = PowerMod[rsaE, -1, rsaPhi];                    (* private exponent d = e^-1 mod phi(n) *)

{"p" -> show[rsaP], "q" -> show[rsaQ], "n" -> show[rsaN], "d" -> show[rsaD],
 "gcd(e,phi)" -> GCD[rsaE, rsaPhi], "e*d mod phi" -> Mod[rsaE*rsaD, rsaPhi]} // Column


(* ===================== CELL 3  ->  fig03_rsa_encrypt_decrypt.png ===================== *)
(* Task 2.1 RSA, encryption with the public key (n, e) and decryption with the private key (n, d) *)
rsaCipher    = PowerMod[m, rsaE, rsaN];               (* c  = m^e mod n *)
rsaRecovered = PowerMod[rsaCipher, rsaD, rsaN];       (* m' = c^d mod n *)

{"0 <= m < n" -> (0 <= m < rsaN), "c" -> show[rsaCipher], "m'" -> rsaRecovered,
 "m' == m" -> (rsaRecovered == m), "text" -> decode[rsaRecovered]} // Column


(* ===================== CELL 4  ->  fig04_group_parameters.png ===================== *)
(* Parameters given in the assignment for Tasks 2.2 and 3: 2048-bit prime p and generator g = 2 *)
p = 32317006071311007300153513477825163362488057133489075174588434139269806834136210002792056362640164685458556357935330816928829023080573472625273554742461245741026202527916572972862706300325263428213145766931414223654220941111348629991657478268034230553086349050635557712219187890332729569696129743856241741236237225197346402691855797767976823014625397933058015226858730761197532436467475855460715043896844940366130497697812854295958659597567051283852132784468522925504568272879113720098931873959143374175837826000278034973198552060607533234122603254684088120031105907484281003994966956119696956248629032338072839127039;
g = 2;
q = (p - 1)/2;                                        (* p = 2q + 1 is a safe prime *)

(* checks: p has 2048 bits, p and q are prime, g has order q (so keys are taken from 1..q-1) *)
{"bits(p)" -> IntegerLength[p, 2], "PrimeQ[p]" -> PrimeQ[p], "PrimeQ[q]" -> PrimeQ[q],
 "g^q mod p" -> PowerMod[g, q, p]} // Column


(* ===================== CELL 5  ->  fig05_elgamal_keygen.png ===================== *)
(* Task 2.2 ElGamal, key generation *)
elgX = RandomInteger[{2, q - 1}];                     (* private key x, random *)
elgY = PowerMod[g, elgX, p];                          (* public key y = g^x mod p *)

{"public key" -> {"p" -> show[p], "g" -> g, "y" -> show[elgY]}, "private key x" -> show[elgX]} // Column


(* ===================== CELL 6  ->  fig06_elgamal_encrypt_decrypt.png ===================== *)
(* Task 2.2 ElGamal, encryption with (p, g, y) and decryption with x *)
elgK  = RandomInteger[{2, q - 1}];                    (* fresh random ephemeral key k *)
elgC1 = PowerMod[g, elgK, p];                         (* c1 = g^k mod p *)
elgS  = PowerMod[elgY, elgK, p];                      (* shared mask s = y^k = g^(xk) mod p *)
elgC2 = Mod[m*elgS, p];                               (* c2 = m s mod p ; ciphertext = (c1, c2) *)

elgS2        = PowerMod[elgC1, elgX, p];              (* receiver recomputes s = c1^x mod p *)
elgRecovered = Mod[elgC2*PowerMod[elgS2, -1, p], p];  (* m' = c2 s^-1 mod p *)

{"0 < m < p" -> (0 < m < p), "c1" -> show[elgC1], "c2" -> show[elgC2],
 "mask equal" -> (elgS == elgS2), "m'" -> elgRecovered, "m' == m" -> (elgRecovered == m),
 "text" -> decode[elgRecovered]} // Column


(* ===================== CELL 7  ->  fig07_dh_exchange.png ===================== *)
(* Task 3 Diffie-Hellman between Alisa and Bob (same p and g) *)
aliceSecret = RandomInteger[{2, q - 1}];              (* a, known only to Alisa *)
bobSecret   = RandomInteger[{2, q - 1}];              (* b, known only to Bob *)
alicePublic = PowerMod[g, aliceSecret, p];            (* A = g^a mod p, sent to Bob *)
bobPublic   = PowerMod[g, bobSecret, p];              (* B = g^b mod p, sent to Alisa *)

aliceShared = PowerMod[bobPublic, aliceSecret, p];    (* Alisa: K = B^a mod p *)
bobShared   = PowerMod[alicePublic, bobSecret, p];    (* Bob:   K = A^b mod p *)

{"a" -> show[aliceSecret], "b" -> show[bobSecret], "A" -> show[alicePublic],
 "B" -> show[bobPublic], "K (Alisa)" -> show[aliceShared], "K (Bob)" -> show[bobShared],
 "K equal" -> (aliceShared == bobShared)} // Column


(* ===================== CELL 8  ->  fig08_dh_aes_key.png ===================== *)
(* Task 3: the 2048-bit shared secret is turned into a 256-bit AES key with SHA-256 *)
toBytes[x_Integer] := ByteArray[IntegerDigits[x, 256, 256]];       (* fixed 256-byte big-endian form *)
aliceKey = Hash[toBytes[aliceShared], "SHA256", "ByteArray"];      (* 32 bytes = 256 bits *)
bobKey   = Hash[toBytes[bobShared],   "SHA256", "ByteArray"];
aesKeyHex = Hash[toBytes[aliceShared], "SHA256", "HexString"];

{"AES key (hex)" -> aesKeyHex, "key bits" -> 8*Length[aliceKey],
 "Alisa key == Bob key" -> (aliceKey == bobKey)} // Column


(* ===================== CELL 9  ->  fig09_aes_demo.png ===================== *)
(* Task 3 (demonstration): Alisa encrypts with AES-256 using her key, Bob decrypts with his key *)
aliceAES = SymmetricKey[<|"Cipher" -> "AES256", "Key" -> aliceKey|>];
bobAES   = SymmetricKey[<|"Cipher" -> "AES256", "Key" -> bobKey|>];
aesCipher = Encrypt[aliceAES, plainText];

{"ciphertext" -> aesCipher, "Bob decrypts" -> Decrypt[bobAES, aesCipher]} // Column


(* ===================== CELL 10  ->  no screenshot, copy the output ===================== *)
(* Full values for the report appendix. Copy the whole output text into
   reports/LAB5/wolfram_output.txt (or open the link printed by CloudExport and save it). *)
line[label_, v_] := label <> " = " <> If[IntegerQ[v], IntegerString[v], ToString[v]];
dump = StringRiffle[{
    line["Message text", plainText], line["Message hex", messageHex], line["Message m", m],
    line["RSA p", rsaP], line["RSA q", rsaQ], line["RSA n", rsaN], line["RSA phi(n)", rsaPhi],
    line["RSA e", rsaE], line["RSA d", rsaD], line["RSA ciphertext c", rsaCipher],
    line["RSA recovered m'", rsaRecovered], line["RSA recovered text", decode[rsaRecovered]],
    line["Group prime p", p], line["Generator g", g], line["Subgroup order q", q],
    line["ElGamal private x", elgX], line["ElGamal public y", elgY], line["ElGamal k", elgK],
    line["ElGamal c1", elgC1], line["ElGamal c2", elgC2], line["ElGamal mask s", elgS],
    line["ElGamal recovered m'", elgRecovered], line["ElGamal recovered text", decode[elgRecovered]],
    line["DH Alisa secret a", aliceSecret], line["DH Bob secret b", bobSecret],
    line["DH Alisa public A", alicePublic], line["DH Bob public B", bobPublic],
    line["DH shared K (Alisa)", aliceShared], line["DH shared K (Bob)", bobShared],
    line["AES-256 key (hex)", aesKeyHex]}, "\n"];
CloudExport[dump, "Text", "lab5_values.txt"]   (* in desktop Mathematica use:  Export["lab5_values.txt", dump] *)
dump
