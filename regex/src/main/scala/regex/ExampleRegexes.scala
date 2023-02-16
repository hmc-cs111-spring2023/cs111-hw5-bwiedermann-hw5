package regex

// bitstring = (0|1)*
val bitstring: RegularLanguage = Star(Union(Character('0'), Character('1')))

// digit = (0|1|2|3|4|5|6|7|8|9)
val digit: RegularLanguage =
  "0123456789".map(Character(_)).reduceLeft(Union(_, _))

// digits = (0|1|2|3|4|5|6|7|8|9)*
val digits: RegularLanguage = Star(digit)

// re1 = ab*
val re1: RegularLanguage = Concat(Character('a'), Star(Character('b')))

// re2 = (ab)*
val re2: RegularLanguage = Star(Concat(Character('a'), Character('b')))

// re3 = a(ab)*
val re3: RegularLanguage = Concat(Character('a'), re2)
